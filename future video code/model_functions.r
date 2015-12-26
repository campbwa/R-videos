print_percent_1 = function(value){pretty_print(value*100,trailing.sym="%",digits=1)}
print_percent_2 = function(value){pretty_print(value*100,trailing.sym="%",digits=2)}



plsmo_hist = function(x_var, y_var){
  par(mfrow = c(1,2))
  plsmo(x = x_var, y = y_var)
  hist(x_var)
  par(mfrow = c(1,1))
}


######################## GBM PDPs ########################
#add an argument for only the most important vars
#argument for pdf output and file name
PDPPlot <- function(model,data,i.var="all",ylab = "Y"){
  localenv = environment() #this is key for getting ggplot to run in the loop, otherwise it outputs the same plot each time!
  if(i.var == "all"){
    for(i in 1:length(model$var.names)){ #model$x
      #### PDP ####
      Points = plot(model, i.var = i, main = "", return.grid = TRUE)
      d = print_percent_2(max(Points$y) - min(Points$y))
      PDP = ggplot(Points, aes(x = Points[,1], y = Points[,2]), environment = localenv) + 
        geom_line(size = 1.5,col = "#0066CC") + 
        geom_text(label = paste("Delta =", d), x = min(Points[,1]) + sd(Points[,1]),
                  y = max(Points[,2]), colour = "black", size = 1, hjust = 0, vjust = 0) +
        xlab(names(Points)[1]) + ylab(ylab) +
        scale_y_continuous(labels = percent)
      
      ## Histogram ##
      Histogram = ggplot(data,aes(x = data[,model$xNames[i]]), environment=localenv) + 
        geom_bar(fill="#0066CC",width=1) +
        xlab(model$xNames[i])
      
      #plot both in one window
      grid.arrange(PDP,Histogram, ncol = 2)
      
      
    }
  } else { #if you only want to plot for one variable
    i = i.var
    
    #### PDP ####
    Points = plot(model, i.var = i, main = "", return.grid = TRUE, type = "response")
    d = print_percent_2(max(Points$y) - min(Points$y))
    
    ## different plots for numeric vs factors ##n_distinct(Points$y)
    if(nrow(Points) < 20){
      PDP = ggplot(Points, aes(x = Points[,1], y = Points[,2]), environment = localenv) + 
        geom_bar(stat = "identity", fill = "#0066CC") +
        geom_text(label = paste("Delta =", d), x = as.numeric(Points[1,1]) + 0.1
                  , y = max(Points[,2]), colour = "black", size = 5, hjust = 0, vjust = 0) +
        xlab(model$var.names[i]) +
        # xlab(names(Points)[1]) + 
        ylab(ylab) +
        scale_y_continuous(labels = percent)
    } else {
      PDP = ggplot(Points, aes(x = Points[,1], y = Points[,2]), environment = localenv) + 
        geom_line(size = 1.5, col = "#0066CC") +
        geom_text(label = paste("Delta =", d), x = min(Points[,1]) 
                  + sd(Points[,1]), y = max(Points[,2]), colour = "black", size = 5, hjust = 0, vjust = 0) +
        xlab(names(Points)[1]) + 
        ylab(ylab) +
        scale_y_continuous(labels = percent)
    }
    ## Histogram ## model$xNames vs model$var.names
    Histogram = ggplot(data, aes(x = data[,model$var.names[i]]), environment = localenv) + 
      geom_bar(stat = "bin",fill = "#0066CC") + #,width=1
      xlab(model$var.names[i])#xlab(model$xNames[i])
    
    #plot both in one window, no pdf now.
    #     filename = paste("PDP", model$xNames[i], ".pdf", sep = "")
    #     pdf(filename)
    #     grid.arrange(PDP, Histogram, ncol = 2)
    #     dev.off()
    grid.arrange(PDP,Histogram, ncol = 2)
    
  }
}
##########################################################


cap_floor = function(var, floor, cap){
  var = ifelse(var > cap, cap, var)
  if (!missing(floor))
    var = ifelse(var < floor, floor, var)
  return(var)
}


calc_gini = function(new_data, gbm_obj){
  auc(new_data[,target_var], predict(gbm_obj, newdata = new_data, n.trees = gbm.perf(gbm_obj, plot.it = FALSE))) * 2 - 1
}

calc_gini_glm = function(new_data, glm_obj){
  auc(new_data[,target_var], predict(glm_obj, newdata = new_data)) * 2 - 1
}



all_missing = function(var){
  return(ifelse(all(is.na(var)), TRUE, FALSE))
}


num_missing = function(var){
  num = sum(ifelse(is.na(var), 1, 0))
} 


# Used to find deciles
cutter = function(x,breaks=20){ cut(x,quantile(x,seq(0,1,by=1/breaks),include.lowest=T))}

# Used to calculate piecewise linear splines
pw<-function(x,a,b){
  (pmin(pmax(x,a),b)-a)
}



function (x, i.var = 1, n.trees = x$n.trees, continuous.resolution = 100, 
          return.grid = FALSE, type = "link", ...) 
{
  if (!is.element(type, c("link", "response"))) {
    stop("type must be either 'link' or 'response'")
  }
  if (all(is.character(i.var))) {
    i <- match(i.var, x$var.names)
    if (any(is.na(i))) {
      stop("Plot variables not used in gbm model fit: ", 
           i.var[is.na(i)])
    }
    else {
      i.var <- i
    }
  }
  if ((min(i.var) < 1) || (max(i.var) > length(x$var.names))) {
    warning("i.var must be between 1 and ", length(x$var.names))
  }
  if (n.trees > x$n.trees) {
    warning(paste("n.trees exceeds the number of trees in the model, ", 
                  x$n.trees, ". Plotting using ", x$n.trees, " trees.", 
                  sep = ""))
    n.trees <- x$n.trees
  }
  if (length(i.var) > 3) {
    warning("gbm.int.plot creates up to 3-way interaction plots.\nplot.gbm will only return the plotting data structure.")
    return.grid = TRUE
  }
  grid.levels <- vector("list", length(i.var))
  for (i in 1:length(i.var)) {
    if (is.numeric(x$var.levels[[i.var[i]]])) {
      grid.levels[[i]] <- seq(min(x$var.levels[[i.var[i]]]), 
                              max(x$var.levels[[i.var[i]]]), length = continuous.resolution)
    }
    else {
      grid.levels[[i]] <- as.numeric(factor(x$var.levels[[i.var[i]]], 
                                            levels = x$var.levels[[i.var[i]]])) - 1
    }
  }
  X <- expand.grid(grid.levels)
  names(X) <- paste("X", 1:length(i.var), sep = "")
  if (is.null(x$num.classes)) {
    x$num.classes <- 1
  }
  y <- .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)), 
             cCols = as.integer(ncol(X)), n.class = as.integer(x$num.classes), 
             i.var = as.integer(i.var - 1), n.trees = as.integer(n.trees), 
             initF = as.double(x$initF), trees = x$trees, c.splits = x$c.splits, 
             var.type = as.integer(x$var.type), PACKAGE = "gbm")
  if (x$distribution$name == "multinomial") {
    X$y <- matrix(y, ncol = x$num.classes)
    colnames(X$y) <- x$classes
    if (type == "response") {
      X$y <- exp(X$y)
      X$y <- X$y/matrix(rowSums(X$y), ncol = ncol(X$y), 
                        nrow = nrow(X$y))
    }
  }
  else if (is.element(x$distribution$name, c("bernoulli", "pairwise")) && 
           type == "response") {
    X$y <- 1/(1 + exp(-y))
  }
  else if ((x$distribution$name == "poisson") && (type == "response")) {
    X$y <- exp(y)
  }
  else if (type == "response") {
    warning("type 'response' only implemented for 'bernoulli', 'poisson', 'multinomial', and 'pairwise'. Ignoring")
  }
  else {
    X$y <- y
  }
  f.factor <- rep(FALSE, length(i.var))
  for (i in 1:length(i.var)) {
    if (!is.numeric(x$var.levels[[i.var[i]]])) {
      X[, i] <- factor(x$var.levels[[i.var[i]]][X[, i] + 
                                                  1], levels = x$var.levels[[i.var[i]]])
      f.factor[i] <- TRUE
    }
  }
  if (return.grid) {
    names(X)[1:length(i.var)] <- x$var.names[i.var]
    return(X)
  }
  if (length(i.var) == 1) {
    if (!f.factor) {
      j <- order(X$X1)
      if (x$distribution$name == "multinomial") {
        if (type == "response") {
          ylabel <- "Predicted class probability"
        }
        else {
          ylabel <- paste("f(", x$var.names[i.var], ")", 
                          sep = "")
        }
        plot(range(X$X1), range(X$y), type = "n", xlab = x$var.names[i.var], 
             ylab = ylabel)
        for (ii in 1:x$num.classes) {
          lines(X$X1, X$y[, ii], xlab = x$var.names[i.var], 
                ylab = paste("f(", x$var.names[i.var], ")", 
                             sep = ""), col = ii, ...)
        }
      }
      else if (is.element(x$distribution$name, c("bernoulli", 
                                                 "pairwise"))) {
        if (type == "response") {
          ylabel <- "Predicted probability"
        }
        else {
          ylabel <- paste("f(", x$var.names[i.var], ")", 
                          sep = "")
        }
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var], 
             ylab = ylabel)
      }
      else if (x$distribution$name == "poisson") {
        if (type == "response") {
          ylabel <- "Predicted count"
        }
        else {
          ylabel <- paste("f(", x$var.names[i.var], ")", 
                          sep = "")
        }
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var], 
             ylab = ylabel)
      }
      else {
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var], 
             ylab = paste("f(", x$var.names[i.var], ")", 
                          sep = ""), ...)
      }
    }
    else {
      if (x$distribution$name == "multinomial") {
        nX <- length(X$X1)
        dim.y <- dim(X$y)
        if (type == "response") {
          ylabel <- "Predicted probability"
        }
        else {
          ylabel <- paste("f(", x$var.names[i.var], ")", 
                          sep = "")
        }
        plot(c(0, nX), range(X$y), axes = FALSE, type = "n", 
             xlab = x$var.names[i.var], ylab = ylabel)
        axis(side = 1, labels = FALSE, at = 0:nX)
        axis(side = 2)
        mtext(as.character(X$X1), side = 1, at = 1:nX - 
                0.5)
        segments(x1 = rep(1:nX - 0.75, each = dim.y[2]), 
                 y1 = as.vector(t(X$y)), x2 = rep(1:nX - 0.25, 
                                                  each = dim.y[2]), col = 1:dim.y[2])
      }
      else if (is.element(x$distribution$name, c("bernoulli", 
                                                 "pairwise")) && type == "response") {
        ylabel <- "Predicted probability"
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var], 
             ylab = ylabel)
      }
      else if (x$distribution$name == "poisson" & type == 
               "response") {
        ylabel <- "Predicted count"
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var], 
             ylab = ylabel)
      }
      else {
        plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var], 
             ylab = paste("f(", x$var.names[i.var], ")", 
                          sep = ""), ...)
      }
    }
  }
  else if (length(i.var) == 2) {
    if (!f.factor[1] && !f.factor[2]) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X$temp <- X$y[, ii]
          print(levelplot(temp ~ X1 * X2, data = X, xlab = x$var.names[i.var[1]], 
                          ylab = x$var.names[i.var[2]], ...))
          title(paste("Class:", dimnames(X$y)[[2]][ii]))
        }
        X$temp <- NULL
      }
      else {
        print(levelplot(y ~ X1 * X2, data = X, xlab = x$var.names[i.var[1]], 
                        ylab = x$var.names[i.var[2]], ...))
      }
    }
    else if (f.factor[1] && !f.factor[2]) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X$temp <- X$y[, ii]
          print(xyplot(temp ~ X2 | X1, data = X, xlab = x$var.names[i.var[2]], 
                       ylab = paste("f(", x$var.names[i.var[1]], 
                                    ",", x$var.names[i.var[2]], ")", sep = ""), 
                       type = "l", panel = panel.xyplot, ...))
          title(paste("Class:", dimnames(X$y)[[2]][ii]))
        }
        X$temp <- NULL
      }
      else {
        print(xyplot(y ~ X2 | X1, data = X, xlab = x$var.names[i.var[2]], 
                     ylab = paste("f(", x$var.names[i.var[1]], ",", 
                                  x$var.names[i.var[2]], ")", sep = ""), type = "l", 
                     panel = panel.xyplot, ...))
      }
    }
    else if (!f.factor[1] && f.factor[2]) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X$temp <- X$y[, ii]
          print(xyplot(temp ~ X1 | X2, data = X, xlab = x$var.names[i.var[1]], 
                       ylab = paste("f(", x$var.names[i.var[1]], 
                                    ",", x$var.names[i.var[2]], ")", sep = ""), 
                       type = "l", panel = panel.xyplot, ...))
          title(paste("Class:", dimnames(X$y)[[2]][ii]))
        }
        X$temp <- NULL
      }
      else {
        print(xyplot(y ~ X1 | X2, data = X, xlab = x$var.names[i.var[1]], 
                     ylab = paste("f(", x$var.names[i.var[1]], ",", 
                                  x$var.names[i.var[2]], ")", sep = ""), type = "l", 
                     panel = panel.xyplot, ...))
      }
    }
    else {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X$temp <- X$y[, ii]
          print(stripplot(X1 ~ temp | X2, data = X, xlab = x$var.names[i.var[2]], 
                          ylab = paste("f(", x$var.names[i.var[1]], 
                                       ",", x$var.names[i.var[2]], ")", sep = ""), 
                          ...))
          title(paste("Class:", dimnames(X$y)[[2]][ii]))
        }
        X$temp <- NULL
      }
      else {
        print(stripplot(X1 ~ y | X2, data = X, xlab = x$var.names[i.var[2]], 
                        ylab = paste("f(", x$var.names[i.var[1]], ",", 
                                     x$var.names[i.var[2]], ")", sep = ""), ...))
      }
    }
  }
  else if (length(i.var) == 3) {
    i <- order(f.factor)
    X.new <- X[, i]
    X.new$y <- X$y
    names(X.new) <- names(X)
    if (sum(f.factor) == 0) {
      X.new$X3 <- equal.count(X.new$X3)
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X.new$temp <- X.new$y[, ii]
          print(levelplot(temp ~ X1 * X2 | X3, data = X.new, 
                          xlab = x$var.names[i.var[i[1]]], ylab = x$var.names[i.var[i[2]]], 
                          ...))
          title(paste("Class:", dimnames(X.new$y)[[2]][ii]))
        }
        X.new$temp <- NULL
      }
      else {
        print(levelplot(y ~ X1 * X2 | X3, data = X.new, 
                        xlab = x$var.names[i.var[i[1]]], ylab = x$var.names[i.var[i[2]]], 
                        ...))
      }
    }
    else if (sum(f.factor) == 1) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X.new$temp <- X.new$y[, ii]
          print(levelplot(temp ~ X1 * X2 | X3, data = X.new, 
                          xlab = x$var.names[i.var[i[1]]], ylab = x$var.names[i.var[i[2]]], 
                          ...))
          title(paste("Class:", dimnames(X.new$y)[[2]][ii]))
        }
        X.new$temp <- NULL
      }
      else {
        print(levelplot(y ~ X1 * X2 | X3, data = X.new, 
                        xlab = x$var.names[i.var[i[1]]], ylab = x$var.names[i.var[i[2]]], 
                        ...))
      }
    }
    else if (sum(f.factor) == 2) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X.new$temp <- X.new$y[, ii]
          print(xyplot(temp ~ X1 | X2 * X3, data = X.new, 
                       type = "l", xlab = x$var.names[i.var[i[1]]], 
                       ylab = paste("f(", paste(x$var.names[i.var[1:3]], 
                                                collapse = ","), ")", sep = ""), panel = panel.xyplot, 
                       ...))
          title(paste("Class:", dimnames(X.new$y)[[2]][ii]))
        }
        X.new$temp <- NULL
      }
      else {
        print(xyplot(y ~ X1 | X2 * X3, data = X.new, 
                     type = "l", xlab = x$var.names[i.var[i[1]]], 
                     ylab = paste("f(", paste(x$var.names[i.var[1:3]], 
                                              collapse = ","), ")", sep = ""), panel = panel.xyplot, 
                     ...))
      }
    }
    else if (sum(f.factor) == 3) {
      if (x$distribution$name == "multinomial") {
        for (ii in 1:x$num.classes) {
          X.new$temp <- X.new$y[, ii]
          print(stripplot(X1 ~ temp | X2 * X3, data = X.new, 
                          xlab = x$var.names[i.var[i[1]]], ylab = paste("f(", 
                                                                        paste(x$var.names[i.var[1:3]], collapse = ","), 
                                                                        ")", sep = ""), ...))
          title(paste("Class:", dimnames(X.new$y)[[2]][ii]))
        }
        X.new$temp <- NULL
      }
      else {
        print(stripplot(X1 ~ y | X2 * X3, data = X.new, 
                        xlab = x$var.names[i.var[i[1]]], ylab = paste("f(", 
                                                                      paste(x$var.names[i.var[1:3]], collapse = ","), 
                                                                      ")", sep = ""), ...))
      }
    }
  }
}
























three_step_gbm = function(target_var = "dlq6p_at_18", ntrees = 200, use_offset = FALSE, depth = 2){
  ## Create a model formula to be passed to the GBM
  ## need to identify factor variables
  
  if(use_offset == TRUE){
    model_formula = paste(target_var, "~ offset(score_v005) + ", paste(var_names, collapse = " + ")) %>% 
      as.formula()
  } else {
    model_formula = paste(target_var, "~", paste(var_names, collapse = " + ")) %>% 
      as.formula()
  }
  
  
  ft_gbm = gbm(model_formula
               , data = ft_train
               , distribution = "bernoulli"
               , n.trees = ntrees
               , interaction.depth = depth
               , n.minobsinnode = 5
               , shrinkage = 0.01
               , bag.fraction = 0.8
               , train.fraction = 0.8
               , keep.data = TRUE
               , verbose = TRUE
  )
  
  
  summary(ft_gbm)
  
  
  
  
  ## identify variables with low influence:
  important_vars = summary(ft_gbm, plotit = FALSE) %>% 
    filter(rel.inf > 0.1) %>% 
    select(var) 
  
  ## Keep only the important variables
  ft_train = ft_train %>% select(one_of(target_var, levels(important_vars$var)[important_vars$var]))
  ## then rerun the gbm
  
  var_names = names(ft_train) 
  var_names = var_names[var_names != target_var]
  
  if(use_offset == TRUE){
    model_formula = paste(target_var, "~ offset(score_v005) + ", paste(var_names, collapse = " + ")) %>% 
      as.formula()
  } else {
    model_formula = paste(target_var, "~", paste(var_names, collapse = " + ")) %>% 
      as.formula()
  }
  
  
  
  ft_gbm = gbm(model_formula
               , data = ft_train
               , distribution = "bernoulli"
               , n.trees = ntrees
               , interaction.depth = depth
               , n.minobsinnode = 5
               , shrinkage = 0.01
               , bag.fraction = 0.8
               , train.fraction = 0.8
               , keep.data = TRUE
               , verbose = TRUE
  )
  
  
  ## identify variables with low influence:
  important_vars = summary(ft_gbm, plotit = FALSE) %>% 
    filter(rel.inf > 1) %>% 
    select(var) 
  
  ## Keep only the important variables
  ft_train = ft_train %>% select(one_of(target_var, levels(important_vars$var)[important_vars$var]))
  ## then rerun the gbm
  
  var_names = names(ft_train) 
  var_names = var_names[var_names != target_var]
  
  if(use_offset == TRUE){
    model_formula = paste(target_var, "~ offset(score_v005) + ", paste(var_names, collapse = " + ")) %>% 
      as.formula()
  } else {
    model_formula = paste(target_var, "~", paste(var_names, collapse = " + ")) %>% 
      as.formula()
  }
  
  
  
  ft_gbm = gbm(model_formula
               , data = ft_train
               , distribution = "bernoulli"
               , n.trees = ntrees
               , interaction.depth = depth
               , n.minobsinnode = 5
               , shrinkage = 0.01
               , bag.fraction = 0.8
               , train.fraction = 0.8
               , keep.data = TRUE
               , verbose = TRUE
  )
  
  return(ft_gbm)
}


