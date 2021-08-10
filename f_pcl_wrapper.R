#' PCL1.0 GBM wrapper for benchmarking
#' 
#' Performs PCL1.0 GBM benchmark simulation
#' 
#' This function has been designed to perform a single benchmark simulation 
#' with optional outlier replacement for the PCL1.0 GBM. 
#' 
#' Disclaimer: Use at your own risk. No responsibility is assumed for a user's
#' application of these materials or related materials. 
#' 
#' @param data A data frame or data table with PCL1.0 feature and response 
#' variables
#' @param method Character string, one of `TukeyIQR` for interquartile range
#' plus or minus 1.5 * IQR, `IDR` for interdecile range, 
#' or `P025975` for the range between the 2.5 and 97.5 percentiles.
#' 
#' @author Dennis Hallema, \email{dwhallem@@ncsu.edu}
#' 
#' @return Object of class `data frame` containing the arguments provided
#' to the model, error terms and benchmark result; log files; work space images


# Function to replace outliers
outlier_replace <- function(data, method = "TukeyIQR") { # Accepts univariate series only
  require(data.table)
  x <- data.table(data)
  names(x) <- "val"
  if(method == "TukeyIQR") {
    x <- x[, val := pmax( ( quantile(x[[1]], 0.25)[[1]] - 1.5 * IQR(x[[1]]) ) , x[[1]]) ]
    x <- x[, val := pmin( ( quantile(x[[1]], 0.75)[[1]] + 1.5 * IQR(x[[1]]) ) , x[[1]]) ]
  } else if(method == "IDR") {
    x <- x[, val := pmax( quantile(x[[1]], 0.1)[[1]] , x[[1]]) ]
    x <- x[, val := pmin( quantile(x[[1]], 0.9)[[1]] , x[[1]]) ]
  } else if(method == "P025975") {
    x <- x[, val := pmax( quantile(x[[1]], 0.025)[[1]] , x[[1]]) ]
    x <- x[, val := pmin( quantile(x[[1]], 0.975)[[1]] , x[[1]]) ]
  }
  return(list(val = x, method = method))
}

# Function to compute summary statistics for a data table
summarize_table <- function(data) {
  DT <- data.table(data)
  out <- cbind(stat = c("min","Q25","mean","median","Q75","max"),
               rbind(
    DT[, lapply(.SD, min, na.rm = TRUE), .SDcols = names(DT)],
    DT[, lapply(.SD, quantile, probs = 0.25, na.rm = TRUE), .SDcols = names(DT)],
    DT[, lapply(.SD, mean, na.rm = TRUE), .SDcols = names(DT)],
    DT[, lapply(.SD, median, na.rm = TRUE), .SDcols = names(DT)],
    DT[, lapply(.SD, quantile, probs = 0.75, na.rm = TRUE), .SDcols = names(DT)],
    DT[, lapply(.SD, max, na.rm = TRUE), .SDcols = names(DT)]
    )
  )
}

pcl_wrapper <- function(formula = formula(data), distribution = "bernoulli",
                 data = list(), weights, var.monotone = NULL, n.trees = 100,
                 interaction.depth = 1, n.minobsinnode = 10, shrinkage = 0.1,
                 bag.fraction = 0.5, train.fraction = 1, cv.folds = 0,
                 keep.data = TRUE, verbose = FALSE, class.stratify.cv = NULL,
                 n.cores = NULL,
                 test_size = 0.3, replaceoutliers = FALSE, 
                 saveimage = FALSE, out_path = "./", node = node){
  require(gbm)
  require(data.table)
  require(PRROC)
  
  z1 <- unclass(Sys.time())
  sid <- paste0(node, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
  DT <- data.table(data)
  
  # Create training and testing sets
  train_index <- sample(1:nrow(DT), (1 - test_size) * nrow(DT))
  test_index <- setdiff(1:nrow(DT), train_index)
  train <- DT[train_index,]
  test <- DT[test_index,]
  
  # Outlier processing
  if(any(replaceoutliers == c("TukeyIQR", "IDR", "P025975"))) {
    cols <- c( "flatdist","valleydist","costdist","roaddist","ridgedist","barrier", "ros01","rtc01","sdi01")
    for (j in cols) set(train, j = j, value = outlier_replace(data = train[[j]], method = replaceoutliers)$val)
  }
  
  # Fit classifier to the training set
  clf <- gbm(formula = formula, distribution = distribution, data = train,
              var.monotone = var.monotone, n.trees = n.trees, 
              interaction.depth = interaction.depth, 
              n.minobsinnode = n.minobsinnode, shrinkage =  shrinkage,
              bag.fraction =  bag.fraction, train.fraction = train.fraction, 
              cv.folds = cv.folds, keep.data = keep.data, verbose = verbose, 
              class.stratify.cv = class.stratify.cv, n.cores = n.cores)
  
  # Save workspace image
  if ((!missing(saveimage)) && (!is.null(saveimage))) {
    if(saveimage == TRUE){
      img_file <- paste0(out_path,"/", sid, "_img.Rdata")
      save.image(file = img_file, compress = "gzip")
    }
  }
  
  # Benchmark
  z2 <- unclass(Sys.time())
  elapsed_min <- (z2 - z1) * 60**(-1)
  nsamples = dim(DT)[1]
  
  # Predict labels of training set, compute probs, metrics
  train_pred <- predict(object = clf, newdata = train, n.trees = clf$n.trees, type = "response")
  y_train <- train$response
  train_pred_prob <- exp(clf$cv.fitted) / (1 + exp(clf$cv.fitted))
  mse_train <- mean((train$response - train_pred)**2)
  auroc_train <- roc.curve(scores.class0 = train_pred_prob, weights.class0 = y_train)$auc
  auprc_train <- pr.curve(scores.class0 = train_pred_prob, weights.class0 = y_train)$auc.integral
  
  # Predict labels of test set, compute probs, metrics
  y_pred <- predict(object = clf, newdata = test, n.trees = clf$n.trees, type = "response")
  y_test <- test$response
  y_pred_prob <- exp(y_pred) / (1 + exp(y_pred))
  mse_test <- mean((test$response - y_pred)**2)
  auroc_test <- roc.curve(scores.class0 = y_pred_prob, weights.class0 = y_test)$auc
  auprc_test <- pr.curve(scores.class0 = y_pred_prob, weights.class0 = y_test)$auc.integral
  
  # Create result object
  res <- data.frame(
    sid = sid,
    response.name = clf$response.name,
    distribution = as.character(clf$distribution),
    n.trees = clf$n.trees,
    interaction.depth = clf$interaction.depth,
    n.minobsinnode = clf$n.minobsinnode,
    shrinkage = clf$shrinkage,
    bag.fraction = clf$bag.fraction,
    train.fraction = clf$train.fraction,
    cv.folds = clf$cv.folds,
    keep.data = as.character(keep.data),
    class.stratify.cv = as.character(class.stratify.cv),
    n.cores = as.character(n.cores),
    clf_last_train.error = clf$train.error[length(clf$train.error)],
    clf_last_valid.error = clf$valid.error[length(clf$valid.error)],
    clf_last_cv.error = clf$cv.error[length(clf$cv.error)],
    mse_train = mse_train,
    auroc_train = auroc_train,
    auprc_train = auprc_train,
    mse_test = mse_test,
    auroc_test = auroc_test,
    auprc_test = auprc_test,
    replaceoutliers = as.character(replaceoutliers),
    nsamples = nsamples,
    saveimage = saveimage,
    elapsed_min = elapsed_min
  )
  
  # Write data summary log
  dts <- summarize_table(DT)
  dts_file <- paste0(out_path, "/", sid, "_dts",replaceoutliers, ".csv")
  if(!file.exists(dts_file)){
    write.table(x = data.frame(dts), file = dts_file, quote = F, sep = ",",
                row.names = F, col.names = names(dts))
  }
  
  # Update result log
  log_file <- paste0(out_path, "/result_log.csv")
  if(!file.exists(log_file)){
    write.table(x = data.frame(res), file = log_file, quote = F, sep = ",", 
                row.names = F, col.names = names(res))
  } else { # Enable multiple connections
    conn <- file( sprintf(log_file , Sys.getpid()) , open = "a" )
    write.table(x = data.frame(res), conn, quote = F, sep = ",", 
                row.names = F, col.names = F, append = T)
    close(conn)
  }
 
  return(res)
  
  rm(clf)
  gc()

}
