#' PCL1.0 GBM sensitivity analysis with benchmarking
#' 
#' Performs a PCL1.0 GBM random search with benchmarking
#' 
#' This function has been designed to perform a random search with benchmarking
#' and optional outlier replacement for the PCL1.0 GBM. 
#' 
#' Disclaimer: Use at your own risk. No responsibility is assumed for a user's
#' application of these materials or related materials. 
#' 
#' @author Dennis Hallema, \email{dwhallem@@ncsu.edu}
#' 
#' @return Log files and work space images.

# Load packages
library(data.table)
source("f_pcl_wrapper.R")
set.seed(123L)
library(doParallel)
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)#, type="FORK")
registerDoParallel(cl)
node <- "TPAD"
# node <- try(Sys.info()["nodename"][[1]])

# Path variables
in_file <- "data/coronado_Sample_01_14v2.csv"
# out_path <- paste0("results/", node, "_", format(Sys.time(), "%m%d"))
# dir.create(out_path, recursive = TRUE)
out_path <- "results/TPAD_0313"
create_newpar <- FALSE

# Read as data table
DT <- fread(in_file, drop = 1)
setnames(DT, "brt_resp02", "response")

# Define hyperparameter grid
if(create_newpar == TRUE) {
  param_grid <- expand.grid(
    interaction.depth = c(1, 2, 5, 8),
    n.minobsinnode = c(2, 5, 10),
    shrinkage = c(0.005, 0.01, 0.02, 0.05, 0.1),
    bag.fraction = 0.5,
    train.fraction = 0.7,
    cv.folds = c(2, 3, 5, 10),
    keep.data = FALSE,
    class.stratify.cv = c(FALSE, TRUE),
    n.trees = c(200,500,1000,2000,5000,10000,20000),
    # n.trees = c(50,100),
    n.cores = 1,
    test_size = 0.3,
    replaceoutliers = c(FALSE, "TukeyIQR","IDR", "P025975"),
    saveimage = c(FALSE,TRUE)
  )
  
  # Random sampling
  search_grid <- param_grid[sample.int(nrow(param_grid), size = 1500, replace = FALSE),]
  
  write.table(param_grid, file = paste0(out_path, "/param_grid.csv"), row.names = FALSE, quote = FALSE, sep = ",")
  write.table(search_grid, file = paste0(out_path, "/search_grid.csv"), row.names = FALSE, quote = FALSE, sep = ",")
} else {
  param_grid <- data.frame(fread(paste0(out_path, "/param_grid.csv")))
  search_grid <- data.frame(fread(paste0(out_path, "/search_grid.csv")))
}
# DEBUG CODE
# REVERSE SAMPLING ORDER
search_grid <- search_grid[order(nrow(search_grid):1),]

# Random search
cat(paste("Total number of simulations scheduled:", nrow(search_grid),"\n"))
print(Sys.time())
result <- foreach(i = 799:nrow(search_grid)) %dopar% { # Parallel processing
# result <- for(i in 1:nrow(param_grid)) {
  try({
    
    cat(paste(i, "of", nrow(search_grid),"\n"))
    pclm1 <- pcl_wrapper(formula = response ~ ., 
                  distribution = "bernoulli", data = DT,
                  var.monotone = NULL, 
                  n.trees = search_grid[i,"n.trees"],
                  interaction.depth = search_grid[i,"interaction.depth"], 
                  n.minobsinnode = search_grid[i,"n.minobsinnode"], 
                  shrinkage =  search_grid[i,"shrinkage"],
                  bag.fraction =  search_grid[i,"bag.fraction"], 
                  train.fraction = search_grid[i,"train.fraction"], 
                  cv.folds =  search_grid[i,"cv.folds"],
                  keep.data = search_grid[i,"keep.data"],
                  verbose = FALSE,
                  class.stratify.cv = search_grid[i,"class.stratify.cv"],
                  n.cores = search_grid[i,"n.cores"],
                  test_size = search_grid[i,"test_size"],
                  replaceoutliers = search_grid[i,"replaceoutliers"],
                  saveimage = search_grid[i,"saveimage"],
                  out_path = out_path,
                  node = node)
    
  })
}

registerDoSEQ()
