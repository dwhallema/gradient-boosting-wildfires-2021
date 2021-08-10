#' PCL1.0 GBM  sensitivity triplots
#' 
#' Plots PCL1.0 GBM sensitivity analysis results
#' 
#' Disclaimer: Use at your own risk. No responsibility is assumed for a user's
#' application of these materials or related materials. 
#' 
#' @author Dennis Hallema, \email{dwhallem@@ncsu.edu}
#' 
#' @return Log files and work space images.

# Load packages
library(data.table)
library(ggplot2)
library(viridis)
# source("multiplot.R")
set.seed(123L)

# Read sensitivity/benchmark results
# in_file = "results/DEFAULT_0312/result_log.csv"
in_file = "results/result_log_all.csv"
# out_path = "results/results/DEFAULT_0312/"
DT <- fread(in_file)
str(DT)

# Apply filter
# DT <- data.frame(DT[ n.trees == 200, ])

# Convert ordinal and logical to factors
cols <- c("sysid","n.trees", "interaction.depth", "n.minobsinnode", "shrinkage", 
          "cv.folds", "class.stratify.cv", "replaceoutliers", "saveimage")
DT[,(cols) := lapply(.SD, as.factor), .SDcols = cols]


# WARNING: Plot these one at a time, else not all data points will be plotted

ggplot(DT, aes(x = n.trees, y = auprc_test, col = elapsed_min))  +
  theme_gray(base_size = 24) +
  scale_color_viridis(trans = "log", begin = 0, end = 1, direction = -1, breaks = c(5,15,60,240)) +
  geom_point(size=20, shape="-") +
  labs(title = paste0("n.trees (n=", dim(DT)[1],")"))

ggplot(DT, aes(x = interaction.depth, y = auprc_test, col = elapsed_min))  +
  theme_gray(base_size = 24) +
  scale_color_viridis(trans = "log", begin = 0, end = 1, direction = -1, breaks = c(5,15,60,240)) +
  geom_point(size=20, shape="-") +
  labs(title = paste0("interaction.depth (n=", dim(DT)[1],")"))

ggplot(DT, aes(x = n.minobsinnode, y = auprc_test, col = elapsed_min))  +
  theme_gray(base_size = 24) +
  scale_color_viridis(trans = "log", begin = 0, end = 1, direction = -1, breaks = c(5,15,60,240)) +
  geom_point(size=20, shape="-") +
  labs(title = paste0("n.minobsinnode (n=", dim(DT)[1],")"))

ggplot(DT, aes(x = shrinkage, y = auprc_test, col = elapsed_min))  +
  theme_gray(base_size = 24) +
  scale_color_viridis(trans = "log", begin = 0, end = 1, direction = -1, breaks = c(5,15,60,240)) +
  geom_point(size=20, shape="-") +
  labs(title = paste0("shrinkage (n=", dim(DT)[1],")"))

ggplot(DT, aes(x = cv.folds, y = auprc_test, col = elapsed_min))  +
  theme_gray(base_size = 24) +
  scale_color_viridis(trans = "log", begin = 0, end = 1, direction = -1, breaks = c(5,15,60,240)) +
  geom_point(size=20, shape="-") +
  labs(title = paste0("cv.folds (n=", dim(DT)[1],")"))

ggplot(DT, aes(x = class.stratify.cv, y = auprc_test, col = elapsed_min))  +
  theme_gray(base_size = 24) +
  scale_color_viridis(trans = "log", begin = 0, end = 1, direction = -1, breaks = c(5,15,60,240)) +
  geom_point(size=20, shape="-") +
  labs(title = paste0("class.stratify.cv (n=", dim(DT)[1],")"))

ggplot(DT, aes(x = replaceoutliers, y = auprc_test, col = elapsed_min))  +
  theme_gray(base_size = 24) +
  scale_color_viridis(trans = "log", begin = 0, end = 1, direction = -1, breaks = c(5,15,60,240)) +
  geom_point(size=20, shape="-") +
  labs(title = paste0("replaceoutliers (n=", dim(DT)[1],")"))

ggplot(DT, aes(x = saveimage, y = auprc_test, col = elapsed_min))  +
  theme_gray(base_size = 24) +
  scale_color_viridis(trans = "log", begin = 0, end = 1, direction = -1, breaks = c(5,15,60,240)) +
  geom_point(size=20, shape="-") +
  labs(title = paste0("saveimage (n=", dim(DT)[1],")"))
