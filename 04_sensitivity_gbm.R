#' PCL1.0 GBM sensitivity and benchmark influence
#' 
#' Determine parameter influence on PCL 1.0 GBM sensitivity and benchmark 
#' ("a GBM of GBMs")
#' 
#' Disclaimer: Use at your own risk. No responsibility is assumed for a user's
#' application of these materials or related materials. 
#' 
#' @author Dennis Hallema, \email{dwhallem@@ncsu.edu}
#' 
#' @return GBM influence plots.

# Load packages
library(data.table)
library(gbm)
set.seed(123L)

# Read sensitivity/benchmark results
# in_file = "results/DEFAULT_0312/result_log.csv"
in_file = "results/result_log_all.csv"
out_path = "results/"
DT <- fread(in_file)
str(DT)

# Create random variables
DT$random.1 <- runif(dim(DT)[1])
DT$random.2 <- runif(dim(DT)[1])

# Apply filter
# DT <- data.frame(DT[ n.trees == 200, ])

# Convert ordinal and logical to factors
cols <- c("sysid", "n.trees", "interaction.depth", "n.minobsinnode", "shrinkage",  
          "cv.folds", "class.stratify.cv", "replaceoutliers", "saveimage")
DT[,(cols) := lapply(.SD, as.factor), .SDcols = cols]

# Fit GBM of performance metric
cols <- c("auprc_test", "sysid", "n.trees", "interaction.depth", "n.minobsinnode", "shrinkage",
          "train.fraction", "cv.folds","class.stratify.cv","replaceoutliers","saveimage", "random.1", "random.2")
gbm1 <- gbm(formula = auprc_test ~ ., data = DT[,cols, with=FALSE], n.trees = 200, cv.folds = 10, interaction.depth = length(cols)-1)
par(oma = c(0,9,0,9))
gbm1.inf <- summary(gbm1, order = TRUE ,las = 1, xlim = c(0, 80), 
                   cex.names = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5,
                   main = "Influence on AUPRC for test data")
random.inf <- max(as.data.table(gbm1.inf)[var %like% "random", "rel.inf"])
abline(v = random.inf, lty = 1, col = "red", lwd = 1.5)
abline(v = seq(10,100,10), lty = 1, col = "grey", lwd = 1)
write.table(data.frame(gbm1.inf), file = paste0(out_path, "gbminf_auprc_test.csv"), row.names = FALSE, quote = FALSE, sep = ",")

# Fit GBM of benchmark (time elapsed)
cols <- c("elapsed_min", "sysid", "n.trees", "interaction.depth", "n.minobsinnode", "shrinkage",
          "train.fraction", "cv.folds","class.stratify.cv","replaceoutliers","saveimage", "random.1", "random.2")
gbm2 <- gbm(formula = elapsed_min ~ ., data = DT[,cols, with=FALSE], n.trees = 200, cv.folds = 10, interaction.depth = length(cols)-1)
par(oma = c(0,9,0,9))
gbm2.inf <- summary(gbm2, order = TRUE ,las = 1, xlim = c(0, 80), 
                   cex.names = 1.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5,
                   main = "Influence on model training time")
random.inf <- max(as.data.table(gbm2.inf)[var %like% "random", "rel.inf"])
abline(v = random.inf, lty = 1, col = "red", lwd = 1.5)
abline(v = seq(10,100,10), lty = 1, col = "grey", lwd = 1)
write.table(data.frame(gbm2.inf), file = paste0(out_path, "gbminf_elapsed_min.csv"), row.names = FALSE, quote = FALSE, sep = ",")
