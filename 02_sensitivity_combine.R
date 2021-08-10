#' PCL1.0 GBM combine sensitivity analysis results
#' 
#' Combines PCL1.0 GBM sensitivity analysis results
#' 
#' Disclaimer: Use at your own risk. No responsibility is assumed for a user's
#' application of these materials or related materials. 
#' 
#' @author Dennis Hallema, \email{dwhallem@@ncsu.edu}
#' 
#' @return Text file and plots.

# Load packages
library(data.table)
set.seed(123L)

# Path variables
in_files <- c("results/DEFAULT_0312/result_log.csv",
              "results/TPAD_0313/result_log.csv"
              )
out_log <- "results/result_log_all.csv"
out_log_desc <- "results/result_log_all_auprc_test_desc.csv"

# Read and combine inputs
lst <- lapply(in_files, fread, stringsAsFactors=T)
sysids <- c("de", "le", "le")
for (i in 1:length(lst))  lst[[i]]$sysid = sysids[i]

# Write output
DT <- rbindlist(lst)
write.table(DT, file = out_log, append = F, quote = F, row.names = F, sep = ",")

# Sort simulations, write output
setorder(DT, -auprc_test)
head(DT)
write.table(DT, file = out_log_desc, append = F, quote = F, row.names = F, sep = ",")
