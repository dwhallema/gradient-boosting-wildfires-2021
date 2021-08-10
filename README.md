# Sensitivity analysis and benchmarking of a gradient boosting model for predicting potential fire control locations (R, gbm, doParallel)

Author: [Dennis Hallema](https://www.linkedin.com/in/dennishallema) 

Description: We performed a sensitivity analysis and benchmark calculation of the gradient boosting classifier of PCL 1.0 GBM, a model for predicting potential fire control locations (PCLs) programmed in R using the gbm package. Data includes 253,513 grid cells of 30 x 30 meters with information on topography (flatdist, valley distance, distance to ridge, cost distance), road network (road distance, barrier distance), rate of spread (ros01), resistance to control (rtc01), and fire suppression difficulty (sdi01), randomly sampled for the 2019 Miller Fire in the Coronado National Forest in southwest Arizona. We created an algorithm to process data outliers, and a wrapper to run the GBM on multiple CPU cores simultaneously. Next, we performed 2,052 simulations on two commercial grade workstations varying 8 parameters: number of trees fitted, interaction depth, minimum number of observations in terminal nodes, learning rate, number of cross-validation folds, cross-validation stratification by class, outlier processing method, and model save option. Our findings suggest that a GBM with 5,000 trees accounting for all possible interactions, can provide as much predictive power in terms of area under precision-recall curve (AUPRC for testing on 30% of data points) as a 20,000-tree GBM but 4 times faster. Large trees did not appear to overfit the training data in terms of AUPRC but provided no additional performance benefit. Likewise, no outlier processing outperformed the three methods for outlier processing (Tukey IQR, interdecile range, and range between the 2.5th and 97.5th percentile).

Depends: R, RStudio, data.table, gbm, PRROC, ggplot2, doParallel

Disclaimer: Use at your own risk. No responsibility is assumed for a user's application of these materials or related materials. 

---

## Cloning this repository

1. Clone this repository onto your machine: 
   `git clone https://github.com/dwhallema/<repo>`, replace `<repo>` with the name of this repository. 
   This creates a new directory "repo" containing the repository files.  
2. Install RStudio  
3. Launch RStudio and open a R file with the extension ".R"  
4. To run, click the "Run" button.  
