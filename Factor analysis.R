
require(psych)
require(GPArotation)
library(sas7bdat)

  #IMPORTING FILE BY BROWISING FILE

  file<-choose.files()        #without outlier wali file                      
  final<- read.sas7bdat(file) 
  str(final)              
  names(final)    
  
  data <- final
  data$cust_id <- NULL
  
  
  # Outlier Treatment and KPI CODES
  
  
  
  ## FACTOR ANALYSIS 
  corrm<- cor(data)                                
  
  ### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)
  ?scree()
  
  scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT
  
  eigen(corrm)$values                                                     ### EIGEN VALUES
  
  require(dplyr)
  eigen_values <- mutate(data.frame(eigen(corrm)$values)
                         ,cum_sum_eigen=cumsum(eigen.corrm..values)
                         , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                         , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 
  
  write.csv(eigen_values, "H:/R/segmentation/EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY
  
  ?fa
  FA<-fa(r=corrm, 9, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
 # print(FA)                                                    ### PRINT THE RESULTS
  FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
#  ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
  FA_SORT$loadings
  #FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
  Loadings<-data.frame(FA_SORT$loadings[1:ncol(data),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME
  
  write.csv(Loadings, "H:/R/segmentation/loadings.csv") ### SAVING THE FILE
  