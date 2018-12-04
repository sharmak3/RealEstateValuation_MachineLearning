#change the current working directory to the following directory
setwd(dir = "C:/Users/Karan/Documents/Praxis Material/Machine Learning/Polynomial regression assignment" )

fram = read.csv("RealEstateValuation.csv")

#creating training and test data
set.seed(1)
random_number_list = sample(1:nrow(fram))
k = (7/8) * nrow(fram)
train_set = fram[random_number_list[1:k], ]
m = as.integer(k) + 1
test_set  = fram[random_number_list[m:nrow(fram)], ]

sample_size   = c(10,30,50,80,100,150,200,250,270,300)

test_error_df = data.frame(matrix(ncol = 2, nrow= 0 ))

for(i in (sample_size)){
  set.seed(1)
  train_list = sample(1:nrow(train_set), i)
  train_data = train_set[train_list,]
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 7
  #=============================================================================================
  
  m7 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
          + I(Distance.to.the.nearest.MRT.station^2)
          + I(Distance.to.the.nearest.MRT.station^3)
          + I(Distance.to.the.nearest.MRT.station^4)
          + I(Distance.to.the.nearest.MRT.station^5)
          + I(Distance.to.the.nearest.MRT.station^6)
          + I(Distance.to.the.nearest.MRT.station^7), train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m7$residuals^2)
  pred_set    = predict(m7, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  test_error_df = rbind(test_error_df, c(i, test_error))
}

colnames(test_error_df) = c('Sample_Size', 'Test_Error')

plot(test_error_df$Sample_Size, test_error_df$Test_Error, pch=19, cex=0.5, type = 'l', 
     main = "Test Error VS Different Sample Size", xlab = 'Sample Size', ylab = 'Test Error')