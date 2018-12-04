#change the current working directory to the following directory
setwd(dir = "C:/Users/Karan/Documents/Praxis Material/Machine Learning/Polynomial regression assignment" )

fram = read.csv("RealEstateValuation.csv")

color = list("green", "blue", "yellow")

#creating training and test data
set.seed(1)
random_number_list = sample(1:nrow(fram))
k = 0.8 * nrow(fram)
train_set = fram[random_number_list[1:k], ]
m = as.integer(k) + 1
test_set  = fram[random_number_list[m:nrow(fram)], ]

set.seed(10)
train_list = sample(1:nrow(train_set), 80)
train_data = train_set[train_list,]

order_list = list(1,2,3,4,5,6,7,8,9)
test_error_list = list()
train_error_list = list()
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

m1 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station, train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m1$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m1, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2), train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m2$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m2, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

m3 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3), train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m3$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m3, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

m4 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4), train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m4$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m4, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

m5 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4)
        + I(Distance.to.the.nearest.MRT.station^5), train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m5$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m5, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

m6 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4)
        + I(Distance.to.the.nearest.MRT.station^5)
        + I(Distance.to.the.nearest.MRT.station^6), train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m6$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m6, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

m7 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4)
        + I(Distance.to.the.nearest.MRT.station^5)
        + I(Distance.to.the.nearest.MRT.station^6)
        + I(Distance.to.the.nearest.MRT.station^7), train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m7$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m7, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

m8 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4)
        + I(Distance.to.the.nearest.MRT.station^5)
        + I(Distance.to.the.nearest.MRT.station^6)
        + I(Distance.to.the.nearest.MRT.station^7)
        + I(Distance.to.the.nearest.MRT.station^8), train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m8$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m8, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

m9 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4)
        + I(Distance.to.the.nearest.MRT.station^5)
        + I(Distance.to.the.nearest.MRT.station^6)
        + I(Distance.to.the.nearest.MRT.station^7)
        + I(Distance.to.the.nearest.MRT.station^8)
        + I(Distance.to.the.nearest.MRT.station^9), train_set)

#TRAIN AND TEST ACCURACY
train_error = sum(m9$residuals^2)
train_rmse  = sqrt(train_error/80)
train_error_list = append(train_error_list, train_rmse)

pred_set    = predict(m9, newdata = test_set)
test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
rmse        = sqrt(test_error/80)
test_error_list = append(test_error_list, rmse)

train_error_list
test_error_list

plot(order_list, train_error_list, pch = 19, cex = 0.5, 
     xlab = "Order Of Polynomial", ylab = "Train RMSE            Test RMSE",
     main = "Test RMSE and Train RMSE VS Order Of Polynomial for 4 different
     samples of same size (n = 80) ", type = "l", col = "red", ylim = c(6,20))
lines(order_list, test_error_list, col = 'red', type = 'l', pch = 19)

c = 1

seed = c(30, 40, 50)
for(i in seed){
  test_error_list = list()
  train_error_list = list()
  set.seed(i)
  train_list = sample(1:nrow(train_set), 80)
  train_data = train_set[train_list,]
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 1
  #=============================================================================================
  
  m1 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station, train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m1$residuals^2)
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m1, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 2
  #=============================================================================================
  
  m2 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
          + I(Distance.to.the.nearest.MRT.station^2), train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m2$residuals^2)
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m2, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 3
  #=============================================================================================
  
  m3 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
          + I(Distance.to.the.nearest.MRT.station^2)
          + I(Distance.to.the.nearest.MRT.station^3), train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m3$residuals^2)
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m3, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 4
  #=============================================================================================
  
  m4 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
          + I(Distance.to.the.nearest.MRT.station^2)
          + I(Distance.to.the.nearest.MRT.station^3)
          + I(Distance.to.the.nearest.MRT.station^4), train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m4$residuals^2)
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m4, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 5
  #=============================================================================================
  
  m5 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
          + I(Distance.to.the.nearest.MRT.station^2)
          + I(Distance.to.the.nearest.MRT.station^3)
          + I(Distance.to.the.nearest.MRT.station^4)
          + I(Distance.to.the.nearest.MRT.station^5), train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m5$residuals^2)
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m5, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 6
  #=============================================================================================
  
  m6 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
          + I(Distance.to.the.nearest.MRT.station^2)
          + I(Distance.to.the.nearest.MRT.station^3)
          + I(Distance.to.the.nearest.MRT.station^4)
          + I(Distance.to.the.nearest.MRT.station^5)
          + I(Distance.to.the.nearest.MRT.station^6), train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m6$residuals^2)
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m6, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  
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
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m7, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 8
  #=============================================================================================
  
  m8 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
          + I(Distance.to.the.nearest.MRT.station^2)
          + I(Distance.to.the.nearest.MRT.station^3)
          + I(Distance.to.the.nearest.MRT.station^4)
          + I(Distance.to.the.nearest.MRT.station^5)
          + I(Distance.to.the.nearest.MRT.station^6)
          + I(Distance.to.the.nearest.MRT.station^7)
          + I(Distance.to.the.nearest.MRT.station^8), train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m8$residuals^2)
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m8, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  #=============================================================================================
  # FITTING A POLYNOMIAL REGRESSION OF ORDER 9
  #=============================================================================================
  
  m9 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
          + I(Distance.to.the.nearest.MRT.station^2)
          + I(Distance.to.the.nearest.MRT.station^3)
          + I(Distance.to.the.nearest.MRT.station^4)
          + I(Distance.to.the.nearest.MRT.station^5)
          + I(Distance.to.the.nearest.MRT.station^6)
          + I(Distance.to.the.nearest.MRT.station^7)
          + I(Distance.to.the.nearest.MRT.station^8)
          + I(Distance.to.the.nearest.MRT.station^9), train_data)
  
  #TRAIN AND TEST ACCURACY
  train_error = sum(m9$residuals^2)
  train_rmse  = sqrt(train_error/80)
  train_error_list = append(train_error_list, train_rmse)
  
  pred_set    = predict(m9, newdata = test_set)
  test_error  = sum((pred_set-test_set$House.price.of.unit.area)^2)
  rmse        = sqrt(test_error/80)
  test_error_list = append(test_error_list, rmse)
  
  lines(order_list,train_error_list, col = as.character(color[c]), type='l',pch = 19)
  lines(order_list,test_error_list, col = as.character(color[c]), type='l',pch = 19)
  c = c + 1
}