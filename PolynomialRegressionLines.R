#change the current working directory to the following directory
setwd(dir = "C:/Users/Karan/Documents/Praxis Material/Machine Learning/Polynomial regression assignment" )

fram = read.csv("RealEstateValuation.csv")

#creating training and test data
set.seed(1)
random_number_list = sample(1:nrow(fram))
k = 0.30 * nrow(fram)
train_set = fram[random_number_list[1:k], ]


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

m1 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station, train_set)

#PLOTTING THE MODEL OVER THE DATA
plot(train_set$Distance.to.the.nearest.MRT.station,
     train_set$House.price.of.unit.area, pch=19, cex=0.5,
     main = "Polynomial Regression Lines For Sample Size Of 30%",
     xlab = "Distance To Nearest MRT Station", ylab = "House Price Of Unit Area")

lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m1)[order(train_set$Distance.to.the.nearest.MRT.station)], col='red', type = "l" ) 


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2), train_set)

lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m2)[order(train_set$Distance.to.the.nearest.MRT.station)], col='blue', type='l') 


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

m3 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3), train_set)
m3

lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m3)[order(train_set$Distance.to.the.nearest.MRT.station)], col='green', type='l') 


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

m4 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4), train_set)

lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m4)[order(train_set$Distance.to.the.nearest.MRT.station)], col='orange', type='l') 

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

m5 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4)
        + I(Distance.to.the.nearest.MRT.station^5), train_set)

lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m5)[order(train_set$Distance.to.the.nearest.MRT.station)], col='magenta', type='l') 


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

m6 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
        + I(Distance.to.the.nearest.MRT.station^2)
        + I(Distance.to.the.nearest.MRT.station^3)
        + I(Distance.to.the.nearest.MRT.station^4)
        + I(Distance.to.the.nearest.MRT.station^5)
        + I(Distance.to.the.nearest.MRT.station^6), train_set)

lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m6)[order(train_set$Distance.to.the.nearest.MRT.station)], col='brown', type='l') 

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

lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m7)[order(train_set$Distance.to.the.nearest.MRT.station)], col='yellow', type='l') 


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


lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m8)[order(train_set$Distance.to.the.nearest.MRT.station)], col='darkgreen', type='l') 


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


lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m9)[order(train_set$Distance.to.the.nearest.MRT.station)], col='black', type='l') 

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================

m10 = lm(House.price.of.unit.area ~ Distance.to.the.nearest.MRT.station
         + I(Distance.to.the.nearest.MRT.station^2)
         + I(Distance.to.the.nearest.MRT.station^3)
         + I(Distance.to.the.nearest.MRT.station^4)
         + I(Distance.to.the.nearest.MRT.station^5)
         + I(Distance.to.the.nearest.MRT.station^6)
         + I(Distance.to.the.nearest.MRT.station^7)
         + I(Distance.to.the.nearest.MRT.station^8)
         + I(Distance.to.the.nearest.MRT.station^9)
         + I(Distance.to.the.nearest.MRT.station^10), train_set)

lines(sort(train_set$Distance.to.the.nearest.MRT.station), fitted(m10)[order(train_set$Distance.to.the.nearest.MRT.station)], col='maroon', type='l') 

legend(4600, 80, legend=c("Order 1", "Order 2", "Order 3", "Order 4",
                          "Order 5", "Order 6", "Order 7", "Order 8",
                          "Order 9", "Order 10"),
       col=c("red", "blue", "green", "orange", "magenta",
             "brown", "yellow", "darkgreen", "black", "maroon"), lty=1, cex=0.8)
