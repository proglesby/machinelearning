#Initialize libraries:
library(dplyr)
#Read in the data:
getwd()
setwd('/home/ross/Documents/Data Science')
getwd()
orings <- read.table('o-ring-erosion-only.data', header = FALSE)
orings
#Add the header to the data:
names(orings)[names(orings)=="V1"] <- "Orings_Num"
names(orings)[names(orings)=="V2"] <- "Num_Rings_Distress"
names(orings)[names(orings)=="V3"] <- "Launch_Temp"
names(orings)[names(orings)=="V4"] <- "Leak_Check_PSI"
names(orings)[names(orings)=="V5"] <- "Order_Flight"
#Verify the data has headers:
orings
# > orings
# Orings_Num Num_Rings_Distress Launch_Temp Leak_Check_PSI Order_Flight
# 1           6                  0          66             50            1
# 2           6                  1          70             50            2
# 3           6                  0          69             50            3
# 4           6                  0          68             50            4
# 5           6                  0          67             50            5
# 6           6                  0          72             50            6
# 7           6                  0          73            100            7
# 8           6                  0          70            100            8
# 9           6                  1          57            200            9
# 10          6                  1          63            200           10
# 11          6                  1          70            200           11
# 12          6                  0          78            200           12
# 13          6                  0          67            200           13
# 14          6                  2          53            200           14
# 15          6                  0          67            200           15
# 16          6                  0          75            200           16
# 17          6                  0          70            200           17
# 18          6                  0          81            200           18
# 19          6                  0          76            200           19
# 20          6                  0          79            200           20
# 21          6                  0          75            200           21
# 22          6                  0          76            200           22
# 23          6                  1          58            200           23
#Starting EDA
#Summarize the data overall:
summary(orings)
# > summary(orings)
# Orings_Num Num_Rings_Distress  Launch_Temp    Leak_Check_PSI   Order_Flight 
# Min.   :6    Min.   :0.0000     Min.   :53.00   Min.   : 50.0   Min.   : 1.0  
# 1st Qu.:6    1st Qu.:0.0000     1st Qu.:67.00   1st Qu.: 75.0   1st Qu.: 6.5  
# Median :6    Median :0.0000     Median :70.00   Median :200.0   Median :12.0  
# Mean   :6    Mean   :0.3043     Mean   :69.57   Mean   :152.2   Mean   :12.0  
# 3rd Qu.:6    3rd Qu.:0.5000     3rd Qu.:75.00   3rd Qu.:200.0   3rd Qu.:17.5  
# Max.   :6    Max.   :2.0000     Max.   :81.00   Max.   :200.0   Max.   :23.0
str(orings)
# > str(orings)
# 'data.frame':	23 obs. of  5 variables:
#   $ Orings_Num        : int  6 6 6 6 6 6 6 6 6 6 ...
# $ Num_Rings_Distress: int  0 1 0 0 0 0 0 0 1 1 ...
# $ Launch_Temp       : int  66 70 69 68 67 72 73 70 57 63 ...
# $ Leak_Check_PSI    : int  50 50 50 50 50 50 100 100 200 200 ...
# $ Order_Flight      : int  1 2 3 4 5 6 7 8 9 10 ...

#Not a ton of data summarized by str, just tells me the small data set that I already expected. 
boxplot(orings$Launch_Temp)
#It looks like a huge spread of temperature around 70 degrees farenheight at launch. The lower data point is where most of the failure seemed to occur.
#We should check the correlation between the o-rings in distress and the temperature at launch:
cor(orings$Launch_Temp, orings$Num_Rings_Distress, method="pearson")
# > cor(orings$Launch_Temp, orings$Num_Rings_Distress, method="pearson")
# [1] -0.725671
#It looks like they are negatively correlated. That's sort of surprising from a statistical standpoint. Would be interested to see the design of these o-rings.
plot(orings$Num_Rings_Distress, orings$Launch_Temp)
#It looks like the number of o-rings in distress came from lower temperatures.
boxplot(orings$Leak_Check_PSI)
#Doesn't look like much of a low spread in the PSI. Leak check is a typical test where they verify the pressure on the O-Ring seal. 
plot(orings$Leak_Check_PSI, orings$Num_Rings_Distress)
#It looks like there's very little thermal distress correlated with the O-ring leak check PSI. This is sort of a surprise considering the leak check would be the first thing I would look at given the physical characteristics of the design. 
cor(orings$Leak_Check_PSI, orings$Num_Rings_Distress, method="pearson")
# > cor(orings$Leak_Check_PSI, orings$Num_Rings_Distress, method="pearson")
# [1] 0.2203256
#These are very weakly correlated, but positively correlated nonetheless. Still, not statistically significant. 

#I am thinking that I can use logistic regression based upon the leak check PSI and the launch temperature to get an idea of if either of them 
#can predict a failure. 

#I might also consider using some randomized data to figure out the best fit to get a better fit for the logistic regression model. Might give us a better idea. 
#Create a binary variable to see whether an o-ring might experience failure or not depending upon the data:
oringsFailure <- ifelse(orings$Num_Rings_Distress >= 1, 1, 0)
orings <- cbind(orings, oringsFailure)
#Verify that the data is ammended with oringsFailure:
# > orings
# Orings_Num Num_Rings_Distress Launch_Temp Leak_Check_PSI Order_Flight oringsFailure
# 1           6                  0          66             50            1             0
# 2           6                  1          70             50            2             1
# 3           6                  0          69             50            3             0
# 4           6                  0          68             50            4             0
# 5           6                  0          67             50            5             0
# 6           6                  0          72             50            6             0
# 7           6                  0          73            100            7             0
# 8           6                  0          70            100            8             0
# 9           6                  1          57            200            9             1
# 10          6                  1          63            200           10             1
# 11          6                  1          70            200           11             1
# 12          6                  0          78            200           12             0
# 13          6                  0          67            200           13             0
# 14          6                  2          53            200           14             1
# 15          6                  0          67            200           15             0
# 16          6                  0          75            200           16             0
# 17          6                  0          70            200           17             0
# 18          6                  0          81            200           18             0
# 19          6                  0          76            200           19             0
# 20          6                  0          79            200           20             0
# 21          6                  0          75            200           21             0
# 22          6                  0          76            200           22             0
# 23          6                  1          58            200           23             1
orings
#Create a training and test data set. Since the data set is small, it might be sort of challenging to get a good fit:
# Split data set into training set and test set
n <- nrow(orings)  # Number of observations = 102
ntrain <- round(n*0.6)    # 60% for training set
set.seed(314)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index

trainOrings <- orings[tindex,]  # Create training set
testOrings <- orings[-tindex,]  # Create test set

#Create the formula to predict whether the o-ring will fail or not.  

failPredict <- glm(oringsFailure ~ Launch_Temp + Leak_Check_PSI, data=trainOrings, family="binomial")

summary(failPredict)

# Call:
#   glm(formula = formula, family = "binomial", data = trainOrings)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.0342  -0.6036  -0.2964   0.1252   1.9806  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)    24.126409  18.904835   1.276    0.202
# Launch_Temp    -0.377485   0.278150  -1.357    0.175
# Leak_Check_PSI  0.009755   0.012036   0.810    0.418
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 18.2492  on 13  degrees of freedom
# Residual deviance:  9.2205  on 11  degrees of freedom
# AIC: 15.22
# 
# Number of Fisher Scoring iterations: 6


#Youch. It looks like my best predictor is the initial launch temperature. Still, it is not super significant. This is unforunate. 
#Check against training data: 
prob <- predict(failPredict,testOrings, type="response")
round(prob,3)*100   # Get percentages
prob

#Calculate some test set error metrics based upon the actual data:
#Calculate the test error of the model:
#If glm() predicts greater than 0.5 then 1, otherwise 0.
#Create a separate variable to play with error tolerance: 
tolerance <- 0.5
oringPredict <- ifelse(prob > tolerance, 1,0)
oringPredict
#Compare the predicted vector with the mpg01 vector:
errorVector <- ifelse(oringPredict!=testOrings$oringsFailure,0,1)
errorVector
mean(errorVector)*100
#78% accuracy rate is actually not as bad as I thought. 

#Plot the output of the failure prediction matrix vs the launch temp in the training set:
plot(errorVector,testOrings$oringsFailure )


#Expository data Analysis
#Visualization of the Leak check pressure spread: 
boxplot(orings$Leak_Check_PSI,
     pch=19,
     cex=0.5,
     ylab="Leak Check PSI",
     main = "Leak Check PSI")
#Visualization of the Ambient Air Temperature at Launch and the Number of O-Rings in Distress
plot(orings$Launch_Temp,
     orings$Num_Rings_Distress,
     pch=19,
     cex=0.5,
     xlab="Ambient Temperature at Launch",
     ylab="Number of Orings In Distress",
     main = "Ambient Temp at Launch vs. Num of Orings in Distress")
#Visualization of the Leak Check PSI vs. Number of O-Rings in Distress
plot(orings$Leak_Check_PSI,
     orings$Num_Rings_Distress,
     pch=19,
     cex=0.5,
     xlab="Leak Check PSI",
     ylab="Number of Orings In Distress",
     main = "Leak Check PSI vs. Number of Orings in Distress")

#Try to get an idea of how all 3 variables interact: O-rings in distress, Ambient Temp at launch
#and leak check PSI. 
install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(orings$Num_Rings_Distress, 
              orings$Leak_Check_PSI, 
              orings$Launch_Temp, 
              highlight.3d=TRUE, 
              col.axis="blue", 
              col.grid="lightblue",
              main="Num O-rings in Distress vs Leak Check PSI vs Ambient Launch Temp", 
              pch=20, 
              xlab="Num Distressed O-rings", 
              ylab="Leak Check PSI", 
              zlab="Launch Temp")
#Overall, the results were not good. Will not be including this in the paper as it does not give
#a very clear example of how the data is interrelated. 

#Visualization of the Ambient Air Temperature at launch:
boxplot(orings$Launch_Temp,
        ylab="Degrees (F)",
        main = "Ambient Temp at Launch")
  

    
