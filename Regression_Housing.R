#Load the libraries
library(sp)
library(raster)
library(usdm)

#Load the housing Dataset to find the selling prince
setwd("E:\\IMS Course Content\\Course Content\\Data Science Term 1\\5. Simple & Multiple Linear Regression\\Case Study 1")
Housing=read.csv("HousingData.csv", header=TRUE)

#remove the 1st column
Housing=Housing[,-1]

#Explore the data
head(Housing)
summary(Housing)
str(Housing)
#find the corelation of the variable
cr <- cor(Housing)
corrplot(cr, type = "lower")
#plotting the graph
pairs(Housing)
plot(Housing)

#test for multicollinearity
vifstep(Housing[,-1], th=5)

#Linear regression for all the variables
fit0 <- lm(SellingPrice000s~HouseSize00Sqft + NumberofBathrms + NumberofBedrms + GarageSize, data = Housing)
summary(fit0)

#Linear regression for only 2 variables
fit1=lm(SellingPrice000s~NumberofBathrms + GarageSize, data=Housing)
summary(fit1)

#Linear regression for only 1 variable
reduced=lm(SellingPrice000s~NumberofBathrms, data=Housing)
summary(reduced)

full=lm(SellingPrice000s~NumberofBedrms + HouseSize00Sqft, data=Housing)
summary(full)

anova(reduced,full)

pred1 <- predict(fit1,data.frame(NumberofBathrms=3,GarageSize=3), 
        interval="confidence", level=0.95)

predict(fit1,data.frame(NumberofBathrms=3,GarageSize=3), 
        interval="prediction", level=0.95)
