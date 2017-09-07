## Loading relavent Packages
library(MASS)
library(car)
library(tidyverse)

## Creating and Setting working directory 
dir.create("~/Desktop/RegressionAssignment")
setwd("~/Desktop/RegressionAssignment")

## Downloading assignment files
filepath1 <- "https://cdn.upgrad.com/UpGrad/temp/a9f2334f-9eb2-4160-8486-701584204e08/CarPrice_Assignment.csv"
filepath2 <- "https://cdn.upgrad.com/UpGrad/temp/a5ae64ac-9490-44a5-ac06-c8b97e76937d/Data%20Dictionary%20-%20carprices.xlsx"
download.file(filepath1, destfile = "CarPrice_Assignment.csv", method="curl")
download.file(filepath2, destfile = "Data Dictionary - carprices.xlsx", method="curl")

## Reading the data in to Rstudio environment for analysis (factors are required)
carPrices <- read.csv("CarPrice_Assignment.csv")

## viewing the imported data 
View(carPrices)

## Seeing the structure of data set
str(carPrices)

## data preparation for regression analysis(modifying data types)
# converting dual level data to c(0,1) and to numeric data type
levels(carPrices$fueltype) <- c(0,1) # diesel (0), gas(1)
carPrices$fueltype <- as.numeric(levels(carPrices$fueltype))[carPrices$fueltype]

levels(carPrices$aspiration) <- c(0,1) # std (0), turbo(1)
carPrices$aspiration <- as.numeric(levels(carPrices$aspiration))[carPrices$aspiration]

levels(carPrices$doornumber) <- c(0,1) # four (0), two (1)
carPrices$doornumber <- as.numeric(levels(carPrices$doornumber))[carPrices$doornumber]

levels(carPrices$enginelocation) <- c(0,1) # front (0), rear(1)
carPrices$enginelocation <- as.numeric(levels(carPrices$enginelocation))[carPrices$enginelocation]

# seperating CarName into CarName, Types, Subtype
carPricesData <- separate(carPrices, CarName,into = c("CarName", "Type"),sep = " ")
View(carPricesData)


# converting multilevel variables to dummy and then to numbers
#1. carbody
dummy_cb <- data.frame(model.matrix( ~carbody, data = carPricesData))
View(dummy_cb)
dummy_cb <- dummy_cb[,-1]

#2. drivewheel
dummy_dw <- data.frame(model.matrix( ~drivewheel, data = carPricesData))
View(dummy_dw)
dummy_dw <- dummy_dw[,-1]

#3. enginetype
dummy_et <- data.frame(model.matrix( ~enginetype, data = carPricesData))
View(dummy_et)
dummy_et <- dummy_et[,-1]

#4. cylindernumber
dummy_cyn <- data.frame(model.matrix( ~cylindernumber, data = carPricesData))
View(dummy_cyn)
dummy_cyn <- dummy_cyn[,-1]

#5. fuelsystem
dummy_fsys <- data.frame(model.matrix( ~fuelsystem, data = carPricesData))
View(dummy_fsys)
dummy_fsys <- dummy_fsys[,-1]

## removing car models and Combining dummy variable in data set called carPrices1
carPrices1 <- cbind(carPricesData[,setdiff(names(carPricesData),
                                           c("Type","carbody","drivewheel","enginetype",
                                                  "cylindernumber","fuelsystem"))], 
                                    dummy_cb, dummy_dw, dummy_et, dummy_cyn, dummy_fsys)
View(carPrices1)
str(carPrices1)

## Derived metrices
#1. Overall mpg
carPrices1$Ompg <- round(mean(carPrices1$citympg + carPrices1$highwaympg),2)

#2. Stroke2Bore Ratio
carPrices1$sbr <- round(carPrices1$stroke/carPrices1$boreratio,2)

#4. Overall mpg to Horsepower ratio
carPrices1$Ohp <- round(carPrices1$Ompg/carPrices1$horsepower, 2)

#5. Overall mpg to curbweight ratio (FE)
carPrices1$FE <- round(carPrices1$Ompg/carPrices1$curbweight, 4)

## correcting carName (company name)
levels(as.factor(carPrices1$CarName))
company <- mapvalues(carPrices1$CarName, from = c("maxda", "porcshce", "vokswagen", 
                                       "vw", "Nissan", "toyouta"), to = c("mazda", 
                                        "porsche", "volkswagen", "volkswagen", "nissan", "toyota"))
carPrices1 <- cbind(carPrices1[,-3],company)
carPrices1$company <- as.factor(carPrices1$company)
## Setting seed to achieve reproducibility
set.seed(9999)

## avoiding scientific notation to increase comparability
options(scipen = 999)

## seperating Training and test datasets
trainindices= sample(1:nrow(carPrices1), 0.7*nrow(carPrices1))
train = carPrices1[trainindices,]
test = carPrices1[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)


## using stepAIC to estimate the model
step <- stepAIC(model_1, direction = "both")

## using last step of AIC for finalisation of our model
model_2 <- lm(price ~ car_ID + aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                sbr + Ohp + company, data = train)
summary(model_2)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_2)

## droping car_id as VIF is too high 
model_3 <- lm(price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                sbr + Ohp + company, data = train)
summary(model_3)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_3)

## droping engine size as it has high VIF and low significance

model_4 <- lm(price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                sbr + Ohp + company, data = train)
summary(model_4)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_4)

## droping stroke as it has high VIF and low significance

model_5 <- lm(price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                sbr + Ohp + company, data = train)
summary(model_5)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_5)

## droping enginetypel, doornumber, peakrpm, carbodyhardtop, drivewheelrwd as these 
# have high VIF and low significance

model_6 <- lm(price ~ aspiration + wheelbase + carlength + 
                carheight + curbweight + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 Ohp + company, data = train)
summary(model_6)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_6)

## droping carbodysedan and Ohp as these 
# have high VIF and low significance

model_7 <- lm(price ~ aspiration + wheelbase + carlength + 
                carheight + curbweight + carbodyhatchback +
                 carbodywagon + cylindernumberfive +
                 cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 company, data = train)
summary(model_7)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_7)

## droping fuelsystemspdi, carbodywagon and aspirations as they have low significance

model_8 <- lm(price ~ wheelbase + carlength + carheight + curbweight 
                + carbodyhatchback  + cylindernumberfive +
                cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + fuelsystemmpfi + 
                company, data = train)
summary(model_8)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_8)

## droping fuelsystemsbbl and mpfi as they have low significance

model_9 <- lm(price ~ wheelbase + carlength + carheight + curbweight 
              + carbodyhatchback  + cylindernumberfive +
                cylindernumberfour + cylindernumbersix + 
                company, data = train)
summary(model_9)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_9)

## droping carbody hatchback as it has low significance and slightly higher VIF

model_10 <- lm(price ~ wheelbase + carlength + carheight + curbweight 
                + cylindernumberfive +
                cylindernumberfour + cylindernumbersix + 
                company, data = train)
summary(model_10)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_10)

## As now our model has only significant parameters in our apart from company so 
## we can use this model for our prediction. 

## NOTE: Company names have not been altered as the manufacturer may have their
## sales based on their brand values too which is not covered in this model and 
## prices are mostly brand specific.

# predicting the results in test dataset
Predict_1 <- predict(model_10,test[,-20])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

