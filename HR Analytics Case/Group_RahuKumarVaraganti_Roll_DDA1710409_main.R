#############################HR Analytics###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:
# The level of attrition is high hence the company wants to investigate on
# the probable factors as it impacting and amounting to significant effort in
# the project deliverables, company image, training to new staff etc.

## AIM:

# The aim is to understand what factors the company should focus on, in order 
# to curb attrition. In other words, they want to know what changes they should 
# make to their workplace, in order to get most of their employees to stay

#1. Behaviour of the employee in the recent past
#2. Determine the factors to help curb the attrition rate

################################################################

### Data Understanding
# Install the required packages
requiredPackages = c('MASS','car','caret','cowplot','ggplot2','GGally',
                     'caTools','lubridate','e1071')

#Installing the required packages
for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    install.packages(item)
  }
  library(item, character.only = TRUE)
}

#Loading the 5 files
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
general_data <- read.csv("general_data.csv", stringsAsFactors = FALSE)
intime_data <- read.csv("in_time.csv", stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
outtime_data <- read.csv("out_time.csv", stringsAsFactors = FALSE)


#The in_time and out_time of an employee which spans roughly over 5 months can 
#be calculated as office_time aggregating to one variable.


######Start - cleaning the intime_data and outtime_data  ####################

#Remove the holiday columns from "in_time" and "out_time" dataset
remove_holiday_col <- function(x){
  time_data <- subset(x, select = -c(X2015.01.01,
                                     X2015.01.14,
                                     X2015.01.26,
                                     X2015.03.05,
                                     X2015.05.01))
  return(time_data)
}

intime_data <- remove_holiday_col(intime_data)
outtime_data <- remove_holiday_col(outtime_data)

sum(is.na(intime_data))
sum(is.na(outtime_data))

str(intime_data)
str(outtime_data)

#Convert all the date columns from character to DateTime format
intime_data[,2:257] <- lapply(intime_data[,2:257], function(x) as_datetime(x))
outtime_data[,2:257] <- lapply(outtime_data[,2:257], function(x) as_datetime(x))

#We Calculate the work hours of each employee everyday outtime-intime
x<-outtime_data[,2:257]-intime_data[,2:257]
x_y<-cbind(intime_data[,c("X")],x)
names(x_y)[1]<-paste("X")
x_y[,2:257] <- lapply(x_y[,2:257], function(x) as.numeric(x))

#Calculaing the avg work hours of each employee
x_y$work_avg <- rowMeans(x_y[,2:257], na.rm = TRUE)


######End - cleaning the intime_data and outtime_data  ####################

#Structure of the data sets
str(employee_survey_data)
str(general_data)
str(manager_survey_data)

#Create a new dataframe consisting of Employee ID and Office time hrs.
office_time <- data.frame(x_y$X, x_y$work_avg)
colnames(office_time) <- c("EmployeeID", "OfficeHrs")
office_time$OfficeHrs <- round(office_time$OfficeHrs, 2)

#Collating the data together
length(unique(tolower(employee_survey_data$EmployeeID)))  # 4410, Employee ID is the primary key
length(unique(tolower(general_data$EmployeeID)))          # 4410, Employee ID is the primary key
length(unique(tolower(manager_survey_data$EmployeeID)))   # 4410, Employee ID is the primary key
length(unique(tolower(office_time$EmployeeID)))           # 4410, Employee ID is the primary key


setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID)
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID)
setdiff(employee_survey_data$EmployeeID,office_time$EmployeeID)

hr_analytics <- merge(employee_survey_data,general_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, manager_survey_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, office_time, by= "EmployeeID", all = F)

#Create a new derived column "Over_time"
hr_analytics$Over_time <- ifelse(hr_analytics$OfficeHrs > 8, 1, 0)
# Overtime, 1 indicates yes while 0 = no

#Create inadequate work time
hr_analytics$inadq_time <- ifelse(hr_analytics$OfficeHrs < 7, 1, 0)
#Inadiquate time means the employee is working mush less than the required hours on average. 

#create no of leaves as derived metric
#it is calculated by counting the no of NA's in (outtime-intime) which we have stored as x
for(i in 1:4410)
{
  hr_analytics$no_leaves[i]<-sum(is.na(x[i,]))
}

View(hr_analytics)   #master_file1

################################################################
#Distribution of categorical variables
par(mfrow = c(4,2))
par(mar =rep(2,4))

barplot(table(hr_analytics$Attrition), main = "Attrition Distribution")

barplot(table(hr_analytics$JobSatisfaction), main="Job Satisfication level")

barplot(table(hr_analytics$BusinessTravel), main = "Business Travel")

barplot(table(hr_analytics$Gender), main ="Gender")

barplot(table(hr_analytics$MaritalStatus), main = "Marital Status")

barplot(table(hr_analytics$JobInvolvement), main = "Job Involvement")

barplot(table(hr_analytics$Over_time), main = "Over time")

barplot(table(hr_analytics$PerformanceRating), main = "Performance rating")

### Data Preparation & Exploratory Data Analysis

str(hr_analytics)
#cleaning the individual NA values from the data set
# if we have na values in the dataset the prediction will be highly unstable
#indexes of columns with na values
xyz<-c(2,3,4,13,18) 
#cleaning NA for Environment Satisfaction(replace with median)
hr_analytics$EnvironmentSatisfaction[which(is.na(hr_analytics$EnvironmentSatisfaction))]<-median(hr_analytics$EnvironmentSatisfaction,na.rm = TRUE)

#cleaning NA for Job Satisfaction(replace with median)
hr_analytics$JobSatisfaction[which(is.na(hr_analytics$JobSatisfaction))]<-median(hr_analytics$JobSatisfaction,na.rm = TRUE)

#cleaning NA for Job Satisfaction(replace with median)
hr_analytics$WorkLifeBalance[which(is.na(hr_analytics$WorkLifeBalance))]<-median(hr_analytics$WorkLifeBalance,na.rm = TRUE)

#cleaning NA for NumCompaniesWorked(replace with mean)
hr_analytics$NumCompaniesWorked[which(is.na(hr_analytics$NumCompaniesWorked))]<-mean(hr_analytics$NumCompaniesWorked,na.rm = TRUE)

#cleaning NA for TotalWorkingYears(replace with mean)
hr_analytics$TotalWorkingYears[which(is.na(hr_analytics$TotalWorkingYears))]<-mean(hr_analytics$TotalWorkingYears,na.rm = TRUE)

#Below are the list of categorical attributes
#Attrition, Gender, Over18  -- 2 levels
# More than 2 levels
#EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,
#JobRole, MaritalStatus,  BusinessTravel, Department,Education, EducationField
#JobInvolvement,JobLevel,PerformanceRating



#Converting the "Attrition,Gender and Over18" attributes with 2 levels into numbers(0,1)
hr_analytics$Attrition <- ifelse(hr_analytics$Attrition == "Yes", 1,0)
hr_analytics$Gender <- ifelse(hr_analytics$Gender == "Female",1,0)
hr_analytics$Over18 <- ifelse(hr_analytics$Over18 == "Y", 1,0)


#Create a dataframe of categorical attributes with more than 2 levels
hr_analytics_fact <- hr_analytics[,c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
                                     "BusinessTravel","Department","EducationField", "Education",
                                     "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                                     "PerformanceRating")]

#Convert categorical attributes to factors
hr_analytics_fact <- data.frame(sapply(hr_analytics_fact, function(x) factor(x)))
str(hr_analytics_fact)

#Creating dummy attributes for factor attributes
dummies <- data.frame(sapply(hr_analytics_fact, function(x)
  data.frame(model.matrix(~x-1, data = hr_analytics_fact))[,-1]))

#Removing the categorical attributes and adding the corresponding dummy attributes.
hr_analytics <- cbind(hr_analytics[,-c(2,3,4,7,8,10,11,14,15,16,28,29)], dummies)
View(hr_analytics)  # 4410 observations with 44 attributes, master_file2

str(hr_analytics)

par(mfrow = c(4,2))
par(mar = rep(2,4))

hist(hr_analytics$Age)
hist(hr_analytics$DistanceFromHome)
hist(hr_analytics$MonthlyIncome)
hist(hr_analytics$NumCompaniesWorked)
hist(hr_analytics$PercentSalaryHike)
hist(hr_analytics$TrainingTimesLastYear)
hist(hr_analytics$YearsAtCompany)
hist(hr_analytics$YearsSinceLastPromotion)


par(mfrow = c(4,2))
par(mar = rep(2,4))
boxplot(hr_analytics$Age)
boxplot(hr_analytics$DistanceFromHome)
boxplot(hr_analytics$MonthlyIncome)
boxplot(hr_analytics$NumCompaniesWorked)
boxplot(hr_analytics$PercentSalaryHike)
boxplot(hr_analytics$TrainingTimesLastYear)
boxplot(hr_analytics$YearsAtCompany)
boxplot(hr_analytics$YearsSinceLastPromotion)

#Few outliers remain but it is not wise to remove them as they represent the 
#company population where such data is bound to exist


summary(hr_analytics)


################################Check which columns needs to be scaled############
#EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,Education
#JobInvolvement,JobLevel,PerformanceRating
#Above variables have been converted into dummy variables. 

#scaling the variables
#indicies of all the columns to be scaled
ind<-c(2,4,7,8,10,c(12:18),21)
hr_analytics_scaled<-hr_analytics
for(i in ind)
{
  hr_analytics_scaled[,i]<-scale(x=hr_analytics[,i],center = TRUE,scale = TRUE)
}
summary(hr_analytics_scaled)
########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(hr_analytics$EmployeeID, SplitRatio = 0.7)

train = hr_analytics_scaled[indices,c(3,2,c(4:61))]

test = hr_analytics_scaled[!(indices),c(3,2,c(4:61))]

########################################################################
#modelling

model_1<-glm(Attrition~.,data=train,family = 'binomial')
summary(model_1)

model_2<-stepAIC(model_1, direction="both")
summary(model_2)

vif(model_2)

#removing EducationField.xLife.Sciences(Insignificant)

model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5 + PerformanceRating, 
             family = "binomial", data = train)
summary(model_3)
vif(model_3)

#removing YearsAtCompany (High vif and Insignificant)

model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5 + PerformanceRating, 
             family = "binomial", data = train)
summary(model_4)
vif(model_4)

#removing MaritalStatus.xMarried (Insignificant)

model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5 + PerformanceRating, 
             family = "binomial", data = train)
summary(model_5)
vif(model_5)

#removing PerformanceRating (Insignificant)

model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5, 
             family = "binomial", data = train)
summary(model_6)
vif(model_6)

#removing PPercentSalaryHike (Insignificant)

model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5, 
             family = "binomial", data = train)
summary(model_7)
vif(model_7)

#removing JobRole.xSales.Executive (Insignificant)

model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director +   
               MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
               JobLevel.x5, 
             family = "binomial", data = train)
summary(model_8)
vif(model_8)

#removing JobInvolvement.x2 (Insignificant)

model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + JobInvolvement.x3 + 
               JobLevel.x5, 
             family = "binomial", data = train)
summary(model_9)
vif(model_9)

#removing JobLevel.x5 (Insignificant)

model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + Education.x2 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + JobInvolvement.x3, 
             family = "binomial", data = train)
summary(model_10)
vif(model_10)

#removing Education.x2 (Insignificant)

model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xSingle + JobInvolvement.x3, 
              family = "binomial", data = train)
summary(model_11)
vif(model_11)

#removing JobRole.xResearch.Director (Insignificant)

model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + JobInvolvement.x3, 
              family = "binomial", data = train)
summary(model_12)
vif(model_12)

#removing Department.xResearch...Development (High Vif and Insignificant)

model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely +  
                Department.xSales + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + JobInvolvement.x3, 
              family = "binomial", data = train)
summary(model_13)
vif(model_13)

#removing Department.xSales (Insignificant)

model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely +  
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + JobInvolvement.x3, 
              family = "binomial", data = train)
summary(model_14)
vif(model_14)

#removing JobInvolvement.x3 (Insignificant)

model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_15)
vif(model_15)

#removing BusinessTravel.xTravel_Rarely (High Vif and Insignificant)

model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_16)
vif(model_16)

#removing TrainingTimesLastYear (Insignificant)

model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_17)
vif(model_17)

#removing Age (Insignificant)

model_18<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Over_time + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_18)
vif(model_18)

final_model<-model_18
#######################################################################

### Model Evaluation

### Test Data ####

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

summary(test_pred)

test$prob <- test_pred
View(test)

test_actual_attrition<-factor(ifelse(test$Attrition==1,"Yes","No"))


#P_Cutoff>=0.5
test_predict_attrition<-factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

#P_Cutoff>=0.4
test_predict_attrition<-factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Finding the Optimal Probability Cutoff

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
dev.off()

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


op_p_cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]

#Optimal P_Cutoff
test_predict_attrition<-factor(ifelse(test_pred >= op_p_cutoff, "Yes", "No"))
conf_final <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
conf_final

#           Accuracy    : 0.7513 
#           Sensitivity : 0.7642          
#           Specificity : 0.7486 



#Creating dataframe for calc LIFT & GAIN Test and KSS test
l_g<-test[,c(1,61)]
#Sorting the df according to Predicted P's
l_g<-l_g[order(l_g$prob,decreasing = TRUE),]

total<-sum(l_g$Attrition)
total2<-1323-total

#Initializing Lif&Gain and KSS Variables
decile<-c(1:10)
Obs<-c(1,133,266,399,531,663,795,927,1059,1191)
Obs2<-c(133,266,399,531,663,795,927,1059,1191,1323)
obs3<-c(133,133,133,132,132,132,132,132,132,132)

Churn<-vector(mode = 'numeric',length = 10)
Cum_Churn<-vector(mode = 'numeric',length = 10)
Gain<-vector(mode = 'numeric',length = 10)
Lift<-vector(mode = 'numeric',length = 10)
Non_Churn<-vector(mode = 'numeric',length = 10)
Cum_Non_Churn<-vector(mode = 'numeric',length = 10)
Gain_rand<-c(1:10)/10

#temp variables
t<-0
t2<-0

#Calculating Lift&Gain and KSS
for(i in 1:10)
{
  Churn[i]<-sum(l_g$Attrition[Obs[i]:Obs2[i]])
  t<-t+Churn[i]
  Cum_Churn[i]<-t
  
  Non_Churn[i]<-obs3[i]-Churn[i]
  t2<-t2+Non_Churn[i]
  Cum_Non_Churn[i]<-t2
  
  Gain[i]<-Cum_Churn[i]/total
  Lift[i]<-Gain[i]/Gain_rand[i]
}
KSS<-Gain-Cum_Non_Churn/total2

Churn
Non_Churn
Cum_Churn
Cum_Non_Churn
Gain
Lift
KSS
plot(x=Lift,y=decile,type = 'o')
plot(x=Gain,y=decile,type ='o')
plot(x=decile,y=KSS,type = 'o')


########################################################################
#Conclusion
#Environment Satisfaction, Job Satisfaction and Work life balance,
#the better these are for employees the less are their chances of leaving the company.

#The more an employee works overtime on an average the more are the chances that he/she will leave the company.

#If an employee works with the same manager for a longer period of time the lesser 
#are the chances that employee will leave the company.

#Hire people with more experience as they are less likely to leave the company. 

#But if the person has worked in many companies then the chances that he/she will leave the company increases.

#Employees who are unmarried are prone to leaving the company.

