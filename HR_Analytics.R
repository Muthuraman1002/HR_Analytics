#_____________________________________________________________________________________________________________________________
# HR Analytics group case study for company XYZ
#_____________________________________________________________________________________________________________________________

# Problem statement: XYZ faces 15% attrition every year which is a considerable management problem. Identify variables that contribute
# to attrition ensuring action can be taken to curtail attrition.

# Goal: Predict the probability of attrition using logistic regression

# Data set: We are given 5 data sets containing records of 4410 employees. EmployeeID is an auto-generated unique key variable that is common
# in all the 5 data sets.

# The data sets are: 
# 1. general_data: Contains HR data such as Age, TotalWorkingYears (Experience), JobRole, JobLevel, MonthlyIncome, Education etc. Attrition is the response variable aka label. A total of 24 columns.
# 2. empl_surevy_data: Contains data seemingly from a employee engagement survey such as EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance. A total of 3 columns.
# 3. manager_survey_data: Contains data provided by managers such as PerformanceRating, JobInvolvement (2 columns)
# 4. in_time_data: For all days in 2015, this gives the log in time for each employee.
# 5. out_time_data: For all days in 2015, this gives the log out time for each employee.

#_____________________________________________________________________________________________________________________________

# Load Libraries needed

if(!require(MASS)) {
  install.packages("MASS")
}

if(!require(car)) {
  install.packages("car")
}

if(!require(e1071)) {
  install.packages("e1071")
}

if(!require(caret)) {
  install.packages("caret")
}

if(!require(ggplot2)) {
  install.packages("ggplot2")
}

if(!require(cowplot)) {
  install.packages("cowplot")
}

if(!require(caTools)) {
  install.packages("caTools")
}

if(!require(dplyr)) {
  install.packages("dplyr")
}

if(!require(ROCR)) {
  install.packages("ROCR")
}

if(!require(data.table)) {
  install.packages("data.table")
}

if(!require(GGally)) {
  install.packages("GGally")
}

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(ROCR)
library(data.table)
library(GGally)

#_____________________________________________________________________________________________________________________________
#Load all the data sets provided
#_____________________________________________________________________________________________________________________________


empl_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
in_time_data <- read.csv("in_time.csv", stringsAsFactors = F)
out_time_data <- read.csv("out_time.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)

#_____________________________________________________________________________________________________________
#Data cleaning and preparation
#_____________________________________________________________________________________________________________

# Do all the 5 data sets contain the same set off employee ids ?
length(setdiff(empl_survey_data$EmployeeID, general_data$EmployeeID))
length(setdiff(empl_survey_data$EmployeeID, in_time_data$X))
length(setdiff(empl_survey_data$EmployeeID, out_time_data$X))
length(setdiff(empl_survey_data$EmployeeID, manager_survey_data$EmployeeID))
# Yes. All the 5 data sets contain exactly the same set off employee ids.

#NA checks in each of the 5 data sets
#empl_survey_data
sapply(empl_survey_data, function(x) { sum(is.na(x))})
#% of NAs is very low. Since these are employee survey results, imputing values for NAs would be falsification. Hence,
#leaving the NAs as is.
empl_survey_data$EnvironmentSatisfaction[is.na(empl_survey_data$EnvironmentSatisfaction)] <- median(empl_survey_data$EnvironmentSatisfaction, na.rm = T)
empl_survey_data$JobSatisfaction[is.na(empl_survey_data$JobSatisfaction)] <- median(empl_survey_data$JobSatisfaction, na.rm = T)
empl_survey_data$WorkLifeBalance[is.na(empl_survey_data$WorkLifeBalance)] <- median(empl_survey_data$WorkLifeBalance, na.rm = T)


#manager_survey_data
sapply(manager_survey_data, function(x) { sum(is.na(x)) / nrow(manager_survey_data)})

#in_time_data
temp <- data.frame(sapply(in_time_data, function(x) { sum(is.na(x) )}))
temp
#The days on which all 4410 employees show as NA must be holidays.

#Treating NAs in in_time and out_time data
#Removing holidays
colsToRemove <- (rownames(temp)[which(temp$sapply.in_time_data..function.x... == 4410)])
colIndicesToRemove <- which(colnames(in_time_data) %in% colsToRemove)
in_time_data <- in_time_data[,-colIndicesToRemove]

#out_time_data
temp <- data.frame(sapply(out_time_data, function(x) { sum(is.na(x) )}))
temp
colsToRemove <- (rownames(temp)[which(temp$sapply.out_time_data..function.x... == 4410)])
colIndicesToRemove <- which(colnames(out_time_data) %in% colsToRemove)
out_time_data <- out_time_data[,-colIndicesToRemove]

#Check NAs again to see if there was any day when more than 85% employees were absent. If so, these could be treated as special days.
sum((data.frame(sapply(in_time_data, function(x) { sum(is.na(x) ) / nrow(in_time_data)}))[,1]) >= 0.85)
sum((data.frame(sapply(out_time_data, function(x) { sum(is.na(x) ) / nrow(out_time_data)}))[,1]) >= 0.85)
# No day was found when more than 85% of employees were absent. That is they had no in_time / out_time

#NA analysis on general_data
sapply(general_data, function(x) { sum(is.na(x)) / nrow(general_data)})
# A low % of NAs found in columns NumCompaniesWorked and TotalWorkingYears

# Identify if there is a correlation between NAs in NumCompaniesWorked and TotalWorkingYears
#When NumCompaniesWorked is NA, is TotalWorkingYears 0/NA ?
general_data$TotalWorkingYears[which(is.na(general_data$NumCompaniesWorked))]
#Not the case.

# When Number of companies worked is NA, then we can impute logically that the employee has worked only for XYZ if their
# TotalWorkingYears == YearsAtCompany.So, imputing NumCompaniesWorked to 1

general_data$NumCompaniesWorked[is.na(general_data$NumCompaniesWorked) & (general_data$TotalWorkingYears == general_data$YearsAtCompany)] <- 1

#Understanding NumCompaniesWorked == 0.
sum(general_data$NumCompaniesWorked == 0, na.rm = T)
#The definition of this column is "Total number of companies the employee has worked for".
#Hence, no employee can have 0, since it should include their current employer XYZ. This appears to be anomolous data.

sum(general_data$NumCompaniesWorked == 1 & general_data$TotalWorkingYears == general_data$YearsAtCompany, na.rm = T)
sum(general_data$NumCompaniesWorked == 1 & general_data$TotalWorkingYears > general_data$YearsAtCompany, na.rm = T)
unique(general_data$NumCompaniesWorked[general_data$TotalWorkingYears > general_data$YearsAtCompany])
unique(general_data$NumCompaniesWorked[general_data$TotalWorkingYears == general_data$YearsAtCompany])
general_data[(is.na(general_data$NumCompaniesWorked) & general_data$TotalWorkingYears == general_data$YearsAtCompany),]

View(general_data[general_data$NumCompaniesWorked == 0,])
# It appears that for all NumCompaniesWorked == 0, general_data$TotalWorkingYears == general_data$YearsAtCompany + 1
# Assuming there was some internship or temp workers for one 1 year, before becoming employees.
sum(general_data$NumCompaniesWorked == 0 & (general_data$TotalWorkingYears != (general_data$YearsAtCompany + 1)), na.rm = T)
# Similarly for NA
sum(is.na(general_data$NumCompaniesWorked) & (general_data$TotalWorkingYears == (general_data$YearsAtCompany + 1)), na.rm = T)
sum(general_data$NumCompaniesWorked == 1 & (general_data$TotalWorkingYears == (general_data$YearsAtCompany + 1)), na.rm = T)
# The 141 entries found above seem alright.

# The above indicates that for some employees this field indicates, number of past employers, and for others it includes
# current employer also. Going by the definition of numCompaniesWorked to include current employer, we can fix only the following,
# If numCompaniesWorked == 0, set it to 1.
# If numCompaniesWorked is NA & totalWorkingYears == YearsAtCompany + 1, set it to 1.

# It cannot fixed for employees where numCompaniesWorked >= 2 & general_data$TotalWorkingYears > general_data$YearsAtCompany


general_data$NumCompaniesWorked[general_data$NumCompaniesWorked == 0] <- 1
general_data$NumCompaniesWorked[is.na(general_data$NumCompaniesWorked) & (general_data$TotalWorkingYears == (general_data$YearsAtCompany + 1))] <- 1

unique(general_data$NumCompaniesWorked)
View(general_data[is.na(general_data$NumCompaniesWorked),])
# There are 9 NAs remaining who appear to have worked for multiple companies. It is not possible to correctly impute values for these.

#Similarly, checking if when TotalWorkingYears is NA, is NumCompaniesWorked 0?
general_data$NumCompaniesWorked[which(is.na(general_data$TotalWorkingYears))]
#When NumCompaniesWorked is 1, it is possible to impute, TotalWorkingYears = YearsAtCompany
general_data$TotalWorkingYears <- ifelse((is.na(general_data$TotalWorkingYears) & general_data$NumCompaniesWorked == 1), general_data$YearsAtCompany, general_data$TotalWorkingYears)
#There are also employees who have not specified total working years, but have worked for multiple companies.
#These are anamolous data values. Yet, it is not possible to decisively impute any value for experience.
#Leaving the data as is.


#Data preparation of in_time_data and out_time_data
# For each employee, calculate their average working hours per day.

# Manipulating the in_time out_time to identify number of working hours

workingHoursData <- data.frame(in_time_data[,1])
absenteeData <- data.frame(in_time_data[,1])
absenteeData$NumLeaves <- 0
colnames(workingHoursData) <- c("EmployeeID")
colnames(absenteeData) <- c("EmployeeID", "NumLeaves")


for(i in 2:ncol(in_time_data)) {
  colName <- colnames(in_time_data)[i]
  in_time_date_col <- as.POSIXct(in_time_data[,colName], "%Y-%m-%d %H:%M:%S")
  out_time_date_col <- as.POSIXct(out_time_data[,colName], "%Y-%m-%d %H:%M:%S")
  absenteeData$NumLeaves <- ifelse(is.na(in_time_date_col) & is.na(out_time_date_col),  (absenteeData$NumLeaves + 1), absenteeData$NumLeaves)
  workingHoursData <- cbind(workingHoursData,round(difftime(out_time_date_col, in_time_date_col, tz = "", units = "hours")))
}
View(workingHoursData)
workingHoursData <- data.frame(sapply(workingHoursData, as.numeric))
#Where working hours come out as NA, mark as 0.
# Assumption here is that the employee was absent on those days where both in_time and out_time were NA.
# It is assumed that the in_time and out_time data accounts for standard hours where the employee is travelling on business, and hence
# has not logged hours at office. This assumption is based on the fact that only about 5% of the data for frequent travellers
# had both NAs for in and out time.
workingHoursData <- workingHoursData %>% mutate_all(funs(replace(., is.na(.), 0)))

#Calculate average working hours per day for each employee.
workingHoursData$avgWorkingHours <- round(rowMeans(workingHoursData[,-1]))
# Merging the data sets

workingHoursDataForMerge <- workingHoursData[,c("EmployeeID", "avgWorkingHours")]
# This data frame holds the average working hours per employee in the year 2015.

#__________________________________________________________________________________________
#Merging all 5 data sets - empl_survey_data, manager_survey_data, general_data, workingHoursDataForMerge, absenteeData
#__________________________________________________________________________________________

merge1 <- merge(general_data, empl_survey_data, by = "EmployeeID")
merge2 <- merge(merge1, manager_survey_data, by = "EmployeeID")
merge3 <- merge(merge2, workingHoursDataForMerge, by = "EmployeeID")
final_hr_data <- merge(merge3, absenteeData, by = "EmployeeID")

#final_hr_data now contains the data set on which dummy variables can be created, outliers handled and numeric variables scale standardized.

#_________________________________________________________________________________________
# De-duplicate the data
#_________________________________________________________________________________________

sum(duplicated(final_hr_data[,-which(colnames(final_hr_data) == "EmployeeID")]) == TRUE)
#10 rows are duplicates

final_hr_data <- final_hr_data[-which(duplicated(final_hr_data[,-which(colnames(final_hr_data) == "EmployeeID")]) == TRUE),]

#_________________________________________________________________________________________
# Outlier Handling
#_________________________________________________________________________________________


#numerical varialbes
numerical_variables <- c("DistanceFromHome","MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsWithCurrManager", "YearsSinceLastPromotion", "avgWorkingHours", "NumLeaves")

View(data.frame(sapply(final_hr_data[,numerical_variables], 
                       function(x) quantile(x,seq(0,1,.01),na.rm = T))))

boxplot(final_hr_data$DistanceFromHome)$out
#No outliers in DistanceFromHome

boxplot(final_hr_data$MonthlyIncome)$out
min(boxplot(final_hr_data$MonthlyIncome)$out)
# 165550 could be a ceiling.
sum(final_hr_data$MonthlyIncome > 165550) / nrow(final_hr_data)
#Outliers are about 7.7% of the data.
#Ceil at 165550
min(final_hr_data$MonthlyIncome[final_hr_data$MonthlyIncome > 165550])
final_hr_data$MonthlyIncome[final_hr_data$MonthlyIncome >= 165950] <- 165550

boxplot(final_hr_data$NumCompaniesWorked)$out
sum(final_hr_data$NumCompaniesWorked, na.rm = T) / nrow(final_hr_data)
#2.6% of data is 9 which is seen as an outlier ( > 1.5 * IQR )
# Instead of handling it here, we can group the data to clump it with 8 later.

boxplot(final_hr_data$PercentSalaryHike)$out
# No outliers in PercentSalaryHike

boxplot(final_hr_data$TotalWorkingYears)$out
min(boxplot(final_hr_data$TotalWorkingYears)$out)
sum(final_hr_data$TotalWorkingYears > 28, na.rm = T) / nrow(final_hr_data)
#4% outliers. Can be handled. 
final_hr_data$TotalWorkingYears[final_hr_data$TotalWorkingYears > 28] <- 28


boxplot(final_hr_data$TrainingTimesLastYear)$out
# 0, 5, 6 are outliers
# what is their percent ?
sum(final_hr_data$TrainingTimesLastYear == 0 | final_hr_data$TrainingTimesLastYear >= 5) / nrow(final_hr_data)
# constitutes 16%
sum(final_hr_data$TrainingTimesLastYear == 0 | final_hr_data$TrainingTimesLastYear >= 6) / nrow(final_hr_data)
sum(final_hr_data$TrainingTimesLastYear >= 6) / nrow(final_hr_data)
#Can't eliminate 0, since it could be a contributing factor for attrition.
# over 5 is only 4%. Not handling this outlier since it is a miniscule percentage only. 
sum(final_hr_data$TrainingTimesLastYear >= 5) / nrow(final_hr_data)
# >= 5 is 12%. It is big enough to influence attrition. Retaining for now.

boxplot(final_hr_data$YearsAtCompany)$out
min(boxplot(final_hr_data$YearsAtCompany)$out)
sum(final_hr_data$YearsAtCompany > 17) / nrow(final_hr_data)
# 7% outliers. Handling here.
final_hr_data$YearsAtCompany[final_hr_data$YearsAtCompany > 17] <- 17

boxplot(final_hr_data$YearsWithCurrManager)$out
min(boxplot(final_hr_data$YearsWithCurrManager)$out)
sum(final_hr_data$YearsWithCurrManager > 14) / nrow(final_hr_data)
# 0.9% outliers. Handling here.
final_hr_data$YearsWithCurrManager[final_hr_data$YearsWithCurrManager > 14] <- 14

boxplot(final_hr_data$YearsSinceLastPromotion)$out
min(boxplot(final_hr_data$YearsSinceLastPromotion)$out)
sum(final_hr_data$YearsSinceLastPromotion > 7) / nrow(final_hr_data)
# 7.2% outliers. This field could have a significant influence on attrition. Retaining outliers for now.

boxplot(final_hr_data$avgWorkingHours)$out
# This factor may be a significant influencing factor. Especially the outliers who work 1.5 times more hours than 3/4th of the workforce.
# Retaining for now.

boxplot(final_hr_data$NumLeaves)$out
#No outliers

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(final_hr_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(final_hr_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(final_hr_data, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(final_hr_data, aes(x="",y=avgWorkingHours))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "h",nrow = 2)


#__________________________________________________________________________________________
#Data understanding and creating derived variables
#__________________________________________________________________________________________

#Understanding the variables.

#1. Age
summary(final_hr_data$Age)
#Age ranges from 18 to 60.
ggplot(final_hr_data, aes(x = Age)) + geom_histogram()
# In general, this is a normal curve with max employees between 30 and 40.
# Binning Age into the following buckets
# 0 - 30: 1
# 30 - 40: 2
# 40 - 50: 3
# 50 - 60: 4

final_hr_data$age_bucket[final_hr_data$Age < 30] <- 1
final_hr_data$age_bucket[final_hr_data$Age >= 30 & final_hr_data$Age < 40] <- 2
final_hr_data$age_bucket[final_hr_data$Age >= 40 & final_hr_data$Age < 50] <- 3
final_hr_data$age_bucket[final_hr_data$Age >= 50] <- 4

summary(factor(final_hr_data$age_bucket))

#Remove Age.
final_hr_data$Age <- NULL


#2. Attrition
sum(final_hr_data$Attrition == "Yes") / nrow(final_hr_data)
#16% employee attrition in 2015.

#Attrition. Converting Attrition into a 1 / 0 column
final_hr_data$Attrition <- ifelse(final_hr_data$Attrition == "Yes", 1, 0) 

#3. Business Travel
summary(factor(final_hr_data$BusinessTravel))
#Max employees of type "Travel_Rarely", followed by "Travel_Frequently" and "Non-Travel"

#4. Department
summary(factor(final_hr_data$Department))
# Max employees in R&D, followed by Sales and HR.

#Understanding distribution of the combination of Department and BusinessTravel
summary(factor(paste(final_hr_data$Department, final_hr_data$BusinessTravel)))

#5. Distance from home
summary(final_hr_data$DistanceFromHome)
# Min of 1 and max of 29. Could have outliers. To be verified later.
summary(factor(final_hr_data$DistanceFromHome))
#Distance from home can be grouped into the following buckets:
# 1 - 5: Short (1)
# 6 - 10: Medium (2)
# 11 - 20: Long (3)
# 21 and above: Very long (4)
final_hr_data$DistanceFromHomeBucket[final_hr_data$DistanceFromHome >= 1 & final_hr_data$DistanceFromHome <= 5] <- 1
final_hr_data$DistanceFromHomeBucket[final_hr_data$DistanceFromHome > 5 & final_hr_data$DistanceFromHome <= 10] <- 2
final_hr_data$DistanceFromHomeBucket[final_hr_data$DistanceFromHome > 10 & final_hr_data$DistanceFromHome <= 20] <- 3
final_hr_data$DistanceFromHomeBucket[final_hr_data$DistanceFromHome > 20] <- 4

final_hr_data$DistanceFromHome <- NULL

summary(factor(final_hr_data$DistanceFromHomeBucket))

#6. Education
summary(factor(final_hr_data$Education))
# 1 'Below College'
# 2 'College'
# 3 'Bachelor'
# 4 'Master'
# 5 'Doctor'
# Max have bachelors degree followed by Masters.
# Understand combination of Education and Department
summary(factor(paste(final_hr_data$Education, final_hr_data$Department)))
# Seems ok

#7. Education Field
unique(final_hr_data$EducationField)
summary(factor(final_hr_data$EducationField))
# Understand combo of Edu field and dept.
summary(factor(paste(final_hr_data$EducationField, final_hr_data$Department)))
# There are HR employees who have studied life sciences, medical etc. Don't understand the significance of this field. 

#8. Employee Count
# Most employees studied Life sciences and Medical.
summary(final_hr_data$EmployeeCount)
#All values in Employee count is 1. Removing this column.
final_hr_data$EmployeeCount <- NULL

#9. Gender
summary(factor(final_hr_data$Gender))

#10. Job Level
summary(factor(final_hr_data$JobLevel))
summary(factor(paste(final_hr_data$Department,final_hr_data$JobLevel)))

#11. Job role
unique(final_hr_data$JobRole)
summary(factor(final_hr_data$JobRole))
summary(factor(paste(final_hr_data$Department, final_hr_data$JobRole)))
(final_hr_data %>% group_by(Department) %>% summarise(jobRoles = paste(unique(JobRole), collapse = ",")))[,2]
#Every department has all roles. This is anomolous. For example, how can there be a research scientist in HR or
# a Human resources job role in R&D ? Also, in HR there are only 3 HR people, but 39 lab technicians and even 36 research scientists!

(final_hr_data %>% group_by(JobRole) %>% summarise(jobLevels = paste(unique(paste(JobLevel, Department, collapse = ",")), collapse = ",")))[,2]
# Again, every JobRole is mapped to every possible Job Level in all departments!

#12. Marital Status 
summary(factor(final_hr_data$MaritalStatus))

#13. Monthly income
summary(final_hr_data$MonthlyIncome)
#Monthly income ranges from 10090 to 199990. The range is huge indicating outliers.
ggplot(final_hr_data, aes(x = MonthlyIncome)) + geom_histogram()
# Though monthly income can be bucketed, we are deciding to keep it as is, since there could be greater data clumping in this field
final_hr_data %>% group_by(JobLevel) %>% summarise(median(MonthlyIncome))
table(final_hr_data$JobLevel[final_hr_data$MonthlyIncome == max(final_hr_data$MonthlyIncome)])
View(final_hr_data[final_hr_data$MonthlyIncome == max(final_hr_data$MonthlyIncome),])

#14. NumCompaniesWorked
summary(factor(final_hr_data$NumCompaniesWorked))
# For most employees, XYZ is their first or second employer.
ggplot(final_hr_data, aes(x = NumCompaniesWorked)) + geom_histogram()
# it may be worthwhile to bin this variable into the following buckets, for the purpose of improved understanding and explainability
# 0 or 1: 1
# 2 or 3: 2
# 4 or 5: 3
# 6 or 7: 4
# 8 or 9: 5

final_hr_data$NumCompaniesWorkedBucket[final_hr_data$NumCompaniesWorked == 0 | final_hr_data$NumCompaniesWorked == 1] <- 1
final_hr_data$NumCompaniesWorkedBucket[final_hr_data$NumCompaniesWorked == 2 | final_hr_data$NumCompaniesWorked == 3] <- 2
final_hr_data$NumCompaniesWorkedBucket[final_hr_data$NumCompaniesWorked == 4 | final_hr_data$NumCompaniesWorked == 5] <- 3
final_hr_data$NumCompaniesWorkedBucket[final_hr_data$NumCompaniesWorked == 6 | final_hr_data$NumCompaniesWorked == 7] <- 4
final_hr_data$NumCompaniesWorkedBucket[final_hr_data$NumCompaniesWorked == 8 | final_hr_data$NumCompaniesWorked == 9] <- 5

#Remove NumCompaniesWorked
final_hr_data$NumCompaniesWorked <- NULL

summary(factor(final_hr_data$NumCompaniesWorkedBucket))

#15. PercentSalaryHike
summary(final_hr_data$PercentSalaryHike)
# Min 11%, max 25% and Average 15% hike!
unique(factor(final_hr_data$PercentSalaryHike))

summary(factor(final_hr_data$PercentSalaryHike))
View(final_hr_data[final_hr_data$PercentSalaryHike == 25,])
#Slicing this data into 3 groups. Low_range_hike (1) : 11 to 15; Mid_range_hike (2): 16 to 20; High-range-hike (3): 21 to 25
# This is done for improved explainability.
final_hr_data$SalaryHikeRangeBucket[final_hr_data$PercentSalaryHike >= 11 & final_hr_data$PercentSalaryHike <= 15] <- 1
final_hr_data$SalaryHikeRangeBucket[final_hr_data$PercentSalaryHike >= 16 & final_hr_data$PercentSalaryHike <= 20] <- 2
final_hr_data$SalaryHikeRangeBucket[final_hr_data$PercentSalaryHike >= 21 & final_hr_data$PercentSalaryHike <= 25] <- 3

final_hr_data$PercentSalaryHike <- NULL
summary(factor(final_hr_data$SalaryHikeRangeBucket))

#16. Over18
summary(factor(final_hr_data$Over18))
#All values are Y. REmoving
final_hr_data$Over18 <- NULL

#17. StandardHours
summary(factor(final_hr_data$StandardHours))
#All values are 8. REmoving
final_hr_data$StandardHours <- NULL

#18. StockOptionLevel
summary(factor(final_hr_data$StockOptionLevel))

#19. TotalWorkingYears
summary(factor(final_hr_data$TotalWorkingYears))
# This field can also be bucketed for better explainability as follows:
# [0 - 5): 1 (low experience level)
# [5 - 10): 2 (mid experience level)
# [10 - 20): 3 (High experience level)
# >= 20: 4 (Very high experience level)

final_hr_data$TotalWorkingYearsBucket[final_hr_data$TotalWorkingYears >= 0 & final_hr_data$TotalWorkingYears <  5] <- 1
final_hr_data$TotalWorkingYearsBucket[final_hr_data$TotalWorkingYears >= 5 & final_hr_data$TotalWorkingYears <  10] <- 2
final_hr_data$TotalWorkingYearsBucket[final_hr_data$TotalWorkingYears >= 10 & final_hr_data$TotalWorkingYears <  20] <- 3
final_hr_data$TotalWorkingYearsBucket[final_hr_data$TotalWorkingYears >= 20] <- 4


final_hr_data$TotalWorkingYears <- NULL
summary(factor(final_hr_data$TotalWorkingYearsBucket))

#20. TrainingTimesLastYear
summary(factor(final_hr_data$TrainingTimesLastYear))

#21. YearsAtCompany 
summary(factor(final_hr_data$YearsAtCompany))
ggplot(final_hr_data, aes(x = YearsAtCompany)) + geom_histogram()
# Verify that this fieldd is always = TotalWorkingYears
sum(general_data$TotalWorkingYears < general_data$YearsAtCompany, na.rm = T)

#22. Years since last promotion
summary(factor(final_hr_data$YearsSinceLastPromotion))
sum(final_hr_data$YearsSinceLastPromotion >= 10)
# This could be a significant factor. Retaining this field as is.
# Check that this field is always <= YearsAtCompany
sum(final_hr_data$YearsAtCompany < final_hr_data$YearsSinceLastPromotion)

#23. YearsWithCurrentManager
summary(factor(final_hr_data$YearsWithCurrManager))
# Again, check that this field is always <= YearsAtCompany
sum(final_hr_data$YearsAtCompany < final_hr_data$YearsWithCurrManager)

#24. EnvironmentSatisfaction
summary(factor(final_hr_data$EnvironmentSatisfaction))
# 1- Low, 2 - Medium, 3 - High, 4 - Very high
# Max is High.

#25. JobSatisfaction
summary(factor(final_hr_data$JobSatisfaction))
(1280 + 1317 ) / 4223
# 1- Low, 2 - Medium, 3 - High, 4 - Very high
# 61% with high/very high rating on job satisfaction

#26. Work life balance
summary(factor(final_hr_data$WorkLifeBalance))
# 1 - Bad, 2 - Good, 3 - Better, 4 - Best
# Max have voted 'Better'.

#27. JobInvolvement
summary(factor(final_hr_data$JobInvolvement))
# 1- Low, 2 - Medium, 3 - High, 4 - Very high
2501 / 4223
#59% have been rated highly involved.

#28. Performance rating
summary(factor(final_hr_data$PerformanceRating))
# 1 - Low, 2 - Good, 3 - Excellent, 4 - Outstanding
# There are only 2 ratings given out, excellent and outstanding.
3574 / 4223
#84% rated excellent!

#29. avg working hours
summary(factor(final_hr_data$avgWorkingHours))
sum(final_hr_data$avgWorkingHours < 8) / 4223
# 65% on an average work less than 8 hours (standard work hours)
sum(final_hr_data$avgWorkingHours > 8) / 4223
# 20% put in over time. 

#30. NumLeaves
summary(factor(final_hr_data$NumLeaves))
#Min 1 and max 24

#31. Employee id
anyDuplicated(final_hr_data$EmployeeID)
#Employee Id is the unique id

#__________________________________________________________________________________________________________________
# Univariate analysis - visualization
#__________________________________________________________________________________________________________________

bar_theme1<-  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                    legend.position="none")


#Business Travel
plot_grid(ggplot(final_hr_data, aes(x = factor(age_bucket), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(BusinessTravel), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(Department), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(Education), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(EducationField), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(Gender), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1, align = 'h')
# 27% ofage_bucket == 1 have attrition
# 24% of those who travel frequently have attrition
# Departments perc attrition: 31% - HR; 15% - R & DD; 14% - Sales
# 18% attrition in Education level 2
# 41% attrition in Education Field "Human Resources"
# 16% among males and 15% among females

plot_grid(ggplot(final_hr_data, aes(x = factor(JobLevel), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(JobRole), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(MaritalStatus), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(StockOptionLevel), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          align = 'h')
#17% of those with job level 2.
# 24% Research director
# 25% Single
# 17% with stock option level 2

plot_grid(ggplot(final_hr_data, aes(x = factor(EnvironmentSatisfaction), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(JobSatisfaction), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) +bar_theme1,
          ggplot(final_hr_data, aes(x = factor(WorkLifeBalance), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(JobInvolvement), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(PerformanceRating), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          align = 'h')

#24% of those with EnvironmentSatisfacction as 1 (low) have attrition.
# 22% with low JobSatisfaction
# 31% of those with bad work life balance
# 21% of low Job Involvement
# 17% of those with 4 rating

plot_grid(ggplot(final_hr_data, aes(x = factor(DistanceFromHomeBucket), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(NumCompaniesWorkedBucket), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(SalaryHikeRangeBucket), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          ggplot(final_hr_data, aes(x = factor(TotalWorkingYearsBucket), fill = factor(Attrition))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..)) + bar_theme1,
          align = 'h')

#18% where distance from home bucket is 3 (10 - 20)
# 22% where num of companies worked is 6 or 7
# 18% where salary hike range is over 20%
# 32% where Total working years is under 5.

# Box plot for numerical variables
box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(final_hr_data, aes(x=factor(Attrition),y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(final_hr_data, aes(x=factor(Attrition),y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(final_hr_data, aes(x=factor(Attrition),y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
# Slightly lowere IQR and median for Monthly Income = 1 vs 0.
# No difference for TrainingTimesLastYear
# Slightly lower IQR and median for Years at company = 1 vs 0.

plot_grid(ggplot(final_hr_data, aes(x=factor(Attrition),y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(final_hr_data, aes(x=factor(Attrition),y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(final_hr_data, aes(x=factor(Attrition),y=avgWorkingHours, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(final_hr_data, aes(x=factor(Attrition),y=NumLeaves, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
# Slightly lower IQR and median for YearsSinceLastPromotion, YearsWithCurrManager for attition == 1
# Slightly higher IQR and median for avgWorkingHours for attition == 1
# No significant difference in distribution for NumLeaves

#_____________________________________________________________________________________________________
#Converting categorical variables into dummy variables
#______________________________________________________________________________________________________

categorical_variables_for_dummyfying<- c("BusinessTravel", "Department", "EducationField", "Gender", "JobRole", "MaritalStatus")
categorical_variables_for_dummyfying_cols <- which(colnames(final_hr_data) %in% categorical_variables_for_dummyfying)
final_hr_data_fact <- data.frame(sapply(final_hr_data[,categorical_variables_for_dummyfying_cols], factor))
dummies<- data.frame(sapply(final_hr_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =final_hr_data_fact))[,-1]))
final_hr <- cbind(final_hr_data[,-categorical_variables_for_dummyfying_cols],dummies) 

final_hr$Gender <- ifelse(final_hr_data$Gender == "Female", 1, 0)

#_____________________________________________________________________________________________________
# Scaling of numerical variables
#______________________________________________________________________________________________________


#Pick all numeric variables for scaling
numerical_variables_for_scaling <- c("MonthlyIncome", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager", "avgWorkingHours", "NumLeaves")

#correlation between these numeric variables
ggpairs(final_hr[, numerical_variables_for_scaling])
#The following factors are positively correlated: 
#YearsAtCompany & YearsSinceLastPromotion, YearsAtCompany & YearsWithCurrManager, YearsWithCurrManager & YearsSinceLastPromotion
#NumLeaves & avgWorkingHours are highly negatively correlated.



for(i in 1:length(numerical_variables_for_scaling)) {
  final_hr[, numerical_variables_for_scaling[i]] <- scale(final_hr[, numerical_variables_for_scaling[i]])
}

#_____________________________________________________________________________________________________
# Converting all ordinal variables to factors so as to treat them as categorical variables
#______________________________________________________________________________________________________

ordinal_variables_for_factorizing <- c("Education", "JobLevel", "StockOptionLevel", "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance", "JobInvolvement", "PerformanceRating")

ordinal_variables_for_factorizing_cols <- which(colnames(final_hr) %in% ordinal_variables_for_factorizing)
final_hr_fact <- data.frame(sapply(final_hr[,ordinal_variables_for_factorizing_cols], factor))
dummies<- data.frame(sapply(final_hr_fact, 
                            function(x) data.frame(model.matrix(~x-1,data = final_hr_fact))[,-1]))
final_hr <- cbind(final_hr[,-ordinal_variables_for_factorizing_cols],dummies) 


str(final_hr)
# 4223 rows & 58 columns

#_____________________________________________________________________________________________________
# Model creation
#_____________________________________________________________________________________________________

final_hr <- final_hr[,-which(colnames(final_hr) == "EmployeeID")] #Removing EmployeeID for model creation

set.seed(100)

indices <- sample.split(final_hr$Attrition, SplitRatio = 0.7)

train <- final_hr[indices,]

test <- final_hr[!(indices),]

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2058.3....57 coeff..nullDev 2580.1..resDev 1944.3

# Stepwise selection

model_2<- stepAIC(model_1, direction="both")

summary(model_2)
nrow(summary(model_2)$coefficients) - 1
#35 coefficients
#AIC - 2028.7, Proceeding to variable selection using vif and p value

step <- model_2

responseVar <- "Attrition"

formula <- as.formula(step$call$formula)
predictorVariables <- labels(terms(formula))

loopUntil <- length(predictorVariables) - 1
pvalueThreshold <- 0.05
betaCheck <- NULL

#Iteratively create the model until all variables are significant with a reasonable vif value.
# 1. Initial model is created with the formula obtained from stepAIC
# 2. Create the model
# 3. If all variables have *** significance, exit loop with that model
# 4. Calculate VIF on the model
# 5. Loop through the variables in descending order of vif (Variation inflation factor).
# 6. Identify a variable for removing based on the following conditions:
#     7.1 If pvalue of a variable is over 0.05, name that variable for removal
# 7. If no variable is selected for removal, exit loop.
# 8. Repeat steps 2 - 7 

for(i in 1:loopUntil) {
  
  
  #Create the formula with the current set of predictor variables.
  formula <- reformulate(termlabels = predictorVariables, response = responseVar)
  
  print(paste("model ", i))
  
  #create the linear regression model
  model <- glm(formula, data = train, family = "binomial")
  
  model_summary <- summary(model)
  
  print(model_summary)
  
  #Check vif for the current model
  vif_model <- data.frame(vif(model))
  setDT(vif_model, keep.rownames = T)
  names(vif_model) <- c("variables", "vif")
  
  #Extract the p-values out of the model summary to make a side-by-side comparison with vif
  p_values_model <- data.frame(model_summary$coefficients[,4])
  setDT(p_values_model, keep.rownames = T)
  names(p_values_model) <- c("variables", "pvalue")
  
  #Stop iterating if all variables in the model have *** significance
  if (all(p_values_model$pvalue <= 0.001)) {
    print("All variables have *** significance. Exit loop.")
    break
  }
  
  #Also check if any variable's co-efficient has changed signs in the current iteration
  if(is.null(betaCheck)) {
    betaCheck <- setDT(data.frame(model$coefficients), keep.rownames = T)
    names(betaCheck) <- c("variables", "oldCoeff")
  }else {
    if("newCoeff" %in% names(betaCheck) ) {
      betaCheck$oldCoeff <- NULL
      names(betaCheck) <- c("variables", "oldCoeff")
    }
    
    newCoeffDT <- setDT(data.frame(model$coefficients), keep.rownames = T)
    names(newCoeffDT) <- c("variables", "newCoeff")
    betaCheck <- merge(betaCheck, newCoeffDT, by = "variables")
    #If there is a change on sign, the product of the 2 co-efficients will be negative
    for(k in 1:nrow(betaCheck)) {
      beta <- betaCheck[k,]
      if((beta$oldCoeff * beta$newCoeff) < 0) {
        print(sprintf("Coefficient of %s changed signs after removing %s", beta$variables, var_to_remove ))
      }
    }
    
  }
  
  #For analysis of vif and p-value together
  vif_p_value <- merge(vif_model, p_values_model, by = "variables")
  #store in descending order of vif
  vif_p_value <- vif_p_value %>% arrange(-vif)
  View(vif_p_value)
  
  
  
  #Identify the varible to remove.
  #Look into vif in descending order, and compare the variable's p-value
  #If the p-value is seen to be over 0.05 (insignificant) and progressively insignificant, remove that variable
  
  var_to_remove <- NULL
  
  for (i in 1:nrow(vif_p_value)) {
    if(vif_p_value[i,"pvalue"] > pvalueThreshold) {
      var_to_remove <- vif_p_value[i,"variables"]
      break
    }
  }
  
  if(is.null(var_to_remove)) {
    break
  }
  
  
  print(sprintf("Removing variable %s", var_to_remove))
  
  #Create the new set of predictor variables by removing var_to_remove
  predictorVariables <- predictorVariables[!(predictorVariables == var_to_remove )]
  
  
}


predictorVariables

summary(model)
nrow(summary(model)$coefficients) - 1
#29 Coefficients
#AIC - 2033.4


model_x1 <- model
vif(model_x1)


createModel <- function(dataset, predictors, response )
{
  #Create the formula with the current set of predictor variables.
  formula <- reformulate(termlabels = predictors, response = response)
  
  #create the linear regression model
  model <- glm(formula, data = dataset, family = "binomial")
  
  print(summary(model))
  
  return(model)
  
}

#Removing NumLeaves its pvalue is found to be high (0.021011) and vif value is greatest among * rated variables

predictorVariables <- predictorVariables[!(predictorVariables == "NumLeaves")]

model_x2 <- createModel(train, predictorVariables, responseVar)
vif(model_x2)
#AIC - 2036.7

#Removing JobInvolvment.x3 since it has only * significance p value and higher vif
predictorVariables <- predictorVariables[!(predictorVariables == "JobInvolvement.x3")]

model_x3 <- createModel(train, predictorVariables, responseVar)
vif(model_x3)

#Removing Education.x2 since it has only * significance p value and higher vif
predictorVariables <- predictorVariables[!(predictorVariables == "Education.x2")]
model_x4 <- createModel(train, predictorVariables, responseVar)
vif(model_x4)

#Removing JobLevel.x5 since it has only * significance p value and higher vif
predictorVariables <- predictorVariables[!(predictorVariables == "JobLevel.x5")]
model_x5 <- createModel(train, predictorVariables, responseVar)
vif(model_x5)

#Removing BusinessTravel.xTravel_Rarely since it has only ** p value
predictorVariables <- predictorVariables[!(predictorVariables == "BusinessTravel.xTravel_Rarely")]
model_x6 <- createModel(train, predictorVariables, responseVar)
vif(model_x6)

#Removing Department.xSales since it has highest VIF 3.87
predictorVariables <- predictorVariables[!(predictorVariables == "Department.xSales")]
model_x7 <- createModel(train, predictorVariables, responseVar)
vif(model_x7)

#Removing Department.xResearch...Development since it is insignificant
predictorVariables <- predictorVariables[!(predictorVariables == "Department.xResearch...Development")]
model_x8 <- createModel(train, predictorVariables, responseVar)
vif(model_x8)

#Removing JobRole.xResearch.Scientist since it has only ** significance
predictorVariables <- predictorVariables[!(predictorVariables == "JobRole.xResearch.Scientist")]
model_x9 <- createModel(train, predictorVariables, responseVar)
vif(model_x9)

#Removing JobRole.xLaboratory.Technician since it has only ** significance
predictorVariables <- predictorVariables[!(predictorVariables == "JobRole.xLaboratory.Technician")]
model_x10 <- createModel(train, predictorVariables, responseVar)
vif(model_x10)

#Removing JobRole.xSales.Executive since it has only ** significance
predictorVariables <- predictorVariables[!(predictorVariables == "JobRole.xSales.Executive")]
model_x11 <- createModel(train, predictorVariables, responseVar)
vif(model_x11)

#Removing WorkLifeBalance.x4 since it has highest VIF and only ** significance
predictorVariables <- predictorVariables[!(predictorVariables == "WorkLifeBalance.x4")]
model_x12 <- createModel(train, predictorVariables, responseVar)
vif(model_x12)

#Removing WorkLifeBalance.x2 since it has highest VIF and only ** significance
predictorVariables <- predictorVariables[!(predictorVariables == "WorkLifeBalance.x2")]
model_x13 <- createModel(train, predictorVariables, responseVar)
vif(model_x13)

#Removing JobRole.xResearch.Director since it has only * significance
predictorVariables <- predictorVariables[!(predictorVariables == "JobRole.xResearch.Director")]
model_x14 <- createModel(train, predictorVariables, responseVar)
vif(model_x14)

# All variables are *** significant, and have vif < 2
# Naming this as the final model

final_model <- model_x14

#_____________________________________________________________________________________________________
# Model Evaluation
#_____________________________________________________________________________________________________

#Predict probabilities of attrition for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

# Let's see the summary 
summary(test_pred)

test$prob <- test_pred
View(test)

# Using probability cutoff of 50% to compare actual versus predicted
test_pred_quit <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_quit <- factor(ifelse(test$Attrition==1,"Yes","No"))

confusionMatrix(test_pred_quit, test_actual_quit, positive = "Yes")
#Accuracy - 86.84%
#Sensitivity - 28.5%
#Specificity - 97.8%

# Using probability cutoff of 40% to compare actual versus predicted
test_pred_quit <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))


test_conf <- confusionMatrix(test_pred_quit, test_actual_quit, positive = "Yes")
test_conf
#Accuracy - 86.5%
#Sensitivity - 40.0%
#Specificity - 95.2%

# Finding the optimal probalility cutoff as the point where the accuracy, sensitivity and specificity converge 

#Given a probabitlity cut off compares actual versus predicted and outputs performance measures
#Accuracy, sensitivity and specificity
perform_fn <- function(cutoff) 
{
  predicted_quit <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_quit, test_actual_quit, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.8 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


#Plotting line graphs for accuracy, specificity and sensitivity at various probability cut off points.
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)

box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]


# Let's choose a cutoff value of 0.169596 for final model

test_cutoff_quit <- factor(ifelse(test_pred >=0.169596, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_quit, test_actual_quit, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#At apprx. 0.17 probability cut off the predicted model has a 
#Accuracy - 73.9%
#Sensitivity - 75%
#Specificity - 73.8%

View(test)

### KS -statistic - Test Data ######

test_cutoff_quit <- ifelse(test_cutoff_quit=="Yes",1,0)
test_actual_quit <- ifelse(test_actual_quit=="Yes",1,0)


#on testing  data
pred_object_test<- prediction(test_cutoff_quit, test_actual_quit)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#KS statistic - 48.8%

# Lift & Gain Chart 

# plotting the lift and gain matrix

gainlift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

#Gain and lift chart 
quit_decile = gainlift(test_cutoff_quit, test_pred, groups = 10)
View(quit_decile)
#This shows the model with .17 probability cut off is a high performing model with 100% attrition
#captured in the top 40% of the predicted probability

#_____________________________________________________________________________________________
# Model interpretation
#_____________________________________________________________________________________________

# Based on the sign of the coefficients in the final model, key factors impacting attrition are as follows:
#
# Positive impact (that is, additive increase in these factors produce a multiplicative increase in odds of attrition):
# MaritalStatus.xSingle, BusinessTravel.xTravel_Frequently, avgWorkingHours, YearsSinceLastPromotion, NumCompaniesWorkedBucket
#
#Negative impact (that, additive increase in these factors produce a multiplicative decrease in odds of attrition)
# JobSatisfaction.x4, EnvironmentSatisfaction.x4, EnvironmentSatisfaction.x3, EnvironmentSatisfaction.x2, JobSatisfaction.x3
# JobSatisfaction.x2, YearsWithCurrManager, TotalWorkingYearsBucket,WorkLifeBalance.x3,age_bucket,TrainingTimesLastYear


#_____________________________________________________________________________________________
#Recommendations
#_____________________________________________________________________________________________

#Looking at coefficients in the model in descending order.

View(summary(final_model)$coefficients)
# Single employees positively impact attrition the most. While very high job satisfaction impacts
# the least.

#Understanding job satisfaction for single employees
final_hr[final_hr$MaritalStatus.xSingle == 1, c("JobSatisfaction.x2", "JobSatisfaction.x3", "JobSatisfaction.x4")] %>% summarise(med = sum(JobSatisfaction.x2), high = sum(JobSatisfaction.x3), vhigh = sum(JobSatisfaction.x4) )
final_hr[final_hr$MaritalStatus.xSingle == 1, c("EnvironmentSatisfaction.x2", "EnvironmentSatisfaction.x3", "EnvironmentSatisfaction.x4")] %>% summarise(med = sum(EnvironmentSatisfaction.x2), high = sum(EnvironmentSatisfaction.x3), vhigh = sum(EnvironmentSatisfaction.x4) )
possibleEnvSatisLow <- sum(final_hr$MaritalStatus.xSingle == 1) - (244+392+444)
mean(final_hr_data[final_hr_data$MaritalStatus == "Single", c("TrainingTimesLastYear")])
#On an average 2.6 times single people have trained last year as against a population average of 3.
summary(final_hr_data$TrainingTimesLastYear)

#avgWorkingHours
summary(final_hr_data$avgWorkingHours)
sum(final_hr_data$avgWorkingHours > 5 & final_hr_data$Attrition == 1)

#YearsSinceLastPromotion
summary(factor(final_hr_data[final_hr_data$Attrition == 1,]$YearsSinceLastPromotion))
summary(factor(final_hr_data[final_hr_data$Attrition == 1 & final_hr_data$YearsSinceLastPromotion == 0,]$TotalWorkingYearsBucket))

#_____________________________________________________________________________________________________________________________
