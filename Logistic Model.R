library(openxlsx)

# Read in the data
setwd("C:/Users/Andrew/Documents/Data Science Masters/Doing Data Science/Case Study 2/Case Study 2 Project/")
DDS <- read.xlsx("Data/CaseStudy2-data.xlsx", sheet= 1, colNames= T)

# Columns 9, 10, 22, and 27 have no meaningful data.  Remove them.
DDS <- DDS[,-c(9, 10, 22, 27)]

# Identify the numerical vs categorical columns
numerical <- c(1,4,6,7,9,11,12,13,15,17,18,19,21,22,23,24,25,26,27,28,29,30,31)
categorical <- c(1:31)[-numerical]

# Convert categorical columns into factors
DDS[categorical] <- lapply(categorical, function(col) as.factor(DDS[,col]))

# Convert Yes to 1 and No to 0
DDS[,2] <- ifelse(DDS[,2] == "Yes", 1, 0)

# In the logit model the log odds of the outcome is modeled as a 
# linear combination of the predictor variables where odds is the
# probablity of success divided by the probability of failure.  Then
# we take the log of that.
logmod <- glm(Attrition ~ Age + BusinessTravel + DailyRate + Department +
                DistanceFromHome + Education + EducationField +
                EnvironmentSatisfaction + Gender + HourlyRate +
                JobInvolvement + JobLevel + JobRole + JobSatisfaction +
                MaritalStatus + MonthlyIncome + MonthlyRate +
                NumCompaniesWorked + OverTime + PercentSalaryHike +
                PerformanceRating + RelationshipSatisfaction +
                StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany +
                YearsInCurrentRole + YearsSinceLastPromotion +
                YearsWithCurrManager, data = DDS, family = "binomial")

# Store the results
results <- summary(logmod)

# Find the statistically significant parameters at alpha = 0.001
significant <- which(results$coefficients[,"Pr(>|z|)"] <= 0.001)

# The statistically significant results are:

# BusinessTravelTravel
# DistanceFromHome 
# EnvironmentSatisfaction
# JobInvolvement
# JobSatisfaction
# MaritalStatusSingle 
# NumCompaniesWorked
# OverTimeYes
# YearsSinceLastPromotion

# Now run the model again on just those parameters
logmod2 <- glm(Attrition ~ BusinessTravel + DistanceFromHome + 
                EnvironmentSatisfaction + JobInvolvement + JobSatisfaction +
                MaritalStatus + NumCompaniesWorked + OverTime +
                YearsSinceLastPromotion, data = DDS, family = "binomial")
results2 <- summary(logmod2)
significant2 <- which(results2$coefficients[,"Pr(>|z|)"] <= 0.001)

# The statistically significant results are:

# BusinessTravelTravel
# DistanceFromHome 
# EnvironmentSatisfaction
# JobInvolvement
# JobSatisfaction
# MaritalStatusSingle
# OverTimeYes

# Run one more time to make sure
logmod3 <- glm(Attrition ~ BusinessTravel + DistanceFromHome + 
                 EnvironmentSatisfaction + JobInvolvement + JobSatisfaction +
                 MaritalStatus + OverTime, data = DDS, family = "binomial")
results3 <- summary(logmod3)
significant3 <- which(results3$coefficients[,"Pr(>|z|)"] <= 0.001)

# Sort the results by order of greatest effect.
answer <- sort(results3$coefficients[,"Estimate"], decreasing = T)

# The winners are:
#                         Estimate      Cause
# 1) BusinessTravel       1.596136      Frequently
# 2) OverTime             1.561474      Yes
# 3) MaritalStatus        1.296487      Single

# Now let's see what this means in terms of percent change
answer <- (exp(answer) - 1) * 100

# The final answer represents what percentage higher or lower a person
# is to quit vs other options in the same parameter.  For example,
# someone who puts in overtime (OverTimeYes) is
# (e^(1.561474) - 1)*100% = 376.58%
# more likely to quit than someone who doesn't put in overtime (OverTimeNo)