# Coding and Schema by Samuel Kadyebo, for Team Stars'
# ie. Samuel Kadyebo, Faiz Ikramulla and Andrew Wilkins 
# in Case Study Project 2 ie - MLR to identify significant[3] 
# parameters that lead to attrition - 

# Load the following packages
library(tidyquant)  # Loads tidyverse and several other pkgs 
library(readxl)     # Super simple excel reader
library(vip)        # ML global interpretation
library(ggplot2)    # visualization pkg leveraged by above packages
library(caret)      # ML model building
library(h2o)        # ML model building
library(lime)       # Explain complex black-box ML models ie ML local interpretation
library(recipes)    # Preprocessing for machine learning

#1st we look at the Data - The Structure
CaseStudy2_data <- read_excel("C:/Users/Kadyebo/Desktop/SMU/6306/CaseStudy2-data.xlsx")
View(CaseStudy2_data[1:10,]) # View first 10 rows
class(CaseStudy2_data)

Starsdataset <- CaseStudy2_data %>%
  mutate_if(is.character, as.factor) %>%
  select(Attrition, everything()) # changing character data types to factors so we can use H2O.

glimpse(Starsdataset) # checking our new processed dataset: Starsdataset

recipe_obj <- Starsdataset %>%
  recipe(formula = Attrition ~ .) %>%
  step_rm(EmployeeNumber) %>%
  step_zv(all_predictors()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  prep(data = Starsdataset)

bakeStarsdataset <- bake(recipe_obj, new_data = Starsdataset) 

# building Employee Attrition model 
h2o.init() # initialize the Java Virtual Machine (JVM) for use with H2O
h2o.no_progress() # Turn off output of progress bars

## Converting the data to an h2o object
# Split data into Train/Validation/Test Sets
Starsdataset_h2o <- as.h2o(bakeStarsdataset)

splitStarsdataset_h2o <- h2o.splitFrame(Starsdataset_h2o, c(0.7, 0.15), seed = 1234 )

trainStarsdataset_h2o <- h2o.assign(splitStarsdataset_h2o[[1]], "train" ) # 70% training data
validStarsdataset_h2o <- h2o.assign(splitStarsdataset_h2o[[2]], "valid" ) # 15% validation data
testStarsdataset_h2o  <- h2o.assign(splitStarsdataset_h2o[[3]], "test" )  # 15% test data

# Setting the target (what we aim to predict) and feature (what we use to model 
# the prediction) names for h2o ie Attrition as target, and every other column 
# as the features
y <- "Attrition"
x <- setdiff(names(trainStarsdataset_h2o), y)

# Now run the automated machine learning 
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = trainStarsdataset_h2o,
  leaderboard_frame = validStarsdataset_h2o,
  max_runtime_secs  = 15
)

# Extract leader model
automl_leader <- automl_models_h2o@leader

# Predict on hold-out set, test_h2o
pred_h2o <- h2o.predict(object = automl_leader, newdata = testStarsdataset_h2o)

# Prep for performance assessment
test_performance <- testStarsdataset_h2o %>%
  tibble::as_tibble() %>%
  select(Attrition) %>%
  add_column(pred = as.vector(pred_h2o$predict)) %>%
  mutate_if(is.character, as.factor)
test_performance

# Confusion table counts
confusion_matrix <- test_performance %>%
  table() 
confusion_matrix

# Performance analysis
tn <- confusion_matrix[1]
tp <- confusion_matrix[4]
fp <- confusion_matrix[3]
fn <- confusion_matrix[2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
misclassification_rate <- 1 - accuracy
recall <- tp / (tp + fn)
precision <- tp / (tp + fp)
null_error_rate <- tn / (tp + tn + fp + fn)

tibble(
  accuracy,
  misclassification_rate,
  recall,
  precision,
  null_error_rate
) %>% 
  transpose() 

# NOW VISUALIZATION

# ranger model --> model type not built in to LIME
index <- 1:5
train_obs <- Starsdataset[-index, ]
local_obs <- Starsdataset[index, ]

fit.ranger <- ranger::ranger(
  Attrition ~ ., 
  data = train_obs, 
  importance = 'impurity',
  probability = TRUE
)
vip(fit.ranger) + ggtitle("RF Hierarchy of Variables Impacting the objective function")


# Using LIME to understand what causes attrition? 
class(automl_leader) # to identify the class of the model leader object

# Seting up lime::model_type() function for h2o to work with LIME
model_type.H2OBinomialModel <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  #
  # x is our h2o model
  
  return("classification")
}

# Seting up lime::predict_model() function for h2o
predict_model.H2OBinomialModel <- function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  pred <- h2o.predict(x, as.h2o(newdata))
  
  # return probs
  return(as.data.frame(pred[,-1]))
  
}

# Test our predict_model() function
predict_model(x = automl_leader, newdata = as.data.frame(testStarsdataset_h2o[,-1]), type = 'raw') %>%
  tibble::as_tibble()

# Run lime() on training set
explainer <- lime::lime(
  as.data.frame(trainStarsdataset_h2o[,-1]), 
  model          = automl_leader, 
  bin_continuous = FALSE)

# Run explain() on explainer
explanation <- lime::explain(
  as.data.frame(testStarsdataset_h2o[1:4,-1]), 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 4,
  n_permutations = 500,
  kernel_width = 1)

# Feature Importance Visualization
plot_features(explanation) +
  labs(title = "HR Predictive Analytics: LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 4 Cases Shown")
plot_explanations(explanation)
# Focus on critical features of attrition
attrition_critical_features <- Starsdataset %>%
  tibble::as_tibble() %>%
  select(Attrition, TrainingTimesLastYear, JobRole, OverTime) %>%
  rowid_to_column(var = "Case")
attrition_critical_features
  
 