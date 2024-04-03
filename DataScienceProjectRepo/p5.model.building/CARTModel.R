# Load necessary packages
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)


# Remove irrelevant variables
ModelData <- ModelData[, !names(ModelData) %in% c("AgentID", "listingagent", "MLSNum", "StatusChangeTimestamp", "PhoneNumber", "Email", "Year", "Year_Month", "Day_of_Week")]

# Define the target variable
ModelData$Budget <- as.factor(ifelse(ModelData$ListPrice < 290000, "Under $290k", "Over $290k"))
summary(ModelData)



#Partitioning
set.seed(99)

InTrain <- createDataPartition(y = ModelData$Budget, p= .75, list = FALSE)


Train_Data <- ModelData[InTrain,]

Test_Data <- ModelData[-InTrain,]


Train_Data$trainortest <- rep("train", nrow(Train_Data))
Test_Data$trainortest <- rep("test", nrow(Test_Data))
All_Data <- rbind(Train_Data, Test_Data)
summary(ModelData)
par(mfrow=c(3,3))

#boxplot(Vehicle$Year ~as.factor(trainortest), data = All_Data, horizontal = TRUE, main = "Vehicle Year Training & Test Boxplot", col = "blue")

# Create a list of numeric variable names
numeric_vars <- names(ModelData)[sapply(ModelData, is.numeric)]

# Loop over each numeric variable
for (var in numeric_vars) {
  # Create a boxplot for the variable, stratified by train/test data
  boxplot(get(var) ~ trainortest, data = All_Data, horizontal = TRUE, xlab = var,
          main = paste(var, "Training & Test Boxplot"), col = "blue")
}


# Perform Kruskal-Wallis test for all numeric variables
num_vars <- names(Filter(is.numeric, All_Data))
p_values <- sapply(num_vars, function(x) {
  kruskal.test(All_Data[[x]] ~ as.factor(trainortest), data = All_Data)$p.value
})

# Combine p-values with variable names and print
p_val_table <- data.frame(Variable = num_vars, P_Value = p_values)
print(p_val_table)


# Get the names of all categorical variables
cat_vars <- names(ModelData)[sapply(ModelData, is.factor)]

# Loop through each categorical variable
for(var in cat_vars) {
  # Create the contingency table for the variable and train/test split
  var_table <- table(All_Data[[var]], All_Data$trainortest)
  
  # Perform the chi-squared test and print the p-value
  p_val <- prop.test(var_table, correct = FALSE)$p.value
  cat(sprintf("The p-value for %s is: %f", var, p_val))
}
par(mfrow = c(1, 1))

# Removing variables: ListPricce & ClosePrice because List Price and ClosePrice would tell my model to just look directly as list price or close price when purchasing a house.
#HouseStatus & PropType because since they're only 1 level factor variables, they dont contribute to my model 
#Address because I don't want to be told to look for a specific address when buying a house.
names(Train_Data)
Train_Data <- Train_Data[,-1:-5]
names(Train_Data)
names(Test_Data)
Test_Data <- Test_Data[,-1:-5]
names(Test_Data)
Train_Data <- Train_Data[,-13]
Test_Data <- Test_Data[,-13]


Train_Cart <- rpart(Budget ~., data = Train_Data, method = "class")
rpart.plot(Train_Cart, type = 2, extra = 104, cex = 0.8)


#Baseline Model is 58%
#1st Node
# Predicting on training data
Pred_Train_Cart <- predict(object = Train_Cart, newdata = Train_Data, type = "class")

# Calculating confusion matrix and performance measures
confusionMatrix(data = Pred_Train_Cart, reference = Train_Data$Budget)

print("Accuracy : 0.8257")

#Testing Cart Model baseline & Accuracy
Test_Cart <- rpart(Budget ~., data = Test_Data, method = "class")
rpart.plot(Test_Cart,type = 2, extra = 104, cex = .8)
#Baseline Model (1st Node)
#59%
# Predicting on training data
Pred_Test_Cart <- predict(object = Test_Cart, newdata = Test_Data, type = "class")

# Calculating confusion matrix and performance measures
confusionMatrix(data = Pred_Test_Cart, reference = Test_Data$Budget)

print("Accuracy : 0.878")

#K-Fold Cross Validation

Train_Control <- trainControl(method = "cv", number = 50)

Train_Data_CCA <- na.omit(Train_Data)
model <- train(Budget ~. , data = Train_Data_CCA, method = "rpart", trControl = Train_Control)


print(model)
#This helped us find our cp (complexity Parameter) #. This # gives us a good balance between accuracy & complexity.
#This means we're not plotting every single data point (Making a crazy complex model) but, we are taking the most useful data points to plot a model.
plot(model$finalModel)
text(model$finalModel)
library(rattle)
#Use Package Rattle to plot the final model
fancyRpartPlot(model$finalModel, cex = 1.2, main = "Houses Less than $290,000 CART Model")
fancyRpartPlot(model$finalModel, cex = 2)


pred_Kfold_model <- rpart.predict(object = model, newdata = Train_Data)
(conf_mat <- confusionMatrix(pred_Kfold_model, Train_Data_CCA$Budget))

(73-58)/58


print("Our Baseline Accuracy for our model is 58%, The KFold Model Accuracy is 73%, our KFold Model Accuracy outpreformed our baseline model by 25%")

pred_kfold <- rpart.predict(object = model, newdata = Test_Data)
(conf_mat2 <- confusionMatrix(pred_kfold, Test_Data$Budget))

(70-59)/59
print("Our BaseLine Accuracy For our Model was 59%. The KFold Model had an accuracy of 70.12% for data it has never seen before (The Testing Data), This is 18.64% improvement over the baseline model.")



