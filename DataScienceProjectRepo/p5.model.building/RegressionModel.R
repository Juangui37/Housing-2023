print('I have switched over from python to R Studio"s to create my "Data Science Models".
Python is too complicated and I will not let it finish me...')

Print("In this R file I will create my regression model that will give me a predictive price given the dataset I have imported.")

# Load the "MASS" library for stepAIC function
library(MASS)
library(dplyr)

summary(ModelData)

#Since I am using AIC, I have to remove all the variables that only have 1 unique value becuase then they become redudant in the regression model.
sapply(ModelData, function(x) length(unique(x)))

#finding the variables that have 1 unique identifier and getting the unique identifier
ModelData %>% 
  select_if(~n_distinct(.) == 1) %>%
  summarise(across(everything(), ~toString(unique(.))))

# HouseStatus is only 'Closed' because this is filtered data to have no N/A's in ClosePrice. PropType is Single Family home and Bedrooms was always 3.
unique_counts <- sapply(ModelData, function(x) length(unique(x)))
aicdf <- ModelData[, unique_counts > 1]


# Now that I removed the variables that only have 1 unique value, I will now remove the variables that are useless to me for this regression
#For instance, all the Agent information variables
#Unique Keys, Change of statues, The extra Year variable's I created on Python which indicate that month year and day the house was sold.
#Finally, I will also remove GaragePark. I will remove this because the variable contains so many unique values that it causes my computer to freeze

aicdf_new <- aicdf[, !names(aicdf) %in% c("AgentID", "listingagent", "MLSNum", "StatusChangeTimestamp", "ListPrice", "PhoneNumber", "Email", "Year", "Year_Month", "Day_of_Week", "Address","GaragePark")]
summary(aicdf_new)

# The code below creates a regression model with all the variables left
model <- lm(ClosePrice ~ ., data = aicdf_new)
summary(model)

model.aic <- stepAIC(model, direction = "both", trace = FALSE)


# Print the model with lowest AIC
summary(model.aic)

#I have deiced not to remove any of the variables included in this model

#I will now make a Prediction based on the variables I have left:

# To make a prediction, I have to include all the attributes that are in my regression model.
new_data <- data.frame(
  City = "East Hartford",
  Acres = 0.5,
  SqFtTotal = 2000,
  SqFtEstHeatedAboveGrade = 1412,
  StyleorRentType = "Ranch",
  RoomsTotal = 7,
  BedsTotal = 4,
  Bathrooms = 3,
  YearBuilt = 2010,
  DaysonMarket = 8)

# use the model to predict ClosePrice for the new data point
(predicted_price <- predict(model, newdata = new_data))

##### Now I will do the same thing but remove the attributes that don't matter to me. ####
aicdf_new <- aicdf[, !names(aicdf) %in% c("AgentID", "listingagent", "MLSNum", "StatusChangeTimestamp", "ListPrice", "PhoneNumber", "Email", "Year", "Year_Month", "Day_of_Week", "Address","GaragePark")]
aicdf_new2 <- aicdf_new[, !names(aicdf_new) %in% c("DaysonMarket","YearBuilt","SqFtEstHeatedAboveGrade")]
summary(aicdf_new2)

# create the regression model using stepAIC
model2 <- lm(ClosePrice ~ ., data = aicdf_new2)
summary(model2)

model.aic2 <- stepAIC(model2, direction = "both", trace = FALSE)


# Print the model with lowest AIC
summary(model.aic2)
### Again.... not changing anything

# I will now make a close price prediction of a house i would want.
new_data2 <- data.frame(
  City = "East Hartford",
  Acres = 0.5,
  SqFtTotal = 2000,
  StyleorRentType = "Ranch",
  RoomsTotal = 7,
  BedsTotal = 4,
  Bathrooms = 3)

# use the model to predict ClosePrice for the new data point
(predicted_price <- predict(model2, newdata = new_data))

print('My future house costs about: $332,202.80  ')
