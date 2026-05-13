library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(corrplot) # Visualization of a Correlation Matrix
library(rpart) # Recursive Partitioning and Regression Trees
library(rpart.plot) # Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'
library(randomForest) # Breiman and Cutlers Random Forests for Classification and Regression
library(caret) # Classification and Regression Training
library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2'

df <- read.csv("insurance.csv")
head(df)
# Conversion Factor Variable
df$sex <- as.factor(df$sex)
df$smoker <- as.factor(df$smoker)
df$region <- as.factor(df$region)
#Basic statistical summary
summary(df)
# Group statistics by smoking status
smoker_stats <- df %>%
  group_by(smoker) %>%
  summarise(
    count = n(),
    mean_charges = mean(charges),
    sd_charges = sd(charges),
    median_charges = median(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )
smoker_stats
sex_stats <- df %>%
  group_by(sex) %>%
  summarise(
    count = n(),
    mean_charges = mean(charges),
    sd_charges = sd(charges),
    mean_bmi = mean(bmi),
    mean_age = mean(age)
  )
sex_stats
# By region
region_stats <- df %>%
  group_by(region) %>%
  summarise(
    count = n(),
    mean_charges = mean(charges),
    median_charges = median(charges),
    mean_bmi = mean(bmi),
    smoker_rate = sum(smoker == "yes") / n() * 100
  )
region_stats

#Correlation matrix (numerical variables)
numeric_vars <- df[, c("age", "bmi", "children", "charges")]
cor_matrix <- cor(numeric_vars)
cor_matrix

#Histogram of Medical Costs Distribution
p1 <- ggplot(df, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
  labs(title = "Distribution of Medical Costs", x = "Medical Costs", y = "Frequency") +
  theme_minimal() +
  geom_vline(aes(xintercept = mean(charges)), 
             color = "red", linetype = "dashed", size = 1)+
  theme(plot.title = element_text(hjust = 0.5))
print(p1)
#Boxplot: Smoker vs Non-Smoker Medical Costs
p2 <- ggplot(df, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Impact of Smoking Status on Medical Costs", 
       x = "Smoker", 
       y = "Medical Costs") +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightgreen", "yes" = "salmon"))+
  theme(plot.title = element_text(hjust = 0.5))
print(p2)

#Scatter Plot: Age vs Medical Costs (Colored by Smoking Status)
p3 <- ggplot(df, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Age and Medical Costs", x = "Age",
       y = "Medical Costs") +
  theme_minimal() +
  scale_color_manual(values = c("no" = "blue", "yes" = "red"))+
  theme(plot.title = element_text(hjust = 0.5))
print(p3)

#Scatter Plot: BMI vs Medical Costs (Faceted by Smoking Status)
p4 <- ggplot(df, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~smoker) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between BMI and Medical Costs \n(By Smoking Status)", 
       x = "BMI", y = "Medical Costs") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
p4
#Boxplot: Number of Children vs Medical Costs
p5 <- ggplot(df, aes(x = factor(children), y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Impact of Number of Children on Medical Costs", 
       x = "Number of Children", y = "Medical Costs") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
p5


ggcorrplot(cor_matrix,method = "circle",type = "lower",lab = T)+
  labs(title = "Correlation Matrix of Numeric Variables")+
  theme(plot.title = element_text(hjust = 0.5))

# 3.7 Combined Plot of Charges by Region and Smoking Status
p6 <- ggplot(df, aes(x = region, y = charges, 
                     fill = smoker)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Impact of Smoking Status on Medical Costs Across Different Regions", 
       x = "Region", y = "Medical Costs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
print(p6)


#Training/Test Set Division
set.seed(42)
train_index <- createDataPartition(df$charges, p = 0.7, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]
cat("Number of training set samples:", nrow(train), "\n")
cat("Number of samples in the test set:", 
    nrow(test), "\n")
# Model 1: Multiple Linear Regression
model_lm <- lm(charges ~ age + sex + bmi + 
                 children + smoker + region, 
               data = train)
summary(model_lm)

# Model 2: Decision Tree Regression
model_tree <- rpart(charges ~ age + sex + bmi + 
                      children + smoker + region,
                    data = train, method = "anova")
# Visual Decision Tree
rpart.plot(model_tree,
           main = "Decision tree regression model", 
           type = 3, extra = 101)

# Model 3: Random Forest Regression
set.seed(1)
model_rf <- randomForest(charges ~ age + sex + 
                           bmi + children + smoker + region,
                         data = train, 
                         ntree = 500, 
                         importance = TRUE)
print(model_rf)
# Variable Importance
importance_df <- importance(model_rf)
varImpPlot(model_rf, main = "Variable importance of random forest")

# Test set prediction and evaluation
pred_lm <- predict(model_lm, newdata = test)
pred_tree <- predict(model_tree, newdata = test)
pred_rf <- predict(model_rf, newdata = test)
actual <- test$charges
# Evaluation Function
evaluate_model <- function(actual, predicted, model_name) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  return(data.frame(
    Model = model_name,
    RMSE = round(rmse, 2),
    MAE = round(mae, 2),
    R2 = round(r2, 4)
  ))
}

# Evaluate three models
results_lm <- evaluate_model(actual, pred_lm, "Linear Regression")
results_tree <- evaluate_model(actual, pred_tree, "Decision Tree")
results_rf <- evaluate_model(actual, pred_rf, "Random Forest")
# Merged Results
comparison <- rbind(results_lm, results_tree, results_rf)
print(comparison)


