# Insurance Cost Prediction Models

Predictive modeling project using R to analyze medical insurance charges with linear regression, decision tree, and random forest models.

## Project Overview

This project analyzes individual medical insurance charges and compares three prediction models: linear regression, decision tree, and random forest.

## Dataset

The dataset contains 1,338 observations with variables including age, sex, BMI, children, smoker, region, and charges.

## Methods

The project includes exploratory data analysis, correlation analysis, and predictive modeling. The dataset was split into a training set and testing set using a 70:30 ratio.

## Model Performance

| Model | RMSE | MAE | R-squared |
|---|---:|---:|---:|
| Linear Regression | 5757.08 | 3967.68 | 0.7624 |
| Decision Tree | 4917.50 | 3212.57 | 0.8266 |
| Random Forest | 4400.76 | 2677.47 | 0.8612 |

## Skills Demonstrated

- Data cleaning and preparation
- Exploratory data analysis
- Data visualization
- Regression modeling
- Decision tree modeling
- Random forest modeling
- Model evaluation using RMSE, MAE, and R-squared
- Statistical interpretation

## Key Findings

Smoking status, age, and BMI were the most important predictors of medical expenses. The random forest model performed the best among the three models.

## Tools Used

- R
- tidyverse
- ggplot2
- caret
- rpart
- randomForest
- ggcorrplot
