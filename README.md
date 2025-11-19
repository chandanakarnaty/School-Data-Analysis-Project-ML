# School-Data-Analysis-and-Predictive-Modeling-Project
Data cleaning, visualization, PCA, and predictive modeling project using R to analyze school performance, economic disadvantage, and SAT outcomes. Includes regression models (full, stepwise, Ridge, Lasso) and insights on academic trends.

# Overview
This project analyzes school-level data containing 565 observations and 22 variables. 

The goal was to clean the dataset, explore insights related to economic disadvantage and academic outcomes, perform visualization, apply dimensionality reduction (PCA), and build predictive models including linear regression, stepwise regression, Ridge, and Lasso.

# Data Cleaning, Verification & Processing

  Converted **EconDisadvantage.Levels** into a factor variable (Low, Medium, High).

  Identified **469 missing values**, including 73 schools missing **Avg.SAT** scores.  

  Schools with **higher economic disadvantage were more likely to have missing SAT scores**.  

  Removed all missing values, reducing dataset from 565 → **434 complete records**.  

   Verified all missing values removed:

```r
sum(is.na(school.data))  # Output: 0
# Key Insights from Data Analysis
Low disadvantage schools → lowest econ disadvantage (avg ~8.9%)

Medium disadvantage schools → ~21.2%

High disadvantage schools → highest (avg ~48.4%)

Schools with higher disadvantage report significantly lower SAT averages and college attendance.

Assistant Levels 1–2 → higher college attendance

Assistant Levels 3–4 → lower college attendance

# Data Visualization
Histogram of economic disadvantage showed strong concentration in high-disadvantage schools.

Heatmap revealed strongest and weakest correlations among school variables.

Scatterplots showed relationship between economic disadvantage and college attendance.

Visuals confirmed that schools with high economic disadvantage have lower academic outcomes.

# Principal Component Analysis (PCA)
Removed non-numeric variables: Asst.Level, Avg.SAT, EconDisadvantage.Levels.

Normalized the remaining variables (mean ~0, SD ~1).

PCA performed using:

r
Copy code
pcs <- prcomp(school.data.pca.s, scale = FALSE)

# PCA Highlights
Total variance explained: 20

PC1 highest loading: 0.343

PC2 highest loading: 0.371

PC1 vs PC2 plot provided strong separation for high SAT score schools (>1600).

PC2 vs PC3 plot showed weaker separation since PC3 explains less variance.

Predictive Modeling
1 Full Multiple Linear Regression
Residuals somewhat random but showed uneven spread and outliers.

Model likely overfitted.

2 Stepwise Regression
Removed predictors with low contribution.

Produced a simpler, more interpretable model.

3️ Ridge Regression
Had lower training error but sometimes worse holdout performance due to shrinkage.

Stable, but not the best performer.

4️ Lasso Regression
Performed feature selection by shrinking coefficients to zero.

Fewer predictors, more stable residuals, less overfitting.

Best Performing Model
Lasso Regression

Best generalization on holdout set

Removes unnecessary predictors

Balanced accuracy + simplicity

Recommended model for production use.

Files in This Repository
Project Report (DOCX / PDF)

R Script (if uploaded — analysis, PCA, regression models)

README.md (documentation)

# Summary
This project demonstrates skills in:

Data cleaning & preprocessing

Exploratory data analysis

Data visualization

PCA & dimensionality reduction

Linear, Ridge & Lasso regression

Model comparison & selection

R programming & statistical modeling
