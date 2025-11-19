rm(list=ls())

library(dplyr)
library(reshape2)
library(ggrepel)
library(gridExtra)
library(leaps)
library(fastDummies)
library(ggplot2)
library(caret)

options(scipen=999)

datasummary = function(data.df) {
    data.frame(median=sapply(data.df, median),
               mean=sapply(data.df, mean),
               sd=sapply(data.df, sd),
               min=sapply(data.df, min),
               max=sapply(data.df, max),
               length=sapply(data.df, length),
               miss.value=sapply(data.df,
                               function(x) sum(length(which(is.na(x))))))
}

collectMetrics <- function(model, train.df, holdout.df, dp_var, nPredictors) {
# model -- model
# train.df -- training set
# holdout.df -- holdout set
# dp_var -- dependent variable name, e.g. "Avg.SAT",
# nPredictors -- number of predictors, only needed for ridge and lasso regression
    if (missing(nPredictors)) {
        coefs = coef(model$finalModel)
        nPredictors = length(coefs) - 1
    }
    return (cbind(
        CV=model$results %>% slice_min(RMSE) %>% dplyr::select(c(RMSE, MAE)),
        Training=mlba::regressionSummary(predict(model, train.df), train.df[,dp_var]),
        Holdout=mlba::regressionSummary(predict(model, holdout.df), holdout.df[,dp_var]),
        nPredictors=nPredictors
    ))
}

# load the data
school.data=readRDS("school.data.rds")

rm(list = ls())
# 1a. Check and remove duplicate records
n_dup <- sum(duplicated(school.data))
n_dup   
# Remove duplicates
school.data <- school.data[!duplicated(school.data), ]
# Verify no duplicates remain and show remaining count
sum(duplicated(school.data))
nrow(school.data)
# 1b. Plot missing values
library(visdat)
vis_miss(school.data)
# 1c. Calculate total missing values
sum(is.na(school.data))
# 1d. Use datasummary to identify variable with most missing values
setwd("C:/Users/chand/Downloads/school data")
rm(list = ls())
datasummary(school.data)
# Identify column with most missing values
miss_by_col <- colSums(is.na(school.data))
top_missing_col <- names(sort(miss_by_col, decreasing = TRUE))[1]
top_missing_col
# 1e. Subset rows where this variable is missing
school.data.missing <- school.data[is.na(school.data[[top_missing_col]])]
# 1f. Number of rows in school.data.missing
nrow(school.data.missing)
# 1g. Histogram of Economically.Disadvantaged.Pct from school.data.missing
library(ggplot2)
ggplot(school.data.missing, aes(x = Economically.Disadvantaged.Pct)) +
geom_histogram(binwidth = 2, fill = "lightgray", color = "black") +
labs(title = paste("Histogram of Econ. Disadvantage for rows missing", top_missing_col))
# 1h. Remove all records with ANY missing values
school.data <- school.data[complete.cases(school.data), ]
nrow(school.data)
# 1i. Confirm missing values removed
sum(is.na(school.data))
# 1j. Verify which percentage group is more accurate: gender or race
gender_total <- school.data$Males.Pct + school.data$Females.Pct
race_total <- school.data$African.Pct + school.data$Asian.Pct + school.data$Hispanic.Pct + school.data$White.Pct + school.data$Native.Pct
mean(abs(gender_total - 100))
mean(abs(race_total - 100))
# 1k. school.data$EconDisadvantage.Levels <- cut(
  school.data$Economically.Disadvantaged.Pct
  breaks = quantile(school.data$Economically.Disadvantaged.Pct, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  include.lowest = TRUE
  labels = c("Low", "Medium", "High")
# 1l. library(dplyr)
school.data %>%
  group_by(EconDisadvantage.Levels) %>%
  summarise(
    count = n(),
    avg_econ = mean(Economically.Disadvantaged.Pct, na.rm = TRUE)
  )
library(dplyr)
# Verify the source column exists
names(school.data)[grepl("Economically|Econ", names(school.data))]

# Recreate the levels column in place
school.data <- school.data %>%
  mutate(
    EconDisadvantage.Levels = cut(
      Economically.Disadvantaged.Pct,
      breaks = quantile(Economically.Disadvantaged.Pct,
                        probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Low", "Medium", "High")
    )
  )
# Confirm it exists now
table(school.data$EconDisadvantage.Levels, useNA = "ifany")
# 1l — compute averages by level
school.data %>%
  group_by(EconDisadvantage.Levels) %>%
  summarise(
    count = n(),
    avg_econ = mean(Economically.Disadvantaged.Pct, na.rm = TRUE)
  )

# 2a. Creating the performance groups
library(dplyr)
school.data <- school.data %>%
  mutate(
    Performance.Score = (Math.Pct.Proficient + Reading.Pct.Proficient) / 2,
    Performance.Level = cut(
      Performance.Score,
      breaks = quantile(Performance.Score, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  )
# 2a. Scatter
ggplot(school.data,
       aes(x = Males.Pct, y = Avg.SAT, color = African.Pct)) +
  geom_point(size = 5, alpha = 0.85) +
  labs(title = "Avg.SAT vs Males % (color = African %)",
       x = "Males (%)", y = "Avg SAT", color = "African %") +
  theme_bw()
# 2b. Boxplots
ggplot(school.data, aes(x = factor(Asst.Level), y = Avg.SAT)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = African.Pct),
             position = position_jitter(width = 0.2, height = 0),
             size = 2, alpha = 0.9) +
  labs(title = "Avg.SAT by Assistant Level",
       x = "Asst.Level", y = "Avg SAT", color = "African %") +
  theme_bw()

# 2c. Highest Avg.SAT outlier at Asst.Level = 1 
# Identify the outliers
idx_out <- which(lvl1$Avg.SAT > upper_fence)
# Reporting result
if (length(idx_out) > 0) {
  highest_idx <- idx_out[which.max(lvl1$Avg.SAT[idx_out])]
  school_name <- rownames(lvl1)[highest_idx]
  cat("Highest outlier at Asst.Level = 1 is:", school_name,
      "with Avg.SAT =", lvl1$Avg.SAT[highest_idx], "\n")
} else {
  best_idx <- which.max(lvl1$Avg.SAT)
  school_name <- rownames(lvl1)[best_idx]
  cat("No outliers. Highest Avg.SAT at Asst.Level = 1 is:", 
      lvl1$Avg.SAT[best_idx], "at", school_name, "\n")
}
best_idx
rownames(lvl1)[best_idx]
lvl1$Avg.SAT[best_idx]
table(school.data$Asst.Level)
head(school.data$Asst.Level)
library(dplyr)

# Filter only Level 1 schools
lvl1 <- school.data %>% filter(Asst.Level == "Level 1")

# Compute IQR and upper fence
lvl1 <- subset(school.data, Asst.Level = "Level 1")

Q1 <- quantile(lvl1$Avg.SAT, 0.25, na.rm = TRUE)
Q3 <- quantile(lvl1$Avg.SAT, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
upper_fence <- Q3 + 1.5 * IQR_value
cat("Highest Avg.SAT school at Assistant Level 1 is:", school_name,
    "with Avg.SAT =", school_sat, "\n")
rownames(school.data) <- school.data$School.Name
lvl1 <- subset(school.data, Asst.Level == "Level 1")
Q1 <- quantile(lvl1$Avg.SAT, 0.25, na.rm = TRUE)
Q3 <- quantile(lvl1$Avg.SAT, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
upper_fence <- Q3 + 1.5 * IQR_value
idx_out <- which(lvl1$Avg.SAT > upper_fence)
if (length(idx_out) > 0) {
  highest_idx <- idx_out[which.max(lvl1$Avg.SAT[idx_out])]
} else {
  highest_idx <- which.max(lvl1$Avg.SAT)
}
school_name <- rownames(lvl1)[highest_idx]
school_sat  <- lvl1$Avg.SAT[highest_idx]

cat("Highest Avg.SAT at Assistant Level 1 is:",
    school_name, "with Avg.SAT =", school_sat, "\n")
names(school.data)
# 2d. Create a histogram
library(ggplot2)
ggplot(school.data, aes(x = Attending.College.Pct, fill = Asst.Level)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  labs(title = "Histogram of College Attendance % by Assistant Level",
       x = "Percent Attending College",
       y = "Count",
       fill = "Assistant Level")
# 2e. create a histogram of Attending.College.Pct colored by EconDisadvantage.Leves using ggplot.
ggplot(school.data, aes(x = Attending.College.Pct, fill = EconDisadvantage.Levels)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  labs(title = "Histogram of College Attendance % by Economic Disadvantage Level",
       x = "Percent Attending College",
       y = "Count",
       fill = "Econ Disadvantage Level")
# 2f. Creating a new data frame
school.data.num <- school.data[, sapply(school.data, is.numeric)]
head(school.data.num)
# 2g. Calculate correlation coefficients for all variables
library(reshape2)
corr_matrix <- cor(school.data.num, use = "complete.obs")
corr_melt <- melt(corr_matrix)
ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Heatmap")
# 2h. Identifying two variables
cor_vals <- corr_matrix[,"Avg.SAT"]
highest_var <- names(sort(cor_vals, decreasing = TRUE))[2]   
lowest_var  <- names(sort(cor_vals, decreasing = FALSE))[1]
highest_var
lowest_var
library(ggrepel)
ggplot(school.data, aes_string(x = highest_var, y = lowest_var, color = "Avg.SAT", label = "Avg.SAT")) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  labs(title = paste("Scatter Plot of", highest_var, "vs", lowest_var, "Colored by Avg.SAT"),
       x = highest_var,
       y = lowest_var,
       color = "Avg.SAT")
# 3a. Create the new data frame
school.data.pca <- school.data[, !(names(school.data) %in% c("Asst.Level", "Avg.SAT", "EconDisadvantage.Levels"))]

# 3b. Normalize the data so the mean and standard deviation of each variable is 0 and 1
school.data.pca.s <- scale(school.data.pca)
# Check column means (should be 0)
apply(school.data.pca.s, 2, mean)
# Check column standard deviations (should be 1)
apply(school.data.pca.s, 2, sd)
# Total variance of all scaled variables
total_variance_original <- sum(apply(school.data.pca.s, 2, var))
total_variance_original

# 3c. principal component analysis
pcs <- prcomp(school.data.pca.s, scale = FALSE)

# 3d. The total variance of all principal components
# Variance of the each principal component
pc_variances <- apply(pcs$x, 2, var)
# Total variance of PCs
total_variance_pcs <- sum(pc_variances)
total_variance_pcs

# 3e. The results of the first two loadings
pcs$rotation[,1:2]

# 3f. Create the new scores data
scores <- data.frame(pcs$x)
scores <- cbind(scores, Avg.SAT = school.data$Avg.SAT)

# 3g. Use ggplot to create a scatter plot of PC1 vs PC2
library(ggplot2)
library(ggrepel)
# PC1 vs PC2
ggplot(scores, aes(x = PC1, y = PC2, color = Avg.SAT, label = Avg.SAT)) +
  geom_point(size = 5) +
  geom_text_repel(size = 3) +
  labs(title = "PC1 vs PC2 Colored by Avg.SAT")
# PC2 vs PC3
ggplot(scores, aes(x = PC2, y = PC3, color = Avg.SAT, label = Avg.SAT)) +
  geom_point(size = 5) +
  geom_text_repel(size = 3) +
  labs(title = "PC2 vs PC3 Colored by Avg.SAT")

# 3h. scatter plots (PC1 vs PC2 and PC2 vs PC3) side by side
library(gridExtra)
p1 <- ggplot(scores, aes(x = PC1, y = PC2, color = Avg.SAT)) +
  geom_point(size = 5) +
  labs(title = "PC1 vs PC2")
p2 <- ggplot(scores, aes(x = PC2, y = PC3, color = Avg.SAT)) +
  geom_point(size = 5) +
  labs(title = "PC2 vs PC3")
grid.arrange(p1, p2, ncol = 2)

install.packages("tidyverse")
library(tidyverse)
library(caret)
library(mlba)
set.seed(1)
install.packages("corrplot")
library(corrplot)
# compute correlation 
school.data.num <- school.data[, sapply(school.data, is.numeric)]
corr_matrix <- cor(school.data.num, use = "complete.obs")
# plot
corrplot(corr_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black")

# 4a. Correlation plot
school.data.mlr <- school.data %>%
  select(-EconDisadvantage.Levels,
         -Attending.College.Pct,
         -Economically.Disadvantaged.Pct)
# 4b. Split data into 60% training and 40% holdout sets
set.seed(1)
idx <- sample(1:nrow(school.data.mlr), size = 0.6 * nrow(school.data.mlr))
train.df   <- school.data.mlr[idx,]
holdout.df <- school.data.mlr[-idx,]

# c(i) Full model
set.seed(1)
trControl <- caret::trainControl(method = "cv", number = 5)
model.full <- caret::train(
  Avg.SAT ~ ., 
  data = train.df,
  method = "lm",
  trControl = trControl
)
# Save the metrics
metric.full <- collectMetrics(model.full, train.df, holdout.df, "Avg.SAT")
# Residual plot
pred_full  <- predict(model.full, newdata = train.df)
resid_full <- train.df$Avg.SAT - pred_full
plot(pred_full, resid_full,
     main = "Residual Plot - Full Model",
     xlab = "Predicted Avg.SAT", ylab = "Residuals",
     pch = 19, col = "dodgerblue3")
abline(h = 0, lty = 2, col = "red", lwd = 2)
names(train.df)

# (ii) Stepwise model
set.seed(1)
trControl <- caret::trainControl(method = "cv", number = 5)
model.both <- caret::train(
  Avg.SAT ~ .,
  data = train.df,
  method = "glmStepAIC",
  direction = "both",
  trControl = trControl
)
metric.stepwise <- collectMetrics(model.both, train.df, holdout.df, "Avg.SAT")
metric.stepwise


# (iii) Ridge regression model
set.seed(1)
lambda <- 10^seq(5, 2, by = -0.1) 
model.ridge <- caret::train(
  Avg.SAT ~ .,
  data = train.df,
  method = "glmnet",
  trControl = trControl,
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
# Plot Ridge Coefficient Shrinkage Path
plot(model.ridge$finalModel, xvar = "lambda", label = TRUE,
     main = "Ridge Regression Coefficient Path")


# (iv) Lasso model
set.seed(1)
lambda <- 10^seq(4, 0, by = -0.1)  
model.lasso <- caret::train(
  Avg.SAT ~ .,
  data = train.df,
  method = "glmnet",
  trControl = trControl,
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Residual plot 
pred_lasso  <- predict(model.lasso, newdata = train.df)
resid_lasso <- train.df$Avg.SAT - pred_lasso
plot(pred_lasso, resid_lasso,
     main = "Residual Plot - Lasso Model",
     xlab = "Predicted Avg.SAT", ylab = "Residuals",
     pch = 19, col = "darkorange")
abline(h = 0, lty = 2, col = "red", lwd = 2)
# number of selected predictors at best lambda
n_pred_lasso <- sum(coef(model.lasso$finalModel, s = model.lasso$bestTune$lambda)!= 0) - 1
metric.lasso <- collectMetrics(model.lasso, train.df, holdout.df, "Avg.SAT", n_pred_lasso)
# (vi) Create a new data frame
model.compare <- rbind(
  Full     = metric.full,
  Stepwise = metric.stepwise,
  Ridge    = metric.ridge,
  Lasso    = metric.lasso
)
model.compare





names(school.data)
school.data.mlr <- subset(school.data, select = -c(EconDisadvantage.Levels, White.Pct))
dim(school.data)
dim(school.data.mlr)









