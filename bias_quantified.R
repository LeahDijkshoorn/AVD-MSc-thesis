### Rename AV variables
names(donor)[names(donor) == "AV"] <- "AV_physical"
names(UMCG)[names(UMCG) == "AV"] <- "AV_echo"
names(lopez)[names(lopez) == "AV"] <- "AV_echo"

### ADULTS

# Subset data s.t. there are similar demographics to UMCG data
{
  donor_filtered_adult <- donor[donor$Age >= 18, ]
  donor_filtered_adult <- donor_filtered_adult[donor_filtered_adult$Weight < 105, ]
}

# Train best model structure on donor subset:
gam_donor_filtered_adult <- bam(logAV ~ Sex + 
                                  s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                                  s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                                  s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15) +
                                  ti(logWeight, sqrtHeight, bs="cs", k=c(5,5)) +
                                  ti(logWeight, sqrtHeight, bs="cs", by = Sex, k=c(5,5)) +
                                  ti(sqrtHeight, logAge, bs = "cs", k=c(5,5)) + 
                                  ti(sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5)) + 
                                  ti(logWeight, logAge, bs = "cs", k=c(5,5)) + 
                                  ti(logWeight, logAge, bs = "cs", by = Sex, k=c(5,5)) +
                                  ti(logWeight, sqrtHeight, logAge, bs = "cs", k=c(5,5,5)) +
                                  ti(logWeight, sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5,5)), data=donor_filtered_adult, family=gaussian())

summary(gam_donor_filtered_adult)

# Create pseudo-physical AV diameter measurements for UMCG data
UMCG$AV_physical <- exp(predict(gam_donor_filtered_adult, newdata = UMCG))

# Compute differences
UMCG$Differences <- UMCG$AV_physical - UMCG$AV_echo

# DIFFERENCE PLOTS:
{
  difference_age_plot <- ggplot(UMCG, aes(x = Age, y = Differences))  +
    geom_point( ) +  
    geom_smooth(data = UMCG, method = "lm", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Age in Years", y = "Difference (mm)") +
    theme_minimal() + theme(legend.position = "none")
  
  difference_weight_plot <- ggplot(UMCG, aes(x = Weight, y = Differences))   +
    geom_point( ) +  
    geom_smooth(data = UMCG, method = "lm",se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Weight (kg)", y = "Difference (mm)") +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
  
  difference_height_plot <- ggplot(UMCG, aes(x = Height, y = Differences))  +
    geom_point( ) +  
    geom_smooth(data = UMCG, method = "lm", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Height (cm)", y = "Difference (mm)") +
    theme_minimal() + theme(legend.position = "top", axis.title.y = element_blank())
}
grid.arrange(difference_age_plot, difference_weight_plot, difference_height_plot, nrow = 1)

# Using LM not GAM
# Not enough data to use a GAM and exploration does not indicate a GAM is necessary
bridging_model_UMCG <- lm(AV_echo ~ AV_physical + Age, data = UMCG)
summary(bridging_model_UMCG)


### 18 AND UNDER

# Regression Equation 3 (Lopez et al.)
  #   AV in cm originally
  Lopez <- function(BSA) {
    y_hat <- (1.48 * BSA^(0.5)) * 10
  }

# Use similar demographics that the models were based on:
{
  donor_filtered_under18 <- donor[donor$Age <= 18, ]
  donor_filtered_under18 <- donor_filtered_under18[donor_filtered_under18$Weight < 100, ]
}

### METHOD (1):

# Pseudo-echo AV Predictions
donor_filtered_under18$AV_echo <- Lopez(donor_filtered_under18$BSA)
  
bridging_model_lopez_1 <- bam(AV_echo ~ s(AV_physical, bs="cs", k=3) + s(Age, bs="cs", k=3) + s(Height, bs="cs", k=3) + s(Weight, bs="cs", k=3), data=donor_filtered_under18, family=gaussian())
summary(bridging_model_lopez_1)

donor_filtered_under18$AV_echo_1 <- predict(bridging_model_lopez_1, newdata = donor_filtered_under18)

### METHOD (2):

# Train the best model structure on the donor data filtered to match Lopez demographics
gam_donor_filtered_under18 <- bam(logAV ~ Sex + 
                                    s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                                    s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                                    s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15) +
                                    ti(logWeight, sqrtHeight, bs="cs", k=c(5,5)) +
                                    ti(logWeight, sqrtHeight, bs="cs", by = Sex, k=c(5,5)) +
                                    ti(sqrtHeight, logAge, bs = "cs", k=c(5,5)) + 
                                    ti(sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5)) + 
                                    ti(logWeight, logAge, bs = "cs", k=c(5,5)) + 
                                    ti(logWeight, logAge, bs = "cs", by = Sex, k=c(5,5)) +
                                    ti(logWeight, sqrtHeight, logAge, bs = "cs", k=c(5,5,5)) +
                                    ti(logWeight, sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5,5)), data=donor_filtered_under18, family=gaussian())

summary(gam_donor_filtered_under18)

# Lopez AVD as ground truth, thus make predictions on Lopez data
lopez$AV_physical <- exp(predict(gam_donor_filtered_under18, newdata = lopez))
lopez$Differences <- lopez$AV_physical - lopez$AV_echo

bridging_model_lopez <- bam(AV_echo ~  s(AV_physical, bs="cs", k=3) + s(Age, bs="cs", k=3) + s(Height, bs="cs", k=3) + s(Weight, bs="cs", k=3), data=lopez, family=gaussian())
summary(bridging_model_lopez)

donor_filtered_under18$AV_echo <- predict(bridging_model_lopez, newdata = donor_filtered_under18)

# Plots comparing method (1) to (2):
  ggplot(donor_filtered_under18, aes(x = AV_echo_1, y = AV_echo)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Pseudo-echo AV method (1)",
         y = "Pseudo-echo AV method (2)") +
    theme_minimal() +
    theme(legend.position = "none")

# METHOD (1) PLOTS:
{
  difference_age_plot <- ggplot(lopez, aes(x = Age, y = Differences))  +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(data = lopez, method = "loess", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Age", y = "Difference") +
    theme_minimal() + theme(legend.position = "none")
  
  difference_weight_plot <- ggplot(lopez, aes(x = Weight, y = Differences))  +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(data = lopez, method = "loess", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Weight", y = "Difference Between Donor Predictions and Lopez Measurements") +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
  
  difference_height_plot <- ggplot(lopez, aes(x = Height, y = Differences))  +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(data = lopez, method = "loess", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Height", y = "Difference Between Donor Predictions and Lopez Measurements") +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
}
grid.arrange(difference_age_plot, difference_weight_plot, difference_height_plot, nrow = 1)


### APPLYING CORRECTION
  # Note: This includes an extrapolation for weights > 100kg

donor_under18 <- donor[donor$Age <= 18, ]
donor_over18 <- donor[donor$Age > 18, ]

donor_over18$AV_echo <- predict(bridging_model_UMCG, newdata = donor_over18)
donor_under18$AV_echo <- predict(bridging_model_lopez, newdata = donor_under18)


# Combine the pseudo-echo AV diameters for each donor subset into the full original donor dataset
donor$AV_echo <- donor$AV
donor$AV_echo[donor$Age <= 18] <- donor_under18$AV_echo
donor$AV_echo[donor$Age > 18] <- donor_over18$AV_echo

donor$logAV_echo <- log(donor$AV_echo)

# Retrain model:
final_model_echo_adjusted <- bam(logAV_echo ~ Sex + 
                                   s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                                   s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                                   s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15) +
                                   ti(logWeight, sqrtHeight, bs="cs", k=c(5,5)) +
                                   ti(logWeight, sqrtHeight, bs="cs", by = Sex, k=c(5,5)) +
                                   ti(sqrtHeight, logAge, bs = "cs", k=c(5,5)) + 
                                   ti(sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5)) + 
                                   ti(logWeight, logAge, bs = "cs", k=c(5,5)) + 
                                   ti(logWeight, logAge, bs = "cs", by = Sex, k=c(5,5)) +
                                   ti(logWeight, sqrtHeight, logAge, bs = "cs", k=c(5,5,5)) +
                                   ti(logWeight, sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5,5)), data=donor, family=gaussian())

summary(final_model_echo_adjusted)

donor$Predictions_echo_corr <- exp(predict(final_model_echo_adjusted, newdata = donor))
donor$Residuals_echo_corr <- donor$AV_echo - donor$Predictions_echo_corr


# Fit splines to residuals
gam_var_echo_corr <- bam(Residuals_echo_corr^2 ~ Sex + s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                           s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                           s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15), data=donor, family=gaussian())

summary(gam_var_echo_corr)

# Save the data frame as an RDS file
saveRDS(donor, file = "echo_trend_corrected_donor_data.rds")

# Save model
save(final_model_echo_adjusted, file = "final_model_trend_echo_adjusted.RData")
save(gam_var_echo_corr, file = "final_var_model_trend_echo_adjusted.RData")

# To load the data
donor <- readRDS("echo_trend_corrected_donor_data.rds")

# Load model
load("final_model_trend_echo_adjusted.RData")
final_model <- final_model_echo_adjusted

# Replace in donor data if necessary for later work
{
donor$Predictions <- donor$Predictions_echo_corr
donor$Residuals <- donor$Residuals_echo_corr 
donor$AV <- donor$AV_echo
donor$logAV <- donor$logAV_echo
}

# Comparing residuals and est. std on the same plot
{
  # Compute estimated standard deviations w.r.t to each variable
  donor$std_prediction <- sqrt(predict(bam(Residuals_echo_corr^2 ~ s(Predictions_echo_corr, bs="cs", k=15) +  s(Predictions_echo_corr, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  
  donor$dissection_date_num <- as.numeric(as.Date(donor$dissection_date))
  donor$std_date <- sqrt(predict(bam(Residuals_echo_corr^2 ~ s(dissection_date_num, bs="cs", k=15) +  s(dissection_date_num, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  
  donor$std_age <- sqrt(predict(bam(Residuals_echo_corr^2 ~ s(Age, bs="cs", k=15) + s(Age, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  donor$std_height <- sqrt(predict(bam(Residuals_echo_corr^2 ~ s(Height, bs="cs", k=15) +  s(Height, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  donor$std_weight <- sqrt(predict(bam(Residuals_echo_corr^2 ~ s(Weight, bs="cs", k=15) +  s(Weight, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  
  # QQ plot for the residuals
  ggplot(donor, aes(sample = Residuals_echo_corr)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Residuals versus predictions
  ggplot(donor, aes(x = Predictions_echo_corr, y = Residuals_echo_corr)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Predictions (mm)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_prediction), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_prediction), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals versus age
  residual_plot_age <- ggplot(donor, aes(x = Age, y = Residuals_echo_corr)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Age in Years", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_age), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_age), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals versus height
  residual_plot_height <- ggplot(donor, aes(x = Height, y = Residuals_echo_corr)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Height (cm)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_height), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_height), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none", axis.title.y = element_blank())
  
  # Residuals versus weight
  residual_plot_weight <- ggplot(donor, aes(x = Weight, y = Residuals_echo_corr)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Weight (kg)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_weight), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_weight), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none", axis.title.y = element_blank())
  
  # Residuals versus date with est. std
  ggplot(donor, aes(x = as.Date(dissection_date), y = Residuals_echo_corr)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Dissection date", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(x = as.Date(dissection_date), y = std_date), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(x = as.Date(dissection_date), y = -std_date), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals without est. std
  ggplot(donor, aes(x = as.Date(dissection_date), y = Residuals_echo_corr)) +
    geom_hex(bins = 40) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Dissection date", y = "Residuals (mm)") +
    theme_minimal() +
    theme(legend.position = "none")
}

grid.arrange(residual_plot_age, residual_plot_height, residual_plot_weight, nrow = 1)

##### EVALUATION METRICS

# Function to calculate RMSE and MAE
calculate_errors <- function(actual, predicted) {
  rmse <- sqrt(mean((predicted - actual) ^ 2))
  mae <- mean(abs(predicted - actual))
  return(list(RMSE = rmse, MAE = mae))
}

# In-sample
calculate_errors(donor$AV_echo, donor$Predictions_echo_corr)

# K-fold

# set.seed(123)  # for reproducibility
folds <- createFolds(donor$logAV_echo, k = 10)

# Full LM  
errors <- list()

for (i in seq_along(folds)) {
  train_set <- donor[-folds[[i]], ]
  test_set <- donor[folds[[i]], ]
  
  model <- bam(logAV_echo ~ Sex + 
                 s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                 s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                 s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15) +
                 ti(logWeight, sqrtHeight, bs="cs", k=c(5,5)) +
                 ti(logWeight, sqrtHeight, bs="cs", by = Sex, k=c(5,5)) +
                 ti(sqrtHeight, logAge, bs = "cs", k=c(5,5)) + 
                 ti(sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5)) + 
                 ti(logWeight, logAge, bs = "cs", k=c(5,5)) + 
                 ti(logWeight, logAge, bs = "cs", by = Sex, k=c(5,5)) +
                 ti(logWeight, sqrtHeight, logAge, bs = "cs", k=c(5,5,5)) +
                 ti(logWeight, sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5,5)), data=train_set, family=gaussian())
  
  predictions <- predict(model, newdata = test_set)
  
  actuals <- test_set$AV_echo
  predictions <- exp(predictions)  
  
  errors[[i]] <- calculate_errors(actuals, predictions)
}

# To aggregate the errors, e.g., mean RMSE and mean MAE across folds
mean_rmse <- mean(sapply(errors, function(e) e$RMSE))
mean_mae <- mean(sapply(errors, function(e) e$MAE))
print(mean_rmse)
print(mean_mae)

# UMCG
predictions_corrected_umcg <- exp(predict(final_model_echo_adjusted, newdata = UMCG))
calculate_errors(UMCG$AV_echo, predictions_corrected_umcg)

# Lopez

predictions_corrected_lopez <- exp(predict(final_model_echo_adjusted, newdata = lopez))
calculate_errors(lopez$AV_echo, predictions_corrected_lopez)

