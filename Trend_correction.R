### FUNCTIONS:

# Cost function to be used in SN Algorithm:
mleLinearModelCostFunction <- function(segment, penalty, numChangepoints) {
  if (length(segment) <= 2) {
    # Apply penalty directly if not enough data to fit a model
    return(penalty * numChangepoints)
  }
  
  x <- seq_along(segment)
  fit <- lm(segment ~ x)
  residuals <- residuals(fit)
  sigma2_hat <- sum(residuals^2) / (length(segment) - 2)
  nll <- -length(segment) / 2 * log(2 * pi * sigma2_hat) - 1 / (2 * sigma2_hat) * sum(residuals^2)
  
  # Include the penalty for the number of changepoints
  return(-nll + penalty * numChangepoints)
}

# Trend correction function:
correct_trend_and_update <- function(data, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Subset the data based on the provided changepoints
  date_subset_data <- data[data$dissection_date >= start_date & data$dissection_date <= end_date, ]
  
  # Fit linear model and correct for trend
  lm_trend_correction <- lm(Residuals ~ as.Date(dissection_date), data = date_subset_data)
  trend_predictions <- predict(lm_trend_correction, newdata = date_subset_data)
  #trend_difference_FromMean <- trend_predictions - mean(data$AV)
  
  date_subset_data$AV_trend_correction <- date_subset_data$AV - trend_predictions
  
  # Merge corrected AV values back into the main dataset
  data <- merge(data, date_subset_data[c("DonorID", "AV_trend_correction")], by = "DonorID", all.x = TRUE)
  
  # Update AV_corrected in the original dataset
  data$AV_corrected <- ifelse(!is.na(data$AV_trend_correction), data$AV_trend_correction, data$AV)
  
  # Replace original column
  data$AV <- data$AV_corrected
  
  # Clean up by removing temporary columns
  data$AV_trend_correction <- NULL
  data$AV_corrected <- NULL
  
  return(data)
}

### CHANGEPOINT IDENTIFICATION
{
  # Data aggregation: weekly average
  {
    donor$dissection_date <- as.Date(donor$dissection_date)
    
    weekly_data_resid <- donor %>%
      mutate(week_start = floor_date(dissection_date, "week")) %>%
      group_by(week_start) %>%
      summarise(weekly_avg_resids = mean(Residuals, na.rm = TRUE))
  }
  
  # Changepoint detection
  result_donor_weekly_avg_resid <- segmentNeighbourhood(weekly_data_resid$weekly_avg_resids, 6, mleLinearModelCostFunction, penalty = 20, 50)
  result_donor_weekly_avg_resid[1]
  
  # Extract dates using the indexes returned
  weekly_data_resid[,1][191,]
  weekly_data_resid[,1][673,]
  weekly_data_resid[,1][988,]
  weekly_data_resid[,1][1222,]
  
  # Plot aggregated data and changepoints
  ggplot(weekly_data_resid, aes(x = week_start, y = weekly_avg_resids)) +
    geom_line() +  
    labs(x = "Date", y = "Weekly Average Residual") + 
    geom_hline(yintercept = 0, color = "red", linewidth = 0.3) +
    geom_vline(xintercept = as.Date(c("1995-05-07", "2004-08-01", "2010-08-15", "2015-02-08")), 
               linetype="dashed", color = "blue") + theme_minimal()
  
  ### TREND CORRECTION
  
  donor_trend_corr <- donor
  donor_trend_corr <- correct_trend_and_update(donor_trend_corr, "1985-06-05", "1995-05-07")
  donor_trend_corr <- correct_trend_and_update(donor_trend_corr, "1995-05-07", "2004-08-01")
  donor_trend_corr <- correct_trend_and_update(donor_trend_corr, "2004-08-01", "2010-08-15")
  donor_trend_corr <- correct_trend_and_update(donor_trend_corr, "2010-08-15", "2015-02-08")
  donor_trend_corr <- correct_trend_and_update(donor_trend_corr, "2015-02-08", "2016-09-27")
  
  donor_trend_corr$logAV <- log(donor_trend_corr$AV)
  
  # Re-run same model specification
  GAM_trend_corr <- bam(logAV ~ Sex + 
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
                           ti(logWeight, sqrtHeight, logAge, bs = "cs", by = Sex, k=c(5,5,5)), data=donor_trend_corr, family=gaussian())
  
  summary(GAM_trend_corr)
  
  # Compute new residuals
  donor_trend_corr$Predictions <- exp(predict(GAM_trend_corr, newdata = donor_trend_corr))
  donor_trend_corr$Residuals <- donor_trend_corr$AV - donor_trend_corr$Predictions
  
  # Data aggregation: weekly average
  {
    weekly_data_resid_trend_corr <- donor_trend_corr %>%
      mutate(week_start = floor_date(dissection_date, "week")) %>%
      group_by(week_start) %>%
      summarise(weekly_avg_resids = mean(Residuals, na.rm = TRUE))
  }
  
  # Previous changepoints on new aggregated data
  ggplot(weekly_data_resid_trend_corr, aes(x = week_start, y = weekly_avg_resids)) +
    geom_line() +  
    labs(x = "Date", y = "Weekly Average Residual") + 
    geom_hline(yintercept = 0, color = "red", linewidth = 0.3) +
    geom_vline(xintercept = as.Date(c("1995-05-07", "2004-08-01", "2010-08-15", "2015-02-08")), 
               linetype="dashed", color = "blue") + theme_minimal()
  
}


# Comparing residuals and est. std on the same plot
{
  # Compute estimated standard deviations w.r.t to each variable
  donor_trend_corr$std_prediction <- sqrt(predict(bam(Residuals^2 ~ s(Predictions, bs="cs", k=15) +  s(Predictions, bs="cs", by = Sex, k=15), data = donor_trend_corr, family=gaussian())))
  
  donor_trend_corr$dissection_date_num <- as.numeric(as.Date(donor_trend_corr$dissection_date))
  donor_trend_corr$std_date <- sqrt(predict(bam(Residuals^2 ~ s(dissection_date_num, bs="cs", k=15) +  s(dissection_date_num, bs="cs", by = Sex, k=15), data = donor_trend_corr, family=gaussian())))
  donor_trend_corr$std_age <- sqrt(predict(bam(Residuals^2 ~ s(Age, bs="cs", k=15) + s(Age, bs="cs", by = Sex, k=15), data = donor_trend_corr, family=gaussian())))
  donor_trend_corr$std_height <- sqrt(predict(bam(Residuals^2 ~ s(Height, bs="cs", k=15) +  s(Height, bs="cs", by = Sex, k=15), data = donor_trend_corr, family=gaussian())))
  donor_trend_corr$std_weight <- sqrt(predict(bam(Residuals^2 ~ s(Weight, bs="cs", k=15) +  s(Weight, bs="cs", by = Sex, k=15), data = donor_trend_corr, family=gaussian())))
  
  # QQ plot for the residuals
  ggplot(donor_trend_corr, aes(sample = Residuals)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Residuals versus predictions
  ggplot(donor_trend_corr, aes(x = Predictions, y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Predictions (mm)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_prediction), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_prediction), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals versus age
  residual_plot_age <- ggplot(donor_trend_corr, aes(x = Age, y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Age in Years", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_age), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_age), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals versus height
  residual_plot_height <- ggplot(donor_trend_corr, aes(x = Height, y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Height (cm)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_height), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_height), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none", axis.title.y = element_blank())
  
  # Residuals versus weight
  residual_plot_weight <- ggplot(donor_trend_corr, aes(x = Weight, y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Weight (kg)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_weight), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_weight), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none", axis.title.y = element_blank())
  
  # Residuals versus date with est. std
  ggplot(donor_trend_corr, aes(x = as.Date(dissection_date), y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Dissection date", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(x = as.Date(dissection_date), y = std_date), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(x = as.Date(dissection_date), y = -std_date), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals without est. std
  ggplot(donor_trend_corr, aes(x = as.Date(dissection_date), y = Residuals)) +
    geom_hex(bins = 40) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Dissection date", y = "Residuals (mm)") +
    theme_minimal() +
    theme(legend.position = "right")
}
grid.arrange(residual_plot_age, residual_plot_height, residual_plot_weight, nrow = 1)


# CHECKPOINT:

# Save the data frame as an RDS file
saveRDS(donor_trend_corr, file = "corrected_donor_data.rds")

# Save model
save(GAM_trend_corr, file = "GAM_trend_corr.RData")

# To load the data
donor <- readRDS("corrected_donor_data.rds")

# Load model
load("GAM_trend_corr.RData")
final_model <- GAM_trend_corr


### EVALUATION METRICS

# Function to calculate RMSE and MAE
calculate_errors <- function(actual, predicted) {
  rmse <- sqrt(mean((predicted - actual) ^ 2))
  mae <- mean(abs(predicted - actual))
  return(list(RMSE = rmse, MAE = mae))
}

# In-sample
calculate_errors(donor_trend_corr$AV, donor_trend_corr$Predictions)

# K-fold

# set.seed(123)  # for reproducibility
folds <- createFolds(donor_trend_corr$logAV, k = 10)

# Full LM  
errors <- list()

for (i in seq_along(folds)) {
  train_set <- donor_trend_corr[-folds[[i]], ]
  test_set <- donor_trend_corr[folds[[i]], ]
  
  model <- bam(logAV ~ Sex + 
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
  predictions <- predict(model, newdata = test_set, type = "response")
  
  actuals <- test_set$AV
  predictions <- exp(predictions)  
  
  errors[[i]] <- calculate_errors(actuals, predictions)
}

# To aggregate the errors, e.g., mean RMSE and mean MAE across folds
mean_rmse <- mean(sapply(errors, function(e) e$RMSE))
mean_mae <- mean(sapply(errors, function(e) e$MAE))
print(mean_rmse)
print(mean_mae)

# UMCG

predictions_corrected_umcg <- exp(predict(GAM_trend_corr, newdata = UMCG))
calculate_errors(UMCG$AV, predictions_corrected_umcg)

# Lopez

predictions_corrected_lopez <- exp(predict(GAM_trend_corr, newdata = lopez))
calculate_errors(lopez$AV, predictions_corrected_lopez)
