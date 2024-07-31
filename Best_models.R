## Best models based on all available data ##

# Full GAM 
final_model <- bam(logAV ~ Sex + 
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

summary(final_model)

# Save model
save(final_model, file = "full_GAM.RData")

# Load model
load("final_model.RData")
final_model <- final_model

# Predictions in mm
donor$Predictions <- exp(predict(final_model, newdata = donor))

# Residuals
donor$Residuals <- donor$AV - donor$Predictions

# Comparing residuals and est. std on the same plot
{
  # Compute estimated standard deviations w.r.t to each variable
  donor$std_prediction <- sqrt(predict(bam(Residuals^2 ~ s(Predictions, bs="cs", k=15) +  s(Predictions, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  
  donor$dissection_date_num <- as.numeric(as.Date(donor$dissection_date))
  donor$std_date <- sqrt(predict(bam(Residuals^2 ~ s(dissection_date_num, bs="cs", k=15) +  s(dissection_date_num, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  
  donor$std_age <- sqrt(predict(bam(Residuals^2 ~ s(Age, bs="cs", k=15) + s(Age, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  donor$std_height <- sqrt(predict(bam(Residuals^2 ~ s(Height, bs="cs", k=15) +  s(Height, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))
  donor$std_weight <- sqrt(predict(bam(Residuals^2 ~ s(Weight, bs="cs", k=15) +  s(Weight, bs="cs", by = Sex, k=15), data = donor, family=gaussian())))

  # QQ plot for the residuals
  ggplot(donor, aes(sample = Residuals)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Residuals versus predictions
  ggplot(donor, aes(x = Predictions, y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Predictions (mm)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_prediction), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_prediction), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals versus age
  residual_plot_age <- ggplot(donor, aes(x = Age, y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Age in Years", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_age), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_age), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals versus height
  residual_plot_height <- ggplot(donor, aes(x = Height, y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Height (cm)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_height), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_height), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none", axis.title.y = element_blank())
  
  # Residuals versus weight
  residual_plot_weight <- ggplot(donor, aes(x = Weight, y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Weight (kg)", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(y = std_weight), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(y = -std_weight), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none", axis.title.y = element_blank())
  
  # Residuals versus date with est. std
  ggplot(donor, aes(x = as.Date(dissection_date), y = Residuals)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Dissection date", y = "Residuals (mm)") +
    theme_minimal() + 
    geom_smooth(aes(x = as.Date(dissection_date), y = std_date), method = "loess", se = FALSE, color = "blue") + 
    geom_smooth(aes(x = as.Date(dissection_date), y = -std_date), method = "loess", se = FALSE, color = "blue") +
    theme(legend.position = "none")
  
  # Residuals without est. std
  ggplot(donor, aes(x = as.Date(dissection_date), y = Residuals)) +
    geom_hex(bins = 40) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Dissection date", y = "Residuals (mm)") +
    theme_minimal() +
    theme(legend.position = "right")
}

grid.arrange(residual_plot_age, residual_plot_height, residual_plot_weight, nrow = 1)

# Residual plots sex disaggregated
{

# Predicted Values vs. Residuals separated by sex
ggplot(donor, aes(x = Predictions, y = Residuals)) +
  geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predicted Values", y = "Residuals") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(Sex ~ .)  # Separate plots for each sex

# Age vs. Residuals separated by sex
ggplot(donor, aes(x = Age, y = Residuals)) +
  geom_hex(bins = 30) +  
  scale_fill_gradient(low = "lightgrey", high = "black") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Age", y = "Residuals") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(Sex ~ .)

# Height vs. Residuals separated by sex
ggplot(donor, aes(x = Height, y = Residuals)) +
  geom_hex(bins = 30) +  
  scale_fill_gradient(low = "lightgrey", high = "black") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Height", y = "Residuals") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(Sex ~ .)

# Weight vs. Residuals separated by sex
ggplot(donor, aes(x = Weight, y = Residuals)) +
  geom_hex(bins = 30) +  
  scale_fill_gradient(low = "lightgrey", high = "black") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Weight", y = "Residuals") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(Sex ~ .)

# Dissection Date vs. Residuals separated by sex
ggplot(donor, aes(x = as.Date(dissection_date), y = Residuals)) +
  geom_hex(bins = 30) +  
  scale_fill_gradient(low = "lightgrey", high = "black") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Dissection Date", y = "Residuals") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(Sex ~ .)
}

### Comparing estimated standard deviations

# Fit splines to residuals
gam_var <- bam(Residuals^2 ~ Sex + s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                 s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                 s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15), data=donor, family=gaussian())

summary(gam_var)

donor$Predictions_var <- predict(gam_var, newdata = donor)

# Computing Z-score example
{
  # Predict for new data
  individual_measurement <- data.frame(
    Weight = 70,
    Height = 170,
    Age = 40,
    logWeight = log(70),
    sqrtHeight = sqrt(170),
    CuberootAge = (40)^(1/3),
    Sex = as.factor(0),
    AV = 25
  )
  
  # Predict the variance and convert to standard deviation
  predicted_sd <- predict(gam_std, newdata = individual_measurement)
  
  # Predict the mean response
  predicted_mean <- exp(predict(lm_donor, newdata = individual_measurement))
  
  # Calculate the Z-score
  z_score <- (individual_measurement$AV - predicted_mean) / predicted_sd
  
  # Output the Z-score
  z_score
}

# STANDARD DEVIATION PLOTS:
# Define all regression equations from each paper
{
  # Regression Equation 1 (Mahgerefteh et al.)
  #   AV in cm originally
  Mahgerefteh_height <- function(Height) {
    y_hat <- (1.17 * Height) * 10
  }
  
  # Regression Equation 2
  #   AV in cm originally
  Mahgerefteh_bsa <- function(BSA) {
    y_hat <- (1.50 * BSA^(0.5)) * 10
  }
  
  # Regression Equation 3 (Lopez et al.)
  #   AV in cm originally
  Lopez <- function(BSA) {
    y_hat <- (1.48 * BSA^(0.5)) * 10
  }
  
  # Regression Equation 4 (Cantinotti et al.)
  #   AV in mm
  Cantinotti <- function(BSA) {
    log_y_hat <- 2.750 + 0.515 * log(BSA)
    out <- exp(log_y_hat)
    return(out)
  }
  
  # Regression Equation 5 (Pettersen et al.)
  #   AV in cm originally
  Pettersen <- function(BSA) {
    log_y_hat <- -0.874 + 2.708 * BSA - 1.841 * BSA^2 + 0.452 * BSA^3
    out <- exp(log_y_hat) * 10
    return(out)
  }
}

# Predictions
{
  donor$Predictions_mahg_height <- Mahgerefteh_height(donor$Height)
  donor$Predictions_mahg_bsa <- Mahgerefteh_bsa(donor$BSA)
  donor$Predictions_lopez <- Lopez(donor$BSA)
  donor$Predictions_cantinotti <- Cantinotti(donor$BSA)
  donor$Predictions_pettersen <- Pettersen(donor$BSA)
}

# Residuals
{
  donor$Residuals_mahg_height <- donor$AV - donor$Predictions_mahg_height
  donor$Residuals_mahg_bsa <- donor$AV - donor$Predictions_mahg_bsa
  donor$Residuals_lopez <- donor$AV - donor$Predictions_lopez
  donor$Residuals_cantinotti <- donor$AV - donor$Predictions_cantinotti
  donor$Residuals_pettersen <- donor$AV - donor$Predictions_pettersen
}

# Plots:
{
  donor$Cantinotti <- sqrt(predict(bam(Residuals_cantinotti^2 ~ Sex + s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                                                 s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                                                 s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15), data=donor, family=gaussian())))
  
  donor$pettersen_std_gam <- sqrt(predict(bam(Residuals_pettersen^2 ~ Sex + s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                                                s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                                                s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15), data=donor, family=gaussian())))
  
  donor$Lopez <- sqrt(predict(bam(Residuals_lopez^2 ~ Sex + s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                                            s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                                            s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15), data=donor, family=gaussian())))
  
  donor$Full_GAM <- sqrt(donor$Predictions_var)
  
  df_long <- pivot_longer(donor, cols = c(Full_GAM, Cantinotti, Lopez), names_to = "Model", values_to = "Standard_deviation")
  
  std_age_comparison <- ggplot(df_long, aes(x = Age, y = Standard_deviation, color = Model, linetype = Sex)) +
    geom_smooth(method = "loess", se = FALSE) +  
    labs(x = "Age in Years", y = "Conditional Standard Deviation (mm)") +
    theme_minimal() +
    scale_color_manual(values = c("Full_GAM" = "darkgreen", "Cantinotti" = "blue", "Lopez" = "red"))+
    theme(legend.position = "none")
  
  std_weight_comparison <- ggplot(df_long, aes(x = Weight, y = Standard_deviation, color = Model, linetype = Sex)) +
    geom_smooth(method = "loess", se = FALSE) +  
    labs(x = "Weight (kg)", y = "Conditional Standard Deviation (mm)") +
    theme_minimal() +
    scale_color_manual(values = c("Full_GAM" = "darkgreen", "Cantinotti" = "blue", "Lopez" = "red"))+
    theme(legend.position = "none", axis.title.y = element_blank())
  
  std_height_comparison <- ggplot(df_long, aes(x = Height, y = Standard_deviation, color = Model, linetype = Sex)) +
    geom_smooth(method = "loess", se = FALSE) +  
    labs(x = "Height (cm)", y = "Conditional Standard Deviation (mm)") +
    theme_minimal() +
    scale_color_manual(values = c("Full_GAM" = "darkgreen", "Cantinotti" = "blue", "Lopez" = "red"))+
    theme(legend.position = "none", axis.title.y = element_blank())
}
grid.arrange(std_age_comparison, std_weight_comparison, std_height_comparison, nrow = 1)

### EVALUATION METRICS

# Function to calculate RMSE and MAE
calculate_errors <- function(actual, predicted) {
  rmse <- sqrt(mean((predicted - actual) ^ 2))
  mae <- mean(abs(predicted - actual))
  return(list(RMSE = rmse, MAE = mae))
}

# Donor data
calculate_errors(donor$AV, donor$Predictions)
calculate_errors(donor$AV, donor$Predictions_lopez)
calculate_errors(donor$AV, donor$Predictions_cantinotti)

# UMCG data

predictions_umcg_donor <- exp(predict(final_model, newdata = UMCG))
calculate_errors(UMCG$AV, predictions_umcg_donor)

predictions_umcg_lopez <- Lopez(UMCG$BSA)
calculate_errors(UMCG$AV, predictions_umcg_lopez)

predictions_umcg_cantinotti <- Cantinotti(UMCG$BSA)
calculate_errors(UMCG$AV, predictions_umcg_cantinotti)


# Lopez data

predictions_lopez_donor <- exp(predict(final_model, newdata = lopez))
calculate_errors(lopez$AV, predictions_lopez_donor)

predictions_lopez_lopez <- Lopez(lopez$BSA)
calculate_errors(lopez$AV, predictions_lopez_lopez)

predictions_lopez_cantinotti <- Cantinotti(lopez$BSA)
calculate_errors(lopez$AV, predictions_lopez_cantinotti)


