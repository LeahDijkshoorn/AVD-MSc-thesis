# Linear model 
lm_donor_full <- lm(logAV ~ logAge*logWeight*sqrtHeight*Sex, data = donor)

lm_donor <- lm(logAV ~ logAge*Sex + logWeight*Sex + sqrtHeight*Sex + 
                 logWeight:sqrtHeight + sqrtHeight:logAge + 
                 logWeight:logAge + logWeight:sqrtHeight:logAge, data = donor)

summary(lm_donor_full)
summary(lm_donor)

AIC(lm_donor_full)
BIC(lm_donor_full)
AIC(lm_donor)
BIC(lm_donor)

# GAM model
gam_donor_full <- bam(logAV ~ Sex + 
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


gam_donor <- bam(logAV ~ Sex + s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                   s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                   s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15), data=donor, family=gaussian())

summary(gam_donor_full)
summary(gam_donor)

AIC(gam_donor_full)
BIC(gam_donor_full)
AIC(gam_donor)
BIC(gam_donor)

# BSA model
gam_donor_BSA <- bam(logAV ~ Sex + s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                       s(logBSA, bs="cs", k=15) +
                       s(logBSA, bs="cs", by = Sex, k=15) +
                       ti(logBSA, logAge, bs = "cs", k=c(5,5))+
                       ti(logBSA, logAge, bs = "cs", by = Sex, k=c(5,5)), data=donor, family=gaussian())

summary(gam_donor_BSA)
AIC(gam_donor_BSA)
BIC(gam_donor_BSA)

# Standard deviation plots
{
  # Compute residuals for each model type:
  {
    # Full LM
    donor$Predictions_full_LM <- exp(predict(lm_donor_full, newdata = donor))
    donor$Residuals_full_LM <- donor$AV - donor$Predictions_full_LM
    
    # Reduced LM
    donor$Predictions_LM <- exp(predict(lm_donor, newdata = donor))
    donor$Residuals_LM <- donor$AV - donor$Predictions_LM
    
    # Full GAM
    donor$Predictions_full_GAM <- exp(predict(gam_donor_full, newdata = donor))
    donor$Residuals_full_GAM <- donor$AV - donor$Predictions_full_GAM
    
    # Reduced GAM
    donor$Predictions_GAM <- exp(predict(gam_donor, newdata = donor))
    donor$Residuals_GAM <- donor$AV - donor$Predictions_GAM
    
    # BSA GAM
    donor$Predictions_BSA <- exp(predict(gam_donor_BSA, newdata = donor))
    donor$Residuals_BSA <- donor$AV - donor$Predictions_BSA
  }
  
  # Proceed with just the full models
  
  # Full residuals model
  {
    donor$LM <- sqrt(predict(bam(Residuals_full_LM^2 ~ Sex + s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                                       s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                                       s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15), data=donor, family=gaussian())))
    
    donor$GAM <- sqrt(predict(bam(Residuals_full_GAM^2 ~ Sex + s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                                        s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                                        s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15), data=donor, family=gaussian())))
    
    donor$BSA <- sqrt(predict(bam(Residuals_BSA^2 ~ Sex + s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                                        s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                                        s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15), data=donor, family=gaussian())))
    
    df_long_std <- pivot_longer(donor, cols = c(LM, GAM, BSA), names_to = "Model", values_to = "Standard_deviation")
    
    std_age_plot <- ggplot(df_long_std, aes(x = Age, y = Standard_deviation, color = Model)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Age", y = "Standard Deviation") +
      theme_minimal() +
      scale_color_manual(values = c("LM" = "red", "GAM" = "blue", "BSA" = "darkgreen"))+
      theme(legend.position = "none")
    
    std_height_plot <- ggplot(df_long_std, aes(x = Height, y = Standard_deviation, color = Model)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Height", y = "Standard Deviation") +
      theme_minimal() +
      scale_color_manual(values = c("LM" = "red", "GAM" = "blue", "BSA" = "darkgreen"))+
      theme(legend.position = "none", axis.title.y = element_blank())
    
    std_weight_plot <- ggplot(df_long_std, aes(x = Weight, y = Standard_deviation, color = Model)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Weight", y = "Standard Deviation") +
      theme_minimal() +
      scale_color_manual(values = c("LM" = "red", "GAM" = "blue", "BSA" = "darkgreen"))+
      theme(legend.position = "none", axis.title.y = element_blank())
    
    # By sex
    std_age_plot_sex <- ggplot(df_long_std, aes(x = Age, y = Standard_deviation, color = Model, linetype = Sex)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Age in Years", y = "Conditional Standard Deviation (mm)") +
      theme_minimal() +
      scale_color_manual(values = c("LM" = "red", "GAM" = "darkgreen", "BSA" = "blue"))+
      theme(legend.position = "none")
    
    std_height_plot_sex <- ggplot(df_long_std, aes(x = Height, y = Standard_deviation, color = Model, linetype = Sex)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Height (cm)", y = "Conditional Standard Deviation (mm)") +
      theme_minimal() +
      scale_color_manual(values = c("LM" = "red", "GAM" = "darkgreen", "BSA" = "blue"))+
      theme(legend.position = "bottom")
    
    std_weight_plot_sex <- ggplot(df_long_std, aes(x = Weight, y = Standard_deviation, color = Model, linetype = Sex)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Weight (kg)", y = "Standard Deviation") +
      theme_minimal() +
      scale_color_manual(values = c("LM" = "red", "GAM" = "darkgreen", "BSA" = "blue"))+
      theme(legend.position = "none", axis.title.y = element_blank())
    
    grid.arrange(std_age_plot_sex, std_weight_plot_sex, nrow = 1)
    
  }
  
  # Separate std models for each variable:
  {
    donor$std_age_LM <- sqrt(predict(bam(Residuals_full_LM^2 ~ s(Age, bs="cs", k=15), data = donor, family=gaussian())))
    donor$std_age_GAM <- sqrt(predict(bam(Residuals_full_GAM^2 ~ s(Age, bs="cs", k=15), data = donor, family=gaussian())))
    donor$std_age_BSA <- sqrt(predict(bam(Residuals_BSA^2 ~ s(Age, bs="cs", k=15), data = donor, family=gaussian())))
    
    df_long_age <- pivot_longer(donor, cols = c(std_age_LM, std_age_GAM, std_age_BSA), names_to = "Model", values_to = "Standard_deviation_age")
    
    std_age_plot <- ggplot(df_long_age, aes(x = Age, y = Standard_deviation_age, color = Model)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Age", y = "Standard Deviation") +
      theme_minimal() +
      scale_color_manual(values = c("std_age_LM" = "red", "std_age_GAM" = "blue", "std_age_BSA" = "darkgreen"))+
      theme(legend.position = "none")
    
    donor$std_height_LM <- sqrt(predict(bam(Residuals_full_LM^2 ~ s(Height, bs="cs", k=15), data = donor, family=gaussian())))
    donor$std_height_GAM <- sqrt(predict(bam(Residuals_full_GAM^2 ~ s(Height, bs="cs", k=15), data = donor, family=gaussian())))
    donor$std_height_BSA <- sqrt(predict(bam(Residuals_BSA^2 ~ s(Height, bs="cs", k=15), data = donor, family=gaussian())))
    
    df_long_height <- pivot_longer(donor, cols = c(std_height_LM, std_height_GAM, std_height_BSA), names_to = "Model", values_to = "Standard_deviation_height")
    
    std_height_plot <- ggplot(df_long_height, aes(x = Age, y = Standard_deviation_height, color = Model)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Height", y = "Standard Deviation") +
      theme_minimal() +
      scale_color_manual(values = c("std_height_LM" = "red", "std_height_GAM" = "blue", "std_height_BSA" = "darkgreen"))+
      theme(legend.position = "none", axis.title.y = element_blank())
    
    donor$std_weight_LM <- sqrt(predict(bam(Residuals_full_LM^2 ~ s(Weight, bs="cs", k=15), data = donor, family=gaussian())))
    donor$std_weight_GAM <- sqrt(predict(bam(Residuals_full_GAM^2 ~ s(Weight, bs="cs", k=15), data = donor, family=gaussian())))
    donor$std_weight_BSA <- sqrt(predict(bam(Residuals_BSA^2 ~ s(Weight, bs="cs", k=15), data = donor, family=gaussian())))
    
    df_long_weight <- pivot_longer(donor, cols = c(std_weight_LM, std_weight_GAM, std_weight_BSA), names_to = "Model", values_to = "Standard_deviation_weight")
    
    std_weight_plot <- ggplot(df_long_weight, aes(x = Age, y = Standard_deviation_weight, color = Model)) +
      geom_smooth(method = "loess", se = FALSE) +  
      labs(x = "Weight", y = "Standard Deviation") +
      theme_minimal() +
      scale_color_manual(values = c("std_weight_LM" = "red", "std_weight_GAM" = "blue", "std_weight_BSA" = "darkgreen"))+
      theme(legend.position = "none", axis.title.y = element_blank())
    
    grid.arrange(std_age_plot, std_height_plot, std_weight_plot, nrow = 1)
  }
}


# RMSE and MAE

# Function to calculate RMSE and MAE
calculate_errors <- function(actual, predicted) {
  rmse <- sqrt(mean((predicted - actual) ^ 2))
  mae <- mean(abs(predicted - actual))
  return(list(RMSE = rmse, MAE = mae))
}

# In-sample
{
  # Full LM
  calculate_errors(donor$AV, donor$Predictions_full_LM)
  
  # Reduced LM
  calculate_errors(donor$AV, donor$Predictions_LM)
  
  # Full GAM
  calculate_errors(donor$AV, donor$Predictions_full_GAM)
  
  # Reduced GAM
  calculate_errors(donor$AV, donor$Predictions_GAM)
  
  # BSA
  calculate_errors(donor$AV, donor$Predictions_BSA)
}

# K-fold
{
  set.seed(123)  # for reproducibility
  folds <- createFolds(donor$logAV, k = 10)
  
  # Full LM  
    errors <- list()
    
    for (i in seq_along(folds)) {
      train_set <- donor[-folds[[i]], ]
      test_set <- donor[folds[[i]], ]
      
      model <- lm(logAV ~ logAge*logWeight*sqrtHeight*Sex, data = train_set)
      predictions <- predict(model, newdata = test_set, type = "response")
      
      actuals <- test_set$AV
      predictions <- exp(predictions)  
      
      errors[[i]] <- calculate_errors(actuals, predictions)
    }
    
    # To aggregate the errors, e.g., mean RMSE and mean MAE across folds
    mean_rmse_full_LM <- mean(sapply(errors, function(e) e$RMSE))
    mean_mae_full_LM <- mean(sapply(errors, function(e) e$MAE))
    print(mean_rmse_full_LM)
    print(mean_mae_full_LM)
    
  # Reduced LM
    errors <- list()
    
    for (i in seq_along(folds)) {
      train_set <- donor[-folds[[i]], ]
      test_set <- donor[folds[[i]], ]
      
      model <- lm(logAV ~ logAge*Sex + logWeight*Sex + sqrtHeight*Sex + 
                    logWeight:sqrtHeight + sqrtHeight:logAge + 
                    logWeight:logAge + logWeight:sqrtHeight:logAge, data = train_set)
      predictions <- predict(model, newdata = test_set, type = "response")
      
      actuals <- test_set$AV
      predictions <- exp(predictions)  
      
      errors[[i]] <- calculate_errors(actuals, predictions)
    }
    
    # To aggregate the errors, e.g., mean RMSE and mean MAE across folds
    mean_rmse_LM <- mean(sapply(errors, function(e) e$RMSE))
    mean_mae_LM <- mean(sapply(errors, function(e) e$MAE))
    print(mean_rmse_LM)
    print(mean_mae_LM)
    
  # Full GAM
    errors <- list()
    
    for (i in seq_along(folds)) {
      train_set <- donor[-folds[[i]], ]
      test_set <- donor[folds[[i]], ]
      
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
    mean_rmse_full_GAM <- mean(sapply(errors, function(e) e$RMSE))
    mean_mae_full_GAM <- mean(sapply(errors, function(e) e$MAE))
    print(mean_rmse_full_GAM)
    print(mean_mae_full_GAM)
    
  # Reduced GAM
    errors <- list()
    
    for (i in seq_along(folds)) {
      train_set <- donor[-folds[[i]], ]
      test_set <- donor[folds[[i]], ]
      
      model <- bam(logAV ~ Sex + s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                     s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                     s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15), data = train_set)
      predictions <- predict(model, newdata = test_set, type = "response")
      
      # Convert predictions if needed
      actuals <- test_set$AV  
      predictions <- exp(predictions)  # transforming back to original scale
      
      # Calculate errors
      errors[[i]] <- calculate_errors(actuals, predictions)
    }
    
    # To aggregate the errors, e.g., mean RMSE and mean MAE across folds
    mean_rmse_GAM <- mean(sapply(errors, function(e) e$RMSE))
    mean_mae_GAM <- mean(sapply(errors, function(e) e$MAE))
    print(mean_rmse_GAM)
    print(mean_mae_GAM)
    
    # BSA
    errors <- list()
    
    for (i in seq_along(folds)) {
      train_set <- donor[-folds[[i]], ]
      test_set <- donor[folds[[i]], ]
      
      model <- bam(logAV ~ Sex + s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                     s(logBSA, bs="cs", k=15) +
                     s(logBSA, bs="cs", by = Sex, k=15) +
                     ti(logBSA, logAge, bs = "cs", k=c(5,5))+
                     ti(logBSA, logAge, bs = "cs", by = Sex, k=c(5,5)), data=train_set, family=gaussian())
      predictions <- predict(model, newdata = test_set, type = "response")
      
      # Convert predictions if needed
      actuals <- test_set$AV  
      predictions <- exp(predictions)  # transforming back to original scale
      
      # Calculate errors
      errors[[i]] <- calculate_errors(actuals, predictions)
    }
    
    # To aggregate the errors, e.g., mean RMSE and mean MAE across folds
    mean_rmse_BSA <- mean(sapply(errors, function(e) e$RMSE))
    mean_mae_BSA <- mean(sapply(errors, function(e) e$MAE))
    print(mean_rmse_BSA)
    print(mean_mae_BSA)
}


# Validation datasets
{
  # UMCG data
    # Full LM
    predictions_full_LM_echo <- exp(predict(lm_donor_full, newdata = echo))
    calculate_errors(echo$AV, predictions_full_LM_echo)
    
    # Reduced LM
    predictions_LM_echo <- exp(predict(lm_donor, newdata = echo))
    calculate_errors(echo$AV, predictions_LM_echo)
    
    # Full GAM
    predictions_full_GAM_echo <- exp(predict(gam_donor_full, newdata = echo))
    calculate_errors(echo$AV, predictions_full_GAM_echo)
    
    # Reduced GAM
    predictions_GAM_echo <- exp(predict(gam_donor, newdata = echo))
    calculate_errors(echo$AV, predictions_GAM_echo)
    
    # BSA
    predictions_BSA_echo <- exp(predict(gam_donor_BSA, newdata = echo))
    calculate_errors(echo$AV, predictions_BSA_echo)
 
  # Lopez et al. data
    # Full LM
    predictions_full_LM_lopez <- exp(predict(lm_donor_full, newdata = lopez))
    calculate_errors(lopez$AV, predictions_full_LM_lopez)
    
    # Reduced LM
    predictions_LM_lopez <- exp(predict(lm_donor, newdata = lopez))
    calculate_errors(lopez$AV, predictions_LM_lopez)
    
    # Full GAM
    predictions_full_GAM_lopez <- exp(predict(gam_donor_full, newdata = lopez))
    calculate_errors(lopez$AV, predictions_full_GAM_lopez)
    
    # Reduced GAM
    predictions_GAM_lopez <- exp(predict(gam_donor, newdata = lopez))
    calculate_errors(lopez$AV, predictions_GAM_lopez)
    
    # BSA
    predictions_BSA_lopez <- exp(predict(gam_donor_BSA, newdata = lopez))
    calculate_errors(lopez$AV, predictions_BSA_lopez)
}

# Extreme cases

# Create extreme subsets:
{
  # Calculate BMI
  donor$BMI <- donor$Weight / (donor$Height / 100)^2
  
  # Very Obese Individuals
  obese_individuals <- donor[donor$BMI > 40, ]
  
  # Very Short Adults
  threshold_Height <- quantile(donor$Height[donor$Age > 18], 0.03)
  very_short_adults <- donor[donor$Age > 18 & donor$Height < threshold_Height, ]
  
  # Older females
  old_females <- donor[donor$Age > 55 & donor$Sex == 1, ]
  
  # Older males
  old_males <- donor[donor$Age > 55 & donor$Sex == 0, ] 
}


sample_and_test_model <- function(subset, full_data, test_size_factor = 0.2, iterations = 100) {
  metrics <- list(rmse_full_gam = numeric(iterations), mae_full_gam = numeric(iterations),
                  rmse_reduced_gam = numeric(iterations), mae_reduced_gam = numeric(iterations),
                  rmse_full_lm = numeric(iterations), mae_full_lm = numeric(iterations),
                  rmse_reduced_lm = numeric(iterations), mae_reduced_lm = numeric(iterations),
                  rmse_bsa = numeric(iterations), mae_bsa = numeric(iterations))
  
  for (i in 1:iterations) {
    
    test_size <- floor(nrow(subset) * test_size_factor)
    
    test_index <- sample(1:nrow(subset), test_size)
    
    test_data <- subset[test_index, ]
    train_data <- full_data[!rownames(full_data) %in% rownames(test_data), ]
    
    # Train models
    lm_donor_full <- lm(logAV ~ logAge*logWeight*sqrtHeight*Sex, data = train_data)
    
    lm_donor <- lm(logAV ~ logAge*Sex + logWeight*Sex + sqrtHeight*Sex + 
                     logWeight:sqrtHeight + sqrtHeight:logAge + 
                     logWeight:logAge + logWeight:sqrtHeight:logAge, data = train_data)
    
    gam_donor_full <- bam(logAV ~ Sex + s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                            s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                            s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15) +
                            ti(logWeight, sqrtHeight, bs="cs", k=c(5,5)) +
                            ti(logWeight, sqrtHeight, bs="cs", by=Sex, k=c(5,5)) +
                            ti(sqrtHeight, logAge, bs="cs", k=c(5,5)) +
                            ti(sqrtHeight, logAge, bs="cs", by=Sex, k=c(5,5)) +
                            ti(logWeight, logAge, bs="cs", k=c(5,5)) +
                            ti(logWeight, logAge, bs="cs", by=Sex, k=c(5,5)) +
                            ti(logWeight, sqrtHeight, logAge, bs="cs", k=c(5,5,5)) +
                            ti(logWeight, sqrtHeight, logAge, bs="cs", by=Sex, k=c(5,5,5)), 
                          data=train_data, family=gaussian())
    
    gam_donor <- bam(logAV ~ Sex + s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                       s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                       s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15), 
                     data=train_data, family=gaussian())
    
    gam_donor_BSA <- bam(logAV ~ Sex + s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                           s(logBSA, bs="cs", k=15) +
                           s(logBSA, bs="cs", by = Sex, k=15) +
                           ti(logBSA, logAge, bs = "cs", k=c(5,5))+
                           ti(logBSA, logAge, bs = "cs", by = Sex, k=c(5,5)), data=train_data, family=gaussian())
    
    # Make predictions and calculate RMSE and MAE
    predictions_full_gam <- exp(predict(gam_donor_full, newdata = test_data))
    predictions_reduced_gam <- exp(predict(gam_donor, newdata = test_data))
    predictions_full_lm <- exp(predict(lm_donor_full, newdata = test_data))
    predictions_reduced_lm <- exp(predict(lm_donor, newdata = test_data))
    predictions_bsa <- exp(predict(gam_donor_BSA, newdata = test_data))
        
    metrics$rmse_full_gam[i] <- sqrt(mean((predictions_full_gam - test_data$AV)^2))
    metrics$mae_full_gam[i] <- mean(abs(predictions_full_gam - test_data$AV))
    metrics$rmse_reduced_gam[i] <- sqrt(mean((predictions_reduced_gam - test_data$AV)^2))
    metrics$mae_reduced_gam[i] <- mean(abs(predictions_reduced_gam - test_data$AV))
    metrics$rmse_full_lm[i] <- sqrt(mean((predictions_full_lm - test_data$AV)^2))
    metrics$mae_full_lm[i] <- mean(abs(predictions_full_lm - test_data$AV))
    metrics$rmse_reduced_lm[i] <- sqrt(mean((predictions_reduced_lm - test_data$AV)^2))
    metrics$mae_reduced_lm[i] <- mean(abs(predictions_reduced_lm - test_data$AV))
    metrics$rmse_bsa[i] <- sqrt(mean((predictions_bsa - test_data$AV)^2))
    metrics$mae_bsa[i] <- mean(abs(predictions_bsa - test_data$AV))
  }

  # Calculate average RMSEs and MAEs
  avg_metrics <- lapply(metrics, mean)
  
  return(avg_metrics)
}

results_obese_individuals <- sample_and_test_model(obese_individuals, donor)
results_very_short_adults <- sample_and_test_model(very_short_adults, donor)
results_old_females <- sample_and_test_model(old_females, donor)
results_old_males <- sample_and_test_model(old_males, donor)

# Print results
print("Average RMSE and MAE Results for Obese Individuals:")
print(results_obese_individuals)

print("Average RMSE and MAE Results for Very Short Adults:")
print(results_very_short_adults)

print("Average RMSE and MAE Results for Old females:")
print(results_old_females)

print("Average RMSE and MAE Results for Old males:")
print(results_old_males)



# WLS Model
# NO LONGER USED
{
  # Estimate variance:
  {
    donor$Predictions <- exp(predict(gam_donor, newdata = donor))
    
    donor$Residuals <- donor$AV - donor$Predictions
    
    gam_var <- bam(Residuals^2 ~ s(Age, bs="cs", k=15) + s(Age, bs="cs", by=Sex, k=15) +
                     s(Weight, bs="cs", k=15) + s(Weight, bs="cs", by=Sex, k=15) +
                     s(Height, bs="cs", k=15) + s(Height, bs="cs", by=Sex, k=15) +
                     te(Weight, Height, bs="cs", k=c(5,5)) +
                     te(Height, Age, bs = "cs", k=c(5,5)) + 
                     te(Weight, Age, bs = "cs", k=c(5,5)), data=donor, family=gaussian())
    
    donor$Predictions_var <- predict(gam_var, newdata = donor)
  }
  
  # Use inverse estimated variance as weights
  weights <- 1 / (donor$Predictions_var)
  
  wgam_donor <- bam(logAV ~ s(logAge, bs="cs", k=15) + s(logAge, bs="cs", by=Sex, k=15) +
                      s(logWeight, bs="cs", k=15) + s(logWeight, bs="cs", by=Sex, k=15) +
                      s(sqrtHeight, bs="cs", k=15) + s(sqrtHeight, bs="cs", by=Sex, k=15) +
                      te(logWeight, sqrtHeight, bs="cs", k=c(5,5)) +
                      te(sqrtHeight, logAge, bs = "cs", k=c(5,5)) + 
                      te(logWeight, logAge, bs = "cs", k=c(5,5)) +
                      te(logWeight, sqrtHeight, logAge, bs = "cs", k=c(5,5,5)), data=donor, weights = as.vector(weights), family=gaussian())
  summary(wgam_donor)
  
  # Predictions in mm
  donor$Predictions_WGAM <- exp(predict(wgam_donor, newdata = donor))
  
  # Residuals plots
  donor$Residuals_WGAM <- donor$AV - donor$Predictions_WGAM
  
  {
    ggplot(donor, aes(x = Predictions_WGAM, y = Residuals_WGAM)) +
      geom_hex(bins = 30) +  # Using hex bins
      scale_fill_distiller(palette = "Spectral") +  # Applying the spectral palette
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Predicted Values", y = "Residuals") +
      theme_minimal()
    
    ggplot(donor, aes(x = Age, y = Residuals_WGAM)) +
      geom_hex(bins = 30) +
      scale_fill_distiller(palette = "Spectral") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Age", y = "Residuals") +
      theme_minimal()
    
    ggplot(donor, aes(x = Height, y = Residuals_WGAM)) +
      geom_hex(bins = 30) +
      scale_fill_distiller(palette = "Spectral") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Height", y = "Residuals") +
      theme_minimal() 
    
    ggplot(donor, aes(x = Weight, y = Residuals_WGAM)) +
      geom_hex(bins = 30) +
      scale_fill_distiller(palette = "Spectral") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Weight", y = "Residuals") +
      theme_minimal()
    
    ggplot(donor, aes(x = as.Date(dissection_date), y = Residuals_WGAM)) +  # Ensure dissection_date is a Date object
      geom_hex(bins = 30) +
      scale_fill_distiller(palette = "Spectral") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Dissection Date", y = "Residuals") +
      theme_minimal() 
  }
}


# Monte Carlo cross-validation function
# NOT USED ANYMORE, SAME RESULTS AS K-FOLD
donor_MC_CV <- function(data, NumberOfRuns){
  
  # Initialise lists and vectors to store results
  predictions_donor_list <- list()
  rmse_list <- vector("numeric", NumberOfRuns)
  r_squared_list <- vector("numeric", NumberOfRuns)
  mae_list <- vector("numeric", NumberOfRuns)
  
  for (i in 1:NumberOfRuns){
    # Test/train split
    set.seed(i)
    train_index <- createDataPartition(data$AV, p = 0.7, list = FALSE)
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
    
    # Fit linear model on transformed data
    lm_donor <- lm(logAV ~ logWeight*sqrtHeight*CuberootAge*Sex, data = donor)
    
    # Predictions in original scale (mm)
    predictions_donor <- exp(predict(lm_donor, newdata = test_data))
    
    # Store predictions
    predictions_donor_list[[i]] <- predictions_donor
    
    # Calculate RMSE
    rmse_list[i] <- sqrt(mean((test_data$AV - predictions_donor)^2))
    
    # Calculate R-squared
    r_squared_list[i] <- cor(test_data$AV, predictions_donor)^2
    
    # Calculate MAE
    mae_list[i] <- mean(abs(test_data$AV - predictions_donor))
  }
  
  # Return list containing predictions and average metrics
  list(predictions = predictions_donor_list, 
       average_RMSE = mean(rmse_list), 
       average_R_squared = mean(r_squared_list), 
       average_MAE = mean(mae_list))
}

results <- donor_MC_CV(donor, 10)
print(results)


