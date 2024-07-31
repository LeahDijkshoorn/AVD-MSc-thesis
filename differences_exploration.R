# Means by source
means_by_source <- aggregate(cbind(AV, Age, Height, Weight) ~ Source, data = data, FUN = mean, na.rm = TRUE)
means_by_source

# Disaggregated by Source 
{
  color_mapping <- c("0" = "royalblue1", "1" = "#12a303", "2" = "red3")
  
  density_Source_AV <- ggplot(data, aes(x=AV, fill=Source)) + geom_density(alpha=0.4) + scale_fill_manual(values = color_mapping) +
    theme_minimal()+
    theme(legend.position = "none") + labs(x = "AV (mm)")
  
  density_Source_weight <- ggplot(data, aes(x=Weight, fill=Source)) + geom_density(alpha=0.4) + scale_fill_manual(values = color_mapping) +
    theme_minimal()+
    theme(legend.position = "right") + theme(axis.title.y = element_blank()) + labs(x = "Weight (kg)")
  
  density_Source_height <- ggplot(data, aes(x=Height, fill=Source)) + geom_density(alpha=0.4) + scale_fill_manual(values = color_mapping) +
    theme_minimal()+
    theme(legend.position = "none") + labs(x = "Height (cm)")
  
  density_Source_age <- ggplot(data, aes(x=Age, fill=Source)) + geom_density(alpha=0.4) + scale_fill_manual(values = color_mapping) +
    theme_minimal()+
    theme(legend.position = "none") + theme(axis.title.y = element_blank()) + labs(x = "Age in Years")
  #density_Source_BSA <- ggplot(data, aes(x=BSA, fill=Source)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+theme(legend.position = "none")
}
grid.arrange(density_Source_AV, density_Source_weight, density_Source_height, density_Source_age, nrow = 2) 

# Boxplots for all three sources
{
  color_mapping2 <- c("0" = "royalblue1", "1" = "darkolivegreen2", "2" = "indianred")
  
  boxplot_Source_AV <- ggplot(data, aes(x = factor(Source), y = AV, fill = factor(Source))) +
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping2) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "AV") +
    theme(axis.title.x = element_blank())
  
  boxplot_Source_weight <- ggplot(data, aes(x = factor(Source), y = Weight, fill = factor(Source))) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping2) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    theme(axis.title.x = element_blank())
  
  boxplot_Source_height <- ggplot(data, aes(x = factor(Source), y = Height, fill = factor(Source))) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping2) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "Height") +
    theme(axis.title.x = element_blank())
  
  boxplot_Source_age <- ggplot(data, aes(x = factor(Source), y = Age, fill = factor(Source))) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping2) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    theme(axis.title.x = element_blank())
}
boxplots <- grid.arrange(boxplot_Source_AV, boxplot_Source_weight, boxplot_Source_height, boxplot_Source_age, nrow = 2)


# Boxplots for UMCG echo
{
  filtered_data1 <- subset(data, Source %in% c(0, 1))
  filtered_data1 <- filtered_data1[filtered_data1$Age >=18, ]
  
  color_mapping3 <- c("0" = "#88a9f7", "1" = "#8ed687")
  
  boxplot_Source_AV <- ggplot(filtered_data1, aes(x = Source, y = AV, fill = Source)) +
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping3) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "AV (mm)") +
    theme(axis.title.x = element_blank())
  
  boxplot_Source_weight <- ggplot(filtered_data1, aes(x = Source, y = Weight, fill = Source)) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping3) + 
    theme_minimal() + 
    theme(legend.position = "right") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "Weight (kg)") +
    theme(axis.title.x = element_blank())
  
  boxplot_Source_height <- ggplot(filtered_data1, aes(x = Source, y = Height, fill = Source)) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping3) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "Height (cm)") +
    theme(axis.title.x = element_blank())
  
  boxplot_Source_age <- ggplot(filtered_data1, aes(x = Source, y = Age, fill = Source)) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping3) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "Age in Years") +
    theme(axis.title.x = element_blank())
}
boxplots <- grid.arrange(boxplot_Source_AV, boxplot_Source_weight, boxplot_Source_height, boxplot_Source_age, nrow = 2)


# Boxplots for Lopez echo
{
  filtered_data2 <- subset(data, Source %in% c(1, 2))
  filtered_data2 <- filtered_data2[filtered_data2$Age < 18, ]
  
  
  color_mapping4 <- c("1" = "#8ed687", "2" = "indianred")
  
  boxplot_Source_AV <- ggplot(filtered_data2, aes(x = Source, y = AV, fill = Source)) +
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping4) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "AV (mm)") +
    theme(axis.title.x = element_blank())
  
  boxplot_Source_weight <- ggplot(filtered_data2, aes(x = Source, y = Weight, fill = Source)) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping4) + 
    theme_minimal() + 
    theme(legend.position = "right") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "Weight (kg)") +
    theme(axis.title.x = element_blank())
  
  boxplot_Source_height <- ggplot(filtered_data2, aes(x = Source, y = Height, fill = Source)) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping4) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "Height (cm)") +
    theme(axis.title.x = element_blank())
  
  boxplot_Source_age <- ggplot(filtered_data2, aes(x = Source, y = Age, fill = Source)) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_mapping4) + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    labs(x = "Source", y = "Age in Years") +
    theme(axis.title.x = element_blank())
}
boxplots <- grid.arrange(boxplot_Source_AV, boxplot_Source_weight, boxplot_Source_height, boxplot_Source_age, nrow = 2)


# LOESS plots
{
  loess_age_plot <- ggplot(data, aes(x = Age, y = AV)) +
    geom_hex(data = subset(data, Source == 1), bins = 28) +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    geom_point(data = subset(data, Source == 2), color = "indianred", size = 1) +
    geom_point(data = subset(data, Source == 0), color = "royalblue1", size = 3, shape = 18) +
    geom_smooth(aes(color = Source), method = "loess", se = FALSE) +
    scale_color_manual(values = c("2" = "red3", "1" = "#0f6e01", "0" = "blue2")) +
    labs(x = "Age in Years", y = "AV (mm)") +
    theme_minimal() + theme(legend.position = "right")
  
  loess_height_plot <- ggplot(data, aes(x = Height, y = AV)) +
    geom_hex(data = subset(data, Source == 1), bins = 28) +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    geom_point(data = subset(data, Source == 2), color = "indianred", size = 1) +
    geom_point(data = subset(data, Source == 0), color = "royalblue1", size = 3, shape = 18) +
    geom_smooth(aes(color = Source), method = "loess", se = FALSE) +
    scale_color_manual(values = c("2" = "red3", "1" = "#0f6e01", "0" = "blue2")) +
    labs(x = "Height (cm)", y = "AV (mm)") +
    theme_minimal() + theme(legend.position = "none")
  
  loess_weight_plot <- ggplot(data, aes(x = Weight, y = AV)) +
    geom_hex(data = subset(data, Source == 1), bins = 28)  +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    geom_point(data = subset(data, Source == 2), color = "indianred", size = 1) +
    geom_point(data = subset(data, Source == 0), color = "royalblue1", size = 3, shape = 18) +
    geom_smooth(aes(color = Source), method = "loess", se = FALSE) +
    scale_color_manual(values = c("2" = "red3", "1" = "#0f6e01", "0" = "blue2")) +
    labs(x = "Weight (kg)", y = "AV (mm)") +
    theme_minimal() + theme(legend.position = "right", axis.title.y = element_blank())
}
loess_age_plot
grid.arrange(loess_height_plot, loess_weight_plot, ncol = 2)


# LOESS plots filtered
{
  data_filtered <- data[data$Age < 18, ]
  data_filtered <- data[data$Weight < 100, ]
  
  loess_age_plot <- ggplot(data_filtered, aes(x = Age, y = AV)) +
    geom_hex(data = subset(data_filtered, Source == 1), bins = 20) +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    geom_point(data = subset(data_filtered, Source == 2), color = "indianred", size = 1) +
    geom_point(data = subset(data_filtered, Source == 0), color = "royalblue1", size = 3, shape = 18) +
    geom_smooth(aes(color = Source), method = "loess", se = FALSE) +
    scale_color_manual(values = c("2" = "red4", "1" = "#0f6e01", "0" = "blue2")) +
    labs(x = "Age", y = "AV") +
    theme_minimal() + theme(legend.position = "none")
  
  loess_height_plot <- ggplot(data_filtered, aes(x = Height, y = AV)) +
    geom_hex(data = subset(data_filtered, Source == 1), bins = 20) +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    geom_point(data = subset(data_filtered, Source == 2), color = "indianred", size = 1) +
    geom_point(data = subset(data_filtered, Source == 0), color = "royalblue1", size = 3, shape = 18) +
    geom_smooth(aes(color = Source), method = "loess", se = FALSE) +
    scale_color_manual(values = c("2" = "red4", "1" = "#0f6e01", "0" = "blue2")) +
    labs(x = "Height", y = "AV") +
    theme_minimal() + theme(legend.position = "none")
  
  loess_weight_plot <- ggplot(data_filtered, aes(x = Weight, y = AV)) +
    geom_hex(data = subset(data_filtered, Source == 1), bins = 20)  +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    geom_point(data = subset(data_filtered, Source == 2), color = "indianred", size = 1) +
    geom_point(data = subset(data_filtered, Source == 0), color = "royalblue1", size = 3, shape = 18) +
    geom_smooth(aes(color = Source), method = "loess", se = FALSE) +
    scale_color_manual(values = c("2" = "red4", "1" = "#0f6e01", "0" = "blue2")) +
    labs(x = "Weight", y = "AV") +
    theme_minimal() + theme(legend.position = "none")
}
loess_age_plot
grid.arrange(loess_height_plot, loess_weight_plot, ncol = 2)

# EXISTING MODELS

# Use similar demographics that the models were based on
{
  donor_filtered <- donor[donor$Age <= 18, ]
  donor_filtered <- donor_filtered[donor_filtered$Weight < 100, ]
}

# Define all regression equations from each paper
{
  # Regression Equation 1 (Mahgerefteh et al.)
  #   AV in cm originally
  Mahgerefteh_height <- function(Height) {
    y_hat <- (1.17 * (Height/10))
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
    out <- log_y_hat
    return(out)
  }
  
  # Regression Equation 5 (Pettersen et al.)
  #   AV in cm 
  Pettersen <- function(BSA) {
    log_y_hat <- -0.874 + 2.708 * BSA - 1.841 * BSA^2 + 0.452 * BSA^3
    out <- log_y_hat
    return(out)
  }
}

# Fit the same model structures on donor data

# Same Mahgerefteh model structure trained on donor data:
mahg_donor <- function(Height) {
  a <- mean(donor_filtered$AV/((donor_filtered$Height)/10))
  y_hat <- a * (Height/10)
}

# Same lopez model structure trained on donor data:
Lopez_mahg_donor <- function(BSA) {
  a <- mean(donor_filtered$AV/sqrt(donor_filtered$BSA))
  y_hat <- a * BSA^(0.5)
}

# Fit the Cantinotti model
cantinotti_donor <- lm(logAV ~ logBSA, donor_filtered)
cantinotti_donor_summary <- summary(cantinotti_donor)
cantinotti_confint <- confint(cantinotti_donor)

# Fit the Pettersen model
pettersen_donor <- lm(log(AV/10) ~ BSA + I(BSA^2) + I(BSA^3),
                      data = donor_filtered)


# Prepare the data for plotting the Cantinotti model with confidence intervals
newdata <- data.frame(logBSA = seq(min(donor_filtered$logBSA), max(donor_filtered$logBSA), length.out = 200))
predictions <- predict(cantinotti_donor, newdata, interval = "confidence", type="response")


# Pettersen predictions
newdata2 <- data.frame(BSA = seq(from = min(donor_filtered$BSA), to = max(donor_filtered$BSA), length.out = 200))
newdata2$logAV_pred <- predict(pettersen_donor, newdata = newdata2)


# Plots: 
{
  # Plot 1: AV_Mahgerefteh_height
  AV_Mahgerefteh_height_plot <- ggplot(donor_filtered, aes(x = Height, y = AV)) +
    geom_hex(bins = 28, show.legend = FALSE) +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    stat_function(fun = function(x) mahg_donor(x), color = "#0f6e01") +
    stat_function(fun = function(x) Mahgerefteh_height(x), color = "purple") +
    labs(x = "Height (cm)",
         y = "AV (mm)") +
    theme_minimal() 
  
  # Plot 2: AV_lopez_Mahgerefteh 
  AV_lopez_Mahgerefteh_plot <- ggplot(donor_filtered, aes(x = BSA, y = AV)) +
    geom_hex(bins = 28, show.legend = FALSE) +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    stat_function(fun = function(x) Lopez_mahg_donor(x), color = "#0f6e01") +
    stat_function(fun = function(x) Lopez(x), color = "red") +
    stat_function(fun = function(x) Mahgerefteh_bsa(x), color = "purple") +
    labs(x = "BSA (m²)",
         y = "AV (mm)") +
    theme_minimal() 
  
  # Plot 3: Cantinotti model
  Cantinotti_model_plot <- ggplot(donor_filtered, aes(x = logBSA, y = logAV)) +
    geom_hex(bins = 35, show.legend = FALSE) +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    geom_line(data = newdata, aes(x = logBSA, y = predictions[,"fit"]), color = "#0f6e01") +  
    #geom_ribbon(data = newdata, aes(x = logBSA, ymin = predictions[,"lwr"], ymax = predictions[,"upr"]), fill = "blue", alpha = 0.2) +  # Use newdata for ribbon
    stat_function(fun = function(x) Cantinotti(exp(x)), color = "deeppink") +  # Predefined Cantinotti model
    labs(x = "Log(BSA)",
         y = "Log(AV_mm)") +
    theme_minimal() 
  
  # Plot 4: Pettersen model
  Pettersen_model_plot <- ggplot(donor_filtered, aes(x = BSA, y = log(AV/10))) +
    geom_hex(bins = 28, show.legend = FALSE) +
    scale_fill_gradient(low = "#cedecc", high = "#12a303") +
    geom_line(data = newdata2, aes(x = BSA, y = logAV_pred), color = "#0f6e01") +  # Fitted Pettersen model
    stat_function(fun = function(x) Pettersen(x), aes(x = BSA), color = "orange2") +  # Predefined Pettersen model
    labs(x = "BSA (m²)",
         y = "Log(AV_cm)") +
    theme_minimal() 
}
grid.arrange(AV_Mahgerefteh_height_plot, AV_lopez_Mahgerefteh_plot, ncol = 2)
grid.arrange(Cantinotti_model_plot, Pettersen_model_plot, ncol = 2)


