# Data, no cleaning necessary:
summary(lopez)

# Basic Histograms with normal distribution overlay
{
  hist_AV <- ggplot(lopez, aes(x = AV)) + 
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(lopez$AV), sd = sd(lopez$AV)), color = "red", linewidth = 1) +  
    theme_minimal() + labs(x = "AV (mm)")
  
  hist_weight <- ggplot(lopez, aes(x = Weight)) + 
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(lopez$Weight), sd = sd(lopez$Weight)), color = "red", linewidth = 1) +  
    theme_minimal() + theme(axis.title.y = element_blank()) + labs(x = "Weight (kg)")
  
  hist_height <- ggplot(lopez, aes(x = Height)) + 
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(lopez$Height), sd = sd(lopez$Height)), color = "red", linewidth = 1) +  
    theme_minimal() + theme(axis.title.y = element_blank()) + labs(x = "Height (cm)")
  
  hist_age <- ggplot(lopez, aes(x = Age)) + 
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(lopez$Age), sd = sd(lopez$Age)), color = "red", linewidth = 1) +  
    theme_minimal() + labs(x = "Age in Years")
  
  hist_BSA <- ggplot(lopez, aes(x = BSA)) + 
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(lopez$BSA), sd = sd(lopez$BSA)), color = "red", linewidth = 1) +  
    theme_minimal() + theme(axis.title.y = element_blank()) + labs(x = "BSA (m²)")
  
  hist_sex <- ggplot(lopez, aes(x = Sex)) +
    geom_bar(stat = "count") +
    scale_x_discrete(labels = c("Male", "Female")) +
    labs(x = "Sex") +
    theme_minimal()
}
grid.arrange(hist_AV, hist_weight, hist_height, hist_age, hist_BSA, hist_sex, nrow = 2) 

hist_logAV <- ggplot(lopez, aes(x = logAV)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(lopez$logAV), sd = sd(lopez$logAV)), color = "red", linewidth = 1) +  
  theme_minimal()+ theme(axis.title.y = element_blank())

grid.arrange(hist_AV, hist_logAV, ncol = 2)

# Normality checks:
{
  # Shapiro-Wilks 
  shapiro.test(lopez$AV)
  shapiro.test(lopez$logAV)
  
  # Standardise data for Kolmogorov-Smirnov test
  ks.test((lopez$AV - mean(lopez$AV)) / sd(lopez$AV), "pnorm")
  ks.test((lopez$logAV - mean(lopez$logAV)) / sd(lopez$logAV), "pnorm")
  
  qqnorm(lopez$AV)
  qqline(lopez$AV, col = "red")
  
  qqnorm(lopez$logAV)
  qqline(lopez$logAV, col = 'red')
}

# Disaggregated by Sex 
{
  # Orange = female
  density_sex_AV <- ggplot(lopez, aes(x=AV, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + theme(legend.position = "none") + labs(x = "AV (mm)")
  
  density_sex_weight <- ggplot(lopez, aes(x=Weight, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Weight (kg)") +
    theme(legend.position = "right") + theme(axis.title.y = element_blank())
  
  density_sex_height <- ggplot(lopez, aes(x=Height, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Height (cm)") +
    theme(legend.position = "none")
  
  density_sex_age <- ggplot(lopez, aes(x=Age, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Age in Years") +
    theme(legend.position = "none") + theme(axis.title.y = element_blank())
  
  #density_sex_BSA <- ggplot(lopez, aes(x=BSA, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+theme(legend.position = "none")
}
grid.arrange(density_sex_AV, density_sex_weight, density_sex_height, density_sex_age, nrow = 2) 

# Outlier investigation:
{
  ggplot(lopez, aes(x = Age, y = AV)) +
    geom_hex(bins = 28, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "AV (mm)") +
    facet_wrap(~Sex, ncol = 2, labeller = labeller(Sex = c('0' = "Male", '1' = "Female"))) +
    theme_minimal()
  
  ggplot(lopez, aes(x = Age, y = Height)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "Height (cm)") +
    facet_wrap(~Sex, ncol = 2, labeller = labeller(Sex = c('0' = "Male", '1' = "Female"))) +
    theme_minimal()
  
  ggplot(lopez, aes(x = Age, y = Weight)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "Weight (kg)") +
    theme_minimal()
}

# Correlations
Lopez_cont <- lopez[, c("BSA", "Height", "Weight", "Age", "AV")]
ggcorr(Lopez_cont, label = TRUE)

# Sex subsets
Lopez_subset <- lopez[, c("BSA", "Height", "Weight", "Age", "AV", "Sex")]
Lopez_female <- subset(Lopez_subset, Sex == '1', -c(Sex)) 
Lopez_male <- subset(Lopez_subset, Sex == '0', -c(Sex)) 

corr_female <- ggcorr(Lopez_female, label = TRUE) + labs(title = "Correlation Matrix for Females") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
corr_male <- ggcorr(Lopez_male, label = TRUE) + labs(title = "Correlation Matrix for Males") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(corr_female, corr_male, nrow = 1)

# Simple Linear Plots after linearising transformations
{
  plot_AV_BSA <- ggplot(lopez, aes(x = BSA, y = AV)) +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  # Add linear fit with confidence intervals
    labs(x = "BSA (m²)", y = "AV (mm)") +
    theme_minimal()
  
  plot_AV_Height <- ggplot(lopez, aes(x = Height, y = AV)) +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Height (cm)", y = "AV") +
    theme_minimal()+ theme(axis.title.y = element_blank())
  
  plot_AV_Weight <- ggplot(lopez, aes(x = Weight, y = AV))  +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Weight (kg)", y = "AV") +
    theme_minimal()+ theme(axis.title.y = element_blank())
  
  plot_AV_Age <- ggplot(lopez, aes(x = Age, y = AV)) +
    geom_hex(bins = 28, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Age in Years", y = "AV (mm)") +
    theme_minimal()
  # Transformations
  
  plot_AV_logBSA <- ggplot(lopez, aes(x = logBSA, y = logAV))  +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "log(BSA)", y = "log(AV)") +
    theme_minimal()
  
  plot_AV_logAge <- ggplot(lopez, aes(x = logAge, y = logAV)) +
    geom_hex(bins = 25, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "log(Age+1)", y = "log(AV)") +
    theme_minimal()
  
  plot_AV_logWeight <- ggplot(lopez, aes(x = logWeight, y = logAV)) +
    geom_hex(bins = 25, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) + 
    labs(x = "log(Weight)", y = "log(AV)") +
    theme_minimal()+ theme(axis.title.y = element_blank())
  
  plot_logAV_sqrtHeight <- ggplot(lopez, aes(x = sqrtHeight, y = logAV)) +
    geom_hex(bins = 25, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) + 
    labs(x = "Squareroot Height", y = "log(AV)") +
    theme_minimal()+ theme(axis.title.y = element_blank())
}
grid.arrange(plot_AV_Age, plot_AV_Height, plot_AV_Weight, plot_AV_logAge,  plot_logAV_sqrtHeight, plot_AV_logWeight, ncol = 3)

# Closer inspection: fitting LOESS
#   - Visualising AV versus variables split by sex
{
  loess_age_plot <- ggplot(lopez, aes(x = Age, y = AV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(data = lopez, aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "Age", y = "AV", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  loess_logage_plot <- ggplot(lopez, aes(x = logAge, y = logAV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "log(Age+1)", y = "log(AV)", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  lm_logage_plot <- ggplot(lopez, aes(x = logAge, y = logAV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = FALSE) +
    labs(x = "log(Age+1)", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.x = element_blank())
  
  loess_height_plot <- ggplot(lopez, aes(x = Height, y = AV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "Height", y = "AV", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  loess_sqrtheight_plot <- ggplot(lopez, aes(x = sqrtHeight, y = logAV))+
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "Squareroot Height", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
  
  lm_sqrtheight_plot <- ggplot(lopez, aes(x = sqrtHeight, y = logAV))+
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = FALSE) +
    labs(x = "Squareroot Height", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(axis.title.y = element_blank(), legend.position = "none", axis.title.x = element_blank())
  
  loess_weight_plot <- ggplot(lopez, aes(x = Weight, y = AV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") +   
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "Weight", y = "AV", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female"))
  theme_minimal() + theme(legend.position = "none")
  
  loess_logweight_plot <- ggplot(lopez, aes(x = logWeight, y = logAV))+
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") +  
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "log(weight)", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
  
  lm_logweight_plot <- ggplot(lopez, aes(x = logWeight, y = logAV))+
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = FALSE) +
    labs(x = "log(weight)", y = "log(AV)", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(axis.title.y = element_blank(), legend.position = "none", axis.title.x = element_blank())
}

grid.arrange(lm_logage_plot, lm_sqrtheight_plot, lm_logweight_plot, loess_logage_plot, loess_sqrtheight_plot, loess_logweight_plot, nrow = 2)
