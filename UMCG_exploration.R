# Raw data:
UMCG_full <- read_excel("UMCG_measurements_AoV_annulus_in_normal_subjects.xlsx")
summary(UMCG_full)

# Cleaned data:
summary(UMCG)

# Basic Histograms
{
  hist_AV <- ggplot(UMCG, aes(x = AV)) + 
    geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(UMCG$AV), sd = sd(UMCG$AV)), color = "red", size = 1) +  
    theme_minimal() + labs(x = "AV (mm)")
  
  hist_weight <- ggplot(UMCG, aes(x = Weight)) + 
    geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(UMCG$Weight), sd = sd(UMCG$Weight)), color = "red", size = 1) +  
    theme_minimal() + theme(axis.title.y = element_blank()) + labs(x = "Weight (kg)")
  
  hist_height <- ggplot(UMCG, aes(x = Height)) + 
    geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(UMCG$Height), sd = sd(UMCG$Height)), color = "red", size = 1) +  
    theme_minimal() + theme(axis.title.y = element_blank()) + labs(x = "Height (cm)")
  
  hist_age <- ggplot(UMCG, aes(x = Age)) + 
    geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(UMCG$Age), sd = sd(UMCG$Age)), color = "red", size = 1) +  
    theme_minimal()  + labs(x = "Age in Years")
  
  hist_BSA <- ggplot(UMCG, aes(x = BSA)) + 
    geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
    stat_function(fun = dnorm, args = list(mean = mean(UMCG$BSA), sd = sd(UMCG$BSA)), color = "red", size = 1) +  
    theme_minimal() + theme(axis.title.y = element_blank())  + labs(x = "BSA (m²)")
  
  hist_sex <- ggplot(UMCG, aes(x = Sex)) +
    geom_bar(stat = "count") +
    scale_x_discrete(labels = c("Male", "Female")) +
    labs(x = "Sex") +
    theme_minimal()
}
grid.arrange(hist_AV, hist_weight, hist_height, hist_age, hist_BSA, hist_sex, nrow = 2) 

# Normality checks:
{
  # Shapiro-Wilks 
  shapiro.test(UMCG$AV)
  shapiro.test(UMCG$logAV)
  
  # Standardise data for Kolmogorov-Smirnov test
  ks.test((UMCG$AV - mean(UMCG$AV)) / sd(UMCG$AV), "pnorm")
  ks.test((UMCG$logAV - mean(UMCG$logAV)) / sd(UMCG$logAV), "pnorm")
  
  qqnorm(UMCG$AV)
  qqline(UMCG$AV, col = "red")
  
  qqnorm(UMCG$logAV)
  qqline(UMCG$logAV, col = 'red')
}

# Disaggregated by Sex 
{
  # Orange = female
  density_sex_AV <- ggplot(UMCG, aes(x=AV, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + theme(legend.position = "none") + labs(x = "AV (mm)")
  
  density_sex_weight <- ggplot(UMCG, aes(x=Weight, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Weight (kg)") +
    theme(legend.position = "right") + theme(axis.title.y = element_blank())
  
  density_sex_height <- ggplot(UMCG, aes(x=Height, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Height (cm)") +
    theme(legend.position = "none")
  
  density_sex_age <- ggplot(UMCG, aes(x=Age, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Age in Years") +
    theme(legend.position = "none") + theme(axis.title.y = element_blank())
  
  #density_sex_BSA <- ggplot(UMCG, aes(x=BSA, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+theme(legend.position = "none")
}
grid.arrange(density_sex_AV, density_sex_weight, density_sex_height, density_sex_age, nrow = 2) 


# Correlations
UMCG_cont <- UMCG[, c("BSA", "Height", "Weight", "Age", "AV")]
ggcorr(UMCG_cont, label = TRUE)

# Sex subsets
UMCG_subset <- UMCG[, c("BSA", "Height", "Weight", "Age", "AV", "Sex")]
UMCG_female <- subset(UMCG_subset, Sex == '1', -c(Sex)) 
UMCG_male <- subset(UMCG_subset, Sex == '0', -c(Sex)) 

corr_female <- ggcorr(UMCG_female, label = TRUE) + labs(title = "Correlation Matrix for Females") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
corr_male <- ggcorr(UMCG_male, label = TRUE) + labs(title = "Correlation Matrix for Males") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(corr_female, corr_male, nrow = 1)

# Simple Linear Plots after linearising transformations
{
  plot_AV_BSA <- ggplot(UMCG, aes(x = BSA, y = AV))  +
    geom_point() + 
    geom_smooth(method = "lm", se = TRUE) +  # Add linear fit with confidence intervals
    labs(x = "BSA (m²)", y = "AV (mm)") +
    theme_minimal()
  
  plot_AV_Height <- ggplot(UMCG, aes(x = Height, y = AV)) +
    geom_point() + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Height (cm)", y = "AV") +
    theme_minimal()+ theme(axis.title.y = element_blank())
  
  plot_AV_Weight <- ggplot(UMCG, aes(x = Weight, y = AV)) +
    geom_point() + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Weight (kg)", y = "AV") +
    theme_minimal()+ theme(axis.title.y = element_blank())
  
  plot_AV_Age <- ggplot(UMCG, aes(x = Age, y = AV)) +
    geom_point() + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Age in Years", y = "AV (mm)") +
    theme_minimal()
  
  # Transformations
  
  plot_AV_logAge <- ggplot(UMCG, aes(x = logAge, y = logAV)) +
    geom_point() + 
    geom_smooth(method = "lm", se = TRUE) + 
    labs(x = "log(Age + 1)", y = "log(AV)") +
    theme_minimal()
  
  plot_AV_sqrtHeight <- ggplot(UMCG, aes(x = sqrtHeight, y = logAV)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) + 
    labs(x = "Squareroot Height", y = "log(AV)") +
    theme_minimal() + theme(axis.title.y = element_blank())
  
  plot_AV_logWeight <- ggplot(UMCG, aes(x = logWeight, y = logAV)) +
    geom_point() +  
    geom_smooth(method = "lm", se = TRUE) + 
    labs(x = "log(Weight)", y = "log(AV)") +
    theme_minimal() + theme(axis.title.y = element_blank())
}
grid.arrange(plot_AV_Age, plot_AV_Height, plot_AV_Weight, plot_AV_logAge, plot_AV_sqrtHeight, plot_AV_logWeight, ncol = 3)

# Closer inspection: fitting LOESS
#   - Visualising AV versus variables split by sex
{
  loess_age_plot <- ggplot(UMCG, aes(x = Age, y = AV)) +
    geom_point(aes(color = Sex)) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(data = UMCG, aes(color = Sex), method = "loess", se = TRUE) +
    labs(x = "Age", y = "AV", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  loess_logage_plot <- ggplot(UMCG, aes(x = logAge, y = logAV)) +
    geom_point(aes(color = Sex)) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = TRUE) +
    labs(x = "log(Age+1)", y = "log(AV)", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  lm_logage_plot <- ggplot(UMCG, aes(x = logAge, y = logAV)) +
    geom_point(aes(color = Sex)) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = TRUE) +
    labs(x = "log(Age+1)", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.x = element_blank())
  
  loess_height_plot <- ggplot(UMCG, aes(x = Height, y = AV)) +
    geom_point(aes(color = Sex)) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = TRUE) +
    labs(x = "Height", y = "AV", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  loess_sqrtheight_plot <- ggplot(UMCG, aes(x = sqrtHeight, y = logAV))+
    geom_point(aes(color = Sex)) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = TRUE) +
    labs(x = "Squareroot Height", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
  
  lm_sqrtheight_plot <- ggplot(UMCG, aes(x = sqrtHeight, y = logAV))+
    geom_point(aes(color = Sex)) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = TRUE) +
    labs(x = "Squareroot Height", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(axis.title.y = element_blank(), legend.position = "none", axis.title.x = element_blank())
  
  loess_weight_plot <- ggplot(UMCG, aes(x = Weight, y = AV)) +
    geom_point(aes(color = Sex)) +  
    scale_fill_gradient(low = "lightgrey", high = "black") +   
    geom_smooth(aes(color = Sex), method = "loess", se = TRUE) +
    labs(x = "Weight", y = "AV", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female"))
  theme_minimal() + theme(legend.position = "none")
  
  loess_logweight_plot <- ggplot(UMCG, aes(x = logWeight, y = logAV))+
    geom_point(aes(color = Sex)) +  
    scale_fill_gradient(low = "lightgrey", high = "black") +  
    geom_smooth(aes(color = Sex), method = "loess", se = TRUE) +
    labs(x = "log(weight)", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
  
  lm_logweight_plot <- ggplot(UMCG, aes(x = logWeight, y = logAV))+
    geom_point(aes(color = Sex))  +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = TRUE) +
    labs(x = "log(weight)", y = "log(AV)", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(axis.title.y = element_blank(), legend.position = "none", axis.title.x = element_blank())
}

grid.arrange(lm_logage_plot, lm_sqrtheight_plot, lm_logweight_plot, loess_logage_plot, loess_sqrtheight_plot, loess_logweight_plot, nrow = 2)


grid.arrange(lm_plot_age, loess_plot_age, ncol = 2)
grid.arrange(lm_plot_height, loess_plot_height, ncol = 2)
grid.arrange(lm_plot_weight, loess_plot_weight, ncol = 2)
