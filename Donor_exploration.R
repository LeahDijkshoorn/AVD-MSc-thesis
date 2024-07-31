# Raw data
donor_full <- read.csv(file = "cryolife_core_dataset_converted.csv")
summary(donor_full)

# Cleaned data:
#   - Run Preprocessing.R

# Basic Histograms with normal distribution overlay
{
hist_AV <- ggplot(donor, aes(x = AV)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(donor$AV), sd = sd(donor$AV)), color = "red", linewidth = 1) +  
  theme_minimal() + labs(x = "AV (mm)")
  
hist_weight <- ggplot(donor, aes(x = Weight)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(donor$Weight), sd = sd(donor$Weight)), color = "red", linewidth = 1) +  
  theme_minimal() + theme(axis.title.y = element_blank()) + labs(x = "Weight (kg)")

hist_height <- ggplot(donor, aes(x = Height)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(donor$Height), sd = sd(donor$Height)), color = "red", linewidth = 1) +  
  theme_minimal() + theme(axis.title.y = element_blank()) + labs(x = "Height (cm)")

hist_age <- ggplot(donor, aes(x = Age)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(donor$Age), sd = sd(donor$Age)), color = "red", linewidth = 1) +  
  theme_minimal() + labs(x = "Age in Years")

hist_BSA <- ggplot(donor, aes(x = BSA)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(donor$BSA), sd = sd(donor$BSA)), color = "red", linewidth = 1) +  
  theme_minimal() + theme(axis.title.y = element_blank()) + labs(x = "BSA (m²)")

hist_sex <- ggplot(donor, aes(x = Sex)) +
  geom_bar(stat = "count") +
  scale_x_discrete(labels = c("Male", "Female")) +
  labs(x = "Sex") +
  theme_minimal()
}
grid.arrange(hist_AV, hist_weight, hist_height, hist_age, hist_BSA, hist_sex, nrow = 2) 

hist_logAV <- ggplot(donor, aes(x = logAV)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(donor$logAV), sd = sd(donor$logAV)), color = "red", linewidth = 1) +  
  theme_minimal()+ theme(axis.title.y = element_blank())

grid.arrange(hist_AV, hist_logAV, ncol = 2)

# Normality checks:
{
  # Shapiro-Wilks doesn't work for large datasets
  
  # Standardise data for Kolmogorov-Smirnov test
  ks_original <- ks.test((donor$AV - mean(donor$AV)) / sd(donor$AV), "pnorm")
  ks_log <- ks.test((donor$logAV - mean(donor$logAV)) / sd(donor$logAV), "pnorm")
  
  qqnorm(donor$AV)
  qqline(donor$AV, col = "red")
  
  qqnorm(donor$logAV)
  qqline(donor$logAV, col = 'red')
}


# Disaggregated by Sex 
{
  # Orange = female
  density_sex_AV <- ggplot(donor, aes(x=AV, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + theme(legend.position = "none") + labs(x = "AV (mm)")
  
  density_sex_weight <- ggplot(donor, aes(x=Weight, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Weight (kg)") +
    theme(legend.position = "right") + theme(axis.title.y = element_blank())
  
  density_sex_height <- ggplot(donor, aes(x=Height, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Height (cm)") +
    theme(legend.position = "none")
  
  density_sex_age <- ggplot(donor, aes(x=Age, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+
    theme_minimal() + labs(x = "Age in Years") +
    theme(legend.position = "none") + theme(axis.title.y = element_blank())
  
  #density_sex_BSA <- ggplot(donor, aes(x=BSA, fill=Sex)) + geom_density(alpha=0.4)+scale_fill_brewer(palette="Dark2")+theme(legend.position = "none")
}
grid.arrange(density_sex_AV, density_sex_weight, density_sex_height, density_sex_age, nrow = 2) 

# Identifying Outliers
{
# Raw data:

  ggplot(donor_full, aes(x = Age, y = diameter_AV_new)) +
    geom_hex(bins = 30, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age in Years", y = "AV (mm)") +
    facet_wrap(~Sex, ncol = 2, labeller = labeller(Sex = c('0' = "Male", '1' = "Female"))) +
    theme_minimal()
  
  ggplot(donor_full, aes(x = Age, y = Height_cm)) +
    geom_hex(bins = 50, show.legend = TRUE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age in Years", y = "Height (cm)") +
    facet_wrap(~Sex, ncol = 2, labeller = labeller(Sex = c('0' = "Male", '1' = "Female"))) +
    theme_minimal()+
    geom_hline( yintercept = 200, 
               linetype="dashed", color = "red")
  
  ggplot(donor_full, aes(x = diameter_AV_new, y = Height_cm)) +
    geom_hex(bins = 33, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "AV (mm)", y = "Height (cm)") +
    facet_wrap(~Sex, ncol = 2, labeller = labeller(Sex = c('0' = "Male", '1' = "Female"))) +
    theme_minimal()
  
  ggplot(donor_full, aes(x = Height_cm, y = diameter_AV_new)) +
    geom_hex(bins = 33, show.legend = TRUE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Height (cm)", y = "AV (mm)") +
    facet_wrap(~Sex, ncol = 2, labeller = labeller(Sex = c('0' = "Male", '1' = "Female"))) +
    theme_minimal()
  
  ggplot(donor_full, aes(x = Age, y = Weight_kg)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "Weight (kg)") +
    theme_minimal()

# After outlier removal
  
  ggplot(donor, aes(x = Age, y = AV)) +
    geom_hex(bins = 28, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "AV (mm)") +
    facet_wrap(~Sex, ncol = 2, labeller = labeller(Sex = c('0' = "Male", '1' = "Female"))) +
    theme_minimal()
  
  ggplot(donor, aes(x = Age, y = Height)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "Height (cm)") +
    facet_wrap(~Sex, ncol = 2, labeller = labeller(Sex = c('0' = "Male", '1' = "Female"))) +
    theme_minimal()
  
  ggplot(donor, aes(x = Age, y = Weight)) +
    geom_hex(bins = 50, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "Weight (kg)") +
    theme_minimal()
  
# Raw data under 5
  
  donor_under18_full <- subset(donor_full, Age < 5)
  
  ggplot(donor_under18_full, aes(x = Age, y = diameter_AV_new)) +
    geom_hex(bins = 30, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "AV (mm)") +
    theme_minimal()
  
  ggplot(donor_under18_full, aes(x = Age, y = Height_cm)) +
    geom_hex(bins = 30, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "Height (cm)") +
    theme_minimal()
  
  ggplot(donor_under18_full, aes(x = Age, y = Weight_kg)) +
    geom_hex(bins = 30, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "Weight (kg)") +
    theme_minimal()

# After outlier removal under 5
  
  donor_under18 <- subset(donor, Age < 5)
  
  ggplot(donor_under18, aes(x = Age, y = AV)) +
    geom_hex(bins = 30, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    labs(x = "Age", y = "AV (mm)") +
    theme_minimal()
  
  ggplot(donor_under18, aes(x = Age, y = Height)) +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    labs(x = "Age", y = "Height (cm)") +
    theme_minimal()
  
  ggplot(donor_under18, aes(x = Age, y = Weight)) +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    labs(x = "Age", y = "Weight (kg)") +
    theme_minimal()
}


# Correlations
donor_cont <- donor[, c("BSA", "Height", "Weight", "Age", "AV")]
ggcorr(donor_cont, label = TRUE)

# Sex subsets
donor_subset <- donor[, c("BSA", "Height", "Weight", "Age", "AV", "Sex")]
donor_female <- subset(donor_subset, Sex == '0', -c(Sex)) 
donor_male <- subset(donor_subset, Sex == '1', -c(Sex)) 

corr_female <- ggcorr(donor_female, label = TRUE) + labs(title = "Correlation Plot for Females") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
corr_male <- ggcorr(donor_male, label = TRUE) + labs(title = "Correlation Plot for Males") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(corr_female, corr_male, nrow = 1)

# Simple Linear Plots after linearising transformations
{
  plot_AV_BSA <- ggplot(donor, aes(x = BSA, y = AV)) +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  # Add linear fit with confidence intervals
    labs(x = "BSA (m²)", y = "AV (mm)") +
    theme_minimal()
  
  plot_AV_Height <- ggplot(donor, aes(x = Height, y = AV)) +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Height (cm)", y = "AV") +
    theme_minimal()+ theme(axis.title.y = element_blank())
  
  plot_AV_Weight <- ggplot(donor, aes(x = Weight, y = AV))  +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Weight (kg)", y = "AV") +
    theme_minimal()+ theme(axis.title.y = element_blank())
  
  plot_AV_Age <- ggplot(donor, aes(x = Age, y = AV)) +
    geom_hex(bins = 28, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "Age in Years", y = "AV (mm)") +
    theme_minimal()
  
  # Transformations
  
  plot_AV_logBSA <- ggplot(donor, aes(x = logBSA, y = logAV))  +
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "log(BSA)", y = "log(AV)") +
    theme_minimal()
  
  plot_AV_logAge <- ggplot(donor, aes(x = logAge, y = logAV)) +
    geom_hex(bins = 25, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) +  
    labs(x = "log(Age+1)", y = "log(AV)") +
    theme_minimal()
  
  plot_AV_logWeight <- ggplot(donor, aes(x = logWeight, y = logAV)) +
    geom_hex(bins = 25, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(method = "lm", se = TRUE) + 
    labs(x = "log(Weight)", y = "log(AV)") +
    theme_minimal()+ theme(axis.title.y = element_blank())
  
  plot_logAV_sqrtHeight <- ggplot(donor, aes(x = sqrtHeight, y = logAV)) +
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
  loess_age_plot <- ggplot(donor, aes(x = Age, y = AV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(data = donor, aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "Age", y = "AV", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  loess_logage_plot <- ggplot(donor, aes(x = logAge, y = logAV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "log(Age+1)", y = "log(AV)", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  lm_logage_plot <- ggplot(donor, aes(x = logAge, y = logAV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = FALSE) +
    labs(x = "log(Age+1)", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.x = element_blank())
  
  loess_height_plot <- ggplot(donor, aes(x = Height, y = AV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "Height", y = "AV", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none")
  
  loess_sqrtheight_plot <- ggplot(donor, aes(x = sqrtHeight, y = logAV))+
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "Squareroot Height", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
  
  lm_sqrtheight_plot <- ggplot(donor, aes(x = sqrtHeight, y = logAV))+
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = FALSE) +
    labs(x = "Squareroot Height", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(axis.title.y = element_blank(), legend.position = "none", axis.title.x = element_blank())
  
  loess_weight_plot <- ggplot(donor, aes(x = Weight, y = AV)) +
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") +   
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "Weight", y = "AV", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female"))
    theme_minimal() + theme(legend.position = "none")
  
  loess_logweight_plot <- ggplot(donor, aes(x = logWeight, y = logAV))+
    geom_hex(bins = 30) +  
    scale_fill_gradient(low = "lightgrey", high = "black") +  
    geom_smooth(aes(color = Sex), method = "loess", se = FALSE) +
    labs(x = "log(weight)", y = "log(AV)", color = "Sex") +
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank())
  
  lm_logweight_plot <- ggplot(donor, aes(x = logWeight, y = logAV))+
    geom_hex(bins = 30, show.legend = FALSE) +  
    scale_fill_gradient(low = "lightgrey", high = "black") + 
    geom_smooth(aes(color = Sex), method = "lm", se = FALSE) +
    labs(x = "log(weight)", y = "log(AV)", color = "Sex")+
    scale_color_manual(values = c("1" = "#ff7f0e", "0" = "#1f77b4"), 
                       labels = c("0" = "Male", "1" = "Female")) +
    theme_minimal() + theme(axis.title.y = element_blank(), legend.position = "none", axis.title.x = element_blank())
}

grid.arrange(lm_logage_plot, lm_sqrtheight_plot, lm_logweight_plot, loess_logage_plot, loess_sqrtheight_plot, loess_logweight_plot, nrow = 2)


# Trend

ggplot(donor, aes(x = as.Date(dissection_date), y = AV)) +  
  geom_hex(bins = 28) +  
  scale_fill_gradient(low = "lightgrey", high = "black") + 
  geom_smooth(method = "loess", se = FALSE, span = 0.15) +
  labs(x = "Dissection Date", y = "AV (mm)") +
  theme_minimal() + theme(legend.position = "right") #+ geom_vline(xintercept = as.Date(c("1994-10-02", "2004-08-01", "2010-08-29", "2015-02-01")), linetype="dashed", color = "red")

trend_plot_age <- ggplot(donor, aes(x = as.Date(dissection_date), y = Age)) + 
  geom_hex(bins = 50) +  
  scale_fill_gradient(low = "lightgrey", high = "black") +  
  geom_smooth(method = "loess", se = FALSE, span = 0.15) +
  labs(x = "Dissection Date", y = "Age") +
  theme_minimal() + theme(legend.position = "none", axis.title.x = element_blank()) #+ geom_vline(xintercept = as.Date(c("1994-10-02", "2004-08-01", "2010-08-29", "2015-02-01")), linetype="dashed", color = "red")

trend_plot_height <- ggplot(donor, aes(x = as.Date(dissection_date), y = Height)) + 
  geom_hex(bins = 50) +  
  scale_fill_gradient(low = "lightgrey", high = "black") +  
  geom_smooth(method = "loess", se = FALSE, span = 0.15) +
  labs(x = "Dissection Date", y = "Height") +
  theme_minimal() + theme(legend.position = "none", axis.title.x = element_blank()) #+ geom_vline(xintercept = as.Date(c("1994-10-02", "2004-08-01", "2010-08-29", "2015-02-01")), linetype="dashed", color = "red")

trend_plot_weight <- ggplot(donor, aes(x = as.Date(dissection_date), y = Weight)) + 
  geom_hex(bins = 50) +  
  scale_fill_gradient(low = "lightgrey", high = "black") +  
  geom_smooth(method = "loess", se = FALSE, span = 0.15) +
  labs(x = "Dissection Date", y = "Weight") +
  theme_minimal() + theme(legend.position = "none") #+ geom_vline(xintercept = as.Date(c("1994-10-02", "2004-08-01", "2010-08-29", "2015-02-01")), linetype="dashed", color = "red")

grid.arrange(trend_plot_age, trend_plot_height, trend_plot_weight, ncol = 1)


# A look at BSA:
{
  donor$BSA_DuBois <- 0.20247 * (donor$Height/100)^0.725 * donor$Weight^0.425
  donor$BSA_Boyd <- 0.017827 * donor$Height^0.5 * donor$Weight^0.4838
  donor$BSA_Fujimoto <- 0.008883 * donor$Height^0.663 * donor$Weight^0.444
  donor$BSA_Haycock <- 0.024265 * donor$Height^0.3964 * donor$Weight^0.5378
  donor$BSA_Mosteller <- sqrt((donor$Height * donor$Weight) / 3600)
  
  # Compute the average BSA per donor
  donor$Average_BSA <- rowMeans(donor[, c("BSA_DuBois", "BSA_Boyd", "BSA_Fujimoto", "BSA_Haycock", "BSA_Mosteller")])
  
  # Calculate deviations from the average
  donor$BSA_DuBois_dev <- donor$BSA_DuBois - donor$Average_BSA
  donor$BSA_Boyd_dev <- donor$BSA_Boyd - donor$Average_BSA
  donor$BSA_Fujimoto_dev <- donor$BSA_Fujimoto - donor$Average_BSA
  donor$BSA_Haycock_dev <- donor$BSA_Haycock - donor$Average_BSA
  donor$BSA_Mosteller_dev <- donor$BSA_Mosteller - donor$Average_BSA
  
  # Reshape data
  donor_long <- donor %>%
    gather(key = "Method", value = "BSA", BSA_DuBois, BSA_Boyd, BSA_Fujimoto, BSA_Haycock, BSA_Mosteller)
  
  # Boxplot
  ggplot(donor_long, aes(x = Method, y = BSA, fill = Method)) +
    geom_boxplot(fill = "lightgrey") +
    labs(x = "BSA Formula",
         y = "Body Surface Area (m²)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")  # Tilts method names
  
  # Boxplots on extreme subgroups of the data
  
  # Very Obese Adults
  donor$BMI <- donor$Weight / (donor$Height / 100)^2
  obese_individuals <- donor[donor$BMI > 40, ]
  
  donor_obese_long <- obese_individuals %>%
    gather(key = "Method", value = "BSA", BSA_DuBois, BSA_Boyd, BSA_Fujimoto, BSA_Haycock, BSA_Mosteller)
  
  ggplot(donor_obese_long, aes(x = Method, y = BSA, fill = Method)) +
    geom_boxplot(fill = "lightgrey") +
    labs(x = "BSA Formula",
         y = "Body Surface Area (m²)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none", axis.title.y = element_blank()) 
  
  # Very Short Adults
  threshold_Height <- quantile(donor$Height[donor$Age > 18], 0.03)
  very_short_adults <- donor[donor$Age > 18 & donor$Height < threshold_Height, ]
  
  donor_short_adults_long <- very_short_adults %>%
    gather(key = "Method", value = "BSA", BSA_DuBois, BSA_Boyd, BSA_Fujimoto, BSA_Haycock, BSA_Mosteller)
  
  ggplot(donor_short_adults_long, aes(x = Method, y = BSA, fill = Method)) +
    geom_boxplot(fill = "lightgrey") +
    labs(x = "BSA Formula",
         y = "Body Surface Area (m²)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 
  
  # Deviations from average BSA
  
  # Reshape data to long format for deviations
  donor_long_dev <- donor %>%
    gather(key = "Method", value = "BSA_Deviation", BSA_DuBois_dev, BSA_Boyd_dev, BSA_Fujimoto_dev, BSA_Haycock_dev, BSA_Mosteller_dev)
  
  # Boxplot for deviations
  ggplot(donor_long_dev, aes(x = Method, y = BSA_Deviation, fill = Method)) +
    geom_boxplot(fill = "lightgrey") +
    labs(x = "BSA Formula", y = "Deviation from Average BSA (m²) computation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  obese_long_dev <- obese_individuals %>%
    gather(key = "Method", value = "BSA_Deviation", BSA_DuBois_dev, BSA_Boyd_dev, BSA_Fujimoto_dev, BSA_Haycock_dev, BSA_Mosteller_dev)
  
  # Boxplot for obese individuals
  ggplot(obese_long_dev, aes(x = Method, y = BSA_Deviation, fill = Method)) +
    geom_boxplot(fill = "lightgrey") +
    labs(x = "BSA Formula", y = "Deviation from Average BSA (m²) computation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  short_adults_long_dev <- very_short_adults %>%
    gather(key = "Method", value = "BSA_Deviation", BSA_DuBois_dev, BSA_Boyd_dev, BSA_Fujimoto_dev, BSA_Haycock_dev, BSA_Mosteller_dev)
  
  # Boxplot for very short adults
  ggplot(short_adults_long_dev, aes(x = Method, y = BSA_Deviation, fill = Method)) +
    geom_boxplot(fill = "lightgrey") +
    labs(x = "BSA Formula", y = "Deviation from Average BSA (m²) computation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  # LOESS fits
  {
    
    LOESS_BSA_Haycock <- ggplot(donor, aes(x = BSA_Haycock, y = AV)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA Haycock", y = "AV") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_BSA_DuBois <- ggplot(donor, aes(x = BSA_DuBois, y = AV)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA DuBois", y = "AV") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_BSA_Boyd <- ggplot(donor, aes(x = BSA_Boyd, y = AV)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA Boyd", y = "AV") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_BSA_Fujimoto <- ggplot(donor, aes(x = BSA_Fujimoto, y = AV)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA Fujimoto", y = "AV") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_BSA_Mosteller <- ggplot(donor, aes(x = BSA_Mosteller, y = AV)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA Mosteller", y = "AV") +
      theme_minimal() + theme(legend.position = "none")
    
    # Weight
    
    LOESS_BSA_Haycock <- ggplot(donor, aes(x = BSA_Haycock, y = Weight)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA Haycock", y = "Weight") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_BSA_DuBois <- ggplot(donor, aes(x = BSA_DuBois, y = Weight)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA DuBois", y = "Weight") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_BSA_Boyd <- ggplot(donor, aes(x = BSA_Boyd, y = Weight)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA Boyd", y = "Weight") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_BSA_Fujimoto <- ggplot(donor, aes(x = BSA_Fujimoto, y = Weight)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA Fujimoto", y = "Weight") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_BSA_Mosteller <- ggplot(donor, aes(x = BSA_Mosteller, y = Weight)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA Mosteller", y = "Weight") +
      theme_minimal() + theme(legend.position = "none")
    
    # BSA versus AV
    
    LOESS_BSA_AV_plot <- ggplot(donor, aes(x = BSA, y = AV)) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "BSA", y = "AV") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_sqrtBSA_AV_plot <- ggplot(donor, aes(x = sqrt(BSA), y = log(AV))) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "sqrt(BSA)", y = "AV") +
      theme_minimal() + theme(legend.position = "none")
    
    LOESS_logBSA_AV_plot <- ggplot(donor, aes(x = log(BSA), y = log(AV))) +
      geom_hex(bins = 30) +  
      scale_fill_gradient(low = "lightgrey", high = "black") + 
      geom_smooth(data = donor, method = "loess", se = FALSE) +
      labs(x = "log(BSA)", y = "AV") +
      theme_minimal() + theme(legend.position = "none")
    }
}
