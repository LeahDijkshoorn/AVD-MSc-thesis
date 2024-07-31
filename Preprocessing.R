library(readxl)
library(plyr)
library(ggplot2)
library(GGally)
library(gridExtra)
library(corrplot)
library(tidyverse)
library(dplyr) # Trend correc.
library(lubridate) # To aggregate the data for trend correc.
library(mgcv) # To apply GAMs
library(caret) # Train-test split
library(lmtest) # KW and BP test
library(MASS) # Robust LM
library(visreg)

setwd("C:/Users/leahd/Documents/MSc_thesis/MScThesis")

# PREPROCESSING

{
  donor <- read.csv(file = "cryolife_core_dataset_converted.csv")
  UMCG <- read_excel("Echo_measurements_AoV_annulus_in_normal_subjects.xlsx")
  # Load in the separate datasets for Lopez et al. data and combine:
  {
    sex_data <- read.csv(file = "demo.csv")
    UMCG_data <- read.csv(file = "echo_ecl.csv")
    demo_data <- read.csv(file = "echo_local.csv")
    
    # Select the required columns from each dataset
    sex_data_selected <- sex_data[, c("Blind_id", "SEX")]
    demo_data_selected <- demo_data[, c("Blind_id", "ECHO_AGE", "ECHO_HT", "ECHO_WT")]
    UMCG_data_selected <- UMCG_data[, c("Blind_id", "ARAVAD")]
    
    # Combine the datasets using merge
    lopez <- merge(sex_data_selected, demo_data_selected, by = "Blind_id", all.x = TRUE)
    lopez <- merge(lopez, UMCG_data_selected, by = "Blind_id", all.x = TRUE)
    
    # Remove temp datasets from environment
    rm(sex_data_selected, demo_data_selected, UMCG_data_selected, demo_data, UMCG_data, sex_data)
  }
  
  # Rename columns for consistency
  names(UMCG)[names(UMCG) == "Seks"] <- "Sex"
  names(UMCG)[names(UMCG) == "diam mean"] <- "AV"
  names(UMCG)[names(UMCG) == "Age at echo"] <- "Age"
  
  names(donor)[names(donor) == "diameter_AV_new"] <- "AV"
  names(donor)[names(donor) == "Weight_kg"] <- "Weight"
  names(donor)[names(donor) == "Height_cm"] <- "Height"
  names(donor)[names(donor) == "BSA_Haycock"] <- "BSA"

  names(lopez)[names(lopez) == "SEX"] <- "Sex"
  names(lopez)[names(lopez) == "ECHO_AGE"] <- "Age"
  names(lopez)[names(lopez) == "ECHO_HT"] <- "Height"
  names(lopez)[names(lopez) == "ECHO_WT"] <- "Weight"
  names(lopez)[names(lopez) == "ARAVAD"] <- "AV"
  
  # Keep only relevant columns
  donor <- donor[, c("Sex", "Height", "Weight", "Age", "BSA", "AV", "DonorID", "dissection_date")]
  UMCG <- UMCG[, c("Sex", "Height", "Weight", "Age", "AV")]
  
  # Clean
  UMCG[UMCG == 999] <- NA
  UMCG <- na.omit(UMCG)
  UMCG$Sex <- ifelse(UMCG$Sex == "F", 1, 0)
  UMCG$Sex <- as.factor(UMCG$Sex)
  UMCG$BSA <- 0.024265 * (UMCG$Height)^0.3964 * (UMCG$Weight)^0.5378
  
  donor <- na.omit(donor)
  donor$Sex <- as.factor(donor$Sex)
  
  lopez <- na.omit(lopez)
  lopez$AV <- (lopez$AV)*10   # Convert AV in cm to mm
  lopez$Sex <- ifelse(lopez$Sex == "Female", 1, 0)
  lopez$Sex <- as.factor(lopez$Sex)
  lopez$BSA <- 0.024265 * (lopez$Height)^0.3964 * (lopez$Weight)^0.5378
  
  # Removing outliers in donor data:
  {
     exclude_rows <- donor$AV > 28 & donor$Sex == "1" # Create a logical vector where TRUE indicates rows to exclude
     donor <- donor[!exclude_rows, ]
     
     donor <- subset(donor, AV < 34)
    
     # Removing outliers outside of 97th percentile for weight
       donor$Age_rounded <- ifelse(donor$Age < 2,
                             round(donor$Age * 5) / 5,  # Rounding to nearest 0.2 for ages < 2 years
                             round(donor$Age))          # Rounding to nearest whole number for ages >= 2 years
    
    
       # Grouping and calculating the 99th percentile for each rounded age group
       percentile_data <- donor %>%
         group_by(Age_rounded) %>%
         summarise(Weight_99th = quantile(Weight, 0.99), .groups = 'drop')
    
       # Merge the percentile thresholds back to the original data
       donor <- merge(donor, percentile_data, by = "Age_rounded")
    
       # Filter out entries where weight exceeds the 97th percentile for their age
       donor <- donor %>%
         filter(Weight <= Weight_99th)
    
     # Removing outliers outside of 0.5th percentile for height
       percentile_data <- donor %>%
         group_by(Age_rounded) %>%
         summarise(Height_05th = quantile(Height, 0.005), .groups = 'drop')
    
       donor <- merge(donor, percentile_data, by = "Age_rounded")
    
       donor <- donor %>%
         filter(Height >= Height_05th)
    
     # Removing outliers outside of 97th percentile for height
       # Splitting the dataset
       data_under_5 <- donor %>% filter(Age < 5)
       data_over_5 <- donor %>% filter(Age >= 5)
    
       # Remove implausible heights for over 5 dataset
       data_over_5 <- subset(data_over_5, Height <= 198)
    
       # Calculate the 99th percentile for height in the under 5 dataset
       percentile_data <- data_under_5 %>%
         group_by(Age_rounded) %>%
         summarise(Height_99th = quantile(Height, 0.99), .groups = 'drop')
    
       data_under_5 <- merge(data_under_5, percentile_data, by = "Age_rounded")
    
       data_under_5 <- data_under_5 %>%
         filter(Height <= Height_99th)
    
       # Now, recombine the data
       donor <- bind_rows(data_under_5, data_over_5)
    
       # Remove temp columns
       donor$Age_rounded <- NULL
       donor$Weight_99th <- NULL
       donor$Height_99th <- NULL
       donor$Height_05th <- NULL
       
       # Remove temp environment objects
       rm(percentile_data, data_over_5, data_under_5, exclude_rows)
  }
  
  # Remove outliers in UMCG data:
  {
    UMCG <- subset(UMCG, AV > 15)
    UMCG <- subset(UMCG, Weight < 120)
  }
  
  # Add data source column and make a combined dataset
  UMCG$Source <- 0
  donor$Source <- 1
  lopez$Source <- 2
  
  data <- rbind(donor[ , !(names(donor) %in% c("DonorID", "dissection_date"))], UMCG)
  data <- rbind(lopez[ , !(names(lopez) %in% c("Blind_id"))], data)
  
  data$Source <- as.factor(data$Source)
}


# TRANSFORMATIONS
{
  donor$logAV <- log(donor$AV)
  donor$logBSA <- log(donor$BSA)
  donor$logAge <- log(donor$Age + 1)
  donor$sqrtHeight <- sqrt(donor$Height)
  donor$logWeight <- log(donor$Weight)
  
  UMCG$logAV <- log(UMCG$AV)
  UMCG$logBSA <- log(UMCG$BSA)
  UMCG$logAge <- log(UMCG$Age + 1)
  UMCG$sqrtHeight <- sqrt(UMCG$Height)
  UMCG$logWeight <- log(UMCG$Weight)
  
  lopez$logAV <- log(lopez$AV)
  lopez$logBSA <- log(lopez$BSA)
  lopez$logAge <- log(lopez$Age + 1)
  lopez$sqrtHeight <- sqrt(lopez$Height)
  lopez$logWeight <- log(lopez$Weight)
  
  data$logAV <- log(data$AV)
  data$logBSA <- log(data$BSA)
  data$logAge <- log(data$Age + 1)
  data$sqrtHeight <- sqrt(data$Height)
  data$logWeight <- log(data$Weight)
}

