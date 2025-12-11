# Subgroup analysis: TRIPOD+AI adherence / Use of external validation

# to clear the workspace
rm(list=ls())

# 1. Load necessary libraries and dataset
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(rstatix)
library(ggplot2)
data <- read_excel("C:/Users/avach/OneDrive/Desktop/HSDA MSc/Semester C/Thesis/Extracted_Data.xlsx", sheet = "Study Data")
View(data)
glimpse(data)

# 2.Convert adherence from a decimal value to a percentage value
data$overall_TRIPOD_ad_percent <- data$overall_TRIPOD_ad_percent*100
View(data)
glimpse(data)

data <- data |>
  mutate(DV_Status = factor(DV_Status))
glimpse(data)

# 3. Check the assumption of Normality 

# No1: graphically with an histogram
data |>
  ggplot(aes(x = overall_TRIPOD_ad_percent, after_stat(density), fill = DV_Status)) + 
  geom_density(fill = "green", color="black", alpha = .5) +
  facet_wrap(~DV_Status, ncol = 2) 


# No2: calculating the summary measures
psych::describeBy(data$overall_TRIPOD_ad_percent, group = data$DV_Status, na.rm = TRUE, ranges = TRUE, IQR = TRUE, quant = c(.25, .75))

# No3: Shapiro-Wilk test for normality
data |>
  group_by(DV_Status) |>
  shapiro_test(overall_TRIPOD_ad_percent) |>
  ungroup()

# Therefore: independent Samples t-test

# 4. Levene's test for equality of variances
data |>
  levene_test(overall_TRIPOD_ad_percent ~ DV_Status)


# 5. t_test
data |> 
  t_test(overall_TRIPOD_ad_percent ~ DV_Status, var.equal = T, detailed = T)