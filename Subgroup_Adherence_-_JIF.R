# Subgroup analysis: TRIPOD+AI adherence / Journal Impact Factor

# to clear the workspace
rm(list=ls())

# 1. Import the necessary libraries and dataset
library(tidyverse)
library(rstatix)
library(psych)
library(ggsci)
library(readxl)
data <- read_excel("C:/Users/avach/OneDrive/Desktop/HSDA MSc/Semester C/Thesis/Extracted_Data.xlsx")
view(data)
glimpse(data)

# 2. prepare the data
data$overall_TRIPOD_ad_percent <- data$overall_TRIPOD_ad_percent*100
data <- data |>
  filter(data$JIF != "N/A")

data$JIF <- as.double(data$JIF)

# 3. Insert new variable "JIF_value" describing high and low impact factor split by the median
data$JIF_value <- ifelse(data$JIF <= median(data$JIF), "low IF", "high IF")

glimpse(data)

# Convert 'JIF_value' into factor
data <- data |>
  mutate(JIF_value = factor(JIF_value))

glimpse(data)   


# 4. Check the assumption of Normality 

# No1: graphically with an histogram
data |>
  ggplot(aes(x = overall_TRIPOD_ad_percent, after_stat(density))) + 
  geom_density(fill = "purple", color="black", alpha = .35) +
  facet_wrap(~JIF_value, ncol = 2) 

# No2: calculating the summary measures
describeBy(data$overall_TRIPOD_ad_percent, group = data$JIF_value, na.rm = TRUE, ranges = TRUE, IQR = TRUE, quant = c(.25, .75))

# No3: Shapiro-Wilk test for normality
data |>
  group_by(JIF_value) |>
  shapiro_test(overall_TRIPOD_ad_percent) |>
  ungroup()

# Therefore: independent Samples t-test

# 4. Levene's test for equality of variances
data |>
  levene_test(overall_TRIPOD_ad_percent ~ JIF_value)

# 5. t_test
data |> 
  t_test(overall_TRIPOD_ad_percent ~ JIF_value, var.equal = T, detailed = T)