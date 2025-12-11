#Subgroup analysis: TRIPOD+AI adherence / Model population

# to clear the workspace
rm(list=ls())

# 1. Load necessary libraries and dataset
library(tidyverse)
library(rstatix)
library(psych)
library(ggsci)
library(readxl)
data <- read_excel("C:/Users/avach/OneDrive/Desktop/HSDA MSc/Semester C/Thesis/Extracted_Data.xlsx")
view(data)
glimpse(data)

data$overall_TRIPOD_ad_percent <- data$overall_TRIPOD_ad_percent*100

# 2. remove NAs
data <- data |>
  filter(data$model_pop != "NA")

glimpse(data)

# 3. Convert 'model_pop' into factor
data <- data |>
  mutate(model_pop = factor(model_pop))

glimpse(data)   


# 3. Check the assumption of Normality 

# No1: graphically with an histogram
data |>
  ggplot(aes(x = overall_TRIPOD_ad_percent, after_stat(density))) + 
  geom_density(fill = "purple", color="black", alpha = .35) +
  facet_wrap(~ model_pop, ncol = 2) 

# No2: calculating the summary measures
describeBy(data$overall_TRIPOD_ad_percent, group = data$model_pop, na.rm = TRUE, ranges = TRUE, IQR = TRUE, quant = c(.25, .75))

# No3: Shapiro-Wilk test for normality
data |>
  group_by(model_pop) |>
  shapiro_test(overall_TRIPOD_ad_percent) |>
  ungroup()

# Therefore: independent Samples t-test

# 4. Levene's test for equality of variances
data |>
  levene_test(overall_TRIPOD_ad_percent ~ model_pop)

# 5. t_test
data |> 
  t_test(overall_TRIPOD_ad_percent ~ model_pop, var.equal = T, detailed = T)