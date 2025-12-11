#Study Summary Tables

# to clear the workspace
rm(list=ls())

# 1. Load necessary libraries and dataset
library(readxl)
library(tidyverse)
library(gtsummary)
setwd("C:/Users/avach/OneDrive/Desktop/HSDA MSc/Semester C/Thesis")
Extracted_Data <- read_excel("Extracted_Data.xlsx")


# 2. Summary Table of Bibliometric Data
bibliometric_tbl <- Extracted_Data |>
  select(c(Year,JIF)) |>
  tbl_summary(type = list(JIF ~ 'continuous2'), statistic = list(
    all_continuous() ~ c("{mean} ({sd})", "{median} ({sd})"),
    all_categorical() ~ "{n} / {N} ({p}%)"
  ))

bibliometric_tbl

bibliometric_tbl |>
  as_gt() |>
  gt::gtsave(filename = "Rplot_Bibliometric.png")


# 3. Summary Table of Model Data
Extracted_Data$n_predictors <- as.numeric(Extracted_Data$n_predictors)
model_data_tbl <- Extracted_Data |>
select(c(Prediction_Type, Data_Type, Sample_Size, n_predictors, Data_Sharing, Code_Availability, Sample_Size_Calculation)) |>
  tbl_summary(type = list(Sample_Size ~ 'continuous2', n_predictors ~ 'continuous2'),
              statistic = list(
                all_continuous() ~ c("{mean} ({sd})", "{median} ({sd})"),
                all_categorical() ~ "{n} / {N} ({p}%)"
              ))

model_data_tbl

model_data_tbl |>
  as_gt() |>
  gt::gtsave(filename = "Rplot_model_data.png")


# 4. Model type extra table
# Load different dataset
Extracted_Data_model_type <- read_excel("Extracted_Data.xlsx", sheet = "Model Type matrix")

mt_tbl <- Extracted_Data_model_type |>
  select(!StudyID) |>
  tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))

mt_tbl

mt_tbl |>
  as_gt() |>
  gt::gtsave(filename = "Rplot_Model_Type.png")


# 5. Performance measures extra table
# Load different dataset
Extracted_Data_performance_measures <- read_excel("Extracted_Data.xlsx", sheet = "Performance measures matrix")

pm_tbl <- Extracted_Data_performance_measures |>
  select(!StudyID) |>
  tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))

pm_tbl

pm_tbl |>
  as_gt() |>
  gt::gtsave(filename = "Rplot_Performance_Measures.png")


#6. Summary Table of Methodological Data
Methodological_tbl <- Extracted_Data |>
  select(c(DV_Status, Internal_V, External_V, Missing_data_handling)) |>
  tbl_summary(statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"))

Methodological_tbl

Methodological_tbl |>
  as_gt() |>
  gt::gtsave(filename = "Rplot_methodological.png")