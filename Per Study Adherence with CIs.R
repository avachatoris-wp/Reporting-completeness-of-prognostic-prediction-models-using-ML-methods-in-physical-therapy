# Calculating Per Study Adherence and CIs

# to clear the workspace
rm(list=ls())

# 1. Load necessary libraries
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
Extracted_Data <- read_excel("C:/Users/avach/OneDrive/Desktop/HSDA MSc/Semester C/Thesis/Extracted_Data.xlsx", sheet = "Per Study Adherence")
View(Extracted_Data)

# 2. Read the data from th Excel file

per_study_adherence <- Extracted_Data

# 3. Rename columns for clarity (optional, but good practice)
# Column B: Number of items which were applicable to this study
# Column C: Number of items that were adhered in this study
colnames(per_study_adherence) <- c("Study_ID", "Applicable_Items", "Adhered_Items", "Adherence_Proportion")


# 4. Calculate confidence intervals for each study
# We will create two new columns: 'CI_lower' and 'CI_upper'

# Initialize the new columns
per_study_adherence$CI_lower <- NA
per_study_adherence$CI_upper <- NA

# Loop through each row to calculate the confidence interval
for (i in 1:nrow(per_study_adherence)) {
  
  # Get values using column index numbers to avoid naming issues.
  # Column B is the 2nd column.
  # Column C is the 3rd column.
  n <- per_study_adherence[[2]][i] # Total number of studies (trials)
  x <- per_study_adherence[[3]][i] # Number of adhered studies (successes)
  
  # Check for valid numbers before calculating
  if (!is.na(n) && !is.na(x) && n > 0) {
    # Use binom.test() to calculate the exact 95% confidence interval
    test_result <- binom.test(x = x, n = n)
    
    # Extract the confidence interval from the results
    ci <- test_result$conf.int
    
    # Store the lower and upper bounds in our new columns
    per_study_adherence$CI_lower[i] <- ci[1]
    per_study_adherence$CI_upper[i] <- ci[2]
  }
}

# 5. Display the final results with the new confidence interval columns
print(per_study_adherence)
tibble::view(per_study_adherence)

# 6. Save the updated data frame to a new Excel file
write_xlsx(per_study_adherence, "Per_Study_Adherence_with_CIs.xlsx")

# 7. Calculate the overall mean adherence and its Confidence Interval

# Sum the total number of adhered items
# na.rm = TRUE is added to safely handle any potential missing values
total_adhered_items <- sum(per_study_adherence[[3]], na.rm = TRUE)

# Sum the total number of applicable items
total_applicable_items <- sum(per_study_adherence[[2]], na.rm = TRUE)

# Calculate the overall mean adherence proportion
mean_study_adherence <- total_adhered_items / total_applicable_items

# Use binom.test() on the total sums to get the overall CI
overall_study_test_result <- binom.test(x = total_adhered_items, n = total_applicable_items)
overall_confidence_interval <- overall_study_test_result$conf.int

# Using sprintf() to format the percentage and CI to two decimal places
cat(sprintf("Mean Adherence: %.2f%% (95%% CI: %.2f%% - %.2f%%)\n",
            mean_study_adherence * 100,
            overall_confidence_interval[1] * 100,
            overall_confidence_interval[2] * 100))

# 8. Summarize the results using a bar chart
per_study_adherence$Study_ID <- factor(per_study_adherence$Study_ID, levels = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33'))
# Create the ggplot object
adherence_study_plot <- ggplot(per_study_adherence, 
                         aes(x = Study_ID, y = Adherence_Proportion)) +
  
  # Add the bars. geom_col is used when the y-value is already calculated.
  geom_col(fill = "skyblue", color = "black", width = 0.7) +
  
  # Add the error bars using the confidence interval columns.
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.3, # Width of the error bar caps
                linewidth = 0.8) + # Thickness of the error bar line
  
  # Add the percentage labels on the bars ***
  # geom_text(aes(label = scales::percent(Adherence_Proportion, accuracy = 0.1)), 
  #         vjust = 2,    # Adjusts position to be just outside the bar
  #        size = 3,        # Sets the font size
  #       color = "black") +
  # Format the axis to show percentages instead of proportions (e.g., 50% vs 0.5).
  # Note: The axis is now x after coord_flip()
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  
  # Add informative labels and a title.
  labs(
    title = "Adherence Rate per Study",
    subtitle = "Bars represent adherence percentage with 95% confidence intervals",
    x = "Study ID",
    y = "Adherence Rate"
  ) +
  
  # Apply a clean theme.
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
    plot.subtitle = element_text(hjust = 0.5) # Center subtitle
  )

  # Display the plot in R
  print(adherence_study_plot)

  # Optional: Save your plot to a file (e.g., a PNG)
  # ggsave("adherence_barchart.png", plot = adherence_plot, width = 10, height = 8, dpi = 300)