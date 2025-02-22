# Install and load necessary libraries
library(ggplot2)

# Load the data
data <- read.csv("/Users/muralikarthikganji/Documents/DATA363_Survey_Data.csv")

# Summary statistics: Mean, Median, Standard Deviation
summary_stats <- data.frame(
  Variable = c("Time on Social Media (min/day)", "Time Using AI Tools (hrs/week)", 
               "Study Hours (hrs/day)", "GPA"),
  Mean = c(mean(data$Time_on_Social_Media_Min_per_day),
           mean(data$Time_Spent_Using_AI_Tools_Hrs_per_week),
           mean(data$Study_Hours_per_day),
           mean(data$GPA)),
  Median = c(median(data$Time_on_Social_Media_Min_per_day),
             median(data$Time_Spent_Using_AI_Tools_Hrs_per_week),
             median(data$Study_Hours_per_day),
             median(data$GPA)),
  SD = c(sd(data$Time_on_Social_Media_Min_per_day),
         sd(data$Time_Spent_Using_AI_Tools_Hrs_per_week),
         sd(data$Study_Hours_per_day),
         sd(data$GPA))
)
print(summary_stats)

# Function to calculate correlation p-value and confidence intervals
correlation_analysis <- function(x, y) {
  cor_result <- cor.test(x, y)
  list(
    correlation = cor_result$estimate,
    p_value = cor_result$p.value,
    conf_int = cor_result$conf.int
  )
}

# Correlation and p-value for Time on Social Media vs. GPA
social_gpa_result <- correlation_analysis(data$Time_on_Social_Media_Min_per_day, data$GPA)
cat("Time on Social Media vs. GPA:\n")
cat("Correlation Coefficient:", social_gpa_result$correlation, "\n")
cat("p-value:", social_gpa_result$p_value, "\n")
cat("95% Confidence Interval:", social_gpa_result$conf_int, "\n\n")

# Correlation and p-value for Time Using AI Tools vs. GPA
ai_gpa_result <- correlation_analysis(data$Time_Spent_Using_AI_Tools_Hrs_per_week, data$GPA)
cat("Time Using AI Tools vs. GPA:\n")
cat("Correlation Coefficient:", ai_gpa_result$correlation, "\n")
cat("p-value:", ai_gpa_result$p_value, "\n")
cat("95% Confidence Interval:", ai_gpa_result$conf_int, "\n\n")

# Correlation and p-value for Study Hours vs. GPA
study_gpa_result <- correlation_analysis(data$Study_Hours_per_day, data$GPA)
cat("Study Hours vs. GPA:\n")
cat("Correlation Coefficient:", study_gpa_result$correlation, "\n")
cat("p-value:", study_gpa_result$p_value, "\n")
cat("95% Confidence Interval:", study_gpa_result$conf_int, "\n\n")

# Scatter plot: Time on Social Media vs. GPA
ggplot(data, aes(x = Time_on_Social_Media_Min_per_day, y = GPA)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Time on Social Media vs. GPA",
       x = "Time on Social Media (minutes/day)",
       y = "GPA") +
  theme_minimal()

# Aggregate mean GPA for each unique value of Time on Social Media
mean_gpa_social <- aggregate(GPA ~ Time_on_Social_Media_Min_per_day, data, mean)

# Scatter Plot: Mean GPA vs. Time on Social Media
ggplot(mean_gpa_social, aes(x = Time_on_Social_Media_Min_per_day, y = GPA)) +
  geom_point() +
  geom_line(color = "blue") +
  labs(title = "Mean GPA vs. Time on Social Media",
       x = "Time on Social Media (minutes/day)",
       y = "Mean GPA") +
  theme_minimal()

# Scatter plot: Time Using AI Tools vs. GPA
ggplot(data, aes(x = Time_Spent_Using_AI_Tools_Hrs_per_week, y = GPA)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Time Using AI Tools vs. GPA",
       x = "Time Spent Using AI Tools (hours/week)",
       y = "GPA") +
  theme_minimal()

# Aggregate mean GPA for each unique value of Time Using AI Tools
mean_gpa_ai <- aggregate(GPA ~ Time_Spent_Using_AI_Tools_Hrs_per_week, data, mean)

# Scatter Plot: Mean GPA vs. Time Using AI Tools
ggplot(mean_gpa_ai, aes(x = Time_Spent_Using_AI_Tools_Hrs_per_week, y = GPA)) +
  geom_point() +
  geom_line(color = "blue") +
  labs(title = "Mean GPA vs. Time Using AI Tools",
       x = "Time Spent Using AI Tools (hours/week)",
       y = "Mean GPA") +
  theme_minimal()

# Bar chart: Perceived Impact of Social Media
ggplot(data, aes(x = Impact_of_Social_Media, fill = Impact_of_Social_Media)) +
  geom_bar() +
  labs(title = "Perceived Impact of Social Media on Academic Performance",
       x = "Perceived Impact",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green", "Neutral" = "gray", "Negative" = "red"))

# Bar chart: Perceived Impact of AI Tools
ggplot(data, aes(x = Impact_of_AI_Tools, fill = Impact_of_AI_Tools)) +
  geom_bar() +
  labs(title = "Perceived Impact of AI Tools on Academic Performance",
       x = "Perceived Impact",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Positive" = "green", "Neutral" = "gray", "Negative" = "red"))

