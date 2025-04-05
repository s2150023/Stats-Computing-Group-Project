
# Place the code needed in the Report_project02.Rmd, including documentation.
setwd("/Users/adamsmith/Downloads/Statistical Computing/Group Project")
getwd()


# Load packages
library(ggplot2)
library(tidyverse)
library(knitr)
library(lubridate)

## Here we load and prepare the data so that is is in a usable form

# Function to load and process demand data
load_demand_data <- function() {
  # Read the CSV file
  demand_data <- read.csv("SCS_demand_modelling.csv", stringsAsFactors = FALSE)
  
  # Convert date to proper date format
  demand_data$Date <- as.Date(demand_data$Date)
  
  return(demand_data)
}

# Function to load hourly temperature data
load_hourly_temp_data <- function() {
  # Read the CSV file
  temp_data <- read.csv("SCS_hourly_temp.csv", stringsAsFactors = FALSE)
  
  # Convert date to proper date format
  temp_data$Date <- as.Date(temp_data$Date)
  
  return(temp_data)
}

# Run the functions to load the data
demand_data <- load_demand_data()
temp_data <- load_hourly_temp_data()


## This gives us the statistics to output a table for the useful variables

# Function to generate summary statistics for the demand data
summarize_demand_data <- function(demand_data) {
  # Calculate summary statistics for key variables
  summary_stats <- data.frame(
    Variable = c("demand_gross", "wind", "solar_S", "temp", "TO", "TE"),
    Min = sapply(demand_data[, c("demand_gross", "wind", "solar_S", "temp", "TO", "TE")], min, na.rm = TRUE),
    Mean = sapply(demand_data[, c("demand_gross", "wind", "solar_S", "temp", "TO", "TE")], mean, na.rm = TRUE),
    Median = sapply(demand_data[, c("demand_gross", "wind", "solar_S", "temp", "TO", "TE")], median, na.rm = TRUE),
    Max = sapply(demand_data[, c("demand_gross", "wind", "solar_S", "temp", "TO", "TE")], max, na.rm = TRUE),
    SD = sapply(demand_data[, c("demand_gross", "wind", "solar_S", "temp", "TO", "TE")], sd, na.rm = TRUE)
  )
  
  # Round the numeric values for better presentation
  summary_stats[, 2:6] <- round(summary_stats[, 2:6], 2)
  
  return(summary_stats)
}

# Run the function to get summary
data_summary <- summarize_demand_data(demand_data)


## Here we make the graphs For intro section and some model analysis

# Make a graph that displays the gross demand
simple_gross_demand_plot <- ggplot(demand_data, aes(x = as.Date(Date), y = demand_gross)) +
  geom_line() +  # Line plot to visualize the demand over time
  labs(title = "Gross Demand Over Time",
       x = "Date",
       y = "Gross Demand") +
  theme_minimal()

# Temporal plots (average by month, average by day of week and demand vs DSN)
plot_temporal_patterns <- function(demand_data) {
  
  # Extract mean demands
  # Mean demand by month
  monthly_avg <- demand_data %>%
    group_by(monthindex) %>%
    summarise(mean_demand = mean(demand_gross, na.rm = TRUE))
  
  # Mean demand by weekday
  weekday_avg <- demand_data %>%
    group_by(wdayindex) %>%
    summarise(mean_demand = mean(demand_gross, na.rm = TRUE))
  
  # Bar chart by month
  p1 <- ggplot(monthly_avg, aes(x = factor(monthindex + 1, levels = 1:12, labels = month.abb), y = mean_demand)) +
    geom_col(fill = "blue") +
    labs(x = "Month", y = "Average Demand", title = "Average Demand by Month") +
    coord_cartesian(ylim = c(40000, 55000)) +
    theme_minimal()
  
  # Bar chart by weekday
  p2 <- ggplot(weekday_avg, aes(x = factor(wdayindex + 1, levels = 1:7, labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")), y = mean_demand)) +
    geom_col(fill = "orange") +
    labs(x = "Day of Week", y = "Average Demand", title = "Average Demand by Day of Week") +
    coord_cartesian(ylim = c(40000, 55000)) +
    theme_minimal()
  
  # Demand vs DSN yearly curves
  p3 <- ggplot(demand_data, aes(x = DSN, y = demand_gross, color = as.factor(start_year))) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(x = "Days Since November 1st", y = "Gross Demand", 
         title = "Demand vs. Days Since November", color = "Winter Start Year") +
    theme_minimal()
  
  # Arrange plots
    gridExtra::grid.arrange(p1, p2, p3, ncol = 1, heights = unit(c(2, 2, 3), "null"))
}

# Plots of relationships between demand and weather variables
plot_weather_relationships <- function(demand_data) {
  # Create individual plots showing relationships with weather variables
  p1 <- ggplot(demand_data, aes(x = temp, y = demand_gross)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(x = "Temperature (°C)", y = "Gross Demand", title = "Demand vs. Temperature") +
    theme_minimal()
  
  p2 <- ggplot(demand_data, aes(x = wind, y = demand_gross)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(x = "Wind Capacity Factor", y = "Gross Demand", title = "Demand vs. Wind") +
    theme_minimal()
  
  p3 <- ggplot(demand_data, aes(x = solar_S, y = demand_gross)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(x = "Solar Capacity Factor", y = "Gross Demand", title = "Demand vs. Solar") +
    theme_minimal()
  
  p4 <- ggplot(demand_data, aes(x = TE, y = demand_gross)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(x = "TE", y = "Gross Demand", title = "Demand vs. TE") +
    theme_minimal()
  
  # Arrange plots
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}


## Analysing the DSN effect on demand
# Function that analyses the DSN effects
analyze_dsn_effects <- function(demand_data) {
  # Build models with different DSN terms
  dsn_linear_model <- lm(demand_gross ~ DSN, data = demand_data)
  dsn_quadratic_model <- lm(demand_gross ~ DSN + I(DSN^2), data = demand_data)
  
  # Create prediction data for plotting lines on the graph
  dsn_seq <- seq(min(demand_data$DSN), max(demand_data$DSN), length.out = 100)
  pred_data <- data.frame(DSN = dsn_seq)
  
  pred_data$linear_pred <- predict(dsn_linear_model, newdata = pred_data)
  pred_data$quadratic_pred <- predict(dsn_quadratic_model, newdata = pred_data)
  
  # Return results for next function to use
  return(list(
    linear_model = dsn_linear_model,
    quadratic_model = dsn_quadratic_model,
    pred_data = pred_data
  ))
}

# Run the function to analyse our data
dsn_effects <- analyze_dsn_effects(demand_data)

# Function that plots DSN effects
plot_dsn_effects <- function(dsn_effects, demand_data) {
  # Plot DSN relationship
  ggplot() +
    geom_point(data = demand_data, aes(x = DSN, y = demand_gross), alpha = 0.3) +
    geom_line(data = dsn_effects$pred_data, aes(x = DSN, y = linear_pred, color = "Linear"), size = 1) +
    geom_line(data = dsn_effects$pred_data, aes(x = DSN, y = quadratic_pred, color = "Quadratic"), size = 1) +
    scale_color_manual(values = c("Linear" = "blue", "Quadratic" = "red")) +
    labs(x = "Days Since November 1st", y = "Gross Demand", 
         title = "Linear vs. Quadratic DSN Effects",
         color = "Model Type") +
    theme_minimal()
}


## 3 Models that I have considered
# Fit the suggested model
suggested_model <- lm(demand_gross ~ wind + solar_S + temp + wdayindex + monthindex, data = demand_data)

# Fit an intermediate model that adds in TE along with interactions instead of temperature,
# 
# Make the relevant variable factor variables
demand_data$start_year_factor <- factor(demand_data$start_year)
demand_data$wday_factor <- factor(demand_data$wdayindex)
# Fit the model
intermediate_model <- lm(
  as.formula("demand_gross ~ wday_factor + wday_factor:TE +
                             start_year_factor + start_year_factor:TE +
                             DSN + I(DSN^2) + DSN:TE + I(DSN^2):TE +
                             TE + wind + I(wind^2) + solar_S + I(sqrt(solar_S))"),
  data = demand_data)

# Fit the final model
final_model <- lm(
  as.formula("demand_gross ~ wday_factor +
                             start_year_factor + start_year_factor:TE +
                             DSN + I(DSN^2) + DSN:TE + I(DSN^2):TE +
                             TE +I(sqrt(solar_S))"),
  data = demand_data)

# Fit models that check if the interactions are neccessary just for QQ-plots
final_model_with_interactions <-lm(
  as.formula("demand_gross ~ wday_factor + wday_factor:TE +
                             start_year_factor + start_year_factor:TE +
                             DSN + I(DSN^2) + DSN:TE + I(DSN^2):TE +
                             TE +I(sqrt(solar_S))"),
  data = demand_data)
final_model_without_year_interactions <-lm(
  as.formula("demand_gross ~ wday_factor + wday_factor:TE +
                             start_year_factor +
                             DSN + I(DSN^2) + DSN:TE + I(DSN^2):TE +
                             TE +I(sqrt(solar_S))"),
  data = demand_data)

  
## Here are some nicer model diagnostic plot functions
# Function to plot model diagnostics
plot_model_diagnostics <- function(model) {
  # Create a data frame with fitted values and residuals
  diagnostics_data <- data.frame(
    fitted = fitted(model),
    residuals = residuals(model),
    std_residuals = rstandard(model)
  )
  
  # Create individual diagnostic plots
  p1 <- ggplot(diagnostics_data, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted") +
    theme_minimal()
  
  p2 <- ggplot(diagnostics_data, aes(sample = std_residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(x = "Theoretical Quantiles", y = "Standardized Residuals", title = "Normal Q-Q Plot") +
    theme_minimal()
  
  p3 <- ggplot(diagnostics_data, aes(x = fitted, y = abs(std_residuals))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(x = "Fitted Values", y = "|Standardized Residuals|", title = "Scale-Location Plot") +
    theme_minimal()
  
  # Arrange plots
  gridExtra::grid.arrange(p1, p2, p3, ncol = 2)
}

# Function that just displays the nicer QQ plot
QQ_plot <- function(model, title) {
  # Create a data frame with fitted values and residuals
  diagnostics_data <- data.frame(
    fitted = fitted(model),
    residuals = residuals(model),
    std_residuals = rstandard(model)
  )
  
  # Create the QQ plot
  p1 <- ggplot(diagnostics_data, aes(sample = std_residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(x = "Theoretical Quantiles", y = "Standardized Residuals", title = title) +
    theme_minimal()
}

