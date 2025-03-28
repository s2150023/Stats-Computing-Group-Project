#
# Place the code needed in the Report_project02.Rmd, including documentation.
#

# Load packages
library(ggplot2)

# Load the data
data <- read.csv("SCS_demand_modelling.csv")

# Make a graph that displays the gross demand
simple_gross_demand_plot <- ggplot(data, aes(x = as.Date(Date), y = demand_gross)) +
  geom_line() +  # Line plot to visualize the demand over time
  labs(title = "Gross Demand Over Time",
       x = "Date",
       y = "Gross Demand") +
  theme_minimal()

# Load necessary library
library(ggplot2)

# Create the plot for 'DSN' vs 'demand_gross'
gross_demand_plot_against_days_since_november <- ggplot(data, aes(x = DSN, y = demand_gross, color = factor(start_year))) +
  geom_line() +  # Line plot to visualize the demand over days
  labs(title = "Gross Demand Over Days Since November",
       x = "Days Since November (DSN)",
       y = "Gross Demand",
       color = "Start Year") +
  theme_minimal() +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(0, 365))

# Testing me changing a file for github
# Another GitHub test