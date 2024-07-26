# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(scales)
library(cowplot)

# Constants
NUM_SIMS <- 10000
CONFIDENCE_LEVEL <- 0.95

# Function to simulate distributions
simulate_distribution <- function(low, central, high, dist_type, num_sims = NUM_SIMS) {
  set.seed(42)
  
  uniform_1 <- function(low, high, size) {
    runif(size, low, high)
  }
  
  normal_2 <- function(low, high, size) {
    mean_x <- (high + low) / 2
    sd_x <- (high - low) / 4
    rnorm(size, mean_x, sd_x)
  }
  
  normal_3 <- function(low, central, high, size) {
    mean_x <- central
    sd_x <- (high - low) / 4
    rnorm(size, mean_x, sd_x)
  }
  
  log_normal_4 <- function(low, central, high, size) {
    f <- function(sigma) {
      mu <- log(central) + sigma^2
      abs(plnorm(high, meanlog = mu, sdlog = sigma) - plnorm(low, meanlog = mu, sdlog = sigma) - CONFIDENCE_LEVEL)
    }
    
    sigma_x <- optimize(f, c(0, 1))$minimum
    mu_x <- log(central) + sigma_x^2
    rlnorm(size, meanlog = mu_x, sdlog = sigma_x)
  }
  
  if (dist_type == "Uniform") {
    results <- uniform_1(low, high, num_sims)
  } else if (dist_type == "Normal (without central)") {
    results <- normal_2(low, high, num_sims)
  } else if (dist_type == "Normal (with central)") {
    results <- normal_3(low, central, high, num_sims)
  } else if (dist_type == "Log-Normal") {
    results <- log_normal_4(low, central, high, num_sims)
  } else {
    stop("Invalid distribution type")
  }
  
  results
}

# Function to plot distributions
plot_distribution <- function(results, title, subtitle) {
  density_data <- density(results)
  x_vals <- density_data$x
  y_vals <- density_data$y
  
  lower_bound <- quantile(results, (1 - CONFIDENCE_LEVEL) / 2)
  upper_bound <- quantile(results, 1 - (1 - CONFIDENCE_LEVEL) / 2)
  modal_point <- x_vals[which.max(y_vals)]
  
  ggplot() +
    geom_line(aes(x = x_vals, y = y_vals), color = "blue") +
    geom_ribbon(aes(x = ifelse(x_vals >= lower_bound & x_vals <= upper_bound, x_vals, NA), 
                    ymin = 0, ymax = y_vals), fill = "lightcoral", alpha = 0.5) +
    geom_vline(xintercept = c(lower_bound, upper_bound), linetype = "dashed", color = "red") +
    annotate("text", x = modal_point, y = max(y_vals), label = paste("Modal Point:", round(modal_point, 2)), 
             vjust = -1, color = "black") +
    annotate("text", x = upper_bound, y = max(y_vals), 
             label = paste("95% CI: [", round(lower_bound, 2), ",", round(upper_bound, 2), "]"), 
             hjust = 0, vjust = 1, color = "black") +
    labs(title = title, subtitle = subtitle, x = "Value", y = "Density") +
    theme_minimal()
}

# Function to simulate and plot for an option
simulate_and_plot_option <- function(option_name, options_data) {
  data <- options_data[[option_name]]
  
  low_cost <- data$cost$low
  central_cost <- data$cost$central
  high_cost <- data$cost$high
  cost_dist_type <- "Log-Normal"
  
  low_benefit <- data$benefit$low
  central_benefit <- data$benefit$central
  high_benefit <- data$benefit$high
  benefit_dist_type <- "Log-Normal"
  
  cost_results <- simulate_distribution(low_cost, central_cost, high_cost, cost_dist_type)
  benefit_results <- simulate_distribution(low_benefit, central_benefit, high_benefit, benefit_dist_type)
  
  cost_title <- "Cost Distribution"
  benefit_title <- "Benefit Distribution"
  bcr_title <- "Benefit-Cost Ratio Distribution"
  
  subtitle <- paste("Option:", option_name)
  
  cost_fig <- plot_distribution(cost_results, cost_title, subtitle)
  benefit_fig <- plot_distribution(benefit_results, benefit_title, subtitle)
  
  bcr_results <- benefit_results / cost_results
  bcr_fig <- plot_distribution(bcr_results, bcr_title, subtitle)
  
  list(cost_fig = cost_fig, benefit_fig = benefit_fig, bcr_fig = bcr_fig)
}

# Function to create grid of plots
create_grid_of_plots <- function(options_data) {
  plots <- list()
  
  for (option_name in names(options_data)) {
    result_plots <- simulate_and_plot_option(option_name, options_data)
    plots <- c(plots, result_plots$cost_fig, result_plots$benefit_fig, result_plots$bcr_fig)
  }
  
  plot_grid(plotlist = plots, ncol = 3, align = 'hv')
}

# Example Usage
options_data <- list(
  option_1 = list(
    benefit = list(low = 22.08, central = 32.8, high = 38.64),
    cost = list(low = 20.5, central = 27.6, high = 36.4)
  ),
  option_2 = list(
    benefit = list(low = 26.24, central = 38, high = 42.64),
    cost = list(low = 25.3, central = 32.8, high = 42.2)
  ),
  option_3 = list(
    benefit = list(low = 66.78, central = 94.7, high = 196.02),
    cost = list(low = 45.09, central = 63.5, high = 86.09)
  )
)

create_grid_of_plots(options_data)