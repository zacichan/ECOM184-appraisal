# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(scales)

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

# Function to calculate BCR results
calculate_bcr_results <- function(options_data) {
  bcr_results_list <- list()
  
  for (option_name in names(options_data)) {
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
    
    bcr_results <- benefit_results / cost_results
    bcr_results_list[[option_name]] <- bcr_results
  }
  
  bcr_results_list
}

# Function to create a combined BCR data frame for faceting
create_bcr_df <- function(bcr_results_list) {
  bcr_df <- data.frame()
  
  for (option_name in names(bcr_results_list)) {
    results <- bcr_results_list[[option_name]]
    df <- data.frame(Value = results, Option = option_name)
    bcr_df <- rbind(bcr_df, df)
  }
  
  bcr_df
}

# Function to plot faceted BCR distribution
plot_faceted_bcr_distribution <- function(bcr_df, original_estimates) {
  # Create a mapping for the option descriptions
  option_labels <- c(
    "option_1" = "Phase 1 Only",
    "option_2" = "Phases 1 and 2a",
    "option_3" = "Full Network"
  )
  
  # Calculate 95% CI and modal points for each option
  stats_df <- bcr_df %>%
    group_by(Option) %>%
    summarise(
      Lower_CI = quantile(Value, probs = 0.025),
      Upper_CI = quantile(Value, probs = 0.975),
      Modal_Point = Value[which.max(density(Value)$y)]
    )
  
  ggplot(bcr_df, aes(x = Value)) +
    geom_density(aes(y = after_stat(density)), color = "blue") +
    geom_ribbon(stat = "density", aes(ymin = 0, ymax = after_stat(density)), fill = "lightcoral", alpha = 0.5) +
    facet_wrap(~Option, ncol = 1, labeller = labeller(Option = option_labels)) +
    geom_vline(data = original_estimates, aes(xintercept = Original, color = Option), linetype = "dashed", show.legend = FALSE) +
    geom_vline(data = stats_df, aes(xintercept = Lower_CI), linetype = "dashed", color = "red") +
    geom_vline(data = stats_df, aes(xintercept = Upper_CI), linetype = "dashed", color = "red") +
    geom_text(data = original_estimates, aes(x = Original, label = paste("Original Estimate:", round(Original, 2)), y = 1.2), 
              color = "black", vjust = -1, size = 3) +
    geom_text(data = stats_df, aes(x = Modal_Point, y = 3, label = paste("Modal Point:", round(Modal_Point, 2))), 
              color = "black", vjust = -1, size = 3) +
    geom_text(data = stats_df, aes(x = Upper_CI, y = 3, label = paste("95% CI: [", round(Lower_CI, 2), ",", round(Upper_CI, 2), "]")), 
              color = "black", hjust = -0.1, vjust = 1, size = 3) +
    labs(
      title = "Probabilistic Assessment of HS2 Project Phases:",
      subtitle = "Monte Carlo Simulation Analysis",
      x = "Benefit-Cost Ratio (BCR)", 
      y = "Probability Density"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      strip.text = element_text(face = "bold", size = 10)
    )
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

# Calculate BCR results
bcr_results_list <- calculate_bcr_results(options_data)

# Create a data frame for BCR values and options
bcr_df <- create_bcr_df(bcr_results_list)

# Define original estimates for BCR values
original_estimates <- data.frame(Option = c("option_1", "option_2", "option_3"),
                                 Original = c(1.2, 1.2, 1.5))

# Plot the faceted BCR distribution
plot_faceted_bcr_distribution(bcr_df, original_estimates)