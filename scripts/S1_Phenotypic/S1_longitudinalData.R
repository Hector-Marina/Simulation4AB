########
### Script to simulate lactations curves
########

# Necessary packages
library(dplyr)
library(truncnorm)
library(ggplot2) 

###################################################
####### Simulating daily milk yield using Wood model
###################################################

#---------------------------------------------------
# 1. Wood lactation curve
#---------------------------------------------------

wood_lactation_curve <- function(a, b, c, t) {
  a * t^b * exp(-c * t)
}

# Reproducibility
set.seed(123)

#---------------------------------------------------
# 2. Simulation parameters
#---------------------------------------------------

n_cows <- 400
days <- 305

# Parameters based on Masia et al.J Dairy Sci (2020) [DOI: 10.3168/jds.2019-17962]
# Parameters from multiparous cows
mean_a <- 18.56
mean_b <- 0.24
mean_c <- 0.0053

# Parameters based on Wu et al.J Dairy Sci (2023) [https://doi.org/10.3168/jdsc.2022-0343]
sd_a <- 2.23
sd_b <- 0.06
sd_c <- 0.0007

# Desired repeatability
repeatability <- 0.70


#---------------------------------------------------
# 3. Simulate Wood parameters for each cow
#---------------------------------------------------

cow_parameters <- data.frame(
  cow_id = 1:n_cows,
  
  a = rtruncnorm(
    n_cows,
    a = 0,
    b = Inf,
    mean = mean_a,
    sd = sd_a
  ),
  
  b = rtruncnorm(
    n_cows,
    a = 0,
    b = Inf,
    mean = mean_b,
    sd = sd_b
  ),
  
  c = rtruncnorm(
    n_cows,
    a = 0,
    b = Inf,
    mean = mean_c,
    sd = sd_c
  )
)

head(cow_parameters)


#---------------------------------------------------
# 4. Generate the lactation curves
#---------------------------------------------------

milk_yield <- data.frame()

for (i in 1:n_cows) {
  
  # Get cow-specific Wood parameters
  a_i <- cow_parameters$a[i]
  b_i <- cow_parameters$b[i]
  c_i <- cow_parameters$c[i]
  
  # Days in milk
  t <- 1:days
  
  # True/expected milk yield
  MY_expected <- wood_lactation_curve(
    a = a_i,
    b = b_i,
    c = c_i,
    t = t
  )
  
  # Store results
  milk_yield <- rbind(
    milk_yield,
    data.frame(
      cow_id = i,
      day = t,
      MY_expected = MY_expected
    )
  )
}

#---------------------------------------------------
# 5. Variance of the simulated true milk yields
#---------------------------------------------------

actual_variance <- var(milk_yield$MY_expected)
#actual_variance


#---------------------------------------------------
# 6. Calculate residual variance
#---------------------------------------------------

residual_variance <- ((1 - repeatability) / repeatability) * actual_variance

residual_sd <- sqrt(residual_variance)
#residual_variance
#residual_sd


#---------------------------------------------------
# 7. Add residual variation
#---------------------------------------------------

#milk_yield <- milk_yield %>%
#  mutate(residual = rnorm(n(), mean = 0, sd = residual_sd),
#        MY_observed = MY_expected + residual)


milk_yield <- milk_yield %>%
  rowwise() %>%
  mutate(
    residual = rtruncnorm(
      1,
      a = -MY_expected,
      b = Inf,
      mean = 0,
      sd = residual_sd
    ),
    MY_observed = MY_expected + residual
  ) %>%
  ungroup()




###################################################
####### Including systematic effects
###################################################


# ---------------------------------------
# Assign cows to farms
# ---------------------------------------

n_farms <- 5

# Create one record per cow
cow_info <- data.frame(
  cow_id = 1:n_cows
)

# Randomly assign each cow to one farm
cow_info$farm_id <- sample(
  1:n_farms,
  n_cows,
  replace = TRUE
)

head(cow_info)


# merging farm with milk yield information

milk_yield <- merge(milk_yield, cow_info, by = "cow_id")


# adding systematic effect to the farm
milk_yield$farm_id <- as.character(milk_yield$farm_id)
farm_effect <- c("1"= 0, "2"= -2,"3"= 1,"4"= 2,"5"= -1)

milk_yield$farm_effec <- farm_effect[milk_yield$farm_id]

# Calculating the new phenotype
milk_yield$MY_observed1 <- milk_yield$MY_observed + milk_yield$farm_effec



######################
#### Plots
#####################

# Plot lactation curve of  a random cow
sample_cow <- sample(1:n_cows, 1)

ggplot(subset(milk_yield, cow_id %in% sample_cow), 
       aes(x = day)) +
  # Continuous line for expected milk
  geom_line(aes(y = MY_expected, color = "Milk_expected"), size = 1) +
  # Dotted line for observed milk
  geom_line(aes(y = MY_observed, color = "Milk_observed"), linetype = "dotted", size = 0.8) +
  facet_wrap(~cow_id, ncol = 2, scales = "free_y") +
  labs(x = "DIM (Days in milk)", y = "Daily milk yield (kg)") +
  scale_color_manual(
    values = c(
      "Milk_expected" = "black",
      "Milk_observed" = "gray"
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


