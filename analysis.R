# Analysis

# Set up - make sure to set your working directory using RStudio
library(tidyr)
library(dplyr)
library(ggplot2)
setwd("~/Desktop/INFO478/workbook-3-bryson716")

# Create the `charts/` directory (you can do this from R!)
dir.create("~/Desktop/INFO478/workbook-3-bryson716/charts")

# Load prepped data
data <- read.csv("~/Desktop/INFO478/workbook-3-bryson716/data/prepped/all_data.csv")

# Are HALE and life expectancy correlated?
# - Plot 2016 life expectancy against 2016 HALE. Save the graph to `charts/`
# - Compute the correlation between 2016 life expectancy against 2016 HALE
data_2016 <- filter(data, year == 2016)

le_hale <- ggplot(data = data_2016, aes(x = le, y = hale)) +
  geom_point() +
  labs(title = "Life expectancy v. HALE in 2016",
       x = "Life Expectancy",
       y = "Health-Adjusted Life Expectancy")

cor(x = data_2016$hale, y = data_2016$le)

ggsave("charts/le_hale_graph.png")

# Are HALE and DALYs correlated?
# - Plot 2016 HALE against 2016 DALYs. Save the graph to `charts/`
# - Compute the correlation between 2016 HALE and DALYs
dalys_hale <- ggplot(data = data_2016, aes(x = dalys, y = hale)) +
  geom_point() +
  labs(title = "DALYs v. HALE in 2016",
       x = "Disability-Adjusted Life Years",
       y = "Health-Adjusted Life Expectancy")

cor(x = data_2016$dalys, data_2016$hale)

ggsave("charts/dalys_hale_graph.png")

# As people live longer, do they live healthier lives 
# (i.e., is a smaller fraction of life spent in poor health)?
# Follow the steps below to attempt to answer this question.

# First, you will need to reshape the data to create columns *by metric-year*
# This will create `hale_2016`, `hale_1990`, `le_2016`, etc.
# To do this, I suggest that you use the `pivot` function in the new
# tidyverse release:https://tidyr.tidyverse.org/articles/pivot.html#wider
data_wide <- data %>%
  pivot_wider(names_from = year,
              values_from = c(hale, le, dalys))

# Create columns to store the change in life expectancy, and change in hale
data_wide <- data_wide %>%
  mutate(hale_diff = hale_2016 - hale_1990,
         le_diff = le_2016 - le_1990)

# Plot the *change in hale* against the *change in life expectancy*
# Add a 45 degree line (i.e., where x = y), and save the graph to `charts/`
# What does this mean?!?! Put your interpretation below
change <- ggplot(data = data_wide, aes(x = le_diff, y = hale_diff)) +
  geom_point() +
  labs(title = "Life Expectancy Difference v. HALE Difference (1990-2016)",
       x = "Change in Life Expectancy",
       y = "Change in Health-Adjusted Life Expectancy") +
  geom_abline(intercept = 0, slope = 1) +
  xlim(-15, 20) +
  ylim(-15, 20)

ggsave("charts/change_graph.png")

#Interpretation: I feel like this means many countries experienced more of
#a change in their life expectancies than their HALYs between 1990 and 2016.
#Most of the dots that diverge from the line (which the line would represent
#le_diff = hale_diff) are below the line, meaning more change was seen in the
#x variable, which is change in life expectancy. This could mean that although
#the average life expectancy is increasing, those extra years are not necessarily
#healthy years.
  
  
  