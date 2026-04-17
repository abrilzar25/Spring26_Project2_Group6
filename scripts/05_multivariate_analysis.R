"Day 4: Task 1: Multivariate Visualization"

# Enhanced plot by General Health Status (PHSTAT_A)
ggplot(nhis_clean, aes(x = HEIGHTTC_A, y = WEIGHTLBTC_A, color = factor(SEX_A))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ PHSTAT_A) +
  labs(
    title = "Height vs Weight by Sex and General Health Status",
    x = "Height (in.)",
    y = "Weight (lbs)",
    color = "Sex"
  ) +
  theme_linedraw()

# or Enhanced plot by Education Level (PHSTAT_A)
ggplot(nhis_clean, aes(x = HEIGHTTC_A, y = WEIGHTLBTC_A, color = factor(SEX_A))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ EDUCP_A) +
  labs(
    title = "Height vs Weight by Sex and Education Level",
    x = "Height (in.)",
    y = "Weight (lbs)",
    color = "Sex"
  ) +
  theme_linedraw()

# Create a correlation scatter plot matrix using pairs.panels() from the psych library to visualize AGEP_A, WEIGHTLBTC_A, and HEIGHTTC_A

# Install psych 
install.packages("psych")
# Load the package
library(psych)

#Isolate three variables 
nhis_pp_variables <- nhis_clean[, c("AGEP_A", "WEIGHTLBTC_A", "HEIGHTTC_A")] 

# Create the pairs.panels correlation plot
pairs.panels(
  nhis_pp_variables,
  method = "pearson",     # correlation method
  hist.col = "chartreuse4", # histogram color on diagonal
  density = TRUE,         # add density plots
  ellipses = TRUE         # add correlation ellipses
)

