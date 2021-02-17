# Find the probability that Biden has more support than Trump in the 2020 election

# First use an uninformative prior, here set all shape parameters in the dirichlet distrubtion = 1

# Data is sourced from a 2020 Qunnipiac University National poll of 1426 self identified likely voters

# Link: https://poll.qu.edu/images/polling/us/us10222020_udxf16.pdf

# Create a table to store the proportions of voters

voter_prop <- tibble("Biden" = 0.51, "Trump" = 0.41, "Other" = 0.02, "No_Opinion" = 0.06)

# Create a table to store the number of likely voters in each category

# Variable to store number of respondents to poll

voter_sample <- 1426

# Multiply sample size by proportions to get counts for each category

voter_count <- voter_sample * voter_prop 

# Voters in each category are not integers. I'll round them to the nearest whole number

voter_count <- map_dbl(voter_count, round, digits = 0)

# We have an extra observation compared to the sample size, not sure what the implication of this would be. 

# If there's a more elegant solution, let me know. 

# Use the vector of voter counts to simulate 10,000 draws from the posterior Dirichlet distribution with an uninformative prior

sim_1 <- rdirichlet(10000, alpha = voter_count)

sim_1

# What is the probability Trump had more support than Biden? ie: alpha2 + y2 > alpha1 + y1

hypo_1 <- sim_1[, 2] - sim_1[, 1]

# Plot distribution of the hypothesis:

plot(ecdf(hypo_1))

# It's pretty unlikely that Trump had more support than Biden according to our simulation

# Find the probability Trump had more support than Biden by counting the proportion of elements in hypo_1 
# that are greater than 0 - this is the number of simualted elections in which Trump had more support than Biden

length(hypo_1[hypo_1 > 0])

# There wasn't a single simulation in which Trump would have had more support than Biden

# We'll now use an informative prior - we'll adjust the shape parameters of the prior to reflect voter preferences 
# from the 2016 Trump v. Clinton election

# I begin by creating a table to store voter preferences from the 2016 election

# Using data sourced from the Federal Elections Committee: https://www.fec.gov/documents/1889/federalelections2016.pdf

# Trump received 46.09% of the popular vote
# Clinton received 48.18% of the popular vote

informative_prior <- tibble("Trump" = 0.4609, "Clinton" = 0.488, "Other" = 5.73)

# I adjust the number of categories from the 2020 election poll by combining "No opinion" and "other" into one category

voter_prop <- voter_prop %>% mutate(
  "Neither" = No_Opinion + Other) %>% 
  select(-c(No_Opinion, Other))

voter_count <- c(voter_count[1:2], voter_count[3] + voter_count[4])

# Apply the informative prior to the number of likely voters to calculate alphas

alpha_params <- informative_prior * voter_sample

# Create vector that contains parameters of posterior Dirichlet distribution by summing alpha parameter and 2020 poll responses

# Convert post_params from a tibble to a vector

post_params <- (alpha_params + voter_count) %>% slice(1) %>% as.numeric()

# Simulate 10,000 draws from the posterior distribution using the informative prior

sim_2 <- rdirichlet(10000, alpha = post_params)

# What is the probability Trump had more support than Biden? 
# ie: alpha2 + y2 > alpha1 + y1

hypo_2 <- sim_2[, 2] - sim_2[, 1]

# Plot cumulative distribution of the probability

plot(ecdf(hypo_2))

# Number of simulated elections where Trump wins more votes than Biden

length(hypo_2[hypo_2 > 0])

# Find porportion to obtain probability

length(hypo_2[hypo_2 > 0]) / length(hypo_2)

# The probability that Trump secures more votes than Biden among likely voters is now 2.51% using the informative prior
