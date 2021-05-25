# Chapter 03 --------------------------------------------------------------

Pr_Positive_Vampire <- 0.95 # P(Positive | Vampire)
Pr_Positive_Mortal <- 0.01 # P(Positive | Mortal)
Pr_Vampire <- 0.001 # P(Vampire)
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire +
  Pr_Positive_Mortal * ( 1 - Pr_Vampire )
( Pr_Vampire_Positive <- Pr_Positive_Vampire*Pr_Vampire / Pr_Positive )

# Example: Hypotheses
Pr_Positive_True <- 0.95 
Pr_Positive_False <- 0.05 
Pr_True <- 0.01 

Pr_Positive <- Pr_Positive_True * Pr_True +
  Pr_Positive_False * ( 1 - Pr_True )
( Pr_True_Positive <- Pr_Positive_True * Pr_True / Pr_Positive )

Pr_Negative <- (1 - Pr_Positive)
( Pr_True_Negative <- Pr_Negative_True * Pr_True / Pr_Negative )


rm(list = ls())

## Sampling from a grid-approximate posterior
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples)


library(rethinking)
dens(samples)


# add up posterior probability where p < 0.5
sum( posterior[ p_grid < 0.5 ] )

sum(samples<0.5) / 1e4 # 10000

sum(samples > 0.5 & samples < 0.75) / 1e4
# sum(posterior[p_grid > 0.5 & p_grid < 0.75])

quantile(samples, 0.8)

quantile(samples, c(0.1, 0.9))

p_grid <- seq(0, 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size = 10000, replace = TRUE, prob = posterior)

# often very similar, but not in this case
PI(samples, prob = 0.5) # misleading
HPDI(samples, prob = 0.5) # narrowest interval containing 50% probability mass 

p_grid[which.max(posterior)] # MAP
chainmode(samples, adj = 0.01)

mean(samples)
median(samples)

## Loss Function
sum(posterior * abs(0.5 - p_grid))

loss_absolute <- sapply(p_grid, function(d) {sum(posterior * abs(d - p_grid))})
loss_quadratic <- sapply(p_grid, function(d) {sum(posterior * abs(d - p_grid)^2)})

p_grid[which.min(loss_absolute)]
median(samples)

p_grid[which.min(loss_quadratic)]
mean(samples)


## Sampling to Simulate Prediction
dbinom(0:2, 2, 0.5)

rbinom(10, 2, 0.7) # random samples

dummy_w <- rbinom(1e5, 2, 0.7)
table(dummy_w)/1e5

dummy_w <- rbinom(1e5, 9, 0.7)
simplehist(dummy_w, xlab = "dummy water count")


w <- rbinom(1e4, size = 9, prob = 0.6)
simplehist(w)

w <- rbinom(1e4, size = 9, prob = samples)
# simplehist(w)



