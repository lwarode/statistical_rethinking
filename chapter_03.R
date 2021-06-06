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



# Practice ----------------------------------------------------------------
rm(list = ls())

## Easy
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

# 3E1
# sum(posterior[p_grid < 0.2])
sum(samples < 0.2) / 1e4

# 3E2
# sum(posterior[p_grid > 0.8])
sum(samples > 0.8) / 1e4

# 3E3
sum(samples > 0.2 & samples < 0.8) / 1e4

# 3E4
quantile(samples, 0.2)

# 3E5
quantile(samples, 0.8)

# 3E6
# narrowest interval containing 66% of posterior probability
rethinking::HPDI(samples, 0.66)

# 3E7
# interval containing 66% of posterior probability, assuming equal posterior probaility on left (below) and right (above) tails
rethinking::PI(samples, 0.66)


## Medium
rm(list = ls())

# 3M1
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

# 3M2
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
rethinking::HPDI(samples, .9)

# 3M3
w <- rbinom(1e4, size = 15, prob = samples)
sum(w[w = 8]) / 1e4

# 3M4
w <- rbinom(1e4, size = 9, prob = samples)
sum(w[w = 6]) / 1e4

# 3M5
rm(list = ls())

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

rethinking::HPDI(samples, .9)

w <- rbinom(1e4, size = 15, prob = samples)
sum(w[w = 8]) / 1e4


# 3M6
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( round(2300*6/9) , size=2300 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
diff(rethinking::PI(samples, 0.99))

# rethinking::HPDI(samples, 0.99)

# rethinking::dens(samples)

## Hard
rm(list = ls())

library(rethinking)

birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)

birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)

sum(birth1 + birth2)

# 3H1
p_grid <- seq(0, 1, length.out=1000 )
prior <- rep(1, 1000)

boys <- sum(birth1 + birth2)
births <- length(birth1) + length(birth2)

likelihood <- dbinom(boys, size=births , prob=p_grid )
# posterior <- likelihood * prior
# posterior <- posterior / sum(posterior)
posterior <- likelihood * prior / sum(likelihood * prior)

p_grid[which.max(posterior)] # MAP

# 3H2
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

HPDI(samples, prob = .5)
HPDI(samples, prob = .89)
HPDI(samples, prob = .97)

# 3H3
birth_simulation <- rbinom(1e4, 200, prob = samples)
birth_simulation[birth_simulation = 111]
sum(birth_simulation[birth_simulation = 111]) / 1e4

# dens(birth_simulation)
# abline(v = sum(birth1 + birth2), col = "red")

ggplot(birth_simulation %>% as.data.frame(), aes(.)) + 
  geom_density() +
  geom_vline(xintercept = 111)


# 3H4
birth_simulation_1 <- rbinom(1e4, 100, prob = samples)

sum(birth1)

ggplot(birth_simulation_1 %>% as.data.frame(), aes(.)) + 
  geom_density() +
  geom_vline(xintercept = sum(birth1))


# 3H5
first_born_girls <- length(birth1) - sum(birth1)

first_girls_simulation <- rbinom(1e4, first_born_girls, prob = samples)

