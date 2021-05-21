## Grid Approximation

# define grid
p_grid <- seq(0, 1, length.out = 20)

# define prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(6, size=9 , prob=p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )

prior <- ifelse( p_grid < 0.5 , 0 , 1)
prior <- exp( -5*abs( p_grid - 0.5 ) )

## Quadratic Approximation

library(rethinking)

globe.ga <- quap(
  
  alist(
    W ~ dbinom(W+L, p), # binomial likelihood
    p ~ dunif(0, 1) # uniform prior
  ),
  
  data = list(W = 6, L = 3)
  
)

precis(globe.ga)  


# analytical calculation
W <- 6
L <- 3
curve( dbeta( x , W+1 , L+1 ) , from=0 , to=1 )

# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )


## MCMC

n_samples <- 1000
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3

for ( i in 2:n_samples ) {
  p_new <- rnorm( 1 , p[i-1] , 0.1 )
  if ( p_new < 0 ) p_new <- abs( p_new )
  if ( p_new > 1 ) p_new <- 2 - p_new
  q0 <- dbinom( W , W+L , p[i-1] )
  q1 <- dbinom( W , W+L , p_new )
  p[i] <- ifelse( runif(1) < q1/q0 , p_new , p[i-1] )
}


dens(p, xlim=c(0,1))
curve(dbeta(x, W+1, L+1), lty = 2, add = TRUE)



## Practice

# 2M1
p_grid <- seq(0, 1, length.out = 42)

# define prior
prior <- rep(1, 42)

# compute likelihood at each value in grid
likelihood <- dbinom(3, size=3 , prob=p_grid)
likelihood <- dbinom(3, size=4 , prob=p_grid)
likelihood <- dbinom(5, size=7 , prob=p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior , type="b" ,
     xlab="probability of water" , ylab="posterior probability" )


# 2M2
p_grid <- seq(0, 1, length.out = 42)

# define prior
prior <- rep(1, 42)

prior <- ifelse( p_grid < 0.5 , 0 , 1)

# compute likelihood at each value in grid
likelihood <- dbinom(3, size=3 , prob=p_grid)
likelihood <- dbinom(3, size=4 , prob=p_grid)
likelihood <- dbinom(5, size=7 , prob=p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior , type="b" ,
     xlab="probability of water" , ylab="posterior probability" )



# 2H1

p_A = 0.5 # prior Species A
p_B = 0.5 # prior Species B

p_twins_A = 0.1 # P(Twins | Species A)
p_twins_B = 0.2 # P(Twins | Species B)

p_twins_unadjusted = p_A * p_twins_A + p_B * p_twins_B
# p_twins_unadjusted = 0.1 * 1/2 + 0.2 * 1/2

p_A_twins = p_twins_A * p_A / p_twins_unadjusted # P(A | Twins), 10% from 30%
p_B_twins = p_twins_B * p_B / p_twins_unadjusted # P(B | Twins), 20% from 30%

p_twins_unadjusted = p_twins_A * p_A_twins + p_twins_B * p_B_twins 
# p_twins_unadjusted = 0.1 * 1/3 + 0.2 * 2/3


# 2H2

# P(A | twins) 1/3
# = P(twins | A) * P(A) / P(twins)
p_A_twins = 0.1 * 0.5 / 0.15


# 2H3

# P(A | twins, singleton) = P(twins | A) * P(singleton | A) * P(A) / P(twins)

# P(twins) = P(twins | A) * P(singleton | A) * P(A) + P(twins | B) * P(singleton | B) * P(B)

p_A_twins_singleton = 0.1 * 0.9 * 0.5 / (0.1 * 0.9 * 0.5 + 0.2 * 0.8 * 0.5)
p_B_twins_singleton = 0.2 * 0.8 * 0.5 / (0.1 * 0.9 * 0.5 + 0.2 * 0.8 * 0.5)


# 2H4

# P(species = A | test = A) = P(test = A | species = A) * P(species = A) / P(test = A)

# P(test = A) = P(test = A | Species = A) * P(species = A) + P(test = A | species = B) * P(species = B)

p_A_testA = 0.8 * 0.5 / (0.8 * 0.5 + 0.35 * 0.5)

# P(species = A | test = A, twins, singleton) = P(test = A, species = A) * P(twins, species = A) * P(singleton, species = A) * P(species = A) / P(test = A, twins, singleton)

p_testA_twins_singleton = 0.8 * 0.1 * 0.9 * 0.5 + 0.35 * 0.2 * 0.8 * 0.5

p_A_testA_twins_singleton =  0.8 * 0.1 * 0.9 * 0.5 / p_testA_twins_singleton
p_A_testA_twins_singleton





