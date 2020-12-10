
# REPRO STATS WITH R

# https://crumplab.github.io/rstatsforpsych/index.html

library(tidyverse)

# Sampling from distributions in R --- --- --- --- --- --- --- --- --- --- --- ----

# sample()
sample(x = 1:5, size = 3)
sample(1:3, size = 10, replace = TRUE)
sample(1:3, size = 10, replace = TRUE, prob = c(.25, .5, .25))
sample(x = c("H", "T"), size = 10, replace = T)
sample(1:20, size = 5, replace = F)


# normal dist: rnorm

N1= rnorm(10, 0, 1)
N2= rnorm(1000, 0, 1)
hist(N2)

dn <- data.frame(
  sample_data = rnorm(1000, 0, 1),
  grp = rep(1:4, each=250))
dn %>% tail

dn %>% ggplot(aes(sample_data)) + 
  geom_histogram() + 
  facet_wrap(~ grp)

# uniform dist: runif
runif(10)
runif(5, 10, 20)
lattice::histogram(runif(1000, 0,1))

# other dist: rnorm
rexp(10)
rexp(10, rate = 3)
lattice::densityplot(rexp(100, rate = 2))

hist(rbinom(10, 2, .5))
hist(rbinom(10, 2, prob= c(.5, .5)))

# Descriptive
library(moments)

s <- rnorm(100000, 0, 1)
s[1:6]
moment(s)
moment(s,order=3,absolute=TRUE)
kurtosis(s)
skewness(s)
summary(s)
fivenum(s)
mean(s)
sd(s)


# Monte Carlo Sim --- --- --- --- --- --- --- --- --- --- --- --- --- --- ----

flip = c()
outcome = c()
prp_head = c()
prp_tail = c()

for (i in 1:100){
  flip[i] <- i
  outcome[i] <- sample(x = c(1, 0), size = 1)
  prp_head[i] <- sum(outcome) / length(outcome)
  prp_tail[i] <- 1 - prp_head[i]
}


spl_data <- tibble(flip, outcome, prp_head, prp_tail)
spl_data %>% head
####

f = c()
obs = c()
p = c()
q = c()
for (i in 1:10000){
  f[i] <- i
  obs[i] <- sample(x = c(0,1), size = 1)
  p[i] <-  mean(obs)
  q[i] <- 1 - p[i]
}
sim_1 <- tibble(f, obs, p, q)
sim_1 %>% head

spl_data %>% 
  ggplot(aes(flip, prp_head)) +
  geom_point()+
  geom_line()+
  geom_hline(yintercept = .5, color = 'red', size = 2)

sim_1 %>% 
  ggplot(aes(f, p)) +
  geom_point()+
  geom_line()+
  geom_hline(yintercept = .5, color = 'red', size = 2)



# CLT --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ----

seq(2, 30, 2)
seq_len(10)
rnorm(3, mean = 100, sd = 50)
sd(rnorm(100, 100, 50))

N <- seq(100, 100000, 100)
M <- c()
S <- c()

for (i in 1:length(N)){
  d <- rnorm(N[i], 100, 50)
  M[i] <- mean(d)
  S[i] <- sd(d)
}
sim_2 <- tibble(N, M, S)
sim_2 %>% nrow()

sim_2 %>% 
  ggplot(aes(N, M))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 100, color = 'red', size = 2)


# Probabilities --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ----

# 1 die
d1 <- sample(1:6, 1000, replace = T)
table(d1)
table(d1)/1000
# 2 dice
d2 <- sample(2:12, 1000, replace = T)
d2 %>% table()
