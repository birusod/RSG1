
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

## true probabilities

# 2 dice
first <- rep(1:6, each = 6)
second <- rep(1:6, 6)
 
# pairing rolls
roll_sum <- first + second
table(roll_sum)
prob_true <- table(roll_sum)/length(roll_sum)

## simulation

spl1 <- sample(1:6, 10000, replace = T)
table(spl1)
table(spl1)/10000

spl2 <- sample(1:6, 10000, replace = T)
spl2 %>% table()
table(spl2)/10000
combined <- spl1 + spl2
prob_sim <-table(combined)/10000

prob_true -prob_sim
probs <- tibble(prob_true, prob_sim)
probs
lattice::barchart(prob_true)
lattice::barchart(prob_sim)
probs %>% 
  mutate(roll = seq(1:11)) %>% 
  ggplot()+
  geom_col(aes(roll, prob_sim), fill = 'orange')+
  geom_col(aes(roll, prob_true), fill = 'wheat',alpha = .5)


## events generator: (predefined prob: A = .8  B = .2)
let1 <- sample(c("A", "B"), 100, replace = TRUE, prob = c(.8, .2))
table(let1)

letters[1:5]
sample(letters, 5)
sample(LETTERS, 5)
sample(LETTERS, 5, replace = T)

# matrices to strings:
matrix(1:9, 3, 3, byrow = T)
let2 <- sample(letters, 50*5, replace = T)
mat1 <- matrix(let2, ncol = 5)
paste(mat1[1, ], collapse = "")

random_strings <- c()
for (i in 1:dim(mat1)[1]){
  random_strings[i] <- paste(mat1[i,], collapse = "")
}
random_strings
# 



# Prob Experiment --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ----

replicate(3, c("H", "T"))
replicate(3, sample(c("H", "T"), 4, replace = T))
replicate(3, 1:4) %>% colSums()


sim3 <- replicate(10000,
                  sample(c(1, 0), 10, replace = T))
sim3[1:10, 1:4]
dim(sim3)
heads <- colSums(sim3)
heads[1:10]
table(heads)/10000
lattice::barchart(table(heads))


# 2 flips
flips <- sample(c("H", "T"), 1000, replace = T)
sequence <- c()
for (i in 2:length(flips)){
  first_element <- flips[i-1]
  second_element <- flips[i]
  sequence[i-1] <- paste0(first_element, second_element)
}
sequence[1:10]
table(sequence)/1000
barplot(table(sequence)/1000)
# 3 flips
flips <- sample(c("H", "T"), 10000000, replace = T)
sequence <- c()
for (i in 3:length(flips)){
  first_element <- flips[i-2]
  second_element <- flips[i-1]
  third_element <- flips[i]
  sequence[i-2] <- paste0(first_element, second_element, third_element)
}
sequence[1:10]
table(sequence)/10000000
barplot(table(sequence)/10000000)


# Subjective Prob: (Bayesian concept) --- --- --- --- --- --- --- --- --- --- --- --- ----

rbinom(10, 1, .5)

subp1 <- c(rbinom(100, 1, .5),
           rbinom(100, 1, .6))
table(subp1[1:100])
table(subp1[101:200])

# with all sequence
knowledge <- c()
belief <- c()
for (i in 1:length(subp1)){
  knowledge[i] <- subp1[i]
  belief[i] <- sum(knowledge) / length(knowledge)
}
plot(belief)

# with just last 20
knowledge <- c()
belief <- c()
for (i in 1:length(subp1)){
  knowledge[i] <- subp1[i]
  if (i <= 20){
    belief[i] <- sum(knowledge) / length(knowledge)
  } else {
    belief[i] <- sum(knowledge[i:(i-20)]) / length(knowledge[i:(i-20)])
  }
}

plot(belief)

# with just last 5
knowledge <- c()
belief <- c()
for (i in 1:length(subp1)){
  knowledge[i] <- subp1[i]
  if (i <= 5){
    belief[i] <- sum(knowledge) / length(knowledge)
  } else {
    belief[i] <- sum(knowledge[i:(i-5)]) / length(knowledge[i:(i-5)])
  }
}

plot(belief)
