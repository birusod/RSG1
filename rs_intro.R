
library(tidyverse)
d1 <- read_csv("rsdata/Liver_Disorders.csv")
d1 %>% colnames()

d <- d1
colnames(d) <- c("mcv", "alkph", "alamatf", "aspatf", 
                     "ggtpd", "nbr_alcbpd", "split_col")




# vix
theme_set(theme_light())
d %>% 
  ggplot(aes(mcv))+geom_density()

