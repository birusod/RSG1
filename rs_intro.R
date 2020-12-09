
library(tidyverse)
d1 <- read_csv("rsdata/Liver_Disorders.csv")
d1 %>% colnames()

# data prep
d <- d1
colnames(d) <- c("mcv", "alkph", "alamatf", "aspatf", 
                     "ggtpd", "nbr_alcbpd", "split_col")

d <- d %>% 
  mutate(alc_cat = case_when(
    nbr_alcbpd < 0.5 ~ "grp_1",
    nbr_alcbpd <= 1 ~ "grp_2",
    nbr_alcbpd <= 5 ~ "grp_3",
    nbr_alcbpd > 5 ~ "grp_4"),
    alc_cat = as_factor(alc_cat))
d


# EDA-1
d %>% count(alc_cat)



# viz-1
theme_set(theme_light())

d %>% 
  ggplot(aes(alc_cat, fill = alc_cat))+
  geom_bar(show.legend = FALSE) + 
  scale_fill_manual(values = c("firebrick", "cyan", "orange", "darkgreen"))

d %>% 
  ggplot(aes(mcv))+geom_density()

lattice::bwplot(d$mcv)

d %>% 
  ggplot(aes(mcv)) + 
  geom_boxplot()
d %>% 
  ggplot(aes(alc_cat, mcv, fill = alc_cat)) + 
  geom_boxplot(show.legend = F) +
  scale_fill_manual(values = c("firebrick", "cyan", "orange", "darkgreen"))

d %>% 
  ggplot(aes(mcv, aspatf)) + 
  geom_point() + 
  geom_smooth()


# regression: prediction of Mean Corpuscular Volume
m <- d %>% 
  lm(mcv ~ aspatf + alkph + alamatf, data =.)

m %>% broom::tidy()

