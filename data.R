library(tidyr)
library(dplyr)
library(ggplot2)
library(faux)

minus_60 <- rnorm_multi(n = 200, 
                        mu = c(30, 60),
                        sd = c(5, 10),
                        r = -0.6, 
                        varnames = c("Age", "Safety"),
                        empirical = FALSE)

minus_60 <- minus_60 %>% 
  mutate_if(is.numeric, round)

minus_30 <- rnorm_multi(n = 200, 
                        mu = c(30, 60),
                        sd = c(5, 10),
                        r = -0.3, 
                        varnames = c("Age", "Safety"),
                        empirical = FALSE)

minus_30 <- minus_30 %>% 
  mutate_if(is.numeric, round)

zero <- rnorm_multi(n = 200, 
                    mu = c(30, 60),
                    sd = c(5, 10),
                    r = 0, 
                    varnames = c("Age", "Safety"),
                    empirical = FALSE)

zero <- zero %>% 
  mutate_if(is.numeric, round)

plus_30 <- rnorm_multi(n = 200, 
                       mu = c(30, 60),
                       sd = c(5, 10),
                       r = 0.3, 
                       varnames = c("Age", "Safety"),
                       empirical = FALSE)

plus_30 <- plus_30 %>% 
  mutate_if(is.numeric, round)

plus_60 <- rnorm_multi(n = 200, 
                       mu = c(30, 60),
                       sd = c(5, 10),
                       r = 0.6, 
                       varnames = c("Age", "Safety"),
                       empirical = FALSE,
                       set.seed(100))

plus_60 <- plus_60 %>% 
  mutate_if(is.numeric, round)

ggplot(plus_60, aes(x = Age, y = Safety)) + 
  geom_point()
