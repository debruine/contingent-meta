library(tidyverse)

reps <- 100 # number of simulation reps
stud <- 2   # number of studies per rep
n <- 20     # number of participants per study
d <- 0      # effect size

df <- data.frame(
  rep = rep(1:reps, each = n*stud),
  study = rep(rep(1:stud, each = n), times = reps),
  x = rnorm(n*stud*reps),
  y = rnorm(n*stud*reps, d),
  mod = sample(c("a", "b"), n*stud*reps, replace = TRUE)
)

analyses <- df %>%
  group_by(rep, study) %>%
  nest() %>%
  mutate(t = map(data, ~t.test(.$x, .$y) %>% broom::tidy())) %>%
  unnest(t, .sep = ".") %>%
  mutate(w = map(data, ~wilcox.test(.$x, .$y) %>% broom::tidy())) %>%
  unnest(w, .sep = ".") %>%
  mutate(better = ifelse(t.p.value < w.p.value, "t", "w"))

         