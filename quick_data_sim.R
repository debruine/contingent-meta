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

p <- df %>%
  group_by(rep, study) %>%
  nest() %>%
  mutate(p = map_dbl(data, ~t.test(.$x, .$y)$p.value))
