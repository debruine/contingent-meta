library(tidyverse)
library(afex)

reps <- 100 # number of simulation reps
stud <- 2   # number of studies per rep
n <- 20     # number of participants per study
d <- 0      # effect size

df <- data.frame(
  rep = rep(1:reps, each = n*stud*2),
  study = rep(rep(1:stud, each = n*2), times = reps),
  grp = rep(rep(c("x", "y"), each = n), times = stud*reps),
  dv = rnorm(n*stud*reps*2),
  mod = sample(c("a", "b"), n*stud*reps*2, replace = TRUE)
) %>%
  mutate(
    id = row_number(),
    dv = dv + (d*(grp == "y"))
  )

ggplot(df, aes(grp, dv)) +
  geom_boxplot()

analyses <- df %>%
  group_by(rep, study) %>%
  nest() %>%
  mutate(t = map(data, ~t.test(dv~grp, .) %>% broom::tidy())) %>%
  unnest(t, .sep = ".") %>%
  mutate(w = map(data, ~wilcox.test(dv~grp, .) %>% broom::tidy())) %>%
  unnest(w, .sep = ".") %>%
  mutate(a = map(data, ~{
    a <- suppressMessages(aov_4(dv~grp+mod(1|id), data = .)$aov)
    broom::tidy(a)[1,]
  })) %>%
  unnest(a, .sep = ".")