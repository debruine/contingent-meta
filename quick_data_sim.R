library(tidyverse)
library(afex)

reps <- 100 # number of simulation reps
stud <- 4   # number of studies per rep
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
  # run all 3 analyses for combined data
  group_by(rep) %>%
  nest() %>%
  mutate(aov.ma = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(aov.ma, .sep = ".") %>%
  mutate(mod.ma = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+mod+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(mod.ma, .sep = ".") %>%
  mutate(ixn.ma = map(data, ~{
    suppressMessages(
      aov_4(dv~grp*mod+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(ixn.ma, .sep = ".") %>%
  unnest(data) %>%
  rename(aov.ma.p = `aov.ma.Pr(>F)`,
         mod.ma.p = `mod.ma.Pr(>F)`,
         ixn.ma.p = `ixn.ma.Pr(>F)`) %>%
  # run all 3 analyses for each study separately
  group_by(rep, study, aov.ma.p, mod.ma.p, ixn.ma.p) %>%
  nest(grp, dv, mod, id) %>%
  mutate(aov = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(aov, .sep = ".") %>%
  mutate(mod = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+mod+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(mod, .sep = ".") %>%
  mutate(ixn = map(data, ~{
    suppressMessages(
      aov_4(dv~grp*mod+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(ixn, .sep = ".")

# how many reps have different analyses are best for study 1 and 2?
decide <- analyses %>%
  rename(aov.p = `aov.Pr(>F)`,
         mod.p = `mod.Pr(>F)`,
         ixn.p = `ixn.Pr(>F)`) %>%
  mutate(min.p = pmin(aov.p, mod.p, ixn.p),
         best = case_when( # inconsistent best analysis by study
           min.p == aov.p ~ "aov",
           min.p == mod.p ~ "mod",
           min.p == ixn.p ~ "ixn"
         )
  ) %>%
  group_by(rep) %>%
  mutate(best_first = best[1]) %>% # consistent best analysis for study 1
  ungroup() %>%
  arrange(rep, min.p) %>%
  group_by(rep) %>%
  mutate(best_lowest = best[1]) %>% # consistent best analysis overall
  ungroup() %>%
  arrange(rep, study) %>%
  mutate(
    best_first.p = case_when(
      best_first == "aov" ~ aov.ma.p,
      best_first == "mod" ~ mod.ma.p,
      best_first == "ixn" ~ ixn.ma.p
    ),
    best_lowest.p = case_when(
      best_lowest == "aov" ~ aov.ma.p,
      best_lowest == "mod" ~ mod.ma.p,
      best_lowest == "ixn" ~ ixn.ma.p
    )
  ) %>%
  group_by(rep, best_first.p, best_lowest.p) %>%
  summarise() %>%
  group_by() %>%
  summarise_all(~mean(. < .05))
