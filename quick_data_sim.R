library(tidyverse)
library(afex)

reps <- 100 # number of simulation reps
stud <- 2   # number of studies per rep
n <- 20     # number of participants per grp
d <- 0      # effect size

df <- data.frame(
  rep = rep(1:reps, each = n*stud*2),
  study = rep(rep(1:stud, each = n*2), times = reps),
  grp = rep(rep(c("x", "y"), each = n), times = stud*reps),
  dv = rnorm(n*stud*reps*2),
  mod1 = sample(c("a", "b"), n*stud*reps*2, replace = TRUE),
  mod2 = sample(c("a", "b"), n*stud*reps*2, replace = TRUE),
  mod3 = sample(c("a", "b"), n*stud*reps*2, replace = TRUE)
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
  mutate(mod1.ma = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+mod1+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(mod1.ma, .sep = ".") %>%
  mutate(mod2.ma = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+mod2+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(mod2.ma, .sep = ".") %>%
  mutate(mod3.ma = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+mod3+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(mod3.ma, .sep = ".") %>%
  unnest(data) %>%
  rename(aov.ma.p = `aov.ma.Pr(>F)`,
         mod1.ma.p = `mod1.ma.Pr(>F)`,
         mod2.ma.p = `mod2.ma.Pr(>F)`,
         mod3.ma.p = `mod3.ma.Pr(>F)`) %>%
  # run all 4 analyses for each study separately
  group_by(rep, study, aov.ma.p, mod1.ma.p, mod2.ma.p,mod3.ma.p,) %>%
  nest(grp, dv, mod1, mod2, mod3, id) %>%
  mutate(aov = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(aov, .sep = ".") %>%
  mutate(mod1 = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+mod1+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(mod1, .sep = ".") %>%
  mutate(mod2 = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+mod2+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(mod2, .sep = ".") %>%
  mutate(mod3 = map(data, ~{
    suppressMessages(
      aov_4(dv~grp+mod3+(1|id), data = ., 
            anova_table = list(es = "pes"))$anova_table[1,]
    )
  })) %>%
  unnest(mod3, .sep = ".")


decide <- analyses %>%
  rename(aov.p = `aov.Pr(>F)`,
         mod1.p = `mod1.Pr(>F)`,
         mod2.p = `mod2.Pr(>F)`,
         mod3.p = `mod3.Pr(>F)`) %>%
  mutate(min.p = pmin(aov.p, mod1.p, mod2.p, mod3.p),
         best = case_when( # inconsistent best analysis by study
           min.p == aov.p ~ "aov",
           min.p == mod1.p ~ "mod1",
           min.p == mod2.p ~ "mod2",
           min.p == mod3.p ~ "mod3"
         )
  ) %>%
  group_by(rep) %>%
  mutate(best_first = best[1]) %>% # consistent best analysis for study 1
  ungroup() %>%
  arrange(rep, min.p) %>%
  group_by(rep) %>%
  mutate(best_lowest = best[1]) %>% # consistent best analysis overall
  ungroup() %>%
  arrange(rep, study)
  
  
fp <- decide %>%
  mutate(
    best_first.p = case_when(
      best_first == "aov" ~ aov.ma.p,
      best_first == "mod1" ~ mod1.ma.p,
      best_first == "mod2" ~ mod2.ma.p,
      best_first == "mod3" ~ mod3.ma.p
    ),
    best_lowest.p = case_when(
      best_lowest == "aov" ~ aov.ma.p,
      best_lowest == "mod1" ~ mod1.ma.p,
      best_lowest == "mod2" ~ mod2.ma.p,
      best_lowest == "mod3" ~ mod3.ma.p
    )
  ) %>%
  group_by(rep, best_first.p, best_lowest.p) %>%
  summarise() %>%
  group_by() %>%
  summarise_all(~mean(. < .05))

mean(decide$min.p < .05)
