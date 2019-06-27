#Based on: 
#########################
#DataColada73
#R Code to reproduce in text calculation of probability of getting project to work with 5 out of 10 studies working
#Code written by Uri Simonsohn (urisohn@gmail.com)
#Please contact Uri directly if you spot an error
#Last update: 2018 10 18
# Posted for http://datacolada.org/73, originally written for Vosgerau et al https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3271372
############################

#Libraries
library(metafor)
library(tidyverse)

#Function to compute var(d) for given cohen d and sample size
#Cooper, Harris, Hedges, "Handbook of Research Synthesis 1993", p.238
vard=function(d,n) (2/n+(d^2)/(2*(2*n-2)))*((2*n)/(2*n-2)) 

n=20            #does not matter, but set n=20

sim <- function(all_studies = 1, reported = 1, simtot = 1000, alpha = 0.05) {

  res=matrix(nrow=simtot, ncol=2)          #Empty results matrix
  df=2*n-2                                 #Compute df

  for (simk in 1:simtot) {
    t=sort(rt(all_studies,df=df))        #Draw and sort all_studies studies
    t=t[(all_studies-reported+1):all_studies] #Keep largest reported
    d=2*t/sqrt(2*n)              #Get d
    v=vard(d,n)                  #Get var(d)
    meta.f=(rma(yi=d,vi=v,method="FE")$pval<alpha)     #Share of significant meta analyses (fixed-effects meta analysis)
    meta.r=(rma(yi=d,vi=v,method="HE")$pval<alpha)     #Share of significant meta analyses (random-effects)
    res[simk,]=c(meta.f,meta.r)                     #Store results
    if (simk%%100==0) cat("...",simk)               #Counter
  }#End loop

  list(
    all = all_studies,
    reported = reported,
    fixed = mean(res[,1]),
    random = mean(res[,2])
  )
}



ns <- expand.grid(all = 1:15, reported = 1:15) %>%
  filter(all >= reported)

simdat <- purrr::map2_df(ns$all, ns$reported, sim)

#write_csv(simdat, "fd.csv")

ggplot(simdat, aes(reported, all)) +
  geom_tile(aes(fill = random), show.legend = F) +
  geom_text(aes(label = round(random, 2)), color = "white") +
  xlab("Number of studies reported") +
  ylab("Number of studies run") +
  scale_y_continuous(breaks=2:15) +
  scale_x_continuous(breaks=1:15) +
  ggtitle("Proportion of significant meta-analyses", "(under null with alpha = .05)")

ggsave("figs/sim_filedrawer.png")
