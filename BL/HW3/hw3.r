
library(here)
library(tidyverse)
library(haven)
library(rstatix)
library(QuantPsyc)
library(sjlabelled)

#####

hw2_first <- read_sav("942_q1h3first_212.sav")

q1_cor <- cor_mat(select(hw2_first,
                     dep,
                     stress,
                     ses,
                     salarysat,
                     priorgrad,
                     mar,
                     findep)) # correlation matrix
q1_cor_p <- cor_get_pval(q1_cor) # p values for correlation

q1_mreg <- lm(dep ~ stress + ses + salarysat + priorgrad + mar + findep, data = hw2_first) # model
summary(q1_mreg) # statistics
  lm.beta(q1_mreg)

get_labels(hw2_first) # get variable labels for qualitative variables 








