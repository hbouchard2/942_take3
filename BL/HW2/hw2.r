
library(here)
library(haven)
library(QuantPsyc)
library(tidyverse)
library(sjlabelled)

#####

hw2_mod <- read_sav("942_q1h2_212_mod.sav") # get data

q1_cor_mat <- cor(select(
  hw2_mod,
  ggpa,
  averate,
  ugpa,
  gre,
  prog,
  upub,
  priorgrad
)) # get correlation matrix for r

# regression for each variable 

q1_averate <- lm(ggpa ~ averate, data = hw2_mod) # regression model
summary(q1_averate) # get statistics
lm.beta(q1_averate) # get standardised beta

q1_ugpa <- lm(ggpa ~ ugpa, data = hw2_mod)
summary(q1_ugpa)
lm.beta(q1_ugpa)

q1_gre <- lm(ggpa ~ gre, data = hw2_mod)
summary(q1_gre)
lm.beta(q1_gre)

q1_prog <- lm(ggpa ~ prog, data = hw2_mod)
summary(q1_prog)
lm.beta(q1_prog)

q1_upub <- lm(ggpa ~ upub, data = hw2_mod)
summary(q1_upub)
lm.beta(q1_upub)

q1_priorgrad <- lm(ggpa ~ priorgrad, data = hw2_mod)
summary(q1_priorgrad)
lm.beta(q1_priorgrad)

get_labels(hw2_mod, values = "as.name") # get labels for qualitative variables 

#####

hw2_app <- read_sav("942_q1h2_212b_app.sav")

#functions

averate_to_ggpa <- function(averate) {
  ggpa = averate * 0.225 + 2.183
  print(ggpa)
}

gre_to_ggpa <- function(gre) {
  ggpa = gre * 0.003 + 1.186
  print(ggpa)
}

prog_to_ggpa <- function(prog) {
  ggpa = prog * 0.256 + 2.775
  print(ggpa)
}

upub_to_ggpa <- function(upub) {
  ggpa = upub * 0.869 + 2.613
  print(ggpa)
}

# computations

averate_to_ggpa(7.257620)
averate_to_ggpa(4.897464)
averate_to_ggpa(4.474166)
averate_to_ggpa(1.117856)

gre_to_ggpa(535)
gre_to_ggpa(545)
gre_to_ggpa(690)
gre_to_ggpa(520)

prog_to_ggpa(1)
prog_to_ggpa(0)
prog_to_ggpa(1)
prog_to_ggpa(1)

upub_to_ggpa(1)
upub_to_ggpa(0)
upub_to_ggpa(0)
upub_to_ggpa(0)



