##942 - Cal's homework 2

install.packages("lm.beta")
library(lm.beta)
library(haven)
data1 <- read_sav("942_q1h2_212_data1.sav")
View(data1)

data2 <- read_sav("942_q1h2_212b_data2.sav")
View(data2)

#Simple linear regression ----
#r = sqrt(Multiple R-squared) - same as beta
#b = predictor Estimate
#a = intercept Estimate
#beta = run lm.beta to give standardized beta; r automatically runs unstandardized
averate <- lm(ggpa ~ averate, data = data1)
summary(averate)
lm.beta(averate)
sqrt(0.4638) #Adj R-squared = r

UGPA <- lm(ggpa ~ ugpa, data = data1)
summary(UGPA)
lm.beta(averate)
sqrt(0.02561) 

GRE <- lm(ggpa ~ gre, data = data1)
summary(GRE)
lm.beta(GRE)

prog <- lm(ggpa ~ prog, data = data1)
summary(prog)
lm.beta(prog)

upub <- lm(ggpa ~ upub, data = data1)
summary(upub)
lm.beta(upub)

grad <- lm(ggpa ~ priorgrad, data = data1)
summary(grad)
lm.beta(grad)

##prediction
predict(averate, data2())