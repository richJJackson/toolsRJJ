### Trial Design example

## loading packages/functions
library(devtools)
devtools::install_github("richJJackson/toolsRJJ")
library(toolsRJJ)
library(clinfun)


# Events
intFrac <- c(1)
intPow <- c(1)
alpha <- 0.1
power <- 0.9
HR <- 1/0.56
tails <- 1
r <- 1

gsdesign.survival(intFrac, HR, r=1,alpha, power, delta.eb = intPow,alternative="one.sided")

## Survival design including recruitment rates and accumated events
rpm <- 5/12
nSite <- 15
openRate <- 2
maxTime <- 24
penal <- 0.5
folUp <- 12


## survival function
lam <- -log(0.6)/12
t <- c(0:36)
S_t <- exp(-lam*t)
S_t

des <- survDesign(alpha,power,tails,HR,r,intFrac,intPow,nSite,rpm,openRate,maxTime,penal,folUp,S_t)
des
