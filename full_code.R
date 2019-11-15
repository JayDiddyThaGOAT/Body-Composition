install.packages("class")
library(tidyverse)
library(magrittr)
library(modelr)
library(class)
source("bodyfatpercentage.R")

bodies <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")

#Remove Outliers
bodies <- bodies %>% filter(round(bodyfat, digits=0) == round(495 / Density - 450, digits=0)) #Fat percentages that don't satisfy Siri's Equation
summary(lm(Density ~ bodyfat, bodies))
