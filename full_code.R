install.packages("class")
library(tidyverse)
library(magrittr)
library(modelr)
library(class)
source("bodyfatpercentage.R")

bodies <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")
bodies <- bodies %>% mutate(bodyfat = round(bodyfat, digits=0))

#Identify candidate variables
bodyfat_cor <- cor(bodies)[,2]
bodyfat_cor <- bodyfat_cor[bodyfat_cor > 0 & bodyfat_cor < 1]
bodyfat_cor <- sort(bodyfat_cor, decreasing=TRUE)

bodyfat_model <- lm(bodyfat ~ Age + Weight + Height + Neck + Chest + Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + Wrist, data = bodies)
bodies <- bodies %>% mutate(calc_bodyfat = bodyfatpercentage(bodyfat_model)) %>% select(bodyfat, calc_bodyfat)
