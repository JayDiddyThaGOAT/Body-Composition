install.packages("class")
library(tidyverse)
library(magrittr)
library(modelr)
library(class)
source("bodyfatpercentage.R")

bodies <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")
bodyfat <- cor(bodies)[,2]
bodyfat <- bodyfat[bodyfat > 0 & bodyfat < 1]