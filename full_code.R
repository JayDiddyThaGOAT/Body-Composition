install.packages("class")
library(tidyverse)
library(magrittr)
library(modelr)
library(class)
source("bodyfatpercentage.R")

bodies <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")
View(cor(bodies))

for (i in 1:nrow(cor(bodies))){
  correlations <-  which((cor(bodies)[i,] > 0.7) & (cor(bodies)[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(bodies)[i])
    print(correlations)
  }
}

View(bodies %>% mutate(BD = bodyfatpercentage(Age, Chest, Abdomen, Thigh)) %>% select(Density, BD))
