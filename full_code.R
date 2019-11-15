install.packages("class")
library(tidyverse)
library(magrittr)
library(modelr)
library(class)
source("bodyfatpercentage.R")

bodies <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")

#Remove fat percentages that don't satisfy Siri's Equation
bodies <- bodies %>% filter(bodyfat == round(495 / Density - 450, digits=1))

#Remove outliers
is_outlier <- function(x){
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

bodies <- bodies %>% mutate(outlier = is_outlier(Weight) | is_outlier(Height) | is_outlier(Neck) |
                            is_outlier(Chest) | is_outlier(Abdomen) | is_outlier(Hip) | 
                            is_outlier(Thigh) | is_outlier(Knee) | is_outlier(Ankle) | 
                            is_outlier(Biceps) | is_outlier(Forearm) | is_outlier(Wrist)) %>% filter(outlier == FALSE)

#Train 50% of the data
trainindex <- seq(1, nrow(bodies), 2)
trainfeatures <- bodies[trainindex, 3:15]
trainlabels <- bodies[trainindex, 2]

#Test 50% of the data
testindex <- seq(2, nrow(bodies), 2)
testfeatures <- bodies[testindex, 3:15]
testlabels <- bodies[testindex, 2]

errorsVec <- vector("integer")
for (k in seq(1, 15, 2))
{
  predictedlabels <- knn(train = trainfeatures, cl = trainlabels, test = testfeatures, k)
  confusiontable <- table(testlabels, predictedlabels)
  errorsVec <- c(errorsVec, sum(confusiontable) - sum(diag(confusiontable)))
}

k_vs_error <- data.frame(k=seq(1, 15, 2), errors=errorsVec)
ggplot(data=k_vs_error) + geom_line(mapping = aes(x=k, y=errors))
