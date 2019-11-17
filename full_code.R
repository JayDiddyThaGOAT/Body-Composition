library(tidyverse)
library(magrittr)
library(modelr)
source("bodyfatpercentage.R")

bodies <- read.csv("http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Bodyfat.csv")

#Remove fat percentages that don't satisfy Siri's Equation & the Density column
bodies <- bodies %>% filter(bodyfat == round(495 / Density - 450, digits=1)) %>% select(-1)

#Pick the best predictors by creating a linear model using the step() function
nullmodel <- lm(bodyfat ~ 1, bodies)
fullmodel <- lm(bodyfat ~ ., bodies)
model <- step(nullmodel, data = bodies, scope = list(upper = fullmodel, lower = nullmodel), direction = "both", k = 2, test = "F")
summary(model)

#Plot to check which powers of predictors are appropriate by checking if the relationships are linear
bodyfat_abdomen <- lm(bodyfat ~ Abdomen, bodies)$coefficients
ggplot(data = bodies) + geom_point(mapping = aes(x = Abdomen, y = bodyfat)) + geom_abline(slope = bodyfat_abdomen[2], intercept = bodyfat_abdomen[1])

bodyfat_weight <- lm(bodyfat ~ Weight, bodies)$coefficients
ggplot(data = bodies) + geom_point(mapping = aes(x = Weight, y = bodyfat)) + geom_abline(slope = bodyfat_weight[2], intercept = bodyfat_weight[1])

bodyfat_wrist <- lm(bodyfat ~ Wrist, bodies)$coefficients
ggplot(data = bodies) + geom_point(mapping = aes(x = Wrist, y = bodyfat)) + geom_abline(slope = bodyfat_wrist[2], intercept = bodyfat_wrist[1])

bodyfat_forearm <- lm(bodyfat ~ Forearm, bodies)$coefficients
ggplot(data = bodies) + geom_point(mapping = aes(x = Forearm, y = bodyfat)) + geom_abline(slope = bodyfat_forearm[2], intercept = bodyfat_forearm[1])

bodyfat_neck <- lm(bodyfat ~ Neck, bodies)$coefficients
ggplot(data = bodies) + geom_point(mapping = aes(x = Neck, y = bodyfat)) + geom_abline(slope = bodyfat_neck[2], intercept = bodyfat_neck[1])

bodyfat_age <- lm(bodyfat ~ Age, bodies)$coefficients
ggplot(data = bodies) + geom_point(mapping = aes(x = Age, y = bodyfat)) + geom_abline(slope = bodyfat_age[2], intercept = bodyfat_age[1])

bodyfat_thigh <- lm(bodyfat ~ Thigh, bodies)$coefficients
ggplot(data = bodies) + geom_point(mapping = aes(x = Thigh, y = bodyfat)) + geom_abline(slope = bodyfat_thigh[2], intercept = bodyfat_thigh[1])

#Significance & Partial F test
modelA <- lm(bodyfat ~ Abdomen + Weight + Wrist + Forearm + Neck + Age, data = bodies) #Testing Thigh
anova(modelA, model) #Remove Thigh (P-Value > 0.05)

modelB <- lm(bodyfat ~ Abdomen + Weight + Wrist + Forearm + Neck, data = bodies) #Testing Age
anova(modelB, modelA) #Remove Age (P-Value > 0.05)

modelC <- lm(bodyfat ~ Abdomen + Weight + Wrist + Forearm, data = bodies) #Testing Neck
anova(modelC, modelB) #Remove Neck (P-Value > 0.05)

modelD <- lm(bodyfat ~ Abdomen + Weight + Wrist, data = bodies) #Testing Forearm
anova(modelD, modelC) #Keep Forearm (P-Value < 0.05)

modelE <- lm(bodyfat ~ Abdomen + Weight + Forearm, data = bodies) #Testing Wrist
anova(modelE, modelC) #Keep Wrist (P-Value < 0.05)

modelF <- lm(bodyfat ~ Abdomen + Wrist + Forearm, data = bodies) #Testing Weight
anova(modelF, modelC) #Keep Weight (P-Value < 0.05)

modelG <- lm(bodyfat ~ Weight + Wrist + Forearm, data = bodies) #Testing Abdomen
anova(modelG, modelC) #Keep Abdomen (P-Value < 0.05)

#Model C is the best linear model out of Models A to G
redmodel <- modelC
summary(redmodel)

#Remove Outliers out of the data and the reduced model
influencers <- influence.measures(redmodel)
outliers <- data.matrix(which(apply(influencers$is.inf, 1, any)))
for (i in 1:nrow(outliers))
{
  bodies <- bodies[-outliers[i] + i - 1, ]
}
redmodel <- lm(bodyfat ~ Abdomen + Weight + Wrist + Forearm, data = bodies)
summary(redmodel)


#Cross-Validation
set.seed(2019)
trainindex <- sample(1:nrow(bodies), 0.8 * nrow(bodies))
trainset <- bodies[trainindex, ]
testset <- bodies[-trainindex, ]

trainmodel <- lm(bodyfat ~ Abdomen + Weight + Wrist + Forearm, data = trainset)
summary(trainmodel)

testframe <- data.frame(Abdomen = testset$Abdomen, 
                        Weight = testset$Weight,
                        Wrist = testset$Wrist,
                        Forearm = testset$Forearm)

#Get 95% confidence interval for the mean and value
predictions_ci <- data.frame(predict(trainmodel, testframe, interval = "predict", level = 0.95))

#Count number of observations that is within the value intervals and within 5 from predicted values
nrow(testset[testset$bodyfat >= predictions_ci$lwr & testset$bodyfat <= predictions_ci$upr, ]) # 40/41
nrow(testset[abs(testset$bodyfat - predictions_ci$fit) <= 5, ]) #31/41
nrow(testset[testset$bodyfat - predictions_ci$fit < 0, ]) #20 / 41
nrow(testset[testset$bodyfat - predictions_ci$fit > 0, ]) #21 / 41