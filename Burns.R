library(aplore3)
library(tidyverse)
library(dplyr)
library(pastecs)
library(ggpubr)

## import the data into R
data("burn1000")

burn1000$facility <- NULL

## Do discriptive statistics for all variables
summary(burn1000)

table(burn1000$death)
table(burn1000$gender)
table(burn1000$race)
table(burn1000$inh_inj)
table(burn1000$flame)

stat.desc(burn1000$id, basic = TRUE, desc = TRUE, norm = FALSE, p=0.95)
stat.desc(burn1000$age, basic = TRUE, desc = TRUE, norm = FALSE, p=0.95)
stat.desc(burn1000$tbsa, basic = TRUE, desc = TRUE, norm = FALSE, p=0.95)




## graphics for all variables
gghistogram(burn1000, x = "age", bins = 500, 
            add = "median")


gghistogram(burn1000, x = "id", bins = 100, 
            add = "mean")

gghistogram(burn1000, x = "tbsa", bins = 10000, 
            add = "mean")

ggplot(burn1000, aes(x = gender)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(burn1000, aes(x = death)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(burn1000, aes(x = race)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(burn1000, aes(x = inh_inj)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(burn1000, aes(x = flame)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Build a logistic regression model
head(burn1000)
table(burn1000$death)

# create training data
input_dead <- burn1000[which(burn1000$death == "Dead"), ]
input_alive <- burn1000[which(burn1000$death == "Alive"), ]
set.seed(100)  # for repeatability of samples
input_dead_trainRows <- sample(1:nrow(input_dead), 0.7 * nrow(input_dead))
input_alive_trainRows <- sample(1:nrow(input_alive), 0.7 * nrow(input_dead))
training_dead <- input_dead[input_dead_trainRows, ]
training_alive <- input_alive[input_alive_trainRows, ]
burn_train <- rbind(training_dead, training_alive) #row bind the Alive and Dead

# create test data
test_dead <- input_dead[-input_dead_trainRows, ]
test_alive <- input_alive[-input_alive_trainRows, ]
burn_test <- rbind(test_dead, test_alive)




glm.fit <- glm(death ~ age + gender + race + tbsa + inh_inj + flame,
               data = burn1000,
               family = binomial)










