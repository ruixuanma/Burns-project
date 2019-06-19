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
gghistogram(burn1000, x = "age", bins = 100, 
            add = "median")


gghistogram(burn1000, x = "id", bins = 100, 
            add = "mean")

gghistogram(burn1000, x = "tbsa", bins = 100, 
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
