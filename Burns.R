library(aplore3)
library(tidyverse)
library(dplyr)
library(pastecs)
library(ggpubr)
library(formattable)
library(class)
library(gmodels)
library(caret)
library(C50)
library(rpart)
library(rpart.plot)
library(randomForest)

#### import the data into R ##########
data("burn1000")
burn1000$facility <- NULL
burnData <- select(burn1000, -id)

#### Do discriptive statistics for all variables ##########
summary(burn1000)

table(burn1000$death)
table(burn1000$gender)
table(burn1000$race)
table(burn1000$inh_inj)
table(burn1000$flame)

stat.desc(burn1000$id, basic = TRUE, desc = TRUE, norm = FALSE, p=0.95)
stat.desc(burn1000$age, basic = TRUE, desc = TRUE, norm = FALSE, p=0.95)
stat.desc(burn1000$tbsa, basic = TRUE, desc = TRUE, norm = FALSE, p=0.95)


#### graphics for all variables ########################
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




#### apply normalization to burnData #############

# creat a normalization function
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# select the continuous variables and normalize them
burnData_n <- burnData %>% 
  select(age,tbsa) %>% 
  map(., normalize) %>% 
  as.data.frame() 

# take the original continuous variables out from dataset
burn_int <- burnData %>% 
  select(-age, -tbsa)

# combine the categorical variables and normalized continuous variables together
burn_norm <- cbind(burnData_n, burn_int)


######## creat train/test ########################

# set a sample size for 75% of total observations
smp_size <- floor(0.75 * nrow(burn_norm))

set.seed(100)
train_ind <- sample(seq_len(nrow(burn_norm)), size = smp_size) 

# creat the train and test data sets based on normalized dataset
train <- burn_norm[train_ind, ]
test <- burn_norm[-train_ind, ]



######### Build logit model and predict #################

# look at the probability of death in the train data and use it as my probabiliry cut point
formattable::percent(prop.table(table(train$death)))

# creat a logistic regression training model
logitMod <- glm(death ~ age + gender + race + tbsa + inh_inj + flame, 
                data = train, family = binomial(link = "logit"))

# do the prediction to the test data set
logPredict <- predict(logitMod, test, type = "response")

# convert predicted and truth of death into factor
predicted <- as.factor(dplyr::if_else(logPredict <= .85, "Alive", "Dead"))
truth <- as.factor(test$death)


# Model evaluation with cross table() function
CrossTable(x = test$death, 
           y = predicted, prop.chisq = FALSE)

# using confusionMatrix() to get the major statitics for judging the quality of prediction
caret::confusionMatrix(predicted,
                       truth, positive = "Dead")


########### Build knn and prediction ###############

burn_train_labels <- train %>% select(death) %>% unlist()
burn_test_labels <- test %>% select(death) %>% unlist()

# convert factor lists into numeric lists
train1=train
train1$gender=as.numeric(train1$gender)
train1$death=as.numeric(train1$death)
train1$race=as.numeric(train1$race)
train1$inh_inj=as.numeric(train1$inh_inj)
train1$flame=as.numeric(train1$flame)

test1=test
test1$gender=as.numeric(test1$gender)
test1$death=as.numeric(test1$death)
test1$race=as.numeric(test1$race)
test1$inh_inj=as.numeric(test1$inh_inj)
test1$flame=as.numeric(test1$flame)

# build knn model and make prediction
burn_test_pred <- knn(train = train1,
                      test = test1,
                      cl = burn_train_labels,
                      k = 21)

# KNN Model evaluation with cross table() function
CrossTable(x = burn_test_labels, 
           y = burn_test_pred, prop.chisq = FALSE)


# using confusionMatrix() to get the major statitics for judging the quality of prediction
confusionMatrix(as.factor(burn_test_pred),
                as.factor(burn_test_labels),
                positive = "Alive")

###### decision tree ######################################


# model training
burn_tree <- C5.0(train[, -3], train$death)
summary(burn_tree)

# visualization

plot(burn_tree) # use simple plot() way

tree.mod=rpart(death~.,data=train)
rpart.plot(tree.mod) # use rpart.plot() way

pdf("rplot.pdf", height=8.5, width = 22) ; plot(burn_tree); dev.off() # dr.balise suggest way


# make prediction to test data set
burn_tree_pred <- predict(burn_tree, test) 


# decision tree model evaluation with cross table() function
CrossTable(test$death, burn_tree_pred, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE, dnn = c("actual death", "predicted death"))

# using confusionMatrix() to get the major statitics for judging the quality of prediction
caret::confusionMatrix(as.factor(burn_tree_pred),
                       as.factor(test$death), positive = "Dead")

# model tuning
set.seed(300)
m <- train(death ~ ., data=burnData, method = "C5.0")
m



#### random forest ##########################


# build the random forest model
rf_mod <- randomForest(death ~ age + gender + race + tbsa + inh_inj + flame,
                        data = train)

# using model to make prediction on test data set
rf_pred <- predict(rf_mod, test, type = "response")

# random forest model evaluation with cross table() function
CrossTable(x = test$death, 
           y = rf_pred, prop.chisq = FALSE)


# using confusionMatrix() to get the major statitics for judging the quality of prediction
caret::confusionMatrix(as.factor(rf_pred),
                       as.factor(test$death), positive = "Dead")

