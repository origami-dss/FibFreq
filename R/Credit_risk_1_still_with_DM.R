library(ggplot2)
library(cowplot)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)


setwd("/Users/annette/Desktop/myR/Credit_Risk")

credit <- read.csv('data/german_credit-2.csv', header = TRUE)
str(credit)

credit$Duration.of.Credit.group <- cut(credit$Duration.of.Credit..month., c(0,12,18,24,Inf), labels = c(1:4))
credit$Credit.Amount.group <- cut(credit$Credit.Amount, c(0,1000,5000,10000,Inf), labels = c(1:4))
credit$Age.group <- cut(credit$Age..years., c(18,25,40,60,Inf), labels = c(1:4))
head(credit[,22:24],5)

for(i in 1:24) credit[, i] <- as.factor(credit[, i])



g <- ggplot(credit, aes(Creditability)) +
  geom_bar(fill = "#4EB25A") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("Bad","Good")) +
  ggtitle("Count of Good and Bad Credit Risks")
g


g <- ggplot(credit, aes(Value.Savings.Stocks, fill = Creditability), stat="identity") +
  geom_bar() +
  scale_fill_manual(values = c("#D3D6D4", "#4EB25A"), labels=c("Bad","Good")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("< 100 DM", "100-500 DM", "500-1000 DM", "> 1000 DM", "Unknown")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=12)) +
  ggtitle("Good and Bad Credit Risks by Credit History")
g


g <- ggplot(credit, aes(Occupation, fill = Creditability), stat="identity") +
  geom_bar() +
  scale_fill_manual(values = c("#D3D6D4", "#4EB25A"), labels=c("Bad","Good")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  scale_x_discrete(labels = c("Unemployed", "Unskilled", "Skilled", "Management")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title=element_text(size=12)) +
  ggtitle("Good and Bad Credit Risks by Occupation")
g


# Statistical Modeling

credit_old = credit
credit = credit[,-c(3,6,14)]

set.seed(2828)
train <- credit[inTraining,]
test <- credit[-inTraining,]


lmModel <- glm(Creditability ~ ., family = binomial, data = train)


# Fit model to test set
lmFit <- predict(lmModel, type = "response", test)

# Compare predictions to test set
lmPred <- prediction(lmFit, test$Creditability)

# Create Area Under the Curve (AUC) plot
plot(performance(lmPred, 'tpr', 'fpr'))

performance(lmPred, measure = 'auc')@y.values[[1]]



## USING DECISION TREES


set.seed(28)
dtModel <- rpart(Creditability ~ ., data=train)
fancyRpartPlot(dtModel)

dtFit <- predict(dtModel, test, type = 'prob')[, 2]
dtPred <- prediction(dtFit, test$Creditability)
plot(performance(dtPred, 'tpr', 'fpr'))

performance(dtPred, measure = 'auc')@y.values[[1]]




## USING  RANDOM FORESTS


set.seed(2828)
rfModel <- randomForest(Creditability ~ ., data=train)
rfFit <- predict(rfModel, test, type = 'prob')[,2]
rfPred <- prediction(rfFit, test$Creditability)
plot(performance(rfPred, 'tpr', 'fpr'))


