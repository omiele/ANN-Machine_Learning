library(readr)
library(psych)
ringnorm <- read_csv("~/Assigns/AE/nov/fwdass4/ringnorm.csv")
View(ringnorm)

## Data description
attach(ringnorm)
describe.by(ringnorm, Class) #descpritive statistics by group
describe(ringnorm) #descriptive statistics withou considering groups

## distribution analysis
library(ggplot2);library(reshape2)
data<- melt(ringnorm[,-21])
ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)

## checking for correlation
library(corrplot)
M <- cor(ringnorm)
corrplot(M, method = "circle")

### ANN model construction
library(nnet)
library(neuralnet)
index <- sample(1:nrow(ringnorm),round(0.75*nrow(ringnorm)))
train <- ringnorm[index,]
test <- ringnorm[-index,]
max = apply(ringnorm , 2 , max)
min = apply(ringnorm, 2 , min)
scaled = as.data.frame(scale(ringnorm, center = min, scale = max - min))
trainNN = scaled[index , ]
testNN = scaled[-index , ]
set.seed(2)
NN = neuralnet(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20, trainNN, hidden = 3 , stepmax=1e6 )
plot(NN)
summary(NN)
predict_testNN = compute(NN, testNN[,c(1:20)])
predict_testNN = (predict_testNN$net.result * (max(Class) - min(Class))) + min(Class)
plot(test$Class, predict_testNN, col='blue', pch=16, ylab = "predicted Class", xlab = "real class")
abline(0,1)
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((test$Class - predict_testNN)^2) / nrow(test)) ^ 0.5  ##0.3691721778
RMSE.NN
