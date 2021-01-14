Spotify_trainingset <- read_csv("C:\\Users\\Passerotte\\Desktop\\COMPETITION\\Spotify_trainingset.csv")
View(Spotify_trainingset)

#installing librairies
library(BCA)
library(relimp)
library(dplyr)
library(car)
library(RcmdrMisc)
library(randomForest)
library(AMORE)
library(readr)

#Random Forest
training <- read_csv("C:\\Users\\Passerotte\\Desktop\\COMPETITION\\Spotify_trainingset.csv")
View(Spotify_trainingset)
#During data preparation, I deleted those 3 variables
training <- select(`training`, -c(`X1`))
`training`<- select(`training`, -c(`song_title`))
`training`<- select(`training`, -c(`artist`))
#then I splitted my dataset into training and validation 
rand = sample(1:nrow(training),0.7*nrow(training)) 
train = training[rand,]
val = training[-rand,]

# random forest
library(caret)
#Confusion Matrix with validation
#with 300 trees
validation_model <- randomForest(target~., data=val, ntrees=300)
x<-predict(validation_model, newdata=val)
x<-round(x,0)
confusionMatrix(factor(x),factor(val$target))
#with 600 trees
validation_model2 <- randomForest(target~., data=val, ntrees=600)
xx<-predict(validation_model, newdata=val)
xx<-round(xx,0)
confusionMatrix(factor(xx),factor(val$target))


#Confusion Matrix with training 
#with 300 trees 
trainmodel <- randomForest(target~., data=train, ntrees=300)
y<-predict(trainmodel, newdata=train)
y<-round(y,0)
confusionMatrix(factor(y),factor(train$target))

#with 600 trees 
trainmodel <- randomForest(target~., data=train, ntrees=600)
yy<-predict(trainmodel, newdata=train)
yy<-round(yy,0)
confusionMatrix(factor(yy),factor(train$target))

#I found an accuracy of 1 for both of my trainmodel. (y and yy)
#Now, I use same model with prediction dataset
 prediction <- read_delim("C:\\Users\\Passerotte\\Desktop\\prediction\\prediction.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
prediction<- select(`prediction`, -c(`X1`))
prediction<- select(`prediction`, -c(`song_title`))
prediction<- select(`prediction`, -c(`artist`))
View(prediction)
yy<-predict(mod1, newdata =prediction)
yy<-round(yy,0)
prediction$prediction<-c(yy)
write.table(prediction, file = "prediction.csv", sep = ",")

#here is my final dataset prédiction with the responses using random forest composing of 600 trees. 


