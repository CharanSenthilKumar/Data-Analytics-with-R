setwd("C:/Users/chara/OneDrive/Desktop/DataScience")
rice=read.csv("riceClassification.csv")
head(rice)
rice1=subset(rice,select = -c(id))
str(rice1)
#feature engineering
#checking for missing values and imputing the missing value
table(is.na(rice$Class))

#checking for outliets
o1=boxplot(rice1$MajorAxisLength)
outlier=boxplot(rice1$MajorAxisLength,plot = FALSE)$out
outlier
length(rice1$MajorAxisLength)
data_no_outlier=rice1$MajorAxisLength[-which(rice1$MajorAxisLength %in% outlier)]
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)

#checking for correlation
library(corrplot)
correlation=cor(rice1)
corrplot(correlation,method = "number")

#PCA
pca1=prcomp(rice1[, -11], scale. = TRUE)
pca1
#reverse the signs  #eigenvectors in R point in the negative direction by default, so we’ll multiply by -1 to reverse the signs.
pca1$rotation = -1*pca1$rotation
pca1$rotation

#calculating variance of pca
pca_var = pca1$sdev^2
pca_var
sum(pca_var)
#proportion of variance
prop_var= (pca_var/sum(pca_var))
sum(prop_var)

#scree plot to identify best pc
plot(prop_var,xlab = "Principal Components",
     ylab = "Proportion of variance",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#extracting pca values
rice2=cbind(rice1,pca1$x[,1:4])
str(rice2)

#finding the relation between pcs and variables
cor(rice1[,-11], rice2[,12:15])
rice2$Class=as.factor(rice2$Class)
str(rice2)
#splitting train and test dataset
set.seed(123)
library(caret)
sample = createDataPartition(rice2$Class, p = .75, list = FALSE)
train = rice2[ sample,]
test = rice2[-sample,]
save(train,file="train.RData")
save(test,file="test.RData")
getwd()
#building randomforest model
library(randomForest)

#random forest model
model1= randomForest(Class~PC1+PC2+PC3+PC4,train,importance=TRUE,ntree=500)
model1
#hyper parameter tuning
seed = 123
metric = "Accuracy"
control = trainControl(method="repeatedcv", number=5, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(rice2))
rf_random <- train(Class~PC1+PC2+PC3+PC4, rice2, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

#model with hyperparameters
model2= randomForest(Class~Area+MajorAxisLength+Extent+Roundness+AspectRation,train,mtry=1,importance=TRUE,ntree=100)
model2

#prediction
prediction = predict(model2,test)
ctable=table(test$Class,prediction)
ctable
acc=sum(diag(ctable)/sum(ctable))
acc
paste("The accuracy is : ", acc)

#calculating sensitivity and specificity
sensitivity(ctable)
specificity(ctable)

#save the model as RDS
saveRDS(model2, "model2.rds")

