#combining datasets 
data.combined <- rbind(xaa,xab,xac,xad,xae,xaf,xag,xah,xai)

#finding the types of data contained in files
str(data.combined)

#renaming the columns of our combined data set
names(data.combined) <- c("COMPACTNESS", "CIRCULARITY", "DISTANCE CIRCULARITY", "RADIUS RATIO",
  "PR.AXIS ASPECT RATIO", "MAX.LENGTH ASPECT RATIO","SCATTER RATIO", "ELONGATEDNESS", 
  "PR.AXIS RECTANGULARITY","MAX.LENGTH RECTANGULARITY","SCALED VARIANCE-MAJOR AXIS","SCALED VARIANCE-MINOR AXIS",
  "SCALED RADIUS-GYRATION","SKEWNESS-MAJOR AXIS","SKEWNESS-MINOR AXIS","KURTOSIS-MINOR AXIS", 
  "KURTOSIS-MAJOR AXIS","HOLLOWS RATIO","CLASSES")


#Computing Covariance 
cov(data.combined$COMPACTNESS,data.combined$CIRCULARITY)
#covariance Matrix 
cov(data.combined[,c(1,2,12)])


#computing correlation 
cor(data.combined[,c(1,2,12)])


#creating a scatter plot
plot(data.combined[,c(2,12)],main = 'Circularity vs Scaled Variance-Minor')


#creating a scatter plot for last two values of our data set: Kurtosis(major) and HOLLOWS RATIO
plot(data.combined[,c(17,18)],main = 'KURTOSIS(MAJOR) vs HOLLOWS RATIO')

#creating histograms with first two features of each dataset

#xaa
par(mfrow=c(2,1))
hist(xaa$V1, main = 'XAA: COMPACTNESS Histogram')
hist(xaa$V2, main = 'XAA: CIRCULARITY Histogram')


#xab
par(mfrow=c(2,1))
hist(xab$V1, main = 'XAB: COMPACTNESS Histogram')
hist(xab$V2, main = 'XAB: CIRCULARITY Histogram')

#xac
par(mfrow=c(2,1))
hist(xac$V1, main = 'XAC: COMPACTNESS Histogram')
hist(xac$V2, main = 'XAC: CIRCULARITY Histogram')


#xad
par(mfrow=c(2,1))
hist(xad$V1, main = 'XAD: COMPACTNESS Histogram')
hist(xad$V2, main = 'XAD: CIRCULARITY Histogram')

#xae
par(mfrow=c(2,1))
hist(xae$V1, main = 'XAE: COMPACTNESS Histogram')
hist(xae$V2, main = 'XAE: CIRCULARITY Histogram')

#xaf
par(mfrow=c(2,1))
hist(xaf$V1, main = 'XAF: COMPACTNESS Histogram')
hist(xaf$V2, main = 'XAF: CIRCULARITY Histogram')

#xag
par(mfrow=c(2,1))
hist(xag$V1, main = 'XAG: COMPACTNESS Histogram')
hist(xag$V2, main = 'XAG: CIRCULARITY Histogram')

#xah
par(mfrow=c(2,1))
hist(xah$V1, main = 'XAH: COMPACTNESS Histogram')
hist(xah$V2, main = 'XAH: CIRCULARITY Histogram')

#xai
par(mfrow=c(2,1))
hist(xai$V1, main = 'XAI: COMPACTNESS Histogram')
hist(xai$V2, main = 'XAI: CIRCULARITY Histogram')

#combined data
par(mfrow=c(2,1))
hist(data.combined$COMPACTNESS, main = 'COMBINED DATA: COMPACTNESS Histogram')
hist(data.combined$CIRCULARITY, main = 'COMBINED DATA: CIRCULARITY Histogram')

#a few features 
lines(density(xaa$V1))
max(xaa$V1,na.rm = TRUE)
mean(xaa$V1,na.rm = TRUE)
min(xaa$V1, na.rm = TRUE)
head(data.combined)
tail(data.combined)

#storing the different classes
OPEL <- data.combined[data.combined$CLASSES=="opel",]
SAAB <- data.combined[data.combined$CLASSES=="saab",]
BUS <- data.combined[data.combined$CLASSES=="bus",]
VAN <- data.combined[data.combined$CLASSES=="van",]


#creating boxplots
boxplot(OPEL$COMPACTNESS, ylab='COMPACTNESS', main='Compactness of OPEL Class')
boxplot(SAAB$COMPACTNESS, ylab='COMPACTNESS', main='Compactness of SAAB Class')
boxplot(BUS$COMPACTNESS, ylab='COMPACTNESS', main='Compactness of BUS Class')
boxplot(VAN$COMPACTNESS, ylab='COMPACTNESS', main='Compactness of VAN Class')
boxplot(data.combined$COMPACTNESS, ylab='COMPACTNESS', main='Compactness of COMBINED DATA')
boxplot(data.combined$COMPACTNESS ~ data.combined$CLASSES, xlab = 'Classes',ylab='COMPACTNESS', main='Compactness of Classes')


#creating supervised Scatter plots
#compactness vs circularity
ggplot(data.combined, aes(x=COMPACTNESS, y=CIRCULARITY, color=CLASSES))+
  geom_point()+ geom_smooth(se=FALSE)+labs(title = "COMPACTNESS VS CIRCULARITY")

#compactness vs Scaled Variance
ggplot(data.combined, aes(x=COMPACTNESS, y=`SCALED VARIANCE-MINOR AXIS`, color=CLASSES))+
  geom_point()+ geom_smooth(se=FALSE)+labs(title = "COMPACTNESS VS SCALED VARIANCE-MINOR AXIS")

#circularity vs Scaled Variance
ggplot(data.combined, aes(x=CIRCULARITY, y=`SCALED VARIANCE-MINOR AXIS`, color=CLASSES))+
  geom_point()+ geom_smooth(se=FALSE)+labs(title = "CIRCULARITY VS SCALED VARIANCE-MINOR AXIS")


#calculating z-score
zscore <- data.combined

zscore$COMPACTNESS<-(data.combined$COMPACTNESS - mean(data.combined$COMPACTNESS))/sd(data.combined$COMPACTNESS)

zscore$CIRCULARITY<-(data.combined$CIRCULARITY - mean(data.combined$CIRCULARITY))/sd(data.combined$CIRCULARITY)

zscore$`DISTANCE CIRCULARITY`<-(data.combined$`DISTANCE CIRCULARITY` - mean(data.combined$`DISTANCE CIRCULARITY`))/sd(data.combined$`DISTANCE CIRCULARITY`)

zscore$`RADIUS RATIO`<-(data.combined$`RADIUS RATIO` - mean(data.combined$`RADIUS RATIO`))/sd(data.combined$`RADIUS RATIO`)

zscore$`PR.AXIS ASPECT RATIO`<-(data.combined$`PR.AXIS ASPECT RATIO` - mean(data.combined$`PR.AXIS ASPECT RATIO`))/sd(data.combined$`PR.AXIS ASPECT RATIO`)

zscore$`MAX.LENGTH ASPECT RATIO`<-(data.combined$`MAX.LENGTH ASPECT RATIO` - mean(data.combined$`MAX.LENGTH ASPECT RATIO`))/sd(data.combined$`MAX.LENGTH ASPECT RATIO`)

zscore$`SCATTER RATIO`<-(data.combined$`SCATTER RATIO` - mean(data.combined$`SCATTER RATIO`))/sd(data.combined$`SCATTER RATIO`)

zscore$ELONGATEDNESS<-(data.combined$ELONGATEDNESS - mean(data.combined$ELONGATEDNESS))/sd(data.combined$ELONGATEDNESS)

zscore$`PR.AXIS RECTANGULARITY`<-(data.combined$`PR.AXIS RECTANGULARITY` - mean(data.combined$`PR.AXIS RECTANGULARITY`))/sd(data.combined$`PR.AXIS RECTANGULARITY`)

zscore$`MAX.LENGTH RECTANGULARITY`<-(data.combined$`MAX.LENGTH RECTANGULARITY` - mean(data.combined$`MAX.LENGTH RECTANGULARITY`))/sd(data.combined$`MAX.LENGTH RECTANGULARITY`)

zscore$`SCALED VARIANCE-MAJOR AXIS`<-(data.combined$`SCALED VARIANCE-MAJOR AXIS` - mean(data.combined$`SCALED VARIANCE-MAJOR AXIS`))/sd(data.combined$`SCALED VARIANCE-MAJOR AXIS`)

zscore$`SCALED VARIANCE-MINOR AXIS`<-(data.combined$`SCALED VARIANCE-MINOR AXIS` - mean(data.combined$`SCALED VARIANCE-MINOR AXIS`))/sd(data.combined$`SCALED VARIANCE-MINOR AXIS`)

zscore$`SCALED RADIUS-GYRATION`<-(data.combined$`SCALED RADIUS-GYRATION` - mean(data.combined$`SCALED RADIUS-GYRATION`))/sd(data.combined$`SCALED RADIUS-GYRATION`)

zscore$`SKEWNESS-MAJOR AXIS`<-(data.combined$`SKEWNESS-MAJOR AXIS` - mean(data.combined$`SKEWNESS-MAJOR AXIS`))/sd(data.combined$`SKEWNESS-MAJOR AXIS`)

zscore$`SKEWNESS-MINOR AXIS`<-(data.combined$`SKEWNESS-MINOR AXIS` - mean(data.combined$`SKEWNESS-MINOR AXIS`))/sd(data.combined$`SKEWNESS-MINOR AXIS`)

zscore$`KURTOSIS-MINOR AXIS`<-(data.combined$`KURTOSIS-MINOR AXIS` - mean(data.combined$`KURTOSIS-MINOR AXIS`))/sd(data.combined$`KURTOSIS-MINOR AXIS`)

zscore$`KURTOSIS-MAJOR AXIS`<-(data.combined$`KURTOSIS-MAJOR AXIS` - mean(data.combined$`KURTOSIS-MAJOR AXIS`))/sd(data.combined$`KURTOSIS-MAJOR AXIS`)

zscore$`HOLLOWS RATIO`<-(data.combined$`HOLLOWS RATIO` - mean(data.combined$`HOLLOWS RATIO`))/sd(data.combined$`HOLLOWS RATIO`)

#ADDING VARIABLES
zscore$B[zscore$CLASSES=='bus'] <- 1
zscore$B[zscore$CLASSES!='bus'] <- 0
zscore$V[zscore$CLASSES=='van'] <- 1
zscore$V[zscore$CLASSES!='van'] <- 0

#finding the fit linear model for Busses(variable B)
modelB <- lm(B~COMPACTNESS+CIRCULARITY+`DISTANCE CIRCULARITY`+ `RADIUS RATIO`+
             `PR.AXIS ASPECT RATIO`+ `MAX.LENGTH ASPECT RATIO`+`SCATTER RATIO`+ `ELONGATEDNESS`+ 
             `PR.AXIS RECTANGULARITY`+`MAX.LENGTH RECTANGULARITY`+`SCALED VARIANCE-MAJOR AXIS`+`SCALED VARIANCE-MINOR AXIS`+
             `SCALED RADIUS-GYRATION`+`SKEWNESS-MAJOR AXIS`+`SKEWNESS-MINOR AXIS`+`KURTOSIS-MINOR AXIS`+ 
             `KURTOSIS-MAJOR AXIS`+`HOLLOWS RATIO`, data=zscore)

plot(modelB)

summary(modelB)

#same for the Van dependency

modelV <- lm(V~COMPACTNESS+CIRCULARITY+`DISTANCE CIRCULARITY`+ `RADIUS RATIO`+
               `PR.AXIS ASPECT RATIO`+ `MAX.LENGTH ASPECT RATIO`+`SCATTER RATIO`+ `ELONGATEDNESS`+ 
               `PR.AXIS RECTANGULARITY`+`MAX.LENGTH RECTANGULARITY`+`SCALED VARIANCE-MAJOR AXIS`+`SCALED VARIANCE-MINOR AXIS`+
               `SCALED RADIUS-GYRATION`+`SKEWNESS-MAJOR AXIS`+`SKEWNESS-MINOR AXIS`+`KURTOSIS-MINOR AXIS`+ 
               `KURTOSIS-MAJOR AXIS`+`HOLLOWS RATIO`, data=zscore)

plot(modelV)

summary(modelV)




#7-Creating Decision Trees


dim(zscore)
#sample, will test about 100
s <- sample(846,size = 746)
s
#creating test and training sambles for data.combined
data.train <- data.combined[s,]
data.test <- data.combined[-s,]

dim(data.train)
dim(data.test)

par(mfrow=c(1,1))
dtm <- rpart(CLASSES~ , data.train)
dtm
plot(dtm)
text(dtm)
rpart.plot(dtm, main="Vehicle Classification")

#creating test and training sambles for zscore

clean_zscore <-zscore 
glimpse(clean_zscore)
data.train <- zscore[s,]
data.test <- zscore[-s,]

#Checking the dimension of the data sets
dim(data.train)
dim(data.test)

#creating a decision tree for busses:
par(mfrow=c(1,1))
dtm1 <- rpart(B~COMPACTNESS+CIRCULARITY+`DISTANCE CIRCULARITY`+ `RADIUS RATIO`+
               `PR.AXIS ASPECT RATIO`+ `MAX.LENGTH ASPECT RATIO`+`SCATTER RATIO`+ `ELONGATEDNESS`+ 
               `PR.AXIS RECTANGULARITY`+`MAX.LENGTH RECTANGULARITY`+`SCALED VARIANCE-MAJOR AXIS`+`SCALED VARIANCE-MINOR AXIS`+
               `SCALED RADIUS-GYRATION`+`SKEWNESS-MAJOR AXIS`+`SKEWNESS-MINOR AXIS`+`KURTOSIS-MINOR AXIS`+ 
               `KURTOSIS-MAJOR AXIS`+`HOLLOWS RATIO`, data = data.train)
dtm1
plot(dtm1)
text(dtm1)
rpart.plot(dtm1, main="Vehicle Classification:Bus")

#creating a decision tree for Vans:
par(mfrow=c(1,1))
dtm2 <- rpart(V~COMPACTNESS+CIRCULARITY+`DISTANCE CIRCULARITY`+ `RADIUS RATIO`+
                `PR.AXIS ASPECT RATIO`+ `MAX.LENGTH ASPECT RATIO`+`SCATTER RATIO`+ `ELONGATEDNESS`+ 
                `PR.AXIS RECTANGULARITY`+`MAX.LENGTH RECTANGULARITY`+`SCALED VARIANCE-MAJOR AXIS`+`SCALED VARIANCE-MINOR AXIS`+
                `SCALED RADIUS-GYRATION`+`SKEWNESS-MAJOR AXIS`+`SKEWNESS-MINOR AXIS`+`KURTOSIS-MINOR AXIS`+ 
                `KURTOSIS-MAJOR AXIS`+`HOLLOWS RATIO`, data = data.train, method = 'class')
dtm2
plot(dtm2)
text(dtm2)
rpart.plot(dtm2, main="Vehicle Classification:Vans")


#different way to create a tree diagram for Van
fit<- rpart(V~COMPACTNESS+CIRCULARITY+`DISTANCE CIRCULARITY`+ `RADIUS RATIO`+
              `PR.AXIS ASPECT RATIO`+ `MAX.LENGTH ASPECT RATIO`+`SCATTER RATIO`+ `ELONGATEDNESS`+ 
              `PR.AXIS RECTANGULARITY`+`MAX.LENGTH RECTANGULARITY`+`SCALED VARIANCE-MAJOR AXIS`+`SCALED VARIANCE-MINOR AXIS`+
              `SCALED RADIUS-GYRATION`+`SKEWNESS-MAJOR AXIS`+`SKEWNESS-MINOR AXIS`+`KURTOSIS-MINOR AXIS`+ 
              `KURTOSIS-MAJOR AXIS`+`HOLLOWS RATIO`, method="class", data=data.train, minsplit=20, maxdepth=20)
rpart.plot(fit, main="Van Classification Tree")
str(zscore)

#testing prediction:
predict.unseen <- predict(fit,data.test,type = 'class')
predict.unseen
table_mat <- table(data.test$V,predict.unseen)
table_mat
accuracy.test <- sum(diag(table_mat))/sum(table_mat)
print(paste('Accuracy for Test', accuracy.test))

#different way to create a tree diagram for B
fitB<- rpart(B~COMPACTNESS+CIRCULARITY+`DISTANCE CIRCULARITY`+ `RADIUS RATIO`+
              `PR.AXIS ASPECT RATIO`+ `MAX.LENGTH ASPECT RATIO`+`SCATTER RATIO`+ `ELONGATEDNESS`+ 
              `PR.AXIS RECTANGULARITY`+`MAX.LENGTH RECTANGULARITY`+`SCALED VARIANCE-MAJOR AXIS`+`SCALED VARIANCE-MINOR AXIS`+
              `SCALED RADIUS-GYRATION`+`SKEWNESS-MAJOR AXIS`+`SKEWNESS-MINOR AXIS`+`KURTOSIS-MINOR AXIS`+ 
              `KURTOSIS-MAJOR AXIS`+`HOLLOWS RATIO`, method="class", data=data.train, minsplit=20, maxdepth=20)
rpart.plot(fitB, main="Van Classification Tree")
str(zscore)

#testing prediction:
predict.unseenB <- predict(fitB,data.test,type = 'class')
predict.unseenB
table_matB <- table(data.test$B,predict.unseen)
table_matB
accuracy.testB <- sum(diag(table_matB))/sum(table_matB)
print(paste('Accuracy Test for Bus', accuracy.testB))

