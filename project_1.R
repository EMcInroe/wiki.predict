wiki = read.csv("wiki4HE.csv", header=T, sep=";", na.strings="?")
summary(wiki)
head(wiki)
length(wiki$UNIVERSITY[wiki$UNIVERSITY==2])
summary(wiki$OTHER_POSITION[wiki$UNIVERSITY==2])
wiki_UPF<-subset(wiki[wiki$UNIVERSITY==2,])
summary(wiki_UPF)

library(dplyr)
library(tidyr)
####################
#### Clean Data ####
####################

### Remove Rows with missing DOMAIN and USERWIKI
wiki_trim = wiki %>% filter(!is.na(DOMAIN))
wiki_trim = wiki_trim %>% filter(!is.na(USERWIKI))
sum(is.na(wiki_trim$DOMAIN))
summary(wiki_trim)

### Replace Missing Values with median
wiki_trim$YEARSEXP[is.na(wiki_trim$YEARSEXP)] <- median(wiki_trim$YEARSEXP, na.rm = T)
summary(wiki_trim)

wiki_trim<-wiki_trim %>% 
  mutate_at(11:53, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
summary(wiki_trim)

### Replace Missing values for positions
wiki_trim$UOC_POSITION[is.na(wiki_trim$UOC_POSITION)] <- 7
summary(wiki_trim$UOC_POSITION)

wiki_trim<- wiki_trim %>% 
  mutate(OTHERSTATUS = ifelse(is.na(OTHER_POSITION)|OTHER_POSITION==2,
                            7,
                            OTHERSTATUS))
summary(wiki_trim$OTHERSTATUS)
wiki_trim = wiki_trim %>% filter(!is.na(OTHERSTATUS))

#### Change USERWIKI 0 answer to 2
wiki_trim<- wiki_trim %>% 
  mutate(USERWIKI = ifelse(USERWIKI==0,
                              2,
                              USERWIKI))
summary(wiki_trim$USERWIKI)

### Remove Other Position Column
wiki_final <- subset(wiki_trim, select = -OTHER_POSITION)
summary(wiki_final)

### add USER_tot
wiki_final$Use_tot <-wiki_final$Use1 + wiki_final$Use2 + 
                         wiki_final$Use3 + wiki_final$Use4 + wiki_final$Use5
summary(wiki_final$Use_tot)

### Add Binary Response variable based on the User_tot
# rearrange data frame to have response variable at the beginning
# remove other Userx columns
wiki_final$Use_Stat <- 0
wiki_final<- wiki_final %>% 
  mutate(Use_Stat = ifelse(Use_tot > 12.5,
                           1,0))
summary(wiki_final$Use_Stat)

wiki_final <- wiki_final %>% relocate(c(Use_Stat, Use_tot), .before = AGE)
names(wiki_final)

wiki_final <- subset(wiki_final, select = -c(Use1, Use2, Use3, Use4, Use5))
names(wiki_final)

###############
####  PCA  ####
###############

#### Split into training and test sets
# split data into 2 parts for pca training (50%) and prediction (50%)
set.seed(1)
samp <- sample(nrow(wiki_final), nrow(wiki_final)*0.5)
wiki.train <- wiki_final[samp,]
wiki.test <- wiki_final[-samp,]
summary(wiki.train)
length(wiki.train$UNIVERSITY[wiki.train$UNIVERSITY==2])
length(wiki.test$UNIVERSITY[wiki.test$UNIVERSITY==2])

# conduct PCA on training dataset
pca <- prcomp(wiki.train[,12:49], retx=TRUE, center=TRUE, scale=TRUE)
pca
pca.var<-pca$sdev^2
pve<-pca.var/sum(pca.var)
pve
cumsum(pve)
plot(cumsum(pve))

# prediction of PCs for validation dataset
pred <- predict(pca, newdata=wiki.test[,12:49])
pred<-data.frame(pred)

#### Merge Datasets
# test set
row_pred<-nrow(pred)
pred$ID<-c(1:row_pred)
row_test<-nrow(wiki.test)
wiki.test$ID<-c(1:row_test)
View(wiki.test)

df_list<-list(pred,wiki.test)
test_data<-Reduce(function(x,y) merge(x,y,all=TRUE),df_list)
View(test_data)
test_data <- test_data[,-c(1,19:39,51:88)]
test_data <- test_data %>% relocate(c(Use_Stat, Use_tot), .before = PC1)

#training data
pca.train<-data.frame(pca$x)
View(pca.train)
row_pca<-nrow(pca.train)
pca.train$ID<-c(1:row_pca)
row_train<-nrow(wiki.train)
wiki.train$ID<-c(1:row_train)

df_list<-list(pca.train,wiki.train)
train_data<-Reduce(function(x,y) merge(x,y,all=TRUE),df_list)
View(train_data)
train_data <- train_data[,-c(1,19:39,51:88)]
train_data <- train_data %>% relocate(c(Use_Stat, Use_tot), .before = PC1)

##########################
## Logistic Regression ##
#########################
# change levels of the response variable
train_data<- train_data %>% 
  mutate(Use_Stat = ifelse(Use_tot > 12.5,
                          1,0))
test_data<- test_data %>% 
  mutate(Use_Stat = ifelse(Use_tot > 12.5,
                           1,0))
Use.test<-test_data$Use_Stat
Use.test<- as.factor(Use.test)
levels(Use.test)<-c("No","Yes")
# glm() with family=binomial for logistic regression
glm.fit<- glm(Use_Stat~.-Use_tot, data=train_data, family=binomial)
glm.probs<-predict(glm.fit, test_data, type="response")
glm.pred<- rep("No", 452)
glm.pred[glm.probs>.5]<-"Yes"
# confusion table and mean
table(glm.pred, Use.test)
mean(glm.pred==Use.test)

###################
###    LDA   ####
##################
library(MASS)
lda.fit<-lda(Use_Stat~.-Use_tot, data=train_data)
lda.pred<-predict(lda.fit, test_data)
lda.pred.class<-ifelse(lda.pred$class==1, "Yes", "No")
table(lda.pred.class, Use.test)
mean(lda.pred.class==Use.test)

################
###    QDA  ####
################
qda.fit<-qda(Use_Stat~.-Use_tot, data=train_data)
qda.pred<-predict(qda.fit, test_data)

qda.pred.class<-ifelse(qda.pred$class==1, "Yes", "No")
table(qda.pred.class, Use.test)
mean(qda.pred.class==Use.test)

##############
###  KNN  ###
#############
library(class)

set.seed(208)
knn.pred<-knn(train_data[,-c(1:2)], test_data[,-c(1:2)], train_data$Use_Stat, k=5)
knn.pred.class<-ifelse(knn.pred==1, "Yes","No")
table(knn.pred.class, Use.test)
mean(knn.pred.class==Use.test)

stand_train<-scale(train_data[,-c(1:2)])
stand_test<-scale(test_data[,-c(1:2)])

set.seed(208)
knn.pred2<-knn(stand_train, stand_test, train_data$Use_Stat, k=3)
knn.pred.class2<-ifelse(knn.pred2==1, "Yes","No")
table(knn.pred.class2, Use.test)
mean(knn.pred.class2==Use.test)
