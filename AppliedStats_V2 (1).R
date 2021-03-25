##################### Importing the required packages ########################

library(ggplot2)
library(dplyr)
library(corrplot)
library(plot3D)
library(plotly)
library(caret)
################### Loading the dataset to R #######################

data=read.csv("m://pc//downloads//Applied Stats//house-data.csv",header = TRUE,sep = ",")
summary(data)


#This tells us about the missing value information for each variable in the dataframe.
NaDetails<-colSums(sapply(data,is.na))
NaDetails
# It can be observed that 4 features(Alley, PoolQC, Fence, MiscFeature) contain a lot of NAs(>80%), it is better to eliminate these features
# since it is useless to impute them. However, 6(LotFrontage, MasVnrArea, BsmtQual, BsmtCond, GarageType, GarageCond) features contain
# reasonable amount of NAs which can be imputed.


#We may not include the features containing huge missing values but we can definitely get some important information
# about the feature which we may use to draw some business conclusions(This could be a part of Research Question).

alley<-data[which(data["Alley"]=="Grvl" | data["Alley"]=="Pave"),]
PoolQC<-data[which(data["PoolQC"]=="Ex" | data["PoolQC"]=="Fa" | data["PoolQC"]=="Gd" |data["PoolQC"]=="TA") ,]
Fence<-data[which(data["Fence"]=="GdPvr" | data["Fence"]=="MnPrv" | data["Fence"]=="GdWo"| data["Fence"]=="MnWw") ,]
MiscFeature<-data[which(data["MiscFeature"]=="Gar2" | data["MiscFeature"]=="Othr" | data["MiscFeature"]=="Shed" | data["MiscFeature"]=="Tenc"),]

#Remove the 4 features.
data1<-data[,-c(1,5,43,44,45)]
#names(data1)






############### 1. Provide Numerical and Graphical summary of the data-set and comment on them. ############

summary(data1)

##Getting the numerical columns
numeric_vars<-names(data1)[which(sapply(data1,is.numeric))]
data1_num<-data1[,c(numeric_vars)]


library(pastecs)



### Descriptive Stats Basic Table
options(scipen=100)
options(digits = 2)
basic_stats<-data.frame(stat.desc(data1_num,basic=F))
basic_stats # This Dataframe has the basic stats for every variable.



##Finding the Correlations among the numerical variables.
cor_num<-cor(data1_num,method = "pearson",use = "complete.obs")

#Plot
#corr_plot<- corrplot(cor_num)
library(GGally)

ggcorr(cor_num,label=TRUE)



# Correlation of all the variables w.r.t to the Sales Price.
corr_mat_wrt_price<-as.matrix(sort(cor_num[,"SalePrice"],decreasing = TRUE))
corr_mat_wrt_price[2:6,] #Top 5 features that has a high positive correlation with the House Price.

#Usually big houses have a higher price and here it makes complete sense that a High Priced house will have a very good Quality with Big living area and
#garage area, a big basement area and finally a big First Floor in terms of area.


######### Neighborhood vs SalePrice #################

Neighborhood_group<- data1 %>%
  group_by(Neighborhood) %>%
  summarize(SalePrice=mean(SalePrice),.groups = 'drop')

Price_wrt_Neighbor<-data.frame(Neighborhood_group)
Price_wrt_Neighbor=Price_wrt_Neighbor[order(-Price_wrt_Neighbor$SalePrice),] #ordering from High SalePrice to low.


df_area_price<-ggplot(data=Price_wrt_Neighbor[1:6,], aes(x=Neighborhood,y=SalePrice,fill=Neighborhood)) +
  geom_bar(stat="identity",color='black',position=position_dodge())+
  geom_text(aes(label=SalePrice), vjust=-0.3, size=3.5)+
  labs(x="Area ", y= "Price", title="Area vs Price Distribution")+
  theme_minimal()+
  scale_fill_manual(values=c('#E69F00','#56B4E9',"blue","yellow","pink","black"))

ggplotly(df_area_price) #top 6 areas where the house prices are expensive.





############################ House Prices according to Foundation Type ########################

Foundation_group<- data1 %>%
  group_by(Foundation) %>%
  summarize(SalePrice=mean(SalePrice),.groups = 'drop')

Price_wrt_foundation<-data.frame(Foundation_group)
Price_wrt_foundation=Price_wrt_foundation[order(-Price_wrt_foundation$SalePrice),]


df_foundation_price<-ggplot(data=Price_wrt_foundation[1:6,], aes(x=Foundation,y=SalePrice,fill=Foundation)) +
  geom_bar(stat="identity",color='black',position=position_dodge())+
  geom_text(aes(label=SalePrice), vjust=-0.3, size=3.5)+
  labs(x="Foundation Type ", y= "Price", title="Foundation type vs Price Distribution")+
  theme_minimal()+
  scale_fill_manual(values=c('#E69F00','#56B4E9',"blue","yellow","pink","black"))

ggplotly(df_foundation_price) # Houses with Foundation type "Poured Concrete"  tend to cost more that other foundation.


########################## Floor vs Foundation ##############################


FoundationVSFloor_group<- data1 %>%
  group_by(HouseStyle,Foundation) %>%
  summarize(count=n(),.groups = 'drop')

foundationVSfloor<-data.frame(FoundationVSFloor_group)
#foundationVSfloor=Price_wrt_foundation[order(-Price_wrt_foundation$SalePrice),]


df_foundationVSfloor<-ggplot(data=foundationVSfloor, aes(x=HouseStyle,y=Foundation,fill=Foundation)) +
  geom_bar(stat="identity",color='black',position=position_dodge())+
  geom_text(aes(label=Foundation), vjust=-0.3, size=3.5)+
  labs(x="House Type", y= "Foundation Type", title="Foundation type vs HouseType Distribution")+
  theme_minimal()+
  scale_fill_manual(values=c('#E69F00','#56B4E9',"blue","yellow","pink","black"))

ggplotly(df_foundationVSfloor)

#Obervations:
#1.It can be clearly observed that only Poured Concrete and Cinder Block have been used for as foundation in SLvl HouseType.
#2. All kinds of Foundations have been used in the construction of 1.5Fin and 2Story buildings.


#################################### Construction Year vs Price ##########################

Construction_Year<- data1 %>%
  group_by(YearBuilt) %>%
  summarize(SalePrice=mean(SalePrice),.groups = 'drop')

Const_year<-data.frame(Construction_Year)
Const_year=Const_year[order(-Const_year$SalePrice),]


df_yearvsPrice<-ggplot(data=Const_year[1:50,], aes(x=YearBuilt,y=SalePrice)) +
  geom_bar(stat="identity",color='black',position=position_dodge())+
  geom_text(aes(label=YearBuilt), vjust=-0.3, size=3.5)+
  labs(x="Year of Construction  ", y= "Price", title="Age of House vs Price of House")+
  theme_minimal()+
  scale_fill_manual(values=c('#E69F00','#56B4E9',"blue","yellow","pink","black"))

ggplotly(df_yearvsPrice)

#Observations:
#1. It can be observed that most of the houses made after 1980s have a high selliing price, and prices tend to increase higher for the houses made after 2005.
#2.A few houses made in early 1800s and late 1800s are also values very high, they might me antique houses.



##################################### 2. Divide the house Based on Overall Condition ################################

# Creating the new feature "OverallCondCat" with values as a "Poor", "Average" and "Good".

data1["OverallCondCat"]<-ifelse(data1["OverallCond"]>=1 & data1["OverallCond"]<4,"Poor",
                               ifelse(data1["OverallCond"]>=4 & data1["OverallCond"]<7,"Average",
                                      ifelse(data1["OverallCond"]>=7 & data1["OverallCond"]<11,"Good","Unknown")))

table(data1["OverallCondCat"])# We can observe that 1130 Houses are Average, 299 are Good and 31 houses are in Poor Condition.




################################## Missing Value Imputation ######################## 

#Now it is time to impute the missing values(LotFrontage, MasVnrArea, BsmtQual, BsmtCond, GarageType, GarageCond).



#We will not perform a blind mean/median/mode imputation since the dataset also contains categorical values. We will perform  a category wise imputation.

Lot_data<-subset(data1,(!is.na(data1[,1]))) # Creating a new dataset where there are no NAs in the LotFrontage.

#Grouping the data accoring to the Neighborhood, and taking the mean values so that we can impute them with the NAs in the LotFrontage column.

LotFrontage<- Lot_data %>%
  group_by(Neighborhood) %>%
  summarize(lot_mean = mean(LotFrontage), .groups = 'drop')

LotFrontage = data.frame(LotFrontage)

#In the below step we will fill all the NAs in the LotFrontage column with the mean values of the corresponding category. 
data1$LotFrontage[is.na(data1$LotFrontage)]<-LotFrontage$lot_mean[match(data1$Neighborhood,LotFrontage$Neighborhood)][which(is.na(data1$LotFrontage))]
#Reference: stackoverflow.com/questions/34697032/fill-in-missing-values-nas-with-values-from-another-dataframe-in-r

################################## Performing the same operation to all the numerical variables that have NAs #############

MasVnrArea_data<-subset(data1,(!is.na(data1[,17])))

MasVnrArea_group<- MasVnrArea_data %>%
  group_by(Neighborhood) %>%
  summarize(mas_mean = mean(MasVnrArea), .groups = 'drop')

MasVnrArea_group = data.frame(MasVnrArea_group)
data1$MasVnrArea[is.na(data1$MasVnrArea)]<-MasVnrArea_group$mas_mean[match(data1$Neighborhood,MasVnrArea_group$Neighborhood)][which(is.na(data1$MasVnrArea))]



############## Mode Imputation for Categorical Features  ###############

table(data1["BsmtQual"]) # "TA" is the most common category
data1$BsmtQual[is.na(data1$BsmtQual)]<-"TA" # Mode imputation

#########################

table(data1["BsmtCond"]) # "TA" is the most common category
data1$BsmtCond[is.na(data1$BsmtCond)]<-"TA" # Mode imputation

#####################

table(data1["GarageType"]) # "Attchd" is the most common category
data1$GarageType[is.na(data1$GarageType)]<-"Attchd" # Mode imputation

#######################

table(data1["GarageCond"]) # "TA" is the most common category
data1$GarageCond[is.na(data1$GarageCond)]<-"TA" # Mode imputation

#write.csv(data1,"M:\\pc\\downloads\\Applied Stats" ,row.names = TRUE)







################## 2.(a): Fit a Logistic Regression that predicts the Over Condition ###################

#Check if the dataframe contains any duplicate values.

cat("The number of duplicate rows in the dataset are: ",nrow(data1)-nrow(unique(data1))) #No duplicate rows found

#str(data1)

data1$OverallCondCat1<-ifelse(data1$OverallCondCat=="Poor",1,ifelse(data1$OverallCondCat=="Average",2,ifelse(data1$OverallCondCat=="Good",3,4)))

#Converting the character columns(categorical variables) to as.factors.
data2<-data1%>%mutate_if(is.character,as.factor)
#str(data2)

data2$OverallCondCat1<-as.factor(data2$OverallCondCat1)
data2<-data2[,-c(13,43)] # Eliminating the "Year Built" and "Year Sold" columns as they are not needed for Model Building.


#W have 3 levels in response variables, so we have to set a reference level, we are setting "Poor" as a the reference level
data2$out<-relevel(data2$OverallCondCat1,ref = "1") 
#https://stats.stackexchange.com/questions/33240/



library(nnet)

set.seed(123)

splitting<-sample(2,nrow(data2),              #### Re-sampling Method. Taking 80% as train and 20% as test. 
                   replace=TRUE,
                   prob = c(0.8,0.2))

train_lr<-data2[splitting==1,]
test_lr<-data2[splitting==2,]

lr_model<-multinom(out~.,data=train_lr)

#-LotFrontage -LotArea -Condition1RRNe -Condition2RRAn -TotalBsmtSF -X1stFlrSF -X2ndFlrSF -GrLivArea -FunctionalSev -GarageArea -SalePrice
summary(lr_model)

z<-summary(lr_model)$coefficients/summary(lr_model)$standard.errors
p<-(1-pnorm(abs(z),0,1))*2
p                            ##P-value of the models 


#Re-training after removing less significant variables p>5%
lr_model<-multinom(out~.-LotFrontage -LotArea -TotalBsmtSF -X1stFlrSF -X2ndFlrSF -GrLivArea -GarageArea -SalePrice
,data=train_lr)

summary(lr_model)

#### Predict on Test Sample(Set) #####

prediction<-predict(lr_model,test_lr)

########## Confusion Matrix #############

confusion<-table(prediction,test_lr$out)

# prediction   1   2   3

# 1            11   0   1
# 2            0 205   1
# 3            0   0  62


Error<-1-sum(diag(confusion))/sum(confusion)
Error
#### Model Assessment ###########

Class_wise<-confusion/colSums(confusion)
Class_wise
# prediction     1           2           3
# 1        1.000000000 0.000000000 0.090909091
# 2        0.000000000 1.000000000 0.004878049
# 3        0.000000000 0.000000000 0.968750000

#Our data is imbalanced, Class 2 is in majority and class 3 is minority.
#We can see that our model is performing good for class 1 and 2 but there 
# is a slight mis-classification issues with class 3 but it is acceptable since the error is only 10%.

# 
# library(ROCR)
# library(gplots)
# 
# eval<-preformance(predicition(predict(lr_model,train_lr,type="prob"),train_lr$OverallCondCat),"acc")
# 
# predict(lr_model,train_lr,type="prob")
# predicition(predict(lr_model,train_lr,type="prob"),train_lr$OverallCondCat)
# 






######################## 2b. Support Vector Machine  #######################

library(e1071)

svm_model<-svm(OverallCondCat~.,data=train_lr)

summary(svm_model)

########## Prediction #############

svm_predict<-predict(svm_model,test_lr)

#### Confusion Matrix ###########

svm_cm<-table(svm_predict,test_lr$OverallCondCat)

# svm_predict_sig   Average Good Poor
# Average              205    2   11
# Good                   0   62    0
# Poor                   0    0    0

svm_Error<-1-sum(diag(svm_cm))/sum(svm_cm) # 4% error, but all the "Poor" Conditioned houses are being classified as "Average".

###### Sigmoid Kernal #########

#Lets change the kernal to Sigmoid and see the results 

svm_model_sig<-svm(OverallCondCat~.,data=train_lr,   
               kernal="sigmoid",cv=10)              # Resampling Method: K-fold Cross validation.

summary(svm_model_sig)

########## Prediction #############
svm_predict_sig<-predict(svm_model_sig,test_lr)

#### Confusion Matrix ###########

svm_cm_sig<-table(svm_predict_sig,test_lr$OverallCondCat)

# svm_predict_sig   Average Good Poor
# Average              205    2   11
# Good                   0   62    0    #### No change in result, the Poor category is performing bad.
# Poor                   0    0    0









##################  3(a). Multiple Linear Regression  ###########################


library(caTools)

# The data what we have pose have categorical variables, we can create dummy and include them in the dataset.
#But we would rather select the highly correlated numerical variables with "SalePrice" as the Independant variables.

corr_mat_wrt_price[2:7,] # this gives us the top 6 variables that are correlated with the "SalePrice".




lin_model<-lm(SalePrice~ OverallQual+GrLivArea+GarageArea+TotalBsmtSF, data = train_lr)

summary(lin_model)

#X1stFlrSF and FullBath have been removed since the p value for these variables are higher than 5%.

############## Multi-Collinearity ##########

library("car")
vif(lin_model) # Theoretically it is okay to to have VIF<10. We have less than 5 here.

########## Other Assumptions ##########

plot(lin_model)  

############### The residual vs fitted line- Plot1: ##########

# The first plot shows us the residuals vs fitted line plot. The red line is horizontal to the 0 on y axis.
#The data points shouldn't have any patterns like a sign curve or a parabola curve, in this case, we have a 
#linear pattern. All the points are evenly distributed around the zero and is homoscedastic. Our first assumption passed.

############## The Normality Assumption- Plot2: ##########

#Most of the residuals are normally distributed along the line at 45degrees. Our second assumption has passed.

############## The scale location plot ###############

#The red line is supposed to be straight and points should be equally distributed around it. Here the line is not straight
#but the points are evenly distributed. We can say that the data is not homoscedastic. The plot of the predicted values
#and the square rooted standardized residuals looks good. Hence our assumption has passed.

############ The Cooks Distance ########################

#Influencial outliers destroy the models, but here we have 3 major outliers, they cannot impact the model much.

########### Prediction ################

linear_pred<-predict(lin_model,test_lr)

#R_Square<-R2(as.integer(linear_pred),(test_lr$SalePrice))

R_Square<- 1-((sum((linear_pred-test_lr$SalePrice)^2))/(sum((test_lr$SalePrice-mean(test_lr$SalePrice))^2)))
R_Square


RMSE_score<-RMSE(linear_pred,test_lr$SalePrice)
RMSE_score





# 
# #################### SVR #####################
# 
SVR_model<-svm(SalePrice~ OverallQual+GrLivArea+GarageArea+TotalBsmtSF,data = train_lr)

summary(SVR_model)


svr_predict<-predict(SVR_model,test_lr)

#################  Evaluations #############

svr_MSE<-(1/nrow(test_lr))*sum((svr_predict-test_lr$SalePrice)^2)
Rmse_svr=(svr_MSE)^0.5
Rmse_svr






############################## 3B Resampling Methods ####################################

#1. Validation Set 80-20 Split
#2. K-Fold Crossvalidation. 

#Both of the methods have been incorporated in  the classification techniques in question 2.





######################### 4. Research Question #######################

# alley<-data[which(data["Alley"]=="Grvl" | data["Alley"]=="Pave"),]
# PoolQC<-data[which(data["PoolQC"]=="Ex" | data["PoolQC"]=="Fa" | data["PoolQC"]=="Gd" |data["PoolQC"]=="TA") ,]
# Fence<-data[which(data["Fence"]=="GdPvr" | data["Fence"]=="MnPrv" | data["Fence"]=="GdWo"| data["Fence"]=="MnWw") ,]
# MiscFeature<-data[which(data["MiscFeature"]=="Gar2" | data["MiscFeature"]=="Othr" | data["MiscFeature"]=="Shed" | data["MiscFeature"]=="Tenc"),]
# 

##### 1. It is quite evident that majority of the house that have been built in the early 1900s still have an alley made of Gravel and the houses made in 1970s have Pavements.
alley_group<- alley %>%
  group_by(Alley) %>%
  summarize(YearBuilt=mean(YearBuilt),.groups = 'drop')

alley_group<-data.frame(alley_group)


alley_plot<-ggplot(data=alley_group, aes(x=Alley,y=round(YearBuilt,0),fill=Alley)) +
  geom_bar(stat="identity",color='black',position=position_dodge())+
  geom_text(aes(label=round(YearBuilt,0)), vjust=-0.3, size=3.5)+
  labs(x="Alley Type  ", y= "Mean Year Built", title="Alley Type vs Year Built")+
  theme_minimal()+
  scale_fill_manual(values=c('#E69F00','#56B4E9'))

ggplotly(alley_plot)


##### 2. It can be seen that has a house that has an excellent pool has a very huge selling price. This is obvious since excellent pool quality can only be seen 
## in new houses and, new houses tend to have a high selling price too.
 
PoolQC_group<-PoolQC%>%
  group_by(PoolQC)%>%
  summarize(SalePrize=mean(SalePrice),.groups="drop")

PoolQC_group<-data.frame(PoolQC_group)

PoolQC_plot<-ggplot(data=PoolQC_group, aes(x=PoolQC,y=SalePrize,fill=PoolQC)) +
  geom_bar(stat="identity",color='black',position=position_dodge())+
  geom_text(aes(label=SalePrize), vjust=-0.3, size=3.5)+
  labs(x="Pool QUALITY  ", y= "Mean House Selling Price", title="Pool qualuty vs House Selling Estimate")+
  theme_minimal()+
  scale_fill_manual(values=c('orange','yellow','blue'))

ggplotly(PoolQC_plot)



#################### Which feature are important in determining the quality/price of the house. #############################

library(randomForest)

rand_model<-randomForest(OverallCondCat~.,data=data2,ntree=30)

#importance(rand_model)
varImpPlot(rand_model)

# We can see that properties like Overall Condition, Exterior COndition, Neighborhood, Height of the Basement play a major role in deciding the quality/price of the house.






####### References ######

#https://ademos.people.uic.edu/Chapter12.html 




################################################### END Thank You# #########################################################
