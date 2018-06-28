data= read.csv(file.choose(),header = T)
data1=as.data.frame(data)
data1=data1[,c('cancel_ind', 'smartcart', 'ONTselfinstallcapable', 'premisetype',
               'strIsWinbackIndicator', 'waitingdayforcompany', 'waitingdayofcustomers',
               'ContractType', 'competitorname', 'InstallType', 'speed')]
str(data1) 


levels(data1$cancel_ind)[levels(data1$cancel_ind)=="NULL"]=0


data1[data1=='#VALUE!']=NA

data1$strIsWinbackIndicator[as.character(data1$strIsWinbackIndicator)=='NULL']='NO'
data1$strIsWinbackIndicator=factor(data1$strIsWinbackIndicator)
levels(data1$strIsWinbackIndicator)


levels(data1$ONTselfinstallcapable)
data1$ONTselfinstallcapable[as.character(data1$ONTselfinstallcapable)=='NULL']='N'
data1$ONTselfinstallcapable=factor(data1$ONTselfinstallcapable)
levels(data1$ONTselfinstallcapable)


data1[data1=='NULL']=NA
data1=na.omit(data1)

str(data1)

#CATEGORY LIMITATION FOR RANDOMFOREST


#Python does not handle factor directly. You can either encode them (turn them 
  #into ints, enforcing an order and making some splits "hard to see" for the algorithm). 
  #It is legitimate to do this if you suspect a natural order of your factors.

#Alternatively, you can "one-hot" encode them (creating dummy variables). 
  #If you create dummy variables for a factor with a large number of levels, 
  #a lot of columns will correspond to this specific value, 
  #giving it more importance (each tree will be grown learning from a several number of these columns).

#Indeed, the trees are grown on a sub-sample of the columns 
#(by default, the columns are chosen randomly with equal probability). 
#If you have two numeric predictors and a factor with 100 levels, almost 
#all your trees will not take the information about the numeric values into account.

#R on the other hand, will try every partition of the levels of the factor. 
# Having un-encoded factors usually makes it much slower.
#If you had more than 53 levels, the number of splits to try is 253 which would be irrealistically slow.
#And the number of split to consider doubles at every new level.

data1$waitingdayofcustomers=factor(data1$waitingdayofcustomers)
levels(data1$waitingdayofcustomers)
data1$waitingdayofcustomers=as.integer(data1$waitingdayofcustomers)


data1$waitingdayforcompany=factor(data1$waitingdayforcompany)
levels(data1$waitingdayforcompany)
data1$waitingdayforcompany=as.integer(data1$waitingdayforcompany)


str(data1)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(data1))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data1)), size = smp_size)

train <- data1[train_ind, ]
test <- data1[-train_ind, ]

library(Boruta)
bormodel = Boruta(train$cancel_ind ~ . , data=train)

plot(bormodel, sort=TRUE,las = 2,xlab="")



require(randomForest)
model =randomForest(train$cancel_ind ~ . , data=train)

na.omit(test)
pred <- predict(model, newdata = test)
table(pred, test$cancel_ind)

