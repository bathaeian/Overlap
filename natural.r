library(Peacock.test)
library(mlbench)
library(caret)
###################################dataset
data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)

df1<-PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes=="pos",]
df2<-PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes=="neg",]
#############################analysis
##########################estimate most important variable=mainest
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
ii<- importance$importance[order(importance$importance[,1]),]
mainest <- length(ii[,1])
######################################partitioning factors
max1<- max(df1[,c(rownames(ii[mainest,]))])
min1<- min(df1[,c(rownames(ii[mainest,]))])
n <- length(df1[,1])
if(n>100) {  
    min_num<-30
    } else {
    min_num<-20
    }
part_num= n/min_num
part_size= (max1-min1)/part_num
#################################comparision of probability distributions
res <- data.frame(matrix(ncol=mainest,nrow=0))
colnames(res)<- rownames(ii)
for (i in 1:part_num)
{
    for (j in 1:(mainest-1))
    {
        t<- peacock2(df1[(df1[,c(rownames(ii[mainest,]))]>min1+(i-1)*part_size)&
            (df1[,c(rownames(ii[mainest,]))]<(min1+i*part_size)),c(rownames(ii[mainest,]),rownames(ii[j,]))],
            df2[(df2[,c(rownames(ii[mainest,]))]>min1+(i-1)*part_size)&
            (df2[,c(rownames(ii[mainest,]))]<(min1+i*part_size)),c(rownames(ii[mainest,]),rownames(ii[j,]))])
        res[i,j]<- t
    }
}
i=i+1
for (j in 1:(mainest-1))
    {
        t<- peacock2(df1[(df1[,c(rownames(ii[mainest,]))]>min1+(i-1)*part_size),
                            c(rownames(ii[mainest,]),rownames(ii[j,]))],
                     df2[(df2[,c(rownames(ii[mainest,]))]>min1+(i-1)*part_size),
                          c(rownames(ii[mainest,]),rownames(ii[j,]))])
        res[i,j]<- t
    }

