library(Peacock.test)
library(mlbench)
library(caret)
###################################datasets
#---------------------PimaIndiansDiabetes
data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)
df1<-PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes=="pos",]
df2<-PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes=="neg",]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
#---------------------BreastCancer---------error
data(BreastCancer)
summary(BreastCancer)
df1<-BreastCancer[BreastCancer$Class=="malignant",-1]
df2<-BreastCancer[BreastCancer$Class=="benign",-1]
newBreastCancer<-BreastCancer[,-1]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Class~., data=newBreastCancer, method="lvq", preProcess="scale", trControl=control)

#############################analysis
##########################estimate most important variable=mainest
# estimate variable importance
importance <- varImp(model, scale=FALSE)
ii<- importance$importance[order(importance$importance[,1]),]
mainest <- length(ii[,1])
######################################partitioning factors
max1<- max(df1[,c(rownames(ii[mainest,]))])
min1<- min(df1[,c(rownames(ii[mainest,]))])
median1 <- median(df1[,c(rownames(ii[mainest,]))])
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

############################## overlapped parts
small_step=length(df1[(df1[,c(rownames(ii[mainest,]))]>median1),c(rownames(ii[mainest,]))])/(max1-median1)
res2 <- data.frame(matrix(ncol=(mainest-1),nrow=0))
colnames(res2)<- rownames(ii[1:(mainest-1),])
i<-min1
k<-1
while(i <max1){
    restemp <- numeric(length = (mainest-1))
    for (j in 1:(mainest-1))
    {
        t<- peacock2(df1[(df1[,c(rownames(ii[mainest,]))]>i)&
            (df1[,c(rownames(ii[mainest,]))]<(i+part_size)),c(rownames(ii[mainest,]),rownames(ii[j,]))],
            df2[(df2[,c(rownames(ii[mainest,]))]>i)&
            (df2[,c(rownames(ii[mainest,]))]<(i+part_size)),c(rownames(ii[mainest,]),rownames(ii[j,]))])
        if(!is.nan(t)){
                    restemp[j]=t
        }

    }
    if(length(restemp[restemp>=0.5])>=(mainest-1)){
        #print(cat("____________",i))
        res2[k,]<- restemp
        k<-k+1
        i<-i+part_size
    }
    else{
        i<-i+small_step
    }
}
    
res2

length(res2[,1])*part_size/length(df1[,1])*100
mean(model$results[,c(3)])

