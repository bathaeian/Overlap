library(Peacock.test)
library(mlbench)
library(caret)
###################################datasets
#---------------------DNA
data(DNA)
summary(DNA)
df1<-	data.matrix(DNA[DNA$Class=="ie",], rownames.force = NA)
df2<-data.matrix(DNA[DNA$Class=="ei",], rownames.force = NA)
df3<-data.matrix(DNA[DNA$Class=="n",], rownames.force = NA)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Class~., data=DNA, method="lvq", preProcess="scale", trControl=control)

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
max2<- max(df2[,c(rownames(ii[mainest,]))])
min2<- min(df2[,c(rownames(ii[mainest,]))])
median2 <- median(df2[,c(rownames(ii[mainest,]))])
n <- length(df1[,1])
if(n>100) {  
    min_num<-30
    } else {
    min_num<-20
    }
part_num= n/min_num
part_size= (max1-min1)/part_num

############################## overlapped parts between df1 & df2
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

############################## overlapped parts between df1 & df3
small_step=length(df1[(df1[,c(rownames(ii[mainest,]))]>median1),c(rownames(ii[mainest,]))])/(max1-median1)
res13 <- data.frame(matrix(ncol=(mainest-1),nrow=0))
colnames(res13)<- rownames(ii[1:(mainest-1),])
i<-min1
k<-1
while(i <max1){
    restemp <- numeric(length = (mainest-1))
    for (j in 1:(mainest-1))
    {
        t<- peacock2(df1[(df1[,c(rownames(ii[mainest,]))]>i)&
            (df1[,c(rownames(ii[mainest,]))]<(i+part_size)),c(rownames(ii[mainest,]),rownames(ii[j,]))],
            df3[(df3[,c(rownames(ii[mainest,]))]>i)&
            (df3[,c(rownames(ii[mainest,]))]<(i+part_size)),c(rownames(ii[mainest,]),rownames(ii[j,]))])
        if(!is.nan(t)){
                    restemp[j]=t
        }

    }
    if(length(restemp[restemp>=0.5])>=(mainest-1)){
        #print(cat("____________",i))
        res13[k,]<- restemp
        k<-k+1
        i<-i+part_size
    }
    else{
        i<-i+small_step
    }
}
    
res13

length(res13[,1])*part_size/length(df1[,1])*100
mean(model$results[,c(3)])
############################## overlapped parts between df2 & df3
small_step=length(df2[(df2[,c(rownames(ii[mainest,]))]>median2),c(rownames(ii[mainest,]))])/(max2-median2)
res23 <- data.frame(matrix(ncol=(mainest-1),nrow=0))
colnames(res23)<- rownames(ii[1:(mainest-1),])
i<- min2
k<-1
while(i<max2){
    restemp <- numeric(length = (mainest-1))
    for (j in 1:(mainest-1))
    {
        t<- peacock2(df2[(df2[,c(rownames(ii[mainest,]))]>i)&
            (df2[,c(rownames(ii[mainest,]))]<(i+part_size)),c(rownames(ii[mainest,]),rownames(ii[j,]))],
            df3[(df3[,c(rownames(ii[mainest,]))]>i)&
            (df3[,c(rownames(ii[mainest,]))]<(i+part_size)),c(rownames(ii[mainest,]),rownames(ii[j,]))])
        if(!is.nan(t)){
                    restemp[j]=t
        }

    }
    if(length(restemp[restemp>=0.5])>=(mainest-1)){
        #print(cat("____________",i))
        res23[k,]<- restemp
        k<-k+1
        i<-i+part_size
    }
    else{
        i<-i+small_step
    }
}
    
res23

length(res23[,1])*part_size/length(df2[,1])*100
mean(model$results[,c(3)])