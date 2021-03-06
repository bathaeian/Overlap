library(mlbench)
library(caret)
library(fasano.franceschini.test)
library(dplyr)
###################################datasets
#---------------------Ionosphere
data(Ionosphere)
summary(Ionosphere)
Ionosphere <- Ionosphere[,-c(1,2)]
df1<-Ionosphere[Ionosphere$Class=="bad",]
df2<-Ionosphere[Ionosphere$Class=="good",]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Class~., data=Ionosphere, method="lvq", preProcess="scale", trControl=control)
##########################estimate most important variable=mainest
# estimate variable importance
importance <- varImp(model, scale=FALSE)
ii<- importance$importance[order(importance$importance[,1]),]
anchor <- grep((rownames(ii)[nrow(ii)]), colnames(df1))[1]

#########result= glucose=2 
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))

######################################initializing
min_s<-5
min_p <- 0.05
n <- length(df1[,1])
#########################function findOV
findOV <- function(part_size) {
    part_num= n/(part_size)
    overlaped <- data.frame(matrix(ncol=4,nrow=0))
    big_step<- (max1-min1)/(part_num)
    small_step<- (max1-min1)/(nrow(df1)) 
    nc<- ncol(df1)-1     
    i<- min1
    while(i<max1){
        dt1<- df1[df1[,anchor]<(i+big_step) & df1[,anchor]>i,]
        dt2<- df2[df2[,anchor]<(i+big_step) & df2[,anchor]>i,]
        isov <- T
        tpv<-1
        tryCatch(
            for(x in (1:nc)){      
                if(nrow(dt1)>min_s & nrow(dt2)>min_s){
                    t<-fasano.franceschini.test(dt1[,c(anchor,x)],dt2[,c(anchor,x)])
                    if(is.na(t$p.value)) {isov<-F}
                    else if(t$p.value<min_p){isov<-F}
                        else{tpv<-min(tpv,t$p.value)}
                }else{isov<-F}
            },
            error = function(e){
                print("bye")
                print(i)} 
            )
        if(isov & tpv<1){
            i2<-i+big_step
            n1<-nrow(df1[df1[,anchor]>i & df1[,anchor]<(i2),])/n*100
            overlaped<- rbind(overlaped,c(tpv,i,min(i2,max1),n1))
            i<-i2
        } else{ i<- i+small_step}
    }
    colnames(overlaped)<- c("pvalue","x1start","x1stop","size")
    print("----------");
    print(part_size);
    print(overlaped)
    ov<-0
    for(j in 1:nrow(overlaped)){
        ov=ov+nrow(df1[df1[,anchor]>overlaped[j,2] & df1[,anchor]<overlaped[j,3],])
    }
    ov_size=ov/n*100
    print(ov_size)
    print("===============");
}
#########################################33Results
classification_error=14
overlaped=0
[1] "----------"
[1] 20
      pvalue   x1start    x1stop       size
1 0.05397916 0.5525889 0.6804206 0.04761905
[1] 4.761905
[1] "==============="
[1] "----------"
[1] 30
      pvalue   x1start    x1stop       size
1 0.05090106 0.5014562 0.6932038 0.07142857
[1] 7.142857
[1] "==============="
[1] "----------"
[1] 40
      pvalue   x1start    x1stop      size
1 0.06511239 0.5014562 0.7571197 0.1031746
[1] 10.31746
[1] "==============="
[1] "----------"
[1] 50
      pvalue   x1start    x1stop      size
1 0.05779691 0.4311487 0.7507281 0.1111111
[1] 11.11111
[1] "==============="
[1] "----------"
[1] 60
     pvalue   x1start   x1stop      size
1 0.0506901 0.4311487 0.814644 0.1190476
[1] 11.90476
[1] "==============="
[1] "----------"
[1] 70
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="