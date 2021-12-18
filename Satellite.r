#######Satellite
library(mlbench)
library(caret)
library(fasano.franceschini.test)
library(dplyr)
###################################datasets
#---------------------Satellite
data(Satellite)
summary(Satellite)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(classes~., data=Satellite, method="lvq", preProcess="scale", trControl=control)
##########################estimate most important variable=mainest
# estimate variable importance
importance <- varImp(model, scale=FALSE)
ii<- importance$importance[order(importance$importance[,1]),]
anchor <- grep((rownames(ii)[nrow(ii)]), colnames(Satellite))[1]
#################initializing
min_s<-5
min_p <- 0.05
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
############################test
df1<-Satellite[Satellite$classes=="damp grey soil",]
df2<-Satellite[Satellite$classes=="grey soil",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
############################test
[1] "----------"
[1] 30
     pvalue  x1start   x1stop     size
1 0.4772389 84.97125 86.02556 2.396166
[1] 2.396166
[1] "==============="
[1] "----------"
[1] 50
     pvalue  x1start   x1stop     size
1 0.4772389 84.26837 86.02556 2.396166
[1] 2.396166
[1] "==============="
[1] 100
     pvalue  x1start   x1stop     size
1 0.3042088 84.02236 87.53674 3.514377
2 0.0964017 88.52077 92.00000 1.277955
[1] 4.313099

####################################
df1<-Satellite[Satellite$classes=="damp grey soil",]
df2<-Satellite[Satellite$classes=="very damp grey soil",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
[1] "----------"
[1] 20
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="

[1] "----------"
[1] 30
      pvalue  x1start   x1stop     size
1 0.13250592 78.81470 80.01278 16.13419
2 0.05751396 82.80831 84.00639 11.98083
[1] 28.11502
[1] "==============="

[1] "----------"
[1] 50
      pvalue  x1start   x1stop     size
1 0.13250592 78.01597 80.01278 16.13419
2 0.05751396 82.00958 84.00639 11.98083
[1] 28.11502
[1] "==============="
 "----------"
[1] 100
      pvalue  x1start  x1stop     size
1 0.09961161 83.00799 87.0016 12.61981
[1] 12.61981
[1] "==============="
classification_error=14%
####################################
df1<-Satellite[Satellite$classes=="grey soil",]
df2<-Satellite[Satellite$classes=="very damp grey soil",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
[1] "----------"
[1] 100
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="

####################################
df1<-Satellite[Satellite$classes=="red soil",]
df2<-Satellite[Satellite$classes=="cotton crop",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
 "----------"
[1] 100
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
####################################
df1<-Satellite[Satellite$classes=="vegetation stubble",]
df2<-Satellite[Satellite$classes=="cotton crop",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
 "----------"
[1] 100
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="

