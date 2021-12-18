#######Vehicle
library(mlbench)
library(caret)
library(fasano.franceschini.test)
###################################datasets
#---------------------Vehicle
data(Vehicle)
summary(Vehicle)
control <- trainControl(method="repeatedcv", number=5, repeats=1)
model <- train(Class~., data=Vehicle, method="lvq", preProcess="scale", trControl=control)
############classification error= 34%
##########################estimate most important variable=mainest
# estimate variable importance
importance <- varImp(model, scale=FALSE)
ii<- importance$importance[order(importance$importance[,1]),]
anchor <- grep((rownames(ii)[nrow(ii)]), colnames(Vehicle))[1]
##########anchor Max.L.Ra--------6
#################initializing
min_s<-5
min_p <- 0.05
#########################function findOV
findOV <- function(part_size) {
    print("in function")
    print(min1)
    print(max1)
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
#############################
############################test  
df1<-Vehicle[Vehicle$Class=="opel",]
df2<-Vehicle[Vehicle$Class=="bus",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
#######
[1] "----------"
[1] 20
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
[1] "----------"
[1] 50
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="



####################################
df1<-Vehicle[Vehicle$Class=="saab",]
df2<-Vehicle[Vehicle$Class=="bus",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
#####333
[1] "----------"
[1] 20
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
[1] "----------"
[1] 30
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
[1] "----------"
[1] 40
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="

####################################199
df1<-Vehicle[Vehicle$Class=="van",]
df2<-Vehicle[Vehicle$Class=="bus",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
###3
[1] "----------"
[1] 20
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
[1] "----------"
[1] 30
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
[1] "----------"
[1] 40
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
####################################212
df1<-Vehicle[Vehicle$Class=="opel",]
df2<-Vehicle[Vehicle$Class=="saab",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
######
[1] "----------"
[1] 20
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
[1] "----------"
[1] 30
     pvalue  x1start   x1stop      size
1 0.3252565 3.764151 5.037736  4.245283
2 0.7247856 5.759434 7.033019 22.641509
3 0.3023726 7.754717 9.028302 28.301887
[1] 55.18868
[1] "==============="
[1] "----------"
[1] 40
     pvalue  x1start   x1stop      size
1 0.3252565 3.339623 5.037736  4.245283
2 0.7247856 5.334906 7.033019 22.641509
3 0.3023726 7.330189 9.028302 28.301887
[1] 55.18868
[1] "==============="

##########################test
df1<-Vehicle[Vehicle$Class=="van",]
df2<-Vehicle[Vehicle$Class=="saab",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
###3333
[1] "----------"
[1] 20
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
[1] "----------"
[1] 30
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
################################
df1<-Vehicle[Vehicle$Class=="van",]
df2<-Vehicle[Vehicle$Class=="opel",]
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))
n <- length(df1[,1])
########
[1] "----------"
[1] 20
[1] pvalue  x1start x1stop  size   
<0 rows> (or 0-length row.names)
[1] 100
[1] "==============="
[1] "----------"
[1] 30
     pvalue  x1start  x1stop     size
1 0.1312973 3.502513 5.01005 6.030151
[1] 6.030151
[1] "==============="
[1] "----------"
[1] 40
     pvalue x1start  x1stop     size
1 0.1312973       3 5.01005 6.030151
[1] 6.030151
[1] "==============="
[1] "----------"
[1] 50
     pvalue x1start   x1stop     size
1 0.1312973       3 5.512563 6.030151
[1] 6.030151
[1] "==============="