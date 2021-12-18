library(mlbench)
library(caret)
library(fasano.franceschini.test)
library(dplyr)
###################################datasets
#---------------------Sonar
data(Sonar)
summary(Sonar)
df1<-Sonar[Sonar$Class=="R",]
df2<-Sonar[Sonar$Class=="M",]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Class~., data=Sonar, method="lvq", preProcess="scale", trControl=control)
##########################estimate most important variable=mainest
# estimate variable importance
importance <- varImp(model, scale=FALSE)
ii<- importance$importance[order(importance$importance[,1]),]
anchor <- grep((rownames(ii)[nrow(ii)]), colnames(df1))

#########result= 
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
#######################results
classification_error=16%
overlaped=0