library(mlbench)
library(caret)
library(fasano.franceschini.test)
library(dplyr)
###################################datasets
#---------------------PimaIndiansDiabetes
data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)
df1<-PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes=="pos",]
df2<-PimaIndiansDiabetes[PimaIndiansDiabetes$diabetes=="neg",]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
##########################estimate most important variable=mainest
# estimate variable importance
importance <- varImp(model, scale=FALSE)
ii<- importance$importance[order(importance$importance[,1]),]
anchor <- grep((rownames(ii)[nrow(ii)]), colnames(df1))

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
#############################
baddata=(nrow(PimaIndiansDiabetes[PimaIndiansDiabetes$glucose==0,])+nrow(PimaIndiansDiabetes[PimaIndiansDiabetes$mass==0,]))/nrow(PimaIndiansDiabetes)*100
baddata=(nrow(PimaIndiansDiabetes[c(2)==0||c(6)==0,]))/nrow(PimaIndiansDiabetes)*100
baddata <- nrow(subset(PimaIndiansDiabetes, glucose==0 | mass==0 | 
                       pressure==0 | triceps==0 | insulin==0 ,))/
           nrow(PimaIndiansDiabetes)*100

#################Results
baddata=56.25
classification_error=26.34
[1] "----------"
[1] 20
      pvalue   x1start   x1stop      size
1 0.05278257  77.91791  92.6194  2.985075
2 0.09522761 137.45896 152.1604 15.298507
3 0.08840380 152.16045 166.8619 11.567164
4 0.11723820 168.33209 183.0336 12.686567
[1] 42.53731
[1] "==============="
[1] "----------"
[1] 30
      pvalue   x1start   x1stop      size
1 0.05760857  73.50746  95.5597  4.477612
2 0.06040247 135.25373 157.3060 20.522388
3 0.13303832 157.30597 179.3582 19.402985
4 0.29015737 179.35821 197.0000 14.552239
[1] 57.08955
[1] "==============="
[1] "----------"
[1] 40
      pvalue   x1start    x1stop      size
1 0.08059787  66.89179  96.29478  4.477612
2 0.07894714 154.36567 183.76866 26.492537
[1] 30.97015
[1] "==============="
[1] "----------"
[1] 50
      pvalue   x1start    x1stop      size
1 0.07412774  59.54104  96.29478  4.477612
2 0.07390350 154.36567 191.11940 31.716418
[1] 36.19403
[1] "==============="
[1] "----------"
[1] 60
     pvalue  x1start    x1stop      size
1 0.0691453  52.1903  96.29478  4.477612
2 0.1188382 154.3657 197.00000 36.194030
[1] 39.1791
[1] "==============="
[1] "----------"
[1] 70
     pvalue   x1start    x1stop      size
1 0.0691453  44.83955  96.29478  4.477612
2 0.1217974 154.36567 197.00000 36.567164
[1] 39.1791
[1] "==============="
[1] "----------"
[1] 80
      pvalue   x1start    x1stop      size
1 0.06614659  37.48881  96.29478  4.477612
2 0.12179738 154.36567 197.00000 36.567164
[1] 39.1791
[1] "==============="
[1] "----------"
[1] 90
      pvalue   x1start    x1stop      size
1 0.06614659  30.13806  96.29478  4.477612
2 0.12179738 154.36567 197.00000 36.567164
[1] 39.1791
[1] "==============="
[1] "----------"
[1] 100
      pvalue   x1start    x1stop      size
1 0.06614659  22.78731  96.29478  4.477612
2 0.12179738 154.36567 197.00000 36.567164
[1] 39.1791
[1] "==============="
[1] "----------"
[1] 110
      pvalue   x1start    x1stop      size
1 0.06614659  15.43657  96.29478  4.477612
2 0.12179738 154.36567 197.00000 36.567164
[1] 39.1791
[1] "==============="
[1] "----------"
[1] 120
      pvalue    x1start    x1stop      size
1 0.06614659   8.085821  96.29478  4.477612
2 0.12179738 154.365672 197.00000 36.567164
[1] 39.1791
[1] "==============="

