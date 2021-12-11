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
#part_size<-30
part_size<-20
part_num= n/(part_size)
overlaped <- data.frame(matrix(ncol=3,nrow=0))
big_step<- (max1-min1)/(part_num)
small_step<- (max1-min1)/(nrow(df1))
          
######################test of PimaIndiansDiabetes
i<- min1
while(i<max1){
    dt1<- df1[df1[,anchor]<(i+big_step) & df1[,anchor]>i,]
    dt2<- df2[df2[,anchor]<(i+big_step) & df2[,anchor]>i,]
    isov <- T
    for(x in c(1,3,4,5,6,7,8)){      
        if(nrow(dt1)>min_s & nrow(dt2)>min_s){
                t<-fasano.franceschini.test(dt1[,c(anchor,x)],dt2[,c(anchor,x)])
                if(t$p.value<min_p){isov<-F}
            }else{isov<-F}
        }
    if(isov){
            overlaped <- rbind(overlaped,c(t$p.value,i,i+big_step))
            i<-i+big_step
    } else{i<- i+small_step}
    
}
colnames(overlaped)<- c("pvalue","x1start","x1stop")

overlaped
dft <- data.frame(matrix(ncol=nrow(ii)+1,nrow=0))
for(i in 1:nrow(overlaped)){
    dft<-rbind(dft,df1[df1[,anchor]>overlaped[i,2] & df1[,anchor]<overlaped[i,3],])
}
overlaped_df <- dft[!duplicated(dft), ]
ov_size<- nrow(overlaped_df)/n*100

ov2<-0
for(i in 1:nrow(overlaped)){
    ov2=ov2+nrow(df1[df1[,anchor]>overlaped[i,2] & df1[,anchor]<overlaped[i,3],])
}
baddata=(nrow(PimaIndiansDiabetes[PimaIndiansDiabetes$glucose==0,])+nrow(PimaIndiansDiabetes[PimaIndiansDiabetes$mass==0,]))/nrow(PimaIndiansDiabetes)*100
baddata=(nrow(PimaIndiansDiabetes[c(2)==0||c(6)==0,]))/nrow(PimaIndiansDiabetes)*100
baddata <- nrow(subset(PimaIndiansDiabetes, glucose==0 | mass==0 | 
                       pressure==0 | triceps==0 | insulin==0 ,))/
           nrow(PimaIndiansDiabetes)*100

#################Results
baddata=56.25
classification_error=26.34
###########part_size=30 , alpha=0.05
ov_size=58.95
      pvalue   x1start   x1stop
1 0.09821325  73.50746  95.5597
2 0.11179817 135.25373 157.3060
3 0.21275554 157.30597 179.3582
4 0.53348207 179.35821 201.4104
###########part_size=30 , alpha=0.5
ov_size=14.55
#############part_size=20  , alpha=0.05
      pvalue   x1start   x1stop
1 0.05278257  77.91791  92.6194
2 0.26145503 137.45896 152.1604
3 0.16350123 152.16045 166.8619
4 0.59078945 168.33209 183.0336
ov_size=42.53731
#############part_size=20  , alpha=0.5
ov_size=12.68
