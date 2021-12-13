library(mlbench)
library(caret)
library(fasano.franceschini.test)
library(dplyr)
###################################datasets
#---------------------Ionosphere
data(Ionosphere)
summary(Ionosphere)
Ionosphere2<- Ionosphere[,-c(1,2,35)]*10
Ionosphere2<-cbind(Ionosphere2,Class=Ionosphere[,c(35)])
###df1<-Ionosphere[Ionosphere$Class=="bad",-c(1,2)]
#####df2<-Ionosphere[Ionosphere$Class=="good",-c(1,2)]
newIonosphere<-Ionosphere[,-c(1,2)]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
##model <- train(Class~., data=newIonosphere, method="lvq", preProcess="scale", trControl=control)
model <- train(Class~., data=Ionosphere2, method="lvq", preProcess="scale", trControl=control)
df1<-Ionosphere2[Ionosphere2$Class=="bad",]
df2<-Ionosphere2[Ionosphere2$Class=="good",]

##########################estimate most important variable=mainest
# estimate variable importance
importance <- varImp(model, scale=FALSE)
ii<- importance$importance[order(importance$importance[,1]),]
anchor <- grep(paste("^",(rownames(ii)[nrow(ii)]),"$", sep=""), colnames(df1))

#########result= V3=1 
max1<- min(max(df1[,anchor]),max(df2[,anchor]))
min1<- max(min(df1[,anchor]),min(df2[,anchor]))

######################################initializing
min_s<-5
min_p <- 0.05
n <- length(df1[,1])
part_size<-30
#part_size<-20
part_num= n/(part_size)
overlaped <- data.frame(matrix(ncol=3,nrow=0))
big_step<- (max1-min1)/(part_num)
small_step<- (max1-min1)/(nrow(df1))
          
######################test of Ionospher
i<- min1
while(i<max1){
    dt1<- df1[df1[,anchor]<(i+big_step) & df1[,anchor]>i,]
    dt2<- df2[df2[,anchor]<(i+big_step) & df2[,anchor]>i,]
    isov <- T
    for(x in c(2:32)){      
        if(nrow(dt1)>min_s & nrow(dt2)>min_s){
                t<-fasano.franceschini.test(dt1[,c(anchor,x)],
                          dt2[,c(anchor,x)])
                if( t$p.value<min_p){isov<-F}#!is.nan(t) &
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


#########################################33Results
classification_error=14
overlaped=0
##########################part_size=20 , alpha=0.05
     pvalue  x1start   x1stop
1 0.2548629 5.398057 6.676375
2 0.1890337 6.676375 7.954692
3 0.3925507 5.525889 6.804206
ov_size=15.07937
##########################part_size=30 , alpha=0.05
     pvalue  x1start   x1stop
1 0.5334233 5.014562 6.932038
