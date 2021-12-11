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
anchor <- length(ii[,1])
#########result= glucose=2 , mass=6
##########Rescaling
max1<- max(max(df1[,2]),max(df2[,2]))
min1<- min(min(df1[,2]),min(df2[,2])
for(i in c(1,3,4,5,6,7,8)){
    max2<-max(max(df1[,i]),max(df2[,i]))
    dif1<- min(min(df1[,i]),min(df2[,i]))-min1)
    scale <- max1/(max(max(df1[,i]),max(df2[,i]))-dif1)
    df1[,i] <- (df1[,i]-dif1)*scale
    df2[,i] <- (df2[,i]-dif1)*scale
}

summary(df1)
summary(df2)
######################################initializing
min_s<-5
min_p <- 0.05
n <- length(df1[,1])
part_size<-30
#part_size<-20
part_num= n/(part_size)
overlaped <- data.frame(matrix(ncol=5,nrow=0))
big_step<- (max1-min1)/sqrt(part_num)
small_step<- (max1-min1)/(nrow(df1))
          
######################test of PimaIndiansDiabetes
i<- min1
while(i<max1){
    dt11<- filter(df1, between(glucose,i,(i+big_step)))
    dt21<- filter(df2, between(glucose,i,(i+big_step)))
    j<-min2
    while(j < max2){
        isov <- T
        dt12<- dt11[dt11[,x]<(j+big_step) & dt11[,x]>j,c(2,x)]
        dt22<- dt21[dt21[,x]<(j+big_step) & dt21[,x]>j,c(2,x)]
        for(x in c(1,3,4,5,6,7,8)){
            #dt12<- filter(dt11[,c(2,x)],between(.[[x]],j,(j+big_step)))
            #dt22<- filter(dt21[,c(2,x)],between(.[[x]],j,(j+big_step)))
            if(nrow(dt11)>min_s && nrow(dt12)>min_s){
                t<-fasano.franceschini.test(dt12,dt22)
                if(t$p.value<min_p){isov<-F}
            }else{isov<-F}
        }
        if(isov){
            overlaped <- rbind(overlaped,c(t$p.value,i,i+big_step,j,j+big_step))
            j<-j+big_step-small_step
        } 
        j<- j+small_step
    }
    i<- i+small_step
}
colnames(overlaped)<- c("pvalue","x1start","x1stop","x2start","x2stop")

overlaped <- within(overlaped, x1start <- (x1start*scale))
overlaped <- within(overlaped, x1stop <- (x1stop*scale))
overlaped
for(i in 1:nrow(overlaped)){
    dft<- filter(filter(PimaIndiansDiabetes, between(glucose,overlaped[i,2],overlaped[i,3])),
            between(mass,overlaped[i,4],overlaped[i,5]) )
}
overlaped_df <- dft[!duplicated(dft), ]
ov_size<- nrow(overlaped_df)/n*100
baddata=(nrow(PimaIndiansDiabetes[PimaIndiansDiabetes$glucose==0,])+nrow(PimaIndiansDiabetes[PimaIndiansDiabetes$mass==0,]))/nrow(PimaIndiansDiabetes)*100
baddata=(nrow(PimaIndiansDiabetes[c(2)==0||c(6)==0,]))/nrow(PimaIndiansDiabetes)*100
baddata <- nrow(subset(PimaIndiansDiabetes, glucose==0 | mass==0 | 
                       pressure==0 | triceps==0 | insulin==0 ,))/
           nrow(PimaIndiansDiabetes)*100

#################Results
ov_size=20.52
baddata=56.25
classification_error=26.34