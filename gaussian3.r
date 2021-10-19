######################constants
n=500
part_size<-30
min_s<-20
min_p <- 0.05
part_num= n/(part_size)
overlaped <- data.frame(matrix(ncol=5,nrow=0))
######################data set 1
library(mlbench)
p <- mlbench.2dnormals(n,2,3)
df<- data.frame(p$x,p$classes)
colnames(df)<- c("X1","X2","Y")
######################data set 2
library(mlbench)
p <- mlbench.2dnormals(n,2)
df<- data.frame(p$x,p$classes)
colnames(df)<- c("X1","X2","Y")
######################data set 3
library(mlbench)
p <- mlbench.2dnormals(n,2,1)
df<- data.frame(p$x,p$classes)
colnames(df)<- c("X1","X2","Y")
######################data set 4
library(mlbench)
p <- mlbench.2dnormals(n,2,0.5)
df<- data.frame(p$x,p$classes)
colnames(df)<- c("X1","X2","Y")
######################data set 5
library(mlbench)
p <- mlbench.2dnormals(n,2,0)
df<- data.frame(p$x,p$classes)
colnames(df)<- c("X1","X2","Y")
###########################################################initializing
plot(df[,c(1,2)],pch=as.numeric(df[,c(3)]),col=df[,c(3)])
df1<-df[(df$Y)=="2",]
df2<-df[df$Y=="1",]
min11=min(df1$X1) # mapply(min,df1[,c(1)])
max11=max(df1$X1)
min21=min(df2$X1)
max21=max(df2$X1)
min12=min(df1$X2)
max12=max(df1$X2)
min22=min(df2$X2)
max22=max(df2$X2)
min1 <- max(min11,min21)
max1 <- min(max11,max21)
min2 <- max(min12,min22)
max2 <- min(max12,max22)
big_step<- max((max(max12,max22)-min(min12,min22)),(max(max11,max21)-min(min11,min21)))/sqrt(part_num)
small_step= max((max(max12,max22)-min(min12,min22)),(max(max11,max21)-min(min11,min21)))/sqrt(n)

######################
#library(Peacock.test)
library(fasano.franceschini.test)
library(dplyr)
i<- min1
while(i<max1){
    j<-min2
    while(j < max2){
        dfx<- filter(filter(df1[,c(1,2)], between(X1,i,(i+big_step))),between(X2,j,(j+big_step)) )
        dfy<- filter(filter(df2[,c(1,2)], between(X1,i,(i+big_step))),between(X2,j,(j+big_step)) )
        #t<- peacock2(dfx,dfy)
        if(nrow(dfx)>min_s && nrow(dfy)>min_s){
            t<-fasano.franceschini.test(dfx,dfy)
            if(t$p.value>=min_p){ 
                overlaped <- rbind(overlaped,c(t$p.value,i,i+big_step,j,j+big_step))
                j<-j+big_step-small_step
            }
        }
        j<- j+small_step
    }
    i<- i+small_step
}
colnames(overlaped)<- c("pvalue","x1start","x1stop","x2start","x2stop")
overlaped

for(i in 1:nrow(overlaped)){
    dft<- filter(filter(df, between(X1,overlaped[i,2],overlaped[i,3])),between(X2,overlaped[i,4],overlaped[i,5]) )
}
overlaped_df <- dft[!duplicated(dft), ]
ov_size<- nrow(overlaped_df)/(n)*100

ov_size
