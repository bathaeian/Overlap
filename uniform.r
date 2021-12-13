##################constants
min1=50
max1=100
min2=50#40 #30#20#10#0 #
max2=100#90 #80#70#60#50 #
n=100
min_p=0.05

######################building data sets
df1<-data.frame(xs=runif(n, min = min1, max = max1),ys=as.factor(0))
df2<-data.frame(xs=runif(n, min = min2, max =max2),ys=as.factor(1))
plot(0:n, 0:n, type = "n")# setting up coord. system
points(as.numeric(rownames(df1)),df1[,1], col = "red")
points(as.numeric(rownames(df2)),df2[,1], col = "blue",  pch = "+")
######################
library(dgof)
part_size<-30
part_num= n/part_size
big_step= (max1-min1)/part_num
small_step=1
res30 <- numeric(length = part_num)
i<-min1
j<-1
while(i <max1){
    t<- ks.test(df1[(df1$xs>i)&(df1$xs<(i+big_step)),1],df2[(df2$xs>i)&(df2$xs<(i+big_step)),1])
    if(t$p.value>=min_p){
        res30[j]<- t$p.value
        j<-j+1
        i<-i+big_step
    }
    else{
        i<-i+small_step
    }
}
    
res30
rate30 <- (j-1)/part_num*100
#################################################3
part_size<-20
part_num= n/part_size
big_step= (max1-min1)/part_num
small_step=1
res20 <- numeric(length = part_num)
i<-min1
j<-1
while(i <max1){
    t<- ks.test(df1[(df1$xs>i)&(df1$xs<(i+big_step)),1],df2[(df2$xs>i)&(df2$xs<(i+big_step)),1])
    if(t$p.value>=min_p){
        res20[j]<- t$p.value
        j<-j+1
        i<-i+big_step
    }
    else{
        i<-i+small_step
    }
}
    
res20
rate20 <- (j-1)/part_num*100