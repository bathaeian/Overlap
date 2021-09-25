##################constants
min1=50
max1=100
min2=50#40#30#20#10#0
max2=100#90#80#70#60#50
n=100
if(n>100) {  
    min_num<-30
    } else {
    min_num<-20
    }
part_num= n/min_num
part_size= (max1-min1)/part_num
######################building data sets
df1<-data.frame(xs=runif(n, min = min1, max = max1),ys=as.factor(0))
df2<-data.frame(xs=runif(n, min = min2, max =max2),ys=as.factor(1))
plot(0:n, 0:n, type = "n")# setting up coord. system
points(as.numeric(rownames(df1)),df1[,1], col = "red")
points(as.numeric(rownames(df2)),df2[,1], col = "blue",  pch = "+")
######################
library(dgof)
#sorted1<- df1[order(df1$xs),1]
#sorted2<- df2[order(df2$xs),1]
res <- numeric(length = part_num)
for (i in 1:part_num){
    t<- ks.test(df1[(df1$xs>min1+(i-1)*part_size)&(df1$xs<(min1+i*part_size)),1],df2[(df2$xs>min1+(i-1)*part_size)&(df2$xs<(min1+i*part_size)),1])
    t
    res[i]<- t$p.value
}
res
############################## overlapped parts
small_step=1
res2 <- numeric(length = part_num)
i<-min1
j<-1
while(i <max1){
    t<- ks.test(df1[(df1$xs>i)&(df1$xs<(i+part_size)),1],df2[(df2$xs>i)&(df2$xs<(i+part_size)),1])
    if(t$p.value>=0.5){
        res2[j]<- t$p.value
        j<-j+1
        i<-i+part_size
    }
    else{
        i<-i+1
    }
    
res2