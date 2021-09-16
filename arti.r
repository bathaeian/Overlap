##################constants
min1=50
max1=100
min2=10#0
max2=60#50
n=100
######################building data sets
df1<-data.frame(xs=runif(n, min = min1, max = max1),ys=as.factor(0))
df2<-data.frame(xs=runif(n, min = min2, max =max2),ys=as.factor(1))
plot(0:n, 0:n, type = "n")# setting up coord. system
points(as.numeric(rownames(df1)),df1[,1], col = "red")
points(as.numeric(rownames(df2)),df2[,1], col = "blue",  pch = "+")
######################
library(dgof)
range=max1-min1
sorted1<- df1[order(df1$xs),1]
sorted2<- df2[order(df2$xs),1]

maxp=0
t<- ks.test(sorted1,sorted2)
t$p.value
res <- numeric(length = length(1:10))
for (part in 1:10) { 
    t<- ks.test(df1[df1$xs>min1&(df1$xs<(min1+part)),1],df2[df2$xs>min1&(df2$xs<(min1+part)),1])
    res[part]<- t$p.value
}
res