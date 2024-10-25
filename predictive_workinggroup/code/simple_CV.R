
x<-c(1,3,5,6,8,9,11,15,2,5,1, 25)



sel<-NULL
mu<-NULL
res<-NULL

for(i in 1:length(x)){
  sel<-i
  mu[i]<-mean(x[-sel])
  res[i]<-(x[-sel]-mu[i])
}

plot(mu)



mean(x)
sd(x)
hist(mu)
sd(mu)
res
hist(res)


#K fold
x<-c(1,3,5,6,8,9,11,15,2,5,1, 25)


xtest<-sample(x,size=10)


sel<-NULL
mu<-NULL
res<-NULL

for(i in 1:length(x)){
  sel<-i
  mu[i]<-mean(x[-sel])
  res[i]<-(x[-sel]-mu[i])
}
