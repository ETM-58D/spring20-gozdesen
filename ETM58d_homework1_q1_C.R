#10d data, 1000 observations, each dimension is generated below:
set.seed(1)
d1=runif(1000,min=-1,max=1)
d2=runif(1000,min=-1,max=1)
d3=runif(1000,min=-1,max=1)
d4=runif(1000,min=-1,max=1)
d5=runif(1000,min=-1,max=1)
d6=runif(1000,min=-1,max=1)
d7=runif(1000,min=-1,max=1)
d8=runif(1000,min=-1,max=1)
d9=runif(1000,min=-1,max=1)
d10=runif(1000,min=-1,max=1)


df1=data.frame(d1)
df2=data.frame(d1,d2)
df3=data.frame(d1,d2,d3)
df4=data.frame(d1,d2,d3,d4)
df5=data.frame(d1,d2,d3,d4,d5)
df6=data.frame(d1,d2,d3,d4,d5,d6)
df7=data.frame(d1,d2,d3,d4,d5,d6,d7)
df8=data.frame(d1,d2,d3,d4,d5,d6,d7,d8)
df9=data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9)
df10=data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)


#10d data, 100 new points, each dimension is generated below:
set.seed(2)
nd1=runif(100,min=-1,max=1)
nd2=runif(100,min=-1,max=1)
nd3=runif(100,min=-1,max=1)
nd4=runif(100,min=-1,max=1)
nd5=runif(100,min=-1,max=1)
nd6=runif(100,min=-1,max=1)
nd7=runif(100,min=-1,max=1)
nd8=runif(100,min=-1,max=1)
nd9=runif(100,min=-1,max=1)
nd10=runif(100,min=-1,max=1)

#they should be in matrix form
m1=as.matrix(nd1)
m2=matrix(nd1,nd2,nrow=100,ncol=2)
m3=cbind(m2, nd3)
m4=cbind(m3, nd4)
m5=cbind(m4, nd5)
m6=cbind(m5, nd6)
m7=cbind(m6, nd7)
m8=cbind(m7, nd8)
m9=cbind(m8, nd9)
m10=cbind(m9, nd10)

#for distance calculation, proxy package will be used 
install.packages('proxy')
library(proxy)

distance1 = proxy::dist(m1,df1)
distance2 = proxy::dist(m2,df2)
distance3 = proxy::dist(m3,df3)
distance4 = proxy::dist(m4,df4)
distance5 = proxy::dist(m5,df5)
distance6 = proxy::dist(m6,df6)
distance7 = proxy::dist(m7,df7)
distance8 = proxy::dist(m8,df8)
distance9 = proxy::dist(m9,df9)
distance10 = proxy::dist(m10,df10)

#distance matrixes are giving 100 x 1000 values. 
#need to find the smallest distance in each row. 
min_dm1 = t(sapply(seq(nrow(distance1)), function(i) {
    j <- which.min(distance1[i,])
    c(i, distance1[i,j])
  }))
min_dm2 = t(sapply(seq(nrow(distance2)), function(i) {
  j <- which.min(distance2[i,])
  c(i, distance2[i,j])
}))
min_dm3 = t(sapply(seq(nrow(distance3)), function(i) {
  j <- which.min(distance3[i,])
  c(i, distance3[i,j])
}))
min_dm4 = t(sapply(seq(nrow(distance4)), function(i) {
  j <- which.min(distance4[i,])
  c(i, distance4[i,j])
}))
min_dm5 = t(sapply(seq(nrow(distance5)), function(i) {
  j <- which.min(distance5[i,])
  c(i, distance5[i,j])
}))
min_dm6 = t(sapply(seq(nrow(distance6)), function(i) {
  j <- which.min(distance6[i,])
  c(i, distance6[i,j])
}))
min_dm7 = t(sapply(seq(nrow(distance7)), function(i) {
  j <- which.min(distance7[i,])
  c(i, distance7[i,j])
}))
min_dm8 = t(sapply(seq(nrow(distance8)), function(i) {
  j <- which.min(distance8[i,])
  c(i, distance8[i,j])
}))
min_dm9 = t(sapply(seq(nrow(distance9)), function(i) {
  j <- which.min(distance9[i,])
  c(i, distance9[i,j])
}))
min_dm10 = t(sapply(seq(nrow(distance10)), function(i) {
  j <- which.min(distance10[i,])
  c(i, distance10[i,j])
}))

#average smallest distances 
ave1 = mean(min_dm1[,2])
ave2 = mean(min_dm2[,2])
ave3 = mean(min_dm3[,2])
ave4 = mean(min_dm4[,2])
ave5 = mean(min_dm5[,2])
ave6 = mean(min_dm6[,2])
ave7 = mean(min_dm7[,2])
ave8 = mean(min_dm8[,2])
ave9 = mean(min_dm9[,2])
ave10 = mean(min_dm10[,2])

ave = c(ave1, ave2, ave3, ave4, ave5, ave6, ave7, ave8, ave9, ave10)

plot(ave)

#increasing averages prove the curse of dimensionality. 
#when the number of dimensions increase, we see that the average smallest distance is increasing also. 
#even if all points are randomly distributed, increasing dimensionality causes more distant points. 
#which makes it harder to find a trend or a similarity.
