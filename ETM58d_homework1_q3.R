setwd("/Users/gozdesen/Desktop/ETM/2/BusinessAnalytics")
install.packages('data.table')
library(data.table)
require(data.table)

#read the data file
data=fread('ETM58D_Spring20_HW1_q3_netflix_data.csv')
data[is.na(data)]=0
str(data)

#since users cannot rate as zero, zero values assumed as not rated. 
data[data == 0] = NA
str(data)

#for replacing nonavailable values with a good substution, i will use mice function
install.packages('mice')
library(mice)
data_imputed=complete(mice(data,m=1))

#for the ease of calculation sample is lowered to 1000 observations. 
bin=sample(1:10000,1000)
data_bin=data_imputed[bin,]

#in order to use mds, we need a distance matrix.
#hence, we make a squared, euclidean matrix which consists of users' distances. 
distance = as.matrix(stats::dist(data.bin, method = "euclidean"))

#mds 
mds = cmdscale(distance, 2)
plot(mds)

#plot seems like an eliptic shape, centered around 0. but we cannot see a trend. 
#users might give similar ratings to the movies. 