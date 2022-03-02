## code to prepare `DATASET` goes here
library(dplyr)
set.seed(123)
m=20
t=5

b0=1
b1=b2=2
rho=(-0.5)
mu=matrix(0,m,t)
y=matrix(0,m,t)
x1=matrix(0,m,t)
x2=matrix(0,m,t)
u=matrix(0,m,t)
eps=matrix(0,m,t)
e=matrix(0,m,t)
vardi=matrix(0,m,t)
v=c()

for(i in 1:m){
  v[i]=rnorm(1,0,1)
  for(j in 1:t){
    x1[i,j]=runif(1,1,2)
    x2[i,j]=runif(1,1,3)
    eps[i,j]=rnorm(1,0,1)
    vardi[i,j]=1/rgamma(1,10,6)
    e[i,j]=rnorm(1,0,vardi[i,j])
  }
  u[i,1] <- eps[i,1]
  for(j in 2:t){
    u[i,j]=rho*u[i,j-1]+eps[i,j]
  }
  for(j in 1:t){
    mu[i,j]=b0+b1*x1[i,j]+b2*x2[i,j]+v[i]+u[i,j]
    y[i,j]=mu[i,j]+e[i,j]
  }
}

area <- c()
period <- c()
ydi <- c()
xdi1 <- c()
xdi2 <- c()
vardir <- c()
k = 0

for (i in 1:m) {
  for (j in 1:t) {
    k=k+1
    xdi1[k]<-x1[i,j]
    xdi2[k]<-x2[i,j]
    ydi[k]<-y[i,j]
    area[k] <- i
    period[k] <- j
    vardir[k] <-vardi[i,j]
  }
}

dataAr1 <- data.frame(ydi,area,period,vardir,xdi1,xdi2)
# idx = sample(1:m, 4, replace = F)
# dataAr1Ns = dataAr1
# dataAr1Ns = dataAr1Ns%>%filter(area%in%idx)
dataAr1Ns = dataAr1
dataAr1Ns[dataAr1Ns$area==5, c(1,4)]=NA
dataAr1Ns[dataAr1Ns$area==17, c(1,4)]=NA

set.seed(123)
m=20
t=5

b0=1
b1=b2=2
rho=0
mu=matrix(0,m,t)
y=matrix(0,m,t)
x1=matrix(0,m,t)
x2=matrix(0,m,t)
u=matrix(0,m,t)
eps=matrix(0,m,t)
e=matrix(0,m,t)
vardi=matrix(0,m,t)
v=c()

for(i in 1:m){
  v[i]=rnorm(1,0,1)
  for(j in 1:t){
    x1[i,j]=runif(1,1,2)
    x2[i,j]=runif(1,1,3)
    eps[i,j]=rnorm(1,0,1)
    vardi[i,j]=1/rgamma(1,10,6)
    e[i,j]=rnorm(1,0,vardi[i,j])
  }
  u[i,1] <- eps[i,1]
  for(j in 2:t){
    u[i,j]=rho*u[i,j-1]+eps[i,j]
  }
  for(j in 1:t){
    mu[i,j]=b0+b1*x1[i,j]+b2*x2[i,j]+v[i]+u[i,j]
    y[i,j]=mu[i,j]+e[i,j]
  }
}

area <- c()
period <- c()
ydi <- c()
xdi1 <- c()
xdi2 <- c()
vardir <- c()
k = 0

for (i in 1:m) {
  for (j in 1:t) {
    k=k+1
    xdi1[k]<-x1[i,j]
    xdi2[k]<-x2[i,j]
    ydi[k]<-y[i,j]
    area[k] <- i
    period[k] <- j
    vardir[k] <-vardi[i,j]
  }
}

dataPanel <- data.frame(ydi,area,period,vardir,xdi1,xdi2)
dataPanelNs = dataPanel
dataPanelNs[dataPanelNs$area==3, c(1,4)]=NA
dataPanelNs[dataPanelNs$area==12, c(1,4)]=NA

usethis::use_data(dataAr1,overwrite = TRUE)
usethis::use_data(dataAr1Ns,overwrite = TRUE)
usethis::use_data(dataPanel,overwrite = TRUE)
usethis::use_data(dataPanelNs,overwrite = TRUE)
