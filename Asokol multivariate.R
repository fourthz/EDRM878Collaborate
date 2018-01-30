#trying to upload file
library(MASS)#randomly generate multivariate data
library(rrcov)#performs wilks.test

Cov <- matrix(c(1,0.3,0.3,1),2,2)

#start loop

Wilks.Lamb.vec=NULL
Wilks.F.vec=NULL
Wilks.pval.vec=NULL
x=2000 #How many times it will run
for(i in 1:x){
  
# groups with varying means
grp1 <- as.data.frame(mvrnorm(n=20,rep(0,2),Cov)) #randomly generate multivariate dist. group 1
grp2 <- as.data.frame(mvrnorm(n=20,rep(0,2),Cov)) #randomly generate multivariate dist. group 2 
grp3 <- as.data.frame(mvrnorm(n=20,rep(0,2),Cov)) #randomly generate multivariate dist. group 3

dat <- rbind(grp1,grp2,grp3)
grp_num <- rep(1:3,c(20,20,20))

dat1 <- cbind(grp_num,dat)

test1 <- Wilks.test(dat1[,2:3],grouping=dat1[,1],method="c",approximation="Rao")

Wilks.Lamb.vec <- c(Wilks.Lamb.vec,test1$wilks)
Wilks.F.vec <- c(Wilks.F.vec,test1$statistic)
Wilks.pval.vec <- c(Wilks.pval.vec,test1$p.value)
}

#large histogram 
#(Wilks.F.vec,freq = F, xlab="Rao's F-Approximation", 
#     ylab="Probability Density",ylim=range(0,0.8),
#     main="Distribution of Rao's \n F-Appromimation (N = 15)")
#curve(df(x,4,22), col="red",lwd=3,add=T)

#hist(Wilks.Lamb.vec,freq = F,ylim=range(0,3), xlab="Wilk's lambda", 
     #ylab="Probability Density",
     #main="Distribution of Wilk's Lambda \n (N = 15)")
#curve(dbeta(x,5.295837,1.963113),col="Red",lwd=3,add=T)

#try new method 
library(fitdistrplus)
#Fit wilk's lambda
descdist(Wilks.Lamb.vec, boot=1000)
fit.Lamb <- fitdist(Wilks.Lamb.vec,"beta",start=list(shape1=5,shape2=2))
summary(fit.Lamb)
plot(fit.Lamb)


#fit Rao's f-approx
descdist(Wilks.F.vec,boot=1000)
fit.f <- fitdist(Wilks.F.vec,"f",start=list(df1=4,df2=22))
fit.beta <- fitdist(Wilks.F.vec,"beta")
summary(fit.f)
plot(fit.f)

