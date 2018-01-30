#################################################
# A function to compute a two-sample t-test and confidence 
# interval (equal-variance, independent samples).  yvec is 
# a numeric vector containing both samples' data.  trtvec 
# is a vector, same length as yvec, of treatment
# identifiers for the data in yvec.  A boxplot comparing
# the treatments' data is constructed.  Output is a one-row
# data frame reporting the results of the test, 
# confidence interval and decision.
##################################################

ttest<-function(yvec,trtvec,alpha=0.05,header="") {
  trtvec=as.factor(trtvec)
  boxplot(split(yvec,trtvec))
  title(header)
  ybar=tapply(yvec,trtvec,mean)
  varvec=tapply(yvec,trtvec,var)
  nvec=table(trtvec)
  error.df=nvec[1]+nvec[2]-2
  pooled.var=((nvec[1]-1)*varvec[1]+(nvec[2]-1)*varvec[2])/error.df
  diff12estimate=ybar[1]-ybar[2]
  stderr=sqrt(pooled.var*((1/nvec[1])+(1/nvec[2])))
  tratio=diff12estimate/stderr
  pval=2*(1-pt(abs(tratio),error.df))
  tcrit=qt(1-alpha/2,error.df)
  lower=diff12estimate-tcrit*stderr
  upper=diff12estimate+tcrit*stderr
  calpha=1-alpha
  #I created an object called “decision” which displays the result of the hypothesis test. I used the object #names “pval” for the p-value and “alpha” for the alpha level so that as the values of these objects #change, the logical operation (the if/else statement) of “decision” will adjust. For instance, if I used .05 #instead of “alpha”, the logical operation would always compare the p-value to .05 not the value stored #in “alpha” which may be changed by the user to something other than the default of .05. 
  decision<- if(pval<=alpha){decision = "Reject Null"
  }else{decision ="Fail to Reject Null"}
  #I added the decision object to the “out” statement so that  the decision will display with the function #output. I also added “Decision” to the name(out) statement so that a name will display #with the #decision output.   
  out=data.frame(diff12estimate,stderr,tratio,pval,lower,upper,alpha,decision)
  names(out)=c("Estimator","SE","T","P-value","Lower CI","Upper CI","Confidence", "Decision")
  out}

#x is a vector of out the scores from both groups
x<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

#y is a vector of the treatment conditions
y<-c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2)

#the arguments are the score vector, the treatment vector,alpha, and header.
#the default alpha of .05 is used if not imput. 
ttest(x,y,0.05,"Test")
