Weibull.mle <- function (x=NULL,n=NULL)
{
  if(is.null(x))
    {
      library(survival)
      x <- lung$time
    }
  r <- length(x)
  if(is.null(n)){n<-r}else{if(r>n||r<2){
    return("x must have length r with: 2 <= r <= n")}}
  xs <- sort(x)
  if(!exists("survreg"))library(survival)
  #tests whether survival package is loaded, if not, then it loads survival
  if(r<n){
    statusx <- c(rep(1,r),rep(0,n-r))
    dat.weibull <- data.frame(c(xs,rep(xs[r],n-r)),statusx)}
  else{statusx <- rep(1,n)
    dat.weibull <- data.frame(xs,statusx)}
  names(dat.weibull)<-c("time","status")
  out.weibull <- survreg(Surv(time,status)~1,dist="weibull",data=dat.weibull)
  scale <- exp(out.weibull$coef)
  shape <- 1/out.weibull$scale
  parms <- c(scale, shape)
  names(parms)<-c("scale","shape")
  list(mles=parms)
}