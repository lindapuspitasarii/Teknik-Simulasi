RNG_multi<-function(a,z0,m,c)
{
  z<-rep(0,5)
  R<-rep(0,5)
  X<-rep(0,5)
  for( i in 1:5)
  {
    z[i]<-((a*z0)+c)%%m
    z0<-z[i]
    R[i]<-z[i]/m
    X[i]<-sqrt(R[i])
  }
  print(z)
  cat("Nilai Random Number:\n",R)
  cat("Nilai Random Variate Kontinu:\n",X,"\n")
  rata2<-mean(X)
  cat("Rata-rata",rata2,"\n")
}
RNG_multi(19,12357,128,237)

VR<-function(a,z0,m)
{
  z<-rep(0,20)
  R<-rep(0,20)
  x<-rep(0,20)
  for(i in 1:20)
  {
    z[i]<-(a*z0) %% m
    z0<-z[i]
    R[i]<-z[i]/m
    
    if(0<R[i] & R[i]<=0.333)
    {x[i]=1}
    else if (0.333<R[i] & R[i]<=0.5)
    {x[i]=2}
    else if (0.5<R[i] & R[i]<=0.667)
    {x[i]=3}
    else if (0.667<R[i] & R[i]<=0.833)
    {x[i]=4}
    else if (0.833<R[i] & R[i]<=1)
    {x[i]=5}
  }
  print(z)
  cat("nilai Random Number : \n",R)
  cat("\n nilai Random Variate Diskrit : \n",x,"\n")
}
VR(43,12357,1257)
