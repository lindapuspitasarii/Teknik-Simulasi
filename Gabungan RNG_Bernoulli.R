Bernouli_2<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}

Gabungan_RNG_ber<-function(a,z0,c,m,n,p) {
  xi<-matrix(NA,n,3)
  di<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  colnames(di)<-c("Xi","0","1")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
    di[i,1]<-xi[i,2]
    di[i,2:3]<-Bernouli_2(xi[i,2],p)
  }
  #hist(xi[,3])
  View(xi)
  View(di)
}
Gabungan_RNG_ber(35,11123,437,138,100,0.65)

