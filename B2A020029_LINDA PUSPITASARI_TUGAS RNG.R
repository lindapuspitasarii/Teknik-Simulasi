# NAMA : LINDA PUSPITASARI
# NIM  : B2A020029
#TUGAS RNG
#NIM GANJIL
#-Menggunakan Additive
#-Menggunakan Bernoulli_2
#Diketahui : z0=11123, a=35, m=138, c=437, n=100, p=0.65

#ADDITIVE
Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
}
Additive_RNG(35,11123,437,138,100)


#BERNOULLI 2
#Angka n diganti sesuai nilai Xi pada output Additive table
Bernouli_2<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}
barplot(tabel,main="Bernoulli")
Bernouli_2(1000, 0.65)
Bernouli_2(107, 0.65)
Bernouli_2(42, 0.65)
Bernouli_2(113, 0.65)
Bernouli_2(114, 0.65)
#Dst ke-Xi n=urut ke 100
#Hasil subtitusi ada di file pdf submitan dibawah
