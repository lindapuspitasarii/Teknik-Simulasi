#Pembangkitan dari bilangan acak bernoulli
Binomial_sim<-function(n,p) {
  i<-1000
  n<-n
  p<-p
  Binom<-NULL
  for (z in 1:i){
    m<-0
    for(k in 1:n){
      y<-(runif(1)<=p)+0
      m<-m+y
    }
    Binom[z]<-m
  }
  (tabel<-table(Binom)/length(Binom))
  print(Binom)
}

#Pembangkitan bilangan acak dari uniform
i<-1000
X<-runif(i)
Binom<-as.numeric(cut(X,breaks=c(0,1/8,4/8,7/8,1),include.lowest=T))-1
(tabel<-table(Binom)/length(Binom))
View(Binom)

#Pembangkit dengan menggunakan fungsi di R
x<-rbinom(16,4,0.5)
x


#Fumgsi inverse transformation method
#VR Geometri
#transformation
i<-1000
p<-0.5
R<-runif(i)
X<-log(1-R)/log(1-p)
hist(X)

#input nilai p (peluang sukses)
i<-1000
sebaran_geom<-function(p){
  R<-runif(i)
  X<-log(1-R)/log(1-p)
  print(X)
}
sebaran_geom(0.5)

#melalui sebaran bernoulli
#sebaran bernoulli
K<-1
p<-0.5
while(runif(1)>p)
  K=K+1;
K
#fungsi di R
# x ~ geometrik(0.4) sebanyal 16 bilangan acak
x<-rgeom(16,0.4)
x

#VARIABEL RANDOM BINOMIAL
#Sebaran Geometric
#VR Binomial Negatif
#sebaran geometri
K<-1
p<-0.5
r<-3
R<-runif(i)
s<-0
while(s<r){
  if(runif(1)>p)
  {K=K+1;
  print=0
  }
  else
  {s=s+1;
  print=1}
}
K+r-1

#Sebaran Uniform
#uniform
n<-1000
U<-runif????
m<-5
p<-0.5
F<-pnbinom(1:20,size=m,p)
negative.binom<-NULL
for (i in 1:n){
  negative.binom[i]<-min(which(U[i]<F))-1
}
table(negative.binom)

#Fungsi di R
#menggunakan fungsi di R
#x~negative binom(4,0.5) sebanyak 16 bilangan acak
x<-rnbinom(16,4,0.5)
x


#VARIABEL RANDOM POISSON
#Sebaran Uniform
#VR Poisson
#Sebaran uniform
i<-100
lambda<-1
K<-NULL
for (z in 1:i){
  k<-0
  sk<-1
  while(sk>=exp(-lambda)){
    u<runif(1)
    sk<-sk*u
    k<-k+1
  }
  K[z]<-k
}
K
(tabel1<-table(K)/length(K))

#Melalui sebaran eksponensial
i<-100
lambda<-1
K<-NULL
for (z in 1:i){
  sk<-0
  k<-1
  while(sk<=1){
    u<runif(1)
    y<--log(u)/lambda
    sk<-y+sk
    k<-k+1
  }
  [K]<-k-1
}
K
(tabel2<-table(K)/length(K))

#fungsi di R
#x~poisson(4)sebanyak 16 bilangan acak
x<-rpois(16,4)
x

