library("markovchain")

#Membuat objek markovchain
mtrx<-matrix(c(0.6, 0.4, 0.2, 0.8), byrow=TRUE, nrow=2,
             dimnames =list(c("B", "T"),c("B", "T")))
mc<-new("markovchain", transitionMatrix =mtrx, name ="Evaluasi Perekrutan Karyawan Baru")

#Menghitung peluang status pada tahun ke-3
steadyStates(mc)[1]

#ANOTHER EXAMPLE

library(markovchain)
statesNames=c("Serba","Ada")
mcA<-new("markovchain", transitionMatrix=matrix(c(0.667,0.333,0.625,0.375),byrow=TRUE,
                                                nrow=2, dimnames=list(statesNames,statesNames)))
mcA

#operations with S4 methods
mcA^2
steadyStates(mcA)
absorbingStates(mcA)
markovchainSequence(n=20, markovchain=mcA, include=TRUE)
