#Pertemuan 06
library(markovchain)
state.Names=c("serba","ada")
mcA<-new("markovchain",
         transitionMatrix=matrix(c(0.667,0.333,0.625,0.375),
                                 byrow=TRUE, nrow=2, dimnames=list(state.Names,state.Names)))
mcA^3
steadyStates(mcA)
absorbingStates(mcA)
markovchainSequence(n=20, markovchain = mcA,include=TRUE)

library(markovchain)
state.Names=c("serba","ada")
mcA<-new("markovchain",
         transitionMatrix=matrix(c(0.667,0.333,0.625,0.375),
                                 byrow=TRUE, nrow=2, dimnames=list(state.Names,state.Names)))
mcA^6
steadyStates(mcA)
absorbingStates(mcA)
markovchainSequence(n=20, markovchain = mcA,include=TRUE)
