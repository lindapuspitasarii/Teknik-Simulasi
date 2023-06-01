library("markovchain")

# membuat objek markovchain
mtrx <- matrix(c(0.6, 0.4, 0.2, 0.8), byrow = TRUE, nrow = 2,
               dimnames = list(c("B", "T"), c("B", "T")))
mc <- new("markovchain", transitionMatrix = mtrx, name = 
            "Evaluasi Perekrutan Karyawan Baru")

# menghitung peluang status pada tahun ke-3
steadyStates(mc)[1]

