# Mengimpor library yang diperlukan
library(gtools)

# Inisialisasi data
X <- c(1500, 2000, 1800, 2200, 2500, 1700, 1900, 2100, 2300, 2400)
Y <- c(550, 650, 590, 720, 800, 620, 670, 690, 750, 780)

# Inisialisasi parameter
N <- 10000  # Jumlah iterasi
burn_in <- 1000  # Jumlah iterasi untuk fase "burn-in"

# Inisialisasi variabel
beta_0 <- numeric(N)
beta_1 <- numeric(N)

# Fungsi untuk menghitung posterior distribusi
compute_posterior <- function(beta_0, beta_1, X, Y) {
  sigma_squared <- 1000  # Variansi kesalahan model (diasumsikan tetap)
  n <- length(X)  # Jumlah observasi
  
  # Menghitung likelihood
  log_likelihood <- -0.5 * n * log(sigma_squared) - (1 / (2 * sigma_squared)) * sum((Y - (beta_0 + beta_1 * X))^2)
  
  # Menghitung prior
  log_prior <- dnorm(beta_0, mean = 0, sd = 100) + dnorm(beta_1, mean = 0, sd = 100)
  
  # Menghitung posterior
  log_posterior <- log_likelihood + log_prior
  
  log_posterior
}

# Algoritma Gibbs Sampling
for (i in 1:N) {
  # Sampel beta_0
  sigma_squared <- 1000
  X_bar <- mean(X)
  X_sum_sq <- sum(X^2)
  X_Y_cov <- sum(X * Y)
  
  beta_0[i] <- rnorm(1, mean = (X_bar * (X_Y_cov / X_sum_sq)) / (1 / sigma_squared), sd = 1 / sqrt(X_sum_sq / sigma_squared))
 
  # Sampel beta_1
  sigma_squared <- 1000
  X_sum <- sum(X)
  Y_sum <- sum(Y)
  
  beta_1[i] <- rnorm(1, mean = (X_Y_cov / X_sum_sq) / (1 / sigma_squared), sd = 1 / sqrt(X_sum_sq / sigma_squared))
}

# Menghilangkan fase "burn-in"
beta_0 <- beta_0[-(1:burn_in)]
beta_1 <- beta_1[-(1:burn_in)]

# Estimasi parameter
est_beta_0 <- mean(beta_0)
est_beta_1 <- mean(beta_1)

# Output hasil estimasi
print(paste("Estimasi koefisien intercept (beta_0):", est_beta_0))
print(paste("Estimasi koefisien slope (beta_1):", est_beta_1))

#------------------------------------------------------------------#
# Mengimpor library yang diperlukan
library(gtools)

# Inisialisasi data
set.seed(123)  # Mengatur seed untuk reproduktibilitas
n <- 100  # Jumlah observasi

# Membangkitkan data secara acak
X <- runif(n, min = 1000, max = 3000)
Y <- 500 + 0.2 * X + rnorm(n, mean = 0, sd = 100)

# Inisialisasi parameter
N <- 10000  # Jumlah iterasi
burn_in <- 1000  # Jumlah iterasi untuk fase "burn-in"

# Inisialisasi variabel
beta_0 <- numeric(N)
beta_1 <- numeric(N)

# Fungsi untuk menghitung posterior distribusi
compute_posterior <- function(beta_0, beta_1, X, Y) {
  sigma_squared <- 1000  # Variansi kesalahan model (diasumsikan tetap)
  n <- length(X)  # Jumlah observasi
  
  # Menghitung likelihood
  log_likelihood <- -0.5 * n * log(sigma_squared) - (1 / (2 * sigma_squared)) * sum((Y - (beta_0 + beta_1 * X))^2)
  
  # Menghitung prior
  log_prior <- dnorm(beta_0, mean = 0, sd = 100) + dnorm(beta_1, mean = 0, sd = 100)
  
  # Menghitung posterior
  log_posterior <- log_likelihood + log_prior
  
  log_posterior
}

# Algoritma Gibbs Sampling
for (i in 1:N) {
  # Sampel beta_0
  sigma_squared <- 1000
  X_bar <- mean(X)
  X_sum_sq <- sum(X^2)
  X_Y_cov <- sum(X * Y)
  
  beta_0[i] <- rnorm(1, mean = (X_bar * (X_Y_cov / X_sum_sq)) / (1 / sigma_squared), sd = 1 / sqrt(X_sum_sq / sigma_squared))
  
  # Sampel beta_1
  sigma_squared <- 1000
  X_sum <- sum(X)
  Y_sum <- sum(Y)
  
  beta_1[i] <- rnorm(1, mean = (X_Y_cov / X_sum_sq) / (1 / sigma_squared), sd = 1 / sqrt(X_sum_sq / sigma_squared))
}

# Menghilangkan fase "burn-in"
beta_0 <- beta_0[-(1:burn_in)]
beta_1 <- beta_1[-(1:burn_in)]

# Estimasi parameter
est_beta_0 <- mean(beta_0)
est_beta_1 <- mean(beta_1)

# Output hasil estimasi
print(paste("Estimasi koefisien intercept (beta_0):", est_beta_0))
print(paste("Estimasi koefisien slope (beta_1):", est_beta_1))

#--------------------------------------------------------------#
#Kelompok 6
# Mengimpor library yang diperlukan
library(gtools)

# Data tingkat pendidikan (X) dalam tahun
pendidikan <- c(12, 10, 14, 16, 13, 12, 15, 11, 13, 12, 14, 16, 11, 13, 15, 12, 14, 10, 11, 13)

# Data tingkat penghasilan (Y) dalam ribu rupiah
penghasilan <- c(40, 25, 50, 60, 45, 35, 55, 30, 42, 38, 48, 58, 32, 44, 52, 36, 47, 28, 32, 43)

# Inisialisasi parameter
N <- 10000  # Jumlah iterasi
burn_in <- 1000  # Jumlah iterasi untuk fase "burn-in"

# Inisialisasi variabel
beta_0 <- numeric(N)
beta_1 <- numeric(N)

# Fungsi untuk menghitung posterior distribusi
compute_posterior <- function(beta_0, beta_1, X, Y) {
  sigma_squared <- 100  # Variansi kesalahan model (diasumsikan tetap)
  n <- length(X)  # Jumlah observasi
  
  # Menghitung likelihood
  log_likelihood <- -0.5 * n * log(sigma_squared) - (1 / (2 * sigma_squared)) * sum((Y - (beta_0 + beta_1 * X))^2)
  
  # Menghitung prior
  log_prior <- dnorm(beta_0, mean = 0, sd = 10) + dnorm(beta_1, mean = 0, sd = 10)
  
  # Menghitung posterior
  log_posterior <- log_likelihood + log_prior
  
  log_posterior
}

# Algoritma Gibbs Sampling
for (i in 1:N) {
  # Sampel beta_0
  sigma_squared <- 100
  X_bar <- mean(pendidikan)
  X_sum_sq <- sum(pendidikan^2)
  X_Y_cov <- sum(pendidikan * penghasilan)
  
  beta_0[i] <- rnorm(1, mean = (X_bar * (X_Y_cov / X_sum_sq)) / (1 / sigma_squared), sd = 1 / sqrt(X_sum_sq / sigma_squared))
  
  # Sampel beta_1
  sigma_squared <- 100
  X_sum <- sum(pendidikan)
  Y_sum <- sum(penghasilan)
  
  beta_1[i] <- rnorm(1, mean = (X_Y_cov / X_sum_sq) / (1 / sigma_squared), sd = 1 / sqrt(X_sum_sq / sigma_squared))
}

# Menghilangkan fase "burn-in"
beta_0 <- beta_0[-(1:burn_in)]
beta_1 <- beta_1[-(1:burn_in)]

# Estimasi parameter
est_beta_0 <- mean(beta_0)
est_beta_1 <- mean(beta_1)

# Output hasil estimasi
print(paste("Estimasi koefisien intercept (beta_0):", est_beta_0))
print(paste("Estimasi koefisien slope (beta_1):", est_beta_1))

