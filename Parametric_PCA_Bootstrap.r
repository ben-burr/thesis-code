### THE FULL PARAMETRIC BOOSTRAP METHOD. VARIABLES SCALED USING STANDARD DEVIATIONS

### Example: The peanut dataset

# Reference: Kang, M. S., Balzarini, and M. G., Guerra, L. L. (2004).
#            Genotype-by-environment interaction.
#            In: A. M. Saxton (ed.) Genetic analysis of complex traits using SAS.
#            Cary, NC, USA: SAS Institute.


Y0 <- matrix(
      c(0.80,	2.17,	2.43,	2.71,	1.13,	3.08,	2.81,	1.74,	2.16,	4.29,	1.82,	5.33,	1.18,	4.39,	3.41,
        0.96,	2.04,	2.58,	2.26,	1.14,	3.22,	2.88,	1.73,	2.44,	4.21,	1.71,	4.93,	1.32,	4.40,	3.45,
        1.24,	1.57,	2.47,	1.77,	1.55,	2.90,	2.96,	2.16,	3.30,	3.55,	2.16,	4.69,	2.24,	4.13,	2.22,
        0.95,	1.29,	2.34,	1.61,	1.86,	2.59,	3.41,	1.44,	3.01,	3.84,	1.88,	4.16,	1.63,	3.79,	2.46,
        1.37,	2.15,	2.19,	2.15,	1.98,	2.36,	3.20,	2.20,	3.37,	3.53,	2.09,	4.70,	1.54,	4.33,	3.09,
        1.41,	3.27,	2.19,	2.04,	1.61,	2.43,	2.96,	0.95,	2.53,	3.22,	1.91,	3.57,	1.15,	3.72,	2.61,
        1.16,	1.08,	2.64,	2.14,	1.71,	3.05,	2.91,	2.86,	2.73,	4.45,	2.53,	5.57,	2.45,	4.28,	2.81,
        1.12,	0.58,	2.24,	1.88,	0.85,	2.90,	2.53,	2.13,	3.00,	4.46,	1.86,	5.43,	1.78,	3.77,	3.15,
        0.87,	1.52,	2.30,	1.72,	1.24,	2.94,	2.73,	1.60,	3.18,	4.24,	1.71,	4.99,	1.54,	4.17,	3.84,
        1.11,	0.86,	2.20,	2.18,	1.21,	2.57,	2.90,	2.29,	3.25,	4.03,	2.27,	4.67,	2.00,	4.75,	3.54),
      nrow = 10, byrow = T, dimnames = list(rownames =
      c("Florman", "Tegua", "manf393", "mf447", "mf478", "mf480", "mf484", "mf485", "mf487", "mf489"),
      colnames =
      c("E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10",
        "E11", "E12", "E13", "E14", "E15")))
Y0

# Specify the significance level and the number of bootstrap samples

alpha <- 0.05

N.Boot <- 1000


# Program start (no changes are needed below)

n <- nrow(Y0) ; n
p <- ncol(Y0) ; p
M <- min(n - 1, p)


# Variables are centred, and divided by standard deviations

Y <- scale(Y0, center = TRUE, scale = TRUE)


# Singular value decomposition

svd.Y <- svd(Y)
lambda <- svd.Y$d


# Test statistic

T <- {lambda^2/rev(cumsum(rev(lambda^2)))} [1:(M - 1)]


# Sampling

T.boot <- matrix(NA, nrow = N.Boot, ncol = M - 1)
Theta.hat <- matrix(NA, ncol = 1, nrow = M - 1)
Matrix.0 <- matrix(0, nrow = n, ncol = p)
Matrix.1 <- matrix(1, nrow = n, ncol = p)

for(Component in 1:(M - 1)){
  K <- Component <- 1
  Matrix.Component <- matrix(Component, nrow = n, ncol = p)
  Theta.hat <- ifelse(Matrix.Component == Matrix.1, Matrix.0, svd.Y$u[, 1:K]%*%diag(svd.Y$d[1:K], nrow = K, ncol = K)%*%t(svd.Y$v[, 1:K]))
  Variance.hat <- t(svd.Y$d[Component:M])%*%svd.Y$d[Component:M]/((n - 1 - K)*(p - K))
  for(Boot in 1:N.Boot){
    Y0.boot <- matrix(rnorm(n*p, as.vector(Theta.hat), sd = sqrt(Variance.hat)), nrow = n, ncol = p)
    Y.boot <- scale(Y0.boot, center = TRUE, scale = TRUE)
    lambda.boot <- svd(Y.boot)$d
    T.boot[Boot, Component] <- lambda.boot[Component]^2 /t(lambda.boot[Component:M])%*%lambda.boot[Component:M]
  }
  if (mean(T.boot[, Component] > T[Component]) > alpha){break}
}


# Results

N.Boot

lambda^2

T

colMeans(T.boot > matrix(rep(T, N.Boot), nrow = N.Boot, byrow = TRUE)) # p-values

