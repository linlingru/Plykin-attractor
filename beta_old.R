gammar <- (1 + sqrt(5))/2
delta <- 1 + 1/gammar**2
C <- 2 * log(gammar)/sqrt(5)
k <- 1.5
beta1 <- function(x) {
  2 * (1 - x) * log(gammar)/(1 + 2 * sqrt(x) + x)
}

beta2 <- function(x) {
  32 * x * (log(gammar) **2)/(x**2 + 6 * x + 1 + 32 * x * log(gammar))
}

beta <- beta2(1/4)

rho <- function(beta){
  beta/ (1/delta + 2/(gammar * delta) + 1/(gammar**2 * delta))
}
rho <- rho(beta)

b <- rho / (sqrt(5) * k * C)



f_x <- function(x, R= 10, R_tilder= 100000000){
#actually it should be f_x <= function(x), where x = abs(weierstrassprime)
  2*R*x/((R*x + 1)**2) + 6*R_tilder/((x + R_tilder)**2) + 3 * (1 + 12 * ((x/4)** (1/3)) )* (1/(1 + R * x) + 3 * x /(x + R_tilder))
}
x <- seq(2, 10000, 1)
plot(x, f_x(x))




