1 + 2
5 - 3
2 * 3
8 / 2
2^3
sqrt(9)

a <- 2; b <- 3
a + b

c(1,2,3,4,5)
1:5
seq(1, 5)
seq(0, 100, 10)

c(1,2,3) + c(4,5,6)
c(1,2,3) - c(4,5,6)
c(1,2,3) * c(4,5,6)
c(1,2,3) / c(4,5,6)

c(1,2,3) + 2
c(1,2,3) - 2
c(1,2,3) * 2
c(1,2,3) / 2

a <- c(1,2,3); b <- c(4,5,6)
sum(a * b)

matrix(c(
1,2,3,
4,5,6),
nrow = 2, ncol = 3,
byrow = TRUE)

A <- matrix(c(
1,2,3,
4,5,6),
nrow =2,
byrow = TRUE)
B <- matrix(c(
4,5,6,
1,2,3),
nrow = 2,
byrow = TRUE)
A + B
A * B
A^2
A * 2

t(B)

A %*% t(B)
t(B) %*% A

diag(3)

A <- matrix(c(
1,2,
3,4),
nrow =2, byrow = TRUE)
solve(A)

A <- matrix(1:4, nrow = 2, byrow =TRUE)
A
eigen(A)

eval <- eigen(A)$values
x <- eigen(A)$vectors
A %*% x[,1]
eval[1] * x[,1]
A %*% x[,2]
eval[2] * x[,2]

A %*% (2 * x[,1])
eval[1] * (2 * x[,1])

sum(x[,1]^2)

x <- c(1,2,3,4)
sum(x)
sum(x) / length(x)
mean(x)

sum((x - mean(x))^2) / length(x)
mean(x^2) - mean(x)^2
sqrt(mean(x^2) - mean(x)^2)

x1 <- c(1,2,3)
x2 <- c(5,7,9)
(s12 <- mean((x1 - mean(x1)) * (x2 - mean(x2)))) 
(s12 <- mean(x1 * x2) - mean(x1) * mean(x2))

(s12 <- mean(x1 * x2) - mean(x1) * mean(x2))
(s1 <- sqrt(mean(x1^2) - mean(x1)^2))
(s2 <- sqrt(mean(x2^2) - mean(x2)^2))
s12 / (s1 * s2)

plot(x1, x2, type = "b")
cor(x1, x2)