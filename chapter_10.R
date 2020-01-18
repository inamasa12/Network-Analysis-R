#10.1
A <- matrix(c(
0,1,1,1,1,0,0,0,0,
1,0,1,1,1,1,0,0,0,
1,1,0,1,0,0,1,0,0,
1,1,1,0,0,0,0,1,1,
1,1,0,0,0,0,0,0,0,
0,1,0,0,0,0,0,0,0,
0,0,1,0,0,0,0,0,0,
0,0,0,1,0,0,0,0,0,
0,0,0,1,0,0,0,0,0),
nrow = 9, ncol = 9)
prop.table(table(rowSums(A)))

library(igraph)
Fig10.1 <- graph_from_adjacency_matrix(A, mode = "undirected")
ddis <- degree_distribution(Fig10.1)
names(ddis) <- 0:max(degree(Fig10.1))
ddis
barplot(ddis, xlab = "Degree")

mean_distance(Fig10.1)

clust.coeff <- function(matrix){
  n <- nrow(matrix)
  output <- rep(0,n)
  for (i in 1:n){
    m <- sum(matrix[i,])
    output[i] <- 
      sum(t(matrix * matrix[i,]) * matrix[i,])/(m * (m - 1))
    }
  output
}

clust.coeff(A)
mean(clust.coeff(A), na.rm = TRUE)

transitivity(Fig10.1, type = "local")

#10.2
plot(sample_gnp(n = 50, p = 0.1),
 vertex.size = 2, vertex.label = NA, layout = layout.circle)

ERg <- sample_gnp(n = 1000, p = 0.01)
ERddis <- degree_distribution(ERg)
names(ERddis) <- 0:max(degree(ERg))
barplot(ERddis, cex.names = 0.7, xlab = "Degree")
mean(degree(ERg))

mean_distance(sample_gnp(n = 10000, p = 10/9999))

#Fig 10.5
n100 <- distance_table(sample_gnp(n = 100, p = 5/99))$res
n1k  <- distance_table(sample_gnp(n = 1000, p = 5/999))$res
n10k <- distance_table(sample_gnp(n = 10000, p = 5/9999))$res
plot(n100 / (100*99/2), type = "b", pch = 16,
 log = "y", xlab = "Distance", ylab = "",
 xlim = c(1, 15), ylim = c(10^(-8),1))
par(new = TRUE)
plot(n1k / (1000*999/2), type = "b",  pch = 15,
 log = "y", xlab = "", ylab = "",
 xlim = c(1, 15), ylim = c(10^(-8),1))
par(new = TRUE)
plot(n10k / (10000*9999/2), type = "b", pch = 17,
 log = "y", xlab = "", ylab = "",
 xlim = c(1, 15), ylim = c(10^(-8),1))
legend("bottomleft", pch = c(16,15,17),
 legend = c("n = 100", "n = 1000", "n = 10000"))

#Fig 10.6
k2  <- distance_table(sample_gnp(n = 1000, p = 2/999))$res
k5  <- distance_table(sample_gnp(n = 1000, p = 5/999))$res
k10 <- distance_table(sample_gnp(n = 1000, p = 10/999))$res
np = 1000 * 999 / 2
plot(k2 / np, type = "b", pch = 16,  log = "y", cex = 1.5,
 xlim = c(1, 25), ylim = c(10^(-6),1), xlab = "Distance", ylab = "")
par(new = TRUE)
plot(k5 / np, type = "b", pch = 15, log = "y", cex = 1.5,
 xlim = c(1, 25), ylim = c(10^(-6),1), xlab = "", ylab = "")
par(new = TRUE)
plot(k10 / np, type = "b", pch = 17,  log = "y", cex = 1.5,
 xlim = c(1, 25), ylim = c(10^(-6),1), xlab = "", ylab = "")
legend("topright",pch = c(16,15,17), cex = 1.5,
 legend = c("<k> = 2","<k> = 5","<k> = 10"))

#10.3
windows(width = 21)
par(mfrow = c(1,3))
for(prob in c(0, 0.2, 1)) {
 plot(sample_smallworld(dim = 1, size = 16, nei = 2, p = prob),
  layout = layout_in_circle, vertex.size = 5, vertex.label = NA)
 text(0, -1.3, paste("p = ", prob), cex = 2)
}

reg <- sample_smallworld(dim = 1, size = 1000, nei = 5, p = 0)
(reg.clust <- transitivity(reg, type = "local")[1])
(reg.dist <- mean_distance(reg))

prob <- 10^seq(-4, 0, by = 0.2)
mean.clust <- 1:length(prob)
mean.dist <- 1:length(prob)
for (i in 1:length(prob)) {
 clust <- 1:20
 dist <- 1:20
 for (j in 1:20) {
  g <- sample_smallworld(dim = 1, size = 1000, nei = 5, p = prob[i])
  clust[j] <- mean(transitivity(g, type = "local"), na.rm = TRUE)
  dist[j] <- mean_distance(g)
 }
 mean.clust[i] <- mean(clust, na.rm = TRUE)
 mean.dist[i] <- mean(dist, na.rm = TRUE)
}

y <- cbind(mean.clust/reg.clust, mean.dist/reg.dist)
windows()
matplot(prob, y, log = "x", pch = 1:2, xlab = "p", ylab = "")
legend("bottomleft", pch = 1:2, col = 1:2,
c("clustering coefficient","average distance"))

windows()
par(mfrow = c(2,2))
for (prob in c(0.01, 0.1, 0.3, 0.5)) {
 g <- sample_smallworld(dim = 1, size = 1000, nei = 5, p = prob)
 barplot(degree_distribution(g), xlab = paste("p = ",prob))
}

#10.4
ba1 <- sample_pa(n = 100)
windows()
plot(ba1, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.5)

ba2 <- sample_pa(n = 100, m = 2, power = 1.5)
plot(ba2, vertex.size = 5, vertex.label = NA, edge.arrow.size = 0.5)

ba3 <- sample_pa(n = 10^6, m = 1)
dd <- degree_distribution(ba3, mode = "in")
plot(dd, log = "xy", xlab = "degree", ylab = "proportion")
lines(1:100, 1 / (1:100)^3, col = "red")

vn <- round(10^seq(1, 4, 0.5))
ba.d <- 1:length(vn)
rdm.d <- 1:length(vn)
ba.c <- 1:length(vn)
rdm.c <- 1:length(vn)
for (i in 1:length(vn)) {
 ba.g <- sample_pa(n = vn[i], m = 2)
 rdm.g <- sample_gnm(n = vn[i], m = 2*vn[i] - 3)
 ba.d[i] <- mean_distance(ba.g, directed = FALSE)
 rdm.d[i] <- mean_distance(rdm.g, directed = FALSE)
 ba.c[i] <- mean(transitivity(ba.g, type = "local"), na.rm = TRUE)
 rdm.c[i] <- mean(transitivity(rdm.g, type = "local"), na.rm = TRUE)
}
plot(vn, ba.d, type = "b", log = "x", ylim = c(0,10),
 pch = 1, xlab = "n", ylab = "average distance")
par(new = TRUE)
plot(vn, rdm.d, type = "b", log = "x", ylim = c(0,10),
 pch = 2, xlab = "", ylab = "")
legend("topright", pch = 1:2,
 c("Barabasi-Albert model","random graph"))
windows()
plot(vn, ba.c, type = "b", log = "x", ylim = c(0, 1),
 pch = 1, xlab = "n", ylab = "clustering coefficient")
par(new = TRUE)
plot(vn, rdm.c, type = "b", log = "x", ylim = c(0, 1),
 pch = 2, xlab = "", ylab = "")
legend("topright", pch = 1:2,
 c("Barabasi-Albert model","random graph"))

#10.5
ba.diff <- 1:20
rdm.diff <- 1:20
mean.d <- 1:20
for (i in 1:20) {
 ba.ds <- 1:100
 rdm.ds <- 1:100
 for (j in 1:100) {
  ba.g <- sample_pa(n = 1000, m = i, directed = FALSE)
  rdm.g <- sample_gnm(n = 1000, m = 1000*i - sum(1:i))
  ba.d <- degree(ba.g)
  rdm.d <- degree(rdm.g)
  ba.ds[j] <- (mean(ba.d^2) - mean(ba.d)^2) / mean(ba.d)
  rdm.ds[j] <- (mean(rdm.d^2) - mean(rdm.d)^2) / mean(rdm.d)
 }
 mean.d[i] <- mean(ba.d)
 ba.diff[i] <- mean(ba.ds)
 rdm.diff[i] <- mean(rdm.ds)
}
plot(mean.d, ba.diff,type = "b", pch = 1, ylim = c(0,max(ba.diff)),
 xlab = "<k>", ylab = "k* - <k>")
par(new = TRUE)
plot(mean.d, rdm.diff, type = "b", pch = 2, ylim = c(0,max(ba.diff)),
 xlab = "", ylab = "")
legend("topleft", pch = 1:2,
 c("Barabasi-Albert model","random graph"))

#10.6
karate <- make_graph("Zachary")
rem.deg <- matrix(degree(karate)[as_edgelist(karate)] - 1, ncol = 2)

A <- mean((rem.deg[,1] * rem.deg[,2]))
B <- mean(rem.deg)^2
C <- mean(rem.deg^2)
(r <- (A - B) / (C - B))

assortativity_degree(karate)

maxdeg <- max(rem.deg)
tab <- matrix(0, nrow = maxdeg + 1, ncol = maxdeg + 1)
rownames(tab) <- 0:maxdeg
colnames(tab) <- 0:maxdeg
for (i in 1:nrow(rem.deg)) {
 tab[rem.deg[i,1] + 1,rem.deg[i,2] + 1] <-
  tab[rem.deg[i,1] + 1,rem.deg[i,2] + 1] + 1
}
e_jk <- (tab + t(tab)) / (2 * ecount(karate))
library(lattice)
levelplot(e_jk, col.regions = grey(100:0/100), xlab = "", ylab = "")

ba.ast <- 1:7
rdm.ast <- 1:7
mean.d <- 1:7
for(i in 1:7) {
 ba.asts <- 1:100
 rdm.asts <- 1:100
 for (j in 1:100) {
  ba.g <- sample_pa(n = 1000, m = 2^i, directed = FALSE)
  rdm.g <- sample_gnm(n = 1000, m = 1000*(2^i) - sum(1:(2^i)))
  ba.asts[j] <- assortativity_degree(ba.g)
  rdm.asts[j] <- assortativity_degree(rdm.g)
 } 
 mean.d[i] <- mean(degree(ba.g))
 ba.ast[i] <- mean(ba.asts)
 rdm.ast[i] <- mean(rdm.asts)
}
plot(mean.d, ba.ast, type = "b", pch = 1, ylim = range(ba.ast),
 log = "x", xlab = "<k>", ylab = "degree correlation coefficient")
par(new = TRUE)
plot(mean.d, rdm.ast, type = "b", pch = 2, ylim = range(ba.ast),
 log = "x", xlab = "", ylab = "")
legend("bottomright", pch = 1:2,
 c("Barabasi-Albert model", "random graph"))

library(ergm)
data(faux.mesa.high)
fmh <- matrix(as.edgelist(faux.mesa.high), ncol = 2)
fmh.g <- graph_from_edgelist(fmh, directed = FALSE)
grade <- faux.mesa.high %v% "Grade"
sex <- faux.mesa.high %v% "Sex"
sex2 <- rep(1, length(sex))
sex2[sex == "F"] <- 2

assortativity(fmh.g, grade)
assortativity_nominal(fmh.g, sex2)