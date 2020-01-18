#6.1.1
g1 <- matrix(c(
0,1,1,1,1,
1,0,1,0,0,
1,1,0,0,0,
1,0,0,0,0,
1,0,0,0,0),
nrow = 5)

g2 <- matrix(c(
0,1,1,1,1,
1,0,0,0,0,
1,0,0,0,0,
1,0,0,0,1,
1,0,0,1,0),
nrow = 5)

sum(abs(g1 - g2))
sum(abs(g1 - g2)) / 2

gstack <- array(dim = c(2,5,5))
gstack[1,,] <- g1
gstack[2,,] <- g2

library(sna)
hdist(gstack, mode = "graph")
structdist(gstack, mode = "graph")

#6.1.2
as.vector(g1)
g1[-c(1,7,13,19,25)] 
g1[-seq(1, length(g1), by = nrow(g1) + 1)]

cor(g1[-seq(1, length(g1), by = nrow(g1) + 1)], 
    g2[-seq(1, length(g2), by = nrow(g2) + 1)])

gcor(gstack)
gscor(gstack)

ADVICE <- matrix(c(
0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,1,
0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
1,1,0,1,0,1,1,1,1,1,1,1,0,1,0,0,1,1,0,1,1,
1,1,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,1,0,1,1,
1,1,0,0,0,1,1,1,0,1,1,0,1,1,0,1,1,1,1,1,1,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
0,1,0,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,
0,1,0,1,0,1,1,0,0,1,1,0,0,0,0,0,0,1,0,0,1,
1,1,0,0,0,1,1,1,0,1,1,1,0,1,0,1,1,1,0,0,1,
1,1,1,1,1,0,0,1,0,0,1,0,1,0,1,1,1,1,1,1,0,
1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
1,1,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,
0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,
1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,
1,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,0,0,1,1,1,
1,1,1,0,1,0,1,0,0,1,1,0,0,1,1,0,0,1,0,1,0,
1,1,0,0,0,1,0,1,0,0,1,1,0,1,1,1,1,1,0,0,1,
0,1,1,1,0,1,1,1,0,0,0,1,0,1,0,0,1,1,0,1,0),
nrow = 21, byrow = TRUE)

FRIEND <- matrix(c(
0,1,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,
1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,
0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,
1,1,0,0,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,0,0,
0,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0,1,0,1,0,1,
0,1,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0,1,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,1,0,1,0,0,1,1,0,0,1,0,0,0,1,0,0,0,1,0,
1,1,1,1,1,0,0,1,1,0,0,1,1,0,1,0,1,1,1,0,0,
1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,
0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
1,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,
1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,0,1,1,1,
0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
1,1,1,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,1,0,
0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,
0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0),
nrow = 21, byrow = TRUE)

REPORT <- matrix(c(
0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
nrow = 21, byrow = TRUE)

dat <- array(dim = c(3,21,21))
dat[1,,] <- ADVICE
dat[2,,] <- FRIEND
dat[3,,] <- REPORT
GCOR <- gcor(dat)
rownames(GCOR) <- 
colnames(GCOR) <-
c("ADVICE","FRIEND","REPORT")
GCOR

GSCOR <- gscor(dat)
rownames (GSCOR) <- c("ADVICE","FRIEND","REPORT")
colnames (GSCOR) <- c("ADVICE","FRIEND","REPORT")
GSCOR

#6.2
library(sna)
d1 <- degree(ADVICE)
d2 <- degree(FRIEND)
d3 <- degree(REPORT)
data.degree <- data.frame(ADVICE = d1, FRIEND = d2, REPORT = d3)
data.degree

library(MASS)
cra <- corresp(data.degree, nf = min(dim(data.degree)) - 1)
biplot(cra)

eig <- cra$cor^2
round(100 * eig / sum(eig),2)
cumsum(round(100 * eig / sum(eig),2))

#6.3
Fig6.6 <- matrix(c(
0,1,1,1,1,0,0,0,0,0,
1,0,1,1,0,1,1,1,0,0,
1,1,0,1,0,0,0,1,1,0,
1,1,1,0,1,0,0,0,0,1,
1,0,0,1,0,0,0,0,0,0,
0,1,0,0,0,0,0,0,0,0,
0,1,0,0,0,0,0,0,0,0,
0,1,1,0,0,0,0,0,0,0,
0,0,1,0,0,0,0,0,0,0,
0,0,0,1,0,0,0,0,0,0),
nrow = 10, ncol = 10)

library(sna)
gplot(Fig6.6, gmode = "graph", vertex.col = "white", vertex.cex = 1.5,
 displaylabels = TRUE, label.pos = 5, label.cex = 1.5)

make.pat.matrix <- function(x) {
 pat.matrix <- matrix(NA, length(x), length(x))
 pat.matrix[which(x == 1), which(x == 1)] <- 1
 pat.matrix[which(x == 0), which(x == 0)] <- 0
 diag(pat.matrix) <- NA
 return(pat.matrix)
}

x <- c(1,1,1,1,0,0,0,0,0,0)
make.pat.matrix(x)

gcor(Fig6.6, make.pat.matrix(x))

install.packages("GA")
library(GA)

matrix.cor <- function(x) {return(gcor(g, make.pat.matrix(x)))}

edge.list <- matrix(c(
1,17,2,13,2,14,2,15,2,16,2,17,2,20,3,13,3,17,3,18,
4,6,4,7,4,14,4,15,4,17,5,6,5,7,5,15,6,7,6,13,6,14,
6,15,6,16,6,17,6,20,7,9,7,14,7,15,7,16,7,17,7,20,
8,15,8,16,8,17,9,15,9,17,10,17,11,15,11,17,12,16,
13,15,13,16,13,17,13,18,13,19,13,20,14,17,15,16,
15,17,15,18,15,19,15,20,16,17,16,18,16,20,17,18,
17,19,17,20,18,19),
ncol = 2, byrow = TRUE)
edge.list <- cbind(edge.list, rep(1, nrow(edge.list)))
attr(edge.list, "n") <- 20
cocitation <- symmetrize(edge.list)

gplot(cocitation, gmode = "graph",
 vertex.col = "white", vertex.cex = 1.5, 
 displaylabels = TRUE, label.pos = 5, label.cex = 1.5)

g <- cocitation
ga.core <- ga(type = "binary", fitness = matrix.cor, nBits = nrow(g))
summary(ga.core)

ga.core@fitnessValue
matrix.cor(ga.core@solution[1,])
gcor(g, make.pat.matrix(ga.core@solution[1,]))

(core <- which(ga.core@solution[1,] == 1))
pmt <- c(core, (1:nrow(cocitation))[-core])
plot.sociomatrix(cocitation[pmt, pmt], labels = list(pmt, pmt))

make.pat.matrix2 <-  function(x){
 pat.matrix <- matrix(NA, length(x), length(x))
 for (i in 1:length(x)) {
  pat.matrix[i,] <- x[i] * x
 }
 return(pat.matrix)
}

edge.list2 <- matrix(c(
1,17,3, 2,13,8, 2,14,13, 2,15,8,
2,16,21, 2,17,73, 2,20,20, 3,13,13,
3,17,9, 3,18,2, 4,6,9, 4,7,12,
4,14,7, 4,15,6, 4,17,8, 5,6,12,
5,7,5, 5,15,3, 6,7,70, 6,13,11,
6,14,7, 6,15,32, 6,16,17, 6,17,58,
6,20,8, 7,9,4, 7,14,6, 7,15,8,
7,16,14, 7,17,28, 7,20,5, 8,15,47,
8,16,20, 8,17,45, 9,15,18, 9,17,9,
10,17,3, 11,15,16, 11,17,18, 12,16,7,
13,15,21, 13,16,16, 13,17,58, 13,18,9,
13,19,7, 13,20,24, 14,17,19, 15,16,36,
15,17,124, 15,18,9, 15,19,20, 15,20,18,
16,17,106, 16,18,7, 16,20,39, 17,18,40,
17,19,43, 17,20,44, 18,19,9),
ncol = 3, byrow = TRUE)

cocitation2 <- matrix(0, nrow = 20, ncol = 20)
for (i in 1:nrow(edge.list2)) {
 cocitation2[edge.list2[i,1], edge.list2[i,2]] <- edge.list2[i,3]
 cocitation2[edge.list2[i,2], edge.list2[i,1]] <- edge.list2[i,3]
}

matrix.cor2 <- function(x) {return(gcor(g, make.pat.matrix2(x)))}

g <- cocitation2
coreness0 <- 1:20

estimation <- optim(par = coreness0, fn = matrix.cor2,
 method = "L-BFGS-B", control = list(fnscale = -1, maxit = 1000))

estimation$value
(coreness <- estimation$par)
(order.coreness <- order(coreness, decreasing = TRUE))

pmt2 <- cocitation2[order.coreness, order.coreness]
rownames(pmt2) <- colnames(pmt2) <- order.coreness
plot.sociomatrix(pmt2,drawlines = FALSE)

library(lattice)
pmt3 <- cocitation2[order.coreness, rev(order.coreness)]
rownames(pmt3) <- order.coreness
colnames(pmt3) <- rev(order.coreness)
windows()
levelplot(pmt3, col.regions = gray(100:0/100), xlab = "", ylab ="",
 scale = list(x = list(alternating = 2)))