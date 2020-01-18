#4.2
D <- matrix(c(
0,1,1,1,1,1,1,2,
1,0,1,1,1,1,2,2,
1,1,0,2,2,2,2,1,
1,1,2,0,2,2,1,3,
1,1,2,2,0,1,2,3,
1,1,2,2,1,0,2,3,
1,2,2,1,2,2,0,3,
2,2,1,3,3,3,3,0),
nrow = 8, ncol = 8, byrow = TRUE)
1 / apply(D, 2, max)

1 / apply(D, 2, sum)

n <- nrow(D)
(n - 1) / apply(D, 2, sum)

#4.3
A <- matrix(c(
0,1,1,1,1,1,1,0,
1,0,1,1,1,1,0,0,
1,1,0,0,0,0,0,1,
1,1,0,0,0,0,1,0,
1,1,0,0,0,1,0,0,
1,1,0,0,1,0,0,0,
1,0,0,1,0,0,0,0,
0,0,1,0,0,0,0,0),
nrow = 8, ncol = 8, byrow = TRUE)
rowSums(A)

#4.4
(evc <- abs(eigen(A)$vectors[,1]))

evc / max(evc)

#4.5
B <- matrix(c(
0,0,0,0,0,0,0,0,0,
1,0,1,0,0,0,0,0,0,
1,0,0,0,0,0,0,0,0,
1,1,1,0,0,0,0,0,0,
1,0,0,0,0,1,0,0,0,
0,0,0,0,0,0,0,0,0,
0,0,0,0,0,1,0,0,0,
0,0,0,0,0,1,0,0,0,
0,0,0,0,0,1,0,0,0),
nrow = 9, ncol = 9, byrow = TRUE)
eigen(t(B))$values

B2 <- B
diag(B2)[which(rowSums(B2) == 0)] <- 1
(M <- t(B2 / rowSums(B2)))

n <- nrow(M)
c <- 0.85
M <- (c * M) + ((1 - c) * matrix(1/n, nrow = n, ncol = n))
ev <- eigen(M)$vectors[,1]
rank <- ev / sum(ev)
as.numeric(rank)

#4.6
1 / eigen(A)$values[1]
n <- nrow(A)
b <- 0.2
x <- rowSums(solve(diag(n) - b * A) %*% A)
a <- sqrt(n / sum(x^2))
a * x

#4.7
betweenness.centrality <- function(A) {
 n <- nrow(A)
 Cb <- rep(0, n)
 for (s in 1:n) {
  S <- c()
  P <- vector("list", n)
  g <- rep(0, n); g[s] <- 1
  d <- rep(-1, n); d[s] <- 0
  Q <- c()
  Q <- c(Q, s)
  while(length(Q) != 0) {
   v <- Q[1]; Q <- Q[-1]
   S <- c(v, S)
   ws <- which(A[v,] == 1)
   if (length(ws) != 0) {
    for (i in 1:length(ws)) {
     w <- ws[i]
     if (d[w] < 0) {
      Q <- c(Q, w)
      d[w] <- d[v] + 1
     }
     if (d[w] == d[v] + 1) {
      g[w] <- g[w] + g[v]
      P[[w]] <- c(P[[w]], v)
     } 
    }
   }
  }
  b <- rep(0, n)
  while (length(S) != 0) {
   w <- S[1]; S <- S[-1]
   for (i in 1:length(P[[w]])) {
    v <- P[[w]][i]
    b[v] <- b[v] + (g[v] / g[w]) * (b[w] + 1)
   }
   if (w != s) {
    Cb[w] <- Cb[w] + b[w]
   }
  } 
 }
 Cb / 2
}

Fig4.7 <- matrix(c(
0,1,1,1,0,0,0,
1,0,0,0,1,1,0,
1,0,0,0,0,0,1,
1,0,0,0,0,0,1,
0,1,0,0,0,0,0,
0,1,0,0,0,0,0,
0,0,1,1,0,0,0),
nrow = 7, ncol = 7, byrow = TRUE)

betweenness.centrality(Fig4.7)

#4.8
D <- matrix(c(2,1,1,3), nrow =2)
solve(D)

information.centrality <- function(A) {
 n <- nrow(A)
 B <- 1 - A
 diag(B) <- rowSums(A) + 1
 C <- solve(B)
 T <- diag(C)
 R <- sum(C[1,])
 1 / (T + (sum(T) - 2 * R) / n)
}

Fig4.15 <- matrix(c(
0,1,1,1,
1,0,1,0,
1,1,0,0,
1,0,0,0),
nrow = 4, ncol = 4, byrow = TRUE)

information.centrality(Fig4.15)

#4.9
library(sna)
graphcent(A)
closeness(A)
closeness(B)
closeness(B, cmode = "undirected")
degree(A)
degree(A, gmode = "graph")
degree(B)
degree(B, cmode = "indegree")
degree(B, cmode = "outdegree")
degree(A, gmode = "graph") / (nrow(A) - 1)
evcent(A)
bonpow(A, exponent = 0.2)
betweenness(Fig4.7, gmode = "graph")
betweenness(B)
betweenness(B, cmode = "undirected")
infocent(Fig4.15)
infocent(Fig4.15, rescale = TRUE)

detach(package:sna)

#4.10
library(igraph)

g1 <- graph_from_adjacency_matrix(A, mode = "undirected")
closeness(g1)
(vcount(g1) - 1) * closeness(g1)

g2 <- graph_from_adjacency_matrix(B, mode = "directed")
 (vcount(g2) - 1) * closeness(g2, mode = "all")

degree(g1)
degree(g1) / (vcount(g1) - 1)

degree(g2, mode = "in")
degree(g2, mode = "out")
degree(g2)

eigen_centrality(g1)$vector

g3 <- graph_from_adjacency_matrix(B2, mode = "directed")
page_rank(g3)$vector

power_centrality(g1, exponent = 0.2)

g4 <- graph_from_adjacency_matrix(Fig4.7, mode = "undirected")
betweenness(g4)

betweenness(g2, directed = FALSE)

#4.11
star <- matrix(c(
0,1,1,1,1,1,1,1,
1,0,0,0,0,0,0,0,
1,0,0,0,0,0,0,0,
1,0,0,0,0,0,0,0,
1,0,0,0,0,0,0,0,
1,0,0,0,0,0,0,0,
1,0,0,0,0,0,0,0,
1,0,0,0,0,0,0,0),
nrow = 8, ncol = 8, byrow = TRUE) 
 (A.degree <- rowSums(A))
(star.degree <- rowSums(star))
sum(max(star.degree) - star.degree)
sum(max(A.degree) - A.degree) / 42

detach(package:igraph)
library(sna)
degree(star, gmode = "graph", tmaxdev = TRUE)

centralization(star, graphcent, mode = "graph")  
centralization(star, closeness, mode = "graph")  
centralization(star, degree, mode = "graph")  
centralization(star, betweenness, mode = "graph")

one.dyad <- matrix(c(
0,1,0,0,0,0,0,0,
1,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0),
nrow = 8, ncol = 8, byrow = TRUE)
centralization(one.dyad, evcent, mode = "graph")　
centralization(one.dyad, bonpow, mode = "graph", exponent = 0.2)
centralization(one.dyad, infocent, mode = "graph")

centralization(A, graphcent, mode = "graph")  
centralization(A, closeness, mode = "graph")  
centralization(A, degree, mode = "graph")  
centralization(A, betweenness, mode = "graph") 
centralization(A, evcent, mode = "graph")　
centralization(A, bonpow, mode = "graph", exponent = 0.2)
centralization(A, infocent, mode = "graph") 

library(igraph)
centr_clo(g1)$centralization 
centr_degree(g1, loops = FALSE)$centralization
centr_betw(g1)$centralization
centr_eigen(g1, scale = FALSE)$centralization

#4.12
detach(package:igraph)
library(sna)
edgelist <- matrix(c(1,2,2,5,3,5,4,5,5,6,5,11,7,8,8,9,9,10,8,11,11,16,
12,16,13,14,14,16,15,16,16,17,16,20,16,21,16,22,18,20,19,20,19,28,22,
23,23,24,22,25,22,26,26,27,26,28,26,31,28,29,29,30,31,32,32,33,32,34,
33,34,34,35,31,36,36,37,26,38,38,39,38,40), ncol = 2, byrow = TRUE)
net <- matrix(0, 40,40)
for (i in 1:nrow(edgelist)) 
  net[edgelist[i,1],edgelist[i,2]] <- 1
net <- symmetrize(net)
#中心性を算出する
information <- infocent(net)
betweenness <- betweenness(net, rescale = TRUE) * 100
closeness <- closeness(net)
degree <- degree(net) / (2 * (40 - 1))
#中心性の高さでソートする
si <- sort(information, index.return = TRUE, decreasing = TRUE)
sb <- sort(betweenness, index.return = TRUE, decreasing = TRUE)
sc <- sort(closeness, index.return = TRUE, decreasing = TRUE)
sd <- sort(degree, index.return = TRUE, decreasing = TRUE)
#一覧表にする
centralities <- 
matrix(c(si[[2]],round(si[[1]],3),sb[[2]],round(sb[[1]],1),
sc[[2]],round(sc[[1]],3),sd[[2]],round(sd[[1]],3)),nrow = 40)
colnames(centralities) <- 
c("id","情報中心性","id","媒介中心性","id", "近接中心性","id","次数中心性")
centralities

#4.13
A <- matrix(c(
1,1,0,
1,0,1,
0,1,1,
0,0,1),
nrow = 4, ncol = 3, 
byrow = TRUE)          #4*3の矩形行列
rownames(A) <- paste("n", 1:4, sep = "")
colnames(A) <- paste("m", 1:3, sep = "")
A
(B <- A %*% t(A))      #4*4の正方行列を作る
B[which(B >= 1)] <- 1  #閾値1で二値に変換
diag(B) <- 0           #対角成分0に
B

m <- ncol(A)
n <- nrow(A)
C <- rbind(cbind(matrix(0,m,m),t(A)), cbind(A,matrix(0,n,n)))
colnames(C) <- c(colnames(A), rownames(A))
C

#4.14
ceo_club <- matrix(c(
0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,
0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,
0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,
0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,
0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,
0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,
0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,
0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,
1,0,0,1,0,0,0,1,0,1,0,0,0,0,0,
0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,
0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,
0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,
0,0,1,1,1,0,0,0,1,0,0,0,0,0,0,
0,1,1,1,0,0,0,0,0,0,1,1,1,0,1,
0,1,1,0,0,1,0,0,0,0,0,0,1,0,1,
0,1,1,0,0,1,0,1,0,0,0,0,0,1,0,
0,1,1,0,1,0,0,0,0,0,1,1,0,0,1,
0,0,0,1,0,0,0,0,1,0,0,1,1,0,1,
1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,
0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,
0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,
0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,
0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,
1,0,1,1,0,1,0,0,0,0,0,0,0,0,1,
0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,
0,1,1,0,0,0,0,0,0,0,0,1,0,0,0),
nrow = 26, ncol = 15, byrow = TRUE)
rownames(ceo_club) <- paste("n", 1:26, sep = "")
colnames(ceo_club) <- paste("m", 1:15, sep = "")

ceo <- ceo_club %*% t(ceo_club)
#ceo[which(ceo >= 1)] <- 1
#diag(ceo) <- 0

club <- t(ceo_club) %*% ceo_club
#club[which(club >= 1)] <- 1
#diag(club) <- 0

bipartite <-
rbind(cbind(matrix(0,15,15), t(ceo_club)),
      cbind(ceo_club, matrix(0,26,26)))

library(sna)
(degree.ceo <- degree(ceo, gmode = "graph"))
(degree.club <- degree(club, gmode = "graph"))
(degree.bi <- degree(bipartite, gmode = "graph"))

(bet.ceo <- betweenness(ceo, gmode = "graph", ignore.eval = TRUE))
(bet.club <- betweenness(club, gmode = "graph", ignore.eval = TRUE))
(bet.bi <- betweenness(bipartite, gmode = "graph"))

(ev.ceo <- abs(eigen(ceo)$vectors[,1]))
(ev.club <- abs(eigen(club)$vectors[,1]))
(ev.bi <- abs(eigen(bipartite)$vectors[,1]))

#次数中心性（CEO）
cor(degree.ceo, degree.bi[16:41])
#次数中心（クラブ）
cor(degree.club, degree.bi[1:15])
#媒介中心性（CEO）
cor(bet.ceo, bet.bi[16:41])
#媒介中心性（クラブ）
cor(bet.club, bet.bi[1:15])
#固有ベクトル中心性（CEO）
cor(ev.ceo, ev.bi[16:41])
#固有ベクトル中心性（クラブ）
cor(ev.club, ev.bi[1:15])
