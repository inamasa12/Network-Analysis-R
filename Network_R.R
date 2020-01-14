
setwd("C:/Users/mas/learning/Network-Analysis-R")
getwd()


# Chapter1 ----------------------------------------------------------------

#隣接行列
dg <- matrix(c(
  0, 1, 1, 0,
  1, 0, 1, 0,
  0, 0, 0, 0,
  1, 0, 0, 0),
  nrow = 4, ncol = 4, byrow = TRUE)
rownames(dg) <- letters[1:4]
colnames(dg) <- letters[1:4]

#辺リスト
e.list <- c(1, 2, 1, 3, 2, 1, 2, 3, 4, 1)
e.matrix <- matrix(e.list, ncol = 2, byrow = TRUE)
e.matrix

#保存
#txt
write.table(dg,
            file = "adj.txt",
            row.names = FALSE,
            col.names = FALSE)
#csv
write.table(dg,
            file = "adj.csv",
            row.names = FALSE,
            col.names = FALSE,
            sep = ", ")

#読み込み
#txt
g <- as.matrix(read.table("adj.txt"))
#csv
g <- as.matrix(read.csv("adj.csv", header = FALSE))

#二部グラフ
#隣接行列形式
bg <- matrix(c(
  0, 0, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 1,
  1, 1, 0, 0, 0, 0, 0,
  1, 0, 1, 0, 0, 0, 0,
  0, 1, 1, 1, 0, 0, 0),
  nrow = 7)
rownames(bg) <- c("a", "b", "c", "d", "A", "B", "C")
colnames(bg) <- c("a", "b", "c", "d", "A", "B", "C")
bg %*% bg


#snaパッケージ
install.packages("statnet")
library(sna)

#Rの行列を用いる

#辺リスト
edgelist <- matrix(c(
  1, 2, 1,
  1, 3, 1,
  2, 1, 1,
  2, 3, 1,
  4, 1, 1),
  ncol = 3, byrow = TRUE)
attr(edgelist, "n") <- 4
attr(edgelist, "vnames") <- letters[1:4]

as.edgelist.sna(dg)

#隣接行列
as.sociomatrix.sna(edgelist)

#多重グラフ、複数のグラフの集合として扱う
g1 <- matrix(c(
  0, 1, 0,
  1, 0, 1,
  0, 1, 0),
  nrow = 3)
g2 <- matrix(c(
  0, 0, 1,
  0, 0, 1,
  1, 1, 0),
  nrow = 3)
g <- array(dim = c(2, 3, 3))
g[1, , ] <- g1
g[2, , ] <- g2

#重み付きグラフ
#隣接行列形式
wg <-  matrix(c(
  0, 2, 0, 4,
  2, 0, 3, 1,
  0, 3, 0, 0,
  4, 1, 0, 0),
  nrow = 4,
  byrow = TRUE)
#辺リスト形式
wg <- matrix(c(
  1, 2, 2,
  1, 4, 4,
  2, 1, 2,
  2, 3, 3,
  2, 4, 1,
  3, 2, 3,
  4, 1, 4,
  4, 2, 1),
  ncol = 3,
  byrow = TRUE)
attr(wg, "n") <- 4

#二部グラフ
#接続行列形式
bg2 <- matrix(c(
  1, 1, 0,
  1, 0, 1,
  0, 1, 1,
  0, 0, 1),
  nrow = 4,
  byrow = TRUE)
rownames(bg2) <- letters[1:4]
colnames(bg2) <- LETTERS[1:3]
#隣接行列形式
as.sociomatrix.sna(bg2)
#辺リスト形式
bg.edgelist <- matrix(c(
  1, 5, 1,
  1, 6, 1,
  2, 5, 1,
  2, 7, 1,
  3, 6, 1,
  4, 7, 1,
  5, 1, 1,
  5, 2, 1,
  6, 1, 1,
  6, 3, 1,
  7, 2, 1,
  7, 3, 1,
  7, 4, 1),
  ncol = 3,
  byrow = TRUE)
attr(bg.edgelist,"n") <- 7
attr(bg.edgelist, "vnames") <- c(letters[1:4], LETTERS[1:3])
attr(bg.edgelist, "bipartite") <- 4



#igraphパッケージ
install.packages("igraph")
library(igraph)

#独自のグラフオブジェクトを作成する

#隣接行列から
dg <- matrix(c(
  0, 1, 1, 0,
  1, 0, 1, 0,
  0, 0, 0, 0,
  1, 0, 0, 0),
  nrow = 4, ncol = 4, byrow = TRUE)
g1 <- graph_from_adjacency_matrix(dg)
g5 <- graph_from_edgelist(matrix(c(
  "A", "B",
  "C", "D",
  "E", "F"),
  ncol = 2
  , byrow = TRUE))

#辺リストから
#デフォルトは有向グラフ
e.list <- c(1, 2, 1, 3, 2, 1, 2, 3, 4, 1)
g2 <- make_graph(e.list)
e.matrix <- matrix(e.list, ncol = 2, byrow = TRUE)
g3 <- graph_from_edgelist(e.matrix)

#簡便法
g6 <- graph_from_literal(A -+B, C -+ D, E -+ F)

#無向グラフ
g4 <- graph_from_edgelist(matrix(c(
  1, 2,
  1, 3,
  1, 4,
  2, 3),
  ncol = 2
  , byrow = TRUE),
  directed = FALSE)

#多重グラフ
#辺リストから
g7 <- graph_from_edgelist(matrix(c(
  "東京", "神田",
  "東京", "神田",
  "東京", "神田"),
  ncol = 2,
  byrow = TRUE),
  directed = FALSE)
#隣接行列から
g8 <- graph_from_adjacency_matrix(matrix(c(
  0, 3,
  3, 0),
  nrow = 2),
  mode = "undirected")

#重み付きグラフ
#隣接行列から
g8 <- graph_from_adjacency_matrix(matrix(c(
  0, 3,
  3, 0),
  nrow = 2),
  mode = "undirected",
  weighted = TRUE)
#辺リストから
g9 <- graph_from_edgelist(
  matrix(c(
    1, 2,
    1, 4,
    2, 3,
    2, 4),
    ncol = 2,
    byrow = TRUE))
E(g9)$weight <- c(2, 4, 3, 1)

#二部グラフ
g10 <- make_bipartite_graph(type = c(0, 0, 0, 0, 1, 1, 1),
                            edges = c(1, 5, 1, 6, 2, 7, 3, 6, 3, 7, 4, 7))
V(g10)$names <- c(letters[1:4], LETTERS[1:3])

#グラフオブジェクトの属性
E(g1)
E(g8)$weight
V(g5)
V(g1)$names



# Chapter2 ----------------------------------------------------------------

A <- matrix(c(
  0, 1, 0, 0, 0, 1,
  0, 0, 1, 0, 0, 0,
  0, 0, 0, 1, 0, 0,
  0, 1, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0),
  nrow = 6,
  byrow = TRUE)

wg <- matrix(c(
  0, 2, 0, 4,
  2, 0, 3, 1,
  0, 3, 0, 0,
  4, 1, 0, 0),
  nrow = 4,
  byrow = TRUE)

#最短距離

#snaの場合

#counts: 頂点間の最短経路数
#gdist: 最短距離
geodist(A)
geodist(wg, ignore.eval = FALSE)

#到達可能性行列
reachability(A)
is.connected(A, connected = "strong")
is.connected(A, connected = "weak")


#igraphの場合

g1 <- graph_from_adjacency_matrix(A)
g2 <- graph_from_adjacency_matrix(wg, weighted = TRUE)

#最短距離
distances(g1, mode = "out")
distances(g2)
distances(g2, weight = c(2, 4, 2, 3, 1, 3, 4, 1))
distances(g2, weight = NA)

#個別の最短距離
shortest_paths(g1, from = 1, to = 5, mode = "out")$vpath

#平均距離
mean_distance(g1)
mean_distance(g1, unconnected = FALSE)

#最短距離の分布
#res: 最短距離別の頻度
#unconnected: 到達できない頂点数
distance_table(g1)

#到達可能性
is_connected(g1, mode = "strong")
is_connected(g1, mode = "weak")



# Chapter3 ----------------------------------------------------------------
rm(list = ls())

Fig3.1 <- matrix(c(
  0, 1, 1, 1, 1,
  1, 0, 0, 1, 1,
  1, 0, 0, 0, 0,
  1, 1, 0, 0, 0,
  1, 1, 0, 0, 0),
  nrow = 5)

Fig3.2 <- matrix(c(
  0, 1, 1, 1, 1,
  0, 0, 0, 1, 1,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0),
  nrow = 5, byrow = TRUE)

Fig3.5 <- matrix(c(
  0, 1, 0, 0, 1,
  1, 0, 1, 0, 0,
  0, 1, 0, 1, 0,
  1, 0, 1, 0, 0,
  0, 0, 0, 1, 0),
  nrow = 5, 
  byrow = TRUE)

Fig3.8 <- matrix(c(
  0, 0, 1, 1, 0,
  0, 0, 0, 1, 1,
  0, 0, 0, 1, 0,
  0, 0, 0, 0, 0,
  0, 1, 0, 0, 0),
  nrow = 5,
  byrow = TRUE)

A <- matrix(c(
  0, 1, 1, 1, 1, 1, 0, 0,
  1, 0, 1, 0, 0, 0, 1, 0,
  1, 1, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 1, 0, 0, 1,
  1, 0, 0, 1, 0, 1, 0, 0,
  1, 0, 0, 0, 1, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0),
  nrow = 8)


#各種指標

#snaの場合
install.packages("statnet")
library(sna)

#密度（辺の密度）
gden(Fig3.1)
gden(Fig3.2)

#推移性（三角関係の割合）
gtrans(A)

#相互性（双方向のペアの割合）
grecip(Fig3.5, measure = "dyadic.nonnull")
mutuality(Fig3.5)

#互報性（双方向及びつながりがないペアの割合）
grecip(Fig3.5, measure = "dyadic")

#連結性
#互いに到達可能な頂点の割合
connectedness(Fig3.8)

#階層性
#一方向性
hierarchy(Fig3.8, measure = "krackhardt")

#効率性
#各頂点に到達するに不要な辺の少なさ
efficiency(Fig3.8)

#最小上界性
#共通の親を持つペアの割合
lubness(Fig3.8)


#igraphの場合
library(igraph)

g3.1 <- graph_from_adjacency_matrix(Fig3.1, mode = "undirected")
g3.2 <- graph_from_adjacency_matrix(Fig3.2)
g3.3 <- graph_from_adjacency_matrix(A, mode = "undirected")
g3.5 <- graph_from_adjacency_matrix(Fig3.5)

#密度
edge_density(g3.1)
edge_density(g3.2)

#推移性
transitivity(g3.3, type = "global")

#相互性
reciprocity(g3.5, mode = "ratio")


#Example
install.packages("statnet")
library(statnet)
data(package = "sna")
data(package = "ergm")
library(igraph)

#coleman

#二時点の隣接行列
data(coleman)
class(coleman)
dim(coleman)
coleman[1, , ]

#描画
coord1 <- gplot(coleman, g = 1)
windows(width = 14, height = 7)
par(mfrow = c(1, 2))
gplot(coleman, g = 1, coord = coord1, main = "Fall, 1957")
gplot(coleman, g = 2, coord = coord1, main = "Spring, 1958")

#密度
gden(coleman)
sum(coleman[2, , ]) - sum(coleman[1, , ])
#推移性
gtrans(coleman)
#相互性
grecip(coleman, measure = "dyadic.nonnull")
#双方向の関係数
mutuality(coleman)
#連結性
connectedness(coleman)
#階層性
hierarchy(coleman, measure = "krackhardt")
#効率性
efficiency(coleman)
#最小上界性
lubness(coleman)


#karate

#igraphのオブジェクト
karate <- make_graph("Zachary")
plot(karate)
class(karate)

#密度
edge_density(karate)
#推移性
transitivity(karate, type = "global")



# Chapter4 ----------------------------------------------------------------
rm(list = ls())

A <- matrix(c(
  0, 1, 1, 1, 1, 1, 1, 0,
  1, 0, 1, 1, 1, 1, 0, 0,
  1, 1, 0, 0, 0, 0, 0, 1,
  1, 1, 0, 0, 0, 0, 1, 0,
  1, 1, 0, 0, 0, 1, 0, 0,
  1, 1, 0, 0, 1, 0, 0, 0,
  1, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0),
  nrow = 8)

B <- matrix(c(
  0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0),
  nrow = 9, byrow = TRUE)

B2 <- B
diag(B2)[which(rowSums(B2) == 0)] <- 1

Fig4.7 <- matrix(c(
  0, 0, 1, 1, 0, 0, 0,
  1, 0, 0, 0, 1, 1, 0,
  1, 0, 0, 0, 0, 0, 1,
  1, 0, 0, 0, 0, 0, 1,
  0, 1, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0,
  0, 0, 1, 1, 0, 0, 0),
  nrow = 7)

Fig4.15 <- matrix(c(
  0, 1, 1, 1,
  1, 0, 1, 0,
  1, 1, 0, 0,
  1, 0, 0, 0),
  nrow = 4)

star <- matrix(c(
  0, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0),
  nrow = 8, byrow = TRUE)

one.dyad <- matrix(c(
  0, 1, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0),
  nrow = 8, byrow = TRUE)

#sna
library(sna)

#離心中心性
#最大距離の逆数
graphcent(A)
#近接中心性
#最短距離の合計の逆数
closeness(A)
#標準化近接中心性
#有効グラフの場合、強連結でないと算出できない
closeness(B)
closeness(B, cmode = "undirected")
is.connected(B)

#次数中心性
#総次数
degree(A)
degree(B)
#入次数
degree(B, cmode = "indegree")
#出次数
degree(B, cmode = "outegree")
#無向グラフ
degree(A, gmode = "graph")
#標準化
degree(A, gmode = "graph") / (nrow(A) - 1)

#固有ベクトル中心性
evcent(A)

#パワー中心性
bonpow(A, exponent = 0.2)

#媒介中心性
#最短経路に含まれる数
betweenness(Fig4.7, gmode = "graph")
betweenness(B)
betweenness(B, cmode = "undirected")

#情報中心性
#全てのペアの長さの調和平均
infocent(Fig4.15)
infocent(Fig4.15, rescale = TRUE)

#集中度
#離心中心性
centralization(star, graphcent, mode = "graph")
#近接中心性
centralization(star, closeness, mode = "graph")
#次数中心性
centralization(star, degree, mode = "graph")
#媒介中心性
centralization(star, betweenness, mode = "graph")
#固有ベクトル中心性
centralization(one.dyad, evcent, mode = "graph")
#パワー中心性
centralization(one.dyad, bonpow, mode = "graph", exponent = 0.2)
#情報中心性
centralization(one.dyad, infocent, mode = "graph")



#igraph
library(igraph)
g1 <- graph_from_adjacency_matrix(A, mode = 'undirected')
g2 <- graph_from_adjacency_matrix(B, mode = 'directed')
g3 <- graph_from_adjacency_matrix(B2, mode = 'directed')
g4 <- graph_from_adjacency_matrix(Fig4.7, mode = 'undirected')


#近接中心性
closeness(g1)
(vcount(g1) - 1) * closeness(g1)
(vcount(g2) - 1) * closeness(g2, mode = "all")

#次数中心性
degree(g1)
degree(g1) / (vcount(g1) - 1)
degree(g2, mode = "in")
degree(g2, mode = "out")
degree(g2)

#固有ベクトル中心性
eigen_centrality(g1)$vector

#ページランク
page_rank(g3)$vector

#パワー中心性
power_centrality(g1, exponent = 0.2)

#媒介中心性
betweenness(g4)
betweenness(g4, directed = FALSE)

#集中度
library(igraph)
#近接中心性
centr_clo(g1)$centralization
#次数中心性
centr_degree(g1, loops = FALSE)$centralization
#媒介中心性
centr_betw(g1)$centralization
#固有ベクトル中心性
centr_eigen(g1, scale = FALSE)$centralization


#Example

#男性同性愛者の性関係ネットワーク

detach(package:igraph)

library(sna)
library(statnet)

edgelist <- matrix(c(1, 2, 2, 5, 3, 5, 4, 5, 5, 6, 5, 11, 7, 8, 8, 9, 9, 10, 8, 11, 11, 16,
                     12, 16, 13, 14, 14, 16, 15, 16, 16, 17, 16, 20, 16, 21, 16, 22, 18, 20, 19, 20, 19, 28, 22,
                     23, 23, 24, 22, 26, 26, 27, 26, 28, 26, 31, 28, 29, 29, 30, 31, 32, 32, 33, 32, 34,
                     33, 34, 34, 35, 31, 36, 36, 37, 26, 38, 38, 39, 38, 40),  ncol = 2, byrow = TRUE)
nrow(edgelist)
net <- matrix(0, 40, 40)

for (i in 1:nrow(edgelist))
  net[edgelist[i, 1], edgelist[i, 2]] <- 1

net <- symmetrize(net)

#情報中心性
information <- infocent(net)
#媒介中心性
betweenness <- betweenness(net, rescale = TRUE) * 100
#近接中心性
closeness <- closeness(net)
#離心中心性
degree <- degree(net) /  (2 * (40 - 1))

#結果の整形
si <- sort(information, index.return = TRUE, decreasing = TRUE)
sb <- sort(betweenness, index.return = TRUE, decreasing = TRUE)
sc <- sort(closeness, index.return = TRUE, decreasing = TRUE)
sd <- sort(degree, index.return = TRUE, decreasing = TRUE)

centralities <- matrix(c(si[[2]], round(si[[1]], 3),
                       sb[[2]], round(sb[[1]], 1),
                       sc[[2]], round(sc[[1]], 3),
                       sd[[2]], round(sd[[1]], 3)), nrow = 40)
colnames(centralities) <- c("id", "情報中心性", "id", "媒介中心性",
                            "id", "近接中心性", "id", "次数中心性")


#二部グラフの中心性
A <- matrix(c(
  1, 1, 0,
  1, 0, 1,
  0, 1, 1,
  0, 0, 1),
  nrow = 4, byrow = TRUE)

rownames(A) <- paste("n", 1:4, sep = "")
colnames(A) <- paste("m", 1:3, sep = "")

#二部グラフを何らかの隣接行列に変換
#頂点を共有する頂点にエッジを引く
B <- A %*% t(A)
B[which(B >= 1)] <-1
diag(B) <- 0

#二部グラフをそのまま正方行列に変換
m <- ncol(A)
n <- nrow(A)
C <- rbind(cbind(matrix(0, m, m), t(A)), cbind(A, matrix(0, n, n)))
colnames(C) <- c(colnames(A), rownames(A))

#変換して作成した正方行列に対して従来の中心性分析を行う

ceo_club <- matrix(c(
  0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
  0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0,
  1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1,
  0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1,
  0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0,
  0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1,
  0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1,
  1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1,
  0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,
  0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  nrow = 26, byrow = TRUE)

#CEO
rownames(ceo_club) <- paste("n", 1:26, sep = "")
#comitte
colnames(ceo_club) <- paste("m", 1:15, sep = "")

#共通のクラブを有するCEO
ceo <-ceo_club %*% t(ceo_club)
ceo[which(ceo >= 1)] <- 1
diag(ceo) <-0

#共通のCEOを有するクラブ
club <- t(ceo_club) %*% ceo_club
club[which(club >= 1)] <- 1
diag(club) <- 0

#そのまま正方行列に変換
bipartite <- rbind(cbind(matrix(0, 15, 15), t(ceo_club)),
                   cbind(ceo_club, matrix(0, 26, 26)))

library(sna)

#次数中心性
degree.ceo <- degree(ceo, gmode = "graph")
degree.club <- degree(club, gmode = "graph")
degree.bi <- degree(bipartite, gmode = "graph")

#媒介中心性
bet.ceo <- betweenness(ceo, gmode = "graph", ignore.eval = TRUE)
bet.club <- betweenness(club, gmode = "graph", ignore.eval = TRUE)
bet.bi <- betweenness(bipartite, gmode = "graph", ignore.eval = TRUE)

#固有ベクトル中心性
ev.ceo <- abs(eigen(ceo)$vectors[, 1])
ev.club <- abs(eigen(club)$vectors[, 1])
ev.bi <- abs(eigen(bipartite)$vectors[, 1])

cor(degree.ceo, degree.bi[16:41])
cor(degree.club,degree.bi[1:15])

cor(bet.ceo, bet.bi[16:41])
cor(bet.club,bet.bi[1:15])

cor(ev.ceo, ev.bi[16:41])
cor(ev.club,ev.bi[1:15])


# Chapter5 ----------------------------------------------------------------

A <- matrix(c(
  0, 1, 1, 1, 0, 0, 0, 0, 0, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 1, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 
  0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  nrow = 10, byrow = TRUE)

B <- matrix(c(
  0, 1, 1, 0, 1, 0, 0,
  1, 0, 1, 0, 0, 0, 1,
  1, 1, 0, 1, 1, 1, 0,
  0, 0, 1, 0, 1, 1, 1,
  1, 0, 1, 1, 0, 1, 0,
  0, 0, 1, 1, 1, 0, 0,
  0, 1, 0, 1, 0, 0, 0),
  nrow = 7, byrow = TRUE)


g1 <- graph_from_adjacency_matrix(A, mode = "undirected")

library(sna)
library(igraph)

#連結成分

#sna

#構成要素
components(A)
#各種
component.dist(A)
#最大グループ
component.largest(A, result = "membership")
component.largest(A, result = "graph")

#igraph

#各種
components(g1)
#サイズ別のサブグループの割合
component_distribution(g1)
component_distribution(g1, cumulative = TRUE)

#クリーク

#sna
clique.census(B, mode = "graph")

#igraph
g2 <- graph_from_adjacency_matrix(B, mode ="undirected")
max_cliques(g2)
