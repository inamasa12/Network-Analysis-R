
getwd()
setwd("C:/Users/mas/learning/Network-Analysis-R")


# sna

library(sna)

# ランダムなネットワークを生成（隣接行列）
# matrix型
rgraph(10)

gplot(rgraph(10), mode="circle")

# 力学的モデル（つながった点が近い）
gplot(rgraph(10), mode="fruchtermanreingold")
gplot(rgraph(10), mode="kamadakawai")
gplot(rgraph(10), mode="spring")

# 固有ベクトル（構造的同値が近い）
gplot(rgraph(10), mode="eigen")
gplot(rgraph(10), mode="hall")
gplot(rgraph(10), mode="princoord")

# 多次元尺度法（構造的同値が近い）
gplot(rgraph(10), mode="mds")

# ex
gplot(rgraph(10), mode="fruchtermanreingold", pad=0.1, edge.lwd=0.2)
class(rgraph(10))

library(ergm)

# 異なる無向グラフを一つの隣接行列で表現（単純グラフなら可能）
data(florentine)
class(flomarriage)
class(marg)
# matrix型の隣接行列に変換
# 婚姻関係（上三角）
marg <- as.sociomatrix.sna(flomarriage)
marg[lower.tri(marg)] <- 0
# ビジネス関係（下三角）
bsns <- as.sociomatrix.sna(flobusiness)
bsns[upper.tri(bsns)] <- 0
# 初めに頂点の座標軸を取得しておく
flo.coord <- gplot(marg + bsns)
ncol(marg)

# 描画
# 余白と背景色
par(mar=c(1, 1, 1, 1), bg="grey95") 
# 婚姻関係の辺と頂点
gplot(marg, gmode="graph", coord=flo.coord, jitter=F,
      usecurve=T, edge.col=rgb(0.8, 0.8, 0.5, 0.5), edge.lwd=5,
      vertex.cex=0.3, vertex.col="white", vertex.border="white")
# ビジネス関係の辺と頂点、頂点のラベル
# 頂点の大きさは次数の対数値に比例させている
gplot(bsns, gmode="graph", coord=flo.coord, jitter=F,
      usecurve=T, edge.col=rgb(0.2, 0.2, 0.5, 0.5), edge.lwd=5,
      vertex.cex=0.3, vertex.col="white", vertex.border="white",
      displaylabels=T, label.pos=3, label.col=rgb(0.1, 0.1, 0.1, 0.8),
      label.cex=log(degree(marg+bsns)+2)/2 , new=F)
# 凡例
legend("bottomleft", col=c(rgb(0.8, 0.8, 0.5, 0.5), rgb(0.2, 0.2, 0.5, 0.3)),
       lty=1, lwd=5, cex=1, c("marriage", "business"))


class(coleman)
typeof(coleman)

# 頂点の色分け
data(coleman)
?coleman
# 二期の交友関係を合計
coleman2 <- coleman[1,,] + coleman[2,,]
indegree <- degree(coleman2, cmode="indegree") + 1
# 両端の色を設定
myPalette <- colorRampPalette(c("blue", "yellow"))
# 色の方向と階層を設定
colors <- myPalette(max(indegree))
par(mar=c(0, 0, 0, 0), bg="grey10")
gplot(coleman2, displayisolates=F, usecurve=T,
      edge.col=rgb(0, 0.6, 1, 0.5), edge.lwd=coleman2 * 3,
      vertex.cex=0.8, vertex.col=colors[indegree],
      vertex.border="white")

# ターゲットダイアグラム
data(sampson)
par(mar=c(0, 0, 0, 0), bg="grey70")
gplot.target(samplike, degree(samplike, cmode="indegree"),
             edge.col=rgb(0, 0, 0, 0.5), edge.lwd=2, arrowhead.cex=0.5,
             usecurve=T, edge.curve=0.02, vertex.col=rgb(1, 1, 1, 0.8),
             #displaylabels=T,
             label.pos=5, label=1:18, 
             circ.lty = "solid", circ.col = "grey90", 
             circ.rad=(1:5)/5, # 同心円の半径
             circ.lab.digits=0, # ラベルの桁数 
             circ.lab.cex=1, circ.lab.col=rgb(1, 1, 1, 0.5))

class(samplike)
as.sociomatrix.sna(samplike)
degree(samplike, cmode="indegree")

# 3Dプロット
install.packages("rgl")
library(rgl)
gplot3d(samplike, edge.col="white", edge.alpha=0.5,
        vertex.col="grey", vertex.alpha=0.5, displaylabels=T,
        label.col="black")


# igraph

library(igraph)

g <- make_graph("Zachary")
tkplot(g, edge.color="grey50", edge.lty=3,
       vertex.color="white", vertex.label.color="black")

edgelist <- matrix(
  c(1,1,1,2,3,4,4,4,4,4,4,5,5,6,8,9,9,11,12,12,13,14,14,15,15,15,15,
    3,7,10,7,7,2,5,6,8,10,11,6,13,7,7,2,6,6,6,10,6,2,10,3,8,10,12),
  ncol=2)

#座標軸の算出
foodweb <- graph_from_edgelist(edgelist)
V(foodweb)$name <- c("Bear", "Bird", "Deer", "Fox", "Gartersnake",
                     "Insect", "Plant", "Rabbit", "Racoon", "Rodent",
                     "Salamander", "Skunk", "Toad", "Wildcat", "Wolf")

foodweb2 <- layout_with_sugiyama(foodweb, attributes="all")$extd_graph

n <- vcount(foodweb)
m <- vcount(foodweb2) - n
windows(width=14)
par(mfrow=c(1, 2), mar=c(1,1,1,1))
plot(foodweb, vertex.shape="rectangle",
     vertex.color="white", vertex.size=nchar(V(foodweb)$name)*4)

plot(foodweb2, vertex.shape=c(rep("rectangle", n), rep("none", m)),
     vertex.size=c(nchar(V(foodweb)$name)*2.5, rep(0, m)),
     vertex.size2=8, vertex.color="white", vertex.label.cex=0.6)

windows(width=14)
par(mfrow=c(1, 2))
par(mar=c(0,0,0,0), cex=2)
pie(rep(1, 8), col=1:8)
pie(rep(1, 8), col=categorical_pal(8))

g <- make_graph("Zachary")
rglplot(g, layout=layout_with_fr(g, dim=3))


# ggnetwork
# network(sna)オブジェクトを描画

library(ggplot2)
install.packages("ggnetwork")
library(ggnetwork)

library(network)
library(ergm)
library(sna)
data(faux.mesa.high)
#次数中心性を属性として追加
faux.mesa.high %v% "Degree" <- degree(faux.mesa.high)
library(ggnetwork)
#aes: ggplotにおけるパラメータ設定関数
fmh <- ggnetwork(faux.mesa.high)
#ggplotではgeom関数を用いて図を足していく
ggplot(fmh, aes(x, y, xend=xend, yend=yend)) +
geom_edges(size=1, color="grey") +
geom_nodes(aes(shape=Race, color=Sex, size=Degree)) +
geom_nodetext(aes(label=Grade), size=3, color="grey40") +
theme_blank()

class(faux.mesa.high)

# GGally
# network(sna)オブジェクトを描画
# igraphオブジェクトもintergraphパッケージを用いれば描画可能
install.packages("GGally")
library(network)
URL <- "https://raw.githubusercontent.com/briatte/ggnet/master/inst/extdata/"
nodeURL <- paste(URL, "nodes.tsv", sep="")
edgeURL <- paste(URL, "network.tsv", sep="")
#
nodes <- read.csv(nodeURL, sep="\t")
edges <- read.csv(edgeURL, sep="\t")
MPnet <- network(edges, directed=T)
#頂点の名前リストをDataFrameに変換、339
party <- data.frame(Twitter=network.vertex.names(MPnet))
#頂点のnodesを抽出し、Groupeを選択
party <- merge(party, nodes, by="Twitter", sort=F)$Groupe
#ネットワークの属性としてGroupeを追加
MPnet %v% "Party" <- as.character(party)
library(GGally)
ggnet2(MPnet, color="Party", palette="Set2",
      alpha=0.75, size=4, edge.alpha=0.5)

#地図との組み合わせ
install.packages("maps")
install.packages("geosphere")
library(network)
library(sna)
library(maps)
library(ggplot2)
library(GGally)

URL <- "http://snatool.g2.xrea.com/sampledata/"
airports <- read.csv(paste(URL, "airport.csv", sep="\t"), header=T)
rownames(airports) <- airports$airport
class(flights)
head(flights)
flights <- read.csv(paste(URL, "top50.csv", sep=""), header=T)

flights.net <- network(flights[, 1:2], directed=F)
flights.net %v% "lat" <- airports[network.vertex.names(flights.net), "latitude"]
flights.net %v% "lon" <- airports[network.vertex.names(flights.net), "longitude"]
flights.net %v% "type" <- as.character(airports[network.vertex.names(flights.net), "type"])
flights.net %v% "degree" <- degree(flights.net, gmode="graph")

jpn <- ggplot(map_data("world2", "japan"), aes(x=long, y=lat)) +
  geom_polygon(aes(group=group), color="grey65", fill="grey90")
ggnetworkmap(jpn, flights.net, size=7, node.group=type,
          node.alpha=0.6, weight=degree, ring.group=degree,
          great.circle=T, segment.color="steelblue",
          segment.alpha=0.4)



install.packages("ggraph")
library(ggraph)
head(highschool)
library(igraph)
coleman <-  graph_from_data_frame(highschool)
E(coleman)$year <- as.character(E(coleman)$year)
V(coleman)$degree <- degree(coleman)

ggraph(coleman) +
  geom_edge_arc(arrow=arrow(angle=20, length=unit(2,"mm")), curvature=0.2, aes(color=year)) +
  geom_node_point(aes(size=degree), col="gray20", alpha=0.5) +
  theme_graph()


install.packages("networkD3")
library(networkD3)
data(MisNodes)
data(MisLinks)
head(MisNodes)
head(MisLinks)
(LesMis <- forceNetwork(Links=MisLinks, Nodes=MisNodes,
                        Source="source", Target="target", Value="value",
                        NodeID="name", Group="group", opacity=0.8))
saveNetwork(LesMis, file="LesMis.html", selfcontained=F)



install.packages("visNetwork")
library(visNetwork)
library(networkD3)
data(MisNodes)
data(MisLinks)
nodes <- data.frame(id=0:(nrow(MisNodes)-1), label=MisNodes$name, group=MisNodes$group)
edges <- data.frame(from=MisLinks$source, to=MisLinks$target)
head(nodes)
head(edges)
(LesMis2 <- visNetwork(nodes, edges, height="700px", width="100%"))
visSave(LesMis2, file="LesMis2.html", selfcontained=FALSE)


# コードダイアグラム
install.packages("circlize")
library(circlize)
URL <- "http://snatool.g2.xrea.com/sampledata/"
airports <- read.csv(paste(URL, "airport.csv", sep=""), header=T, fileEncoding="cp932")
flights <- read.csv(paste(URL, "top50.csv", sep=""), header=T, fileEncoding="cp932")
library(RColorBrewer)
my.palette <- rep(c(brewer.pal(8, "Set2"), brewer.pal(9, "Set1")), 2)
chordDiagram(flights, order=as.character(airports$airport),
             grid.col=my.palette, transparency=0.6,
             annotationTrack="grid", preAllocateTracks=1)
circos.trackPlotRegion(track.index=1,
                       panel.fun=function(x, y){
                         xlim=get.cell.meta.data("xlim")
                         ylim=get.cell.meta.data("ylim")
                         sector.name=get.cell.meta.data("sector.index")
                         circos.text(mean(xlim), ylim[1] + 0.1, sector.name,
                                     facing="clockwise", niceFacing=T, adj=c(0, 0.5),
                                     cex=0.7)
                       }, bg.border=NA)


# アークダイアグラム
install.packages("devtools")
library(devtools)
install_github("gastonstat/arcdiagram")
library(arcdiagram)

URL <- "http://snatool.g2.xrea.com/sampledata/"
airports <- read.csv(paste(URL, "airport.csv", sep=""), header=T, fileEncoding="cp932")
flights <- read.csv(paste(URL, "top50.csv", sep=""), header=T, fileEncoding="cp932")
edgelist <- as.matrix(flights[, 1:2])
names <- unique(as.vector(t(edgelist)))
col <- c("darkgoldenrod1", "cyan2")

windows(width=12)
arcplot(edgelist, lwd.arcs=flights[,3]/10^6, labels=names,
        ordering=as.vector(airports$airport), col.label="grey30",
        col.arcs=hsv(0, 0, 0.2, 0.25), cex.nodes=2,
        col.nodes=col[as.integer(airports[,"type"])],
        pch.nodes=18 - as.integer(airports[,"type"]))



#ハイブプロット
#ノードの座標に意味がある

#有向グラフの隣接行列
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

tenure <- c(9.333,19.583,12.75,7.5,3.333,
            28,30,11.333,5.417,9.25,27,8.917,0.25,10.417,
            8.417,4.667,12.417,9.083,4.833,11.667,12.5)
dpt <- c(4,4,2,4,2,1,0,1,2,3,3,1,2,2,2,4,1,3,2,2,1) 
level <- c(3,2,3,3,3,3,1,3,3,3,3,3,3,2,3,3,3,2,3,3,2)



dim(ADVICE)
length(tenure)
length(dpt)
length(level)

install.packages("HiveR")
library(HiveR)
library(RColorBrewer)

rownames(ADVICE) <- 1:nrow(ADVICE)
colnames(ADVICE) <- 1:ncol(ADVICE)
advHPD <- adj2HPD(ADVICE + t(ADVICE))

advHPD$nodes$axis <- as.integer(dpt + 1)
advHPD$nodes$radius <- tenure
advHPD$nodes$size <- colSums(ADVICE)/10
advHPD$nodes$color <- brewer.pal(3, "Greys")[level]
head(advHPD$nodes)

advHPD$edges$color <- brewer.pal(3, "Set2")[advHPD$edges$weight]
head(advHPD$edges)

advHPD$axis.cols <- RColorBrewer::brewer.pal(5, "Set1")
plotHive(advHPD, ch=0, bkgnd="grey10",
         axLabs=paste("Dpt.", 0:4), axLab.pos=5)


plotHive(advHPD, ch=0, method="rank", bkgnd="grey10",
         axLabs=paste("Dpt.", 0:4), axLab.pos=1,
         rot=c(90, -18, 54, -54, 18))




library(igraph)
advice_igraph <- graph_from_adjacency_matrix(ADVICE)
V(advice_igraph)$tenure <- tenure
V(advice_igraph)$dpt <- paste("Dpt.", dpt)
library(ggraph)
library(purrr)
install.packages("purrr")

plot(advice_igraph)
ggraph(advice_igraph, layout = "hive", axis = "dpt", sort.by = "tenure") +
  geom_edge_hive(aes(alpha = ..index..), show.legend = FALSE) +
  geom_node_point() +
  geom_axis_hive() +
  coord_fixed() +
  theme_graph()


ggraph(advice_igraph, layout="hive", axis="dpt", sort.by="tenure") +
  geom_edge_hive(aes(alpha=..index..), show.legend=FALSE) +
  geom_node_point() +
  geom_axis_hive() +
  coord_fixed() +
  theme_graph()

?ggraph

library(sna)
library(network)
URL <- "http://snatool.g2.xrea.com/sampledata/"
flights <- read.csv(paste(URL, "top50.csv", sep=""), header=T, fileEncoding="cp932")
flights.net <- network(flights[, 1:2], directed = FALSE)
pdf(file="airports.pdf", family="Japan1")
gplot(flights.net, gmode="graph", vertex.cex=0,
      edge.col="grey50", displaylabels=T, label.pos=5)
dev.off()

InterData <- Data1[Data1[, 1] %in% Data2[, 1],] #共通部分の抽出

#パッケージ保存先のパス
.libPaths()

変更前 > URL <- "http://www.geocities.jp/snatool/sampledata/"
変更後 > URL <- "http://snatool.g2.xrea.com/sampledata/"

変更前 > install_github('arcdiagram', username='gastonstat')
変更後 > install_github("gastonstat/arcdiagram")
