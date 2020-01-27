getwd()
setwd("C:/Users/mas/learning/Network-Analysis-R")

#業種ネットワークの作成
#S01-S50

n <- 1:50
n <- as.character(n)
sen.n <- paste("S", n, sep="")

wgt <- matrix(rchisq(50*50, 1), nrow=50, byrow=T)
diag(wgt) <- 0
wgt <- t(t(wgt) / colSums(wgt))
colSums(wgt)


# 辺リストに変換

wgt.list <- matrix(NA, nrow=2500, ncol=3)

for (j in 1:50) {
  for (i in 1:50){
    wgt.list[(j - 1) * 50 + i, 1] <- i
    wgt.list[(j - 1) * 50 + i, 2] <- j
    wgt.list[(j - 1) * 50 + i, 3] <- wgt[i, j]
  }
}

# ネットワークオブジェクトの生成
library(sna)

attr(wgt.list, "n") <- 50
attr(wgt.list, "vnames") <- sen.n
wgt.adj <- as.sociomatrix.sna(wgt.list)


#circle, random
#エッジの値を示せないか
gplot(wgt.adj, gmode="digraph", mode='fruchtermanreingold', jitter=F, 
      displayisolates=F, 
      edge.lwd=0.1, edge.lty=1, edge.col=rgb(0.2, 0.2, 0.2, 0.3), 
      arrowhead.cex=1.5, thresh=0.1, usecurve=T,
      pad=0.1,
      vertex.cex=1.5, vertex.col=rgb(0.1, 0.1, 0.1, 0.3), 
      vertex.border=rgb(0.5, 0.4, 0.3, 0.3), vertex.lty=2, displaylabels=T,
      label.pos=5, label.cex=0.5)

#ターゲットダイアグラム
#与えた値が大きいほど中央に来る
windows(width=12)
gplot.target(wgt.adj, degree(wgt.adj, cmode="outdegree"),
             edge.col=rgb(0.2, 0.2, 0.2, 0.3), arrowhead.cex=1.5,
             displayisolates=F, thresh=0.1,
             usecurve=T, edge.curve=0.02, vertex.col=rgb(0.1, 0.1, 0.1, 0.3),
             label.pos=5, circ.lty="solid", circ.col="black", circ.rad=(1:5)/4,
             circ.lab.digits=1, circ.lab.cex=1, circ.lab.col="black")

#3Dネットワーク
gplot3d(wgt.adj, gmode="digraph", edge.col="white", edge.alpha=0.5,
        vertex.col="grey", vertex.alpha=0.5, displaylabels=T,
        thresh=0.1, displayisolates=F, 
        label.col="black")


#igraph
library(igraph)


g <- graph_from_edgelist(wgt.list[wgt.list[, 3] > 0.1, c(1, 2)])

V(g)$name <- sen.n
E(g)$weight <- wgt.list[wgt.list[, 3] > 0.1, 3]

#インタラクティブグラフ
tkplot(g, edge.color="grey50", edge.lty=3,
       vertex.color="white", vertex.label.color="black")

#igraph標準
plot(g, vertex.shape="circle", vertex.size=13, vertex.color="grey90",
     vertex.label.cex=0.7, vertex.label.color="black",
     edge.arrow.size=0.6, edge.label=round(E(g)$weight, 2), margin=-0.1,
     edge.label.cex=0.6, layout=layout_with_fr(g, dim=2))

#3D
rglplot(g, layout=layout_with_fr(g, dim=3),
        vertex.color="grey90",
        edge.arrow.size=3,
        new=T
        )


?rglplot





#mode
gplot.layout.adj(d, layout.par)
gplot.layout.circle(d, layout.par)
gplot.layout.circrand(d, layout.par)
gplot.layout.eigen(d, layout.par)
gplot.layout.fruchtermanreingold(d, layout.par)
gplot.layout.geodist(d, layout.par)
gplot.layout.hall(d, layout.par)
gplot.layout.kamadakawai(d, layout.par)
gplot.layout.mds(d, layout.par)
gplot.layout.princoord(d, layout.par)
gplot.layout.random(d, layout.par)
gplot.layout.rmds(d, layout.par)
gplot.layout.segeo(d, layout.par)
gplot.layout.seham(d, layout.par)
gplot.layout.spring(d, layout.par)
gplot.layout.springrepulse(d, layout.par)
gplot.layout.target(d, layout.par)

