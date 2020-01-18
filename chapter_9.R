#9.2.1
install.packages("twitteR")
install.packages("base64enc")

library(twitteR)
ConsumerKey <- "*****"
ConsumerSecret <- "*****"
AccesToken <- "*****"
AccessTokenSecret <- "*****"
setup_twitter_oauth(ConsumerKey, ConsumerSecret,
 AccesToken, AccessTokenSecret)

ego <- getUser("RdeManabu")
friendsCount(ego)

(follow.list <- ego$getFriends(n = friendsCount(ego)))
(star <- as.matrix(expand.grid(name(ego), sapply(follow.list, name))))

library(igraph)
star.igraph <- graph_from_edgelist(star)
plot(star.igraph, layout = layout_as_star(star.igraph))

(n.friends <- sapply(follow.list, friendsCount))

friends.list <- list() 
for(i in 1:length(follow.list)) {
 nf <- n.friends[i]
 if (nf > 0) {
  friends.list[[i]] <- follow.list[[i]]$getFriends(n = nf)
 }
 else {
  friends.list[[i]] <- NA
 }
}

egonet.IDs <- c(id(ego), sapply(follow.list, id))
egonet <- star
for(i in 1:length(friends.list)) {
 int.friends <- intersect(names(friends.list[[i]]), egonet.IDs)
 if (length(int.friends) > 0) {
  for (j in 1:length(int.friends)) {
   egonet <- rbind(egonet, c(name(follow.list[[i]]), 
    name(follow.list[[which(names(follow.list) == int.friends[j])]])))
  }
 }
}
egonet

egonet.igraph <- graph_from_edgelist(egonet)
plot(egonet.igraph, layout = layout_as_star(egonet.igraph))

edge_density(egonet.igraph)
reciprocity(egonet.igraph, mode = "ratio")

retweeters("641766384918093824")

st <- showStatus("641766384918093824")
st$retweetCount
st$favoriteCount

user <- "hadleywickham"
tw.data <- twListToDF(userTimeline(user, n = 100))

tw.text <- tw.data$text
targets <- unlist(regmatches(tw.text, gregexpr("@[^ ]*", tw.text)))

mention.net <- expand.grid(user, substring(targets, 2))
head(mention.net)

mention.igraph <- graph_from_data_frame(mention.net)
plot(mention.igraph)

rtweets <- twListToDF(searchTwitter("#rstats", n = 500))
rtweets2 <- rtweets[-which(rtweets$isRetweet == TRUE),]
mention.net2 <- matrix(nrow = 0, ncol = 2)
for (i in 1:nrow(rtweets2)) {
 targets <- 
  unlist(regmatches(rtweets2$text[i], gregexpr("@[^ ]*", rtweets2$text[i])))
 if (length(targets) > 0) {
  mention.net2 <- 
   rbind(mention.net2, expand.grid(rtweets2$screenName[i],substring(targets, 2)))
 }
}
plot(graph_from_data_frame(mention.net2), vertex.size = 8)

#9.2.2
install.packages("rtweet")
library(rtweet)

appname <- "*****"
key <-"*****"
secret <- "*****"
twitter_token <- 
 create_token(app=appname, consumer_key=key, consumer_secret=secret)

ego <- "RdeManabu"
ego.data <- lookup_users(ego)
ego.data$friends_count

follow.list <- get_friends(ego)
follow.data <- apply(follow.list, 2, lookup_users)$user_id
follow.data$screen_name

star <- as.matrix(expand.grid(ego, follow.data$name))
library(igraph)
star.igraph <- graph_from_edgelist(star)
plot(star.igraph, layout = layout_as_star(star.igraph))

(n.friends <- follow.data$friends_count)
friends.list <- list()
for(i in 1:nrow(follow.list)) {
 nf <- n.friends[i]
 if (nf > 0) {
  friends.list[[i]] <- get_friends(follow.list$user_id[i])$user_id
 }
 else {
 friends.list[[i]] <- NA
 }
}
egonet.IDs <- c(ego.data$user_id, follow.data$user_id)
egonet <- star
for(i in 1:nrow(follow.list)) {
 int.friends <- intersect(friends.list[[i]], egonet.IDs)
 if (length(int.friends) > 0) {
  for (j in 1:length(int.friends)) {
   egonet <- rbind(egonet, c(follow.data$name[i],
    follow.data$name[which(follow.list[,2] == int.friends[j])]))
  }
 }
}
egonet.igraph <- graph_from_edgelist(egonet)
plot(egonet.igraph, layout =layout_as_star(egonet.igraph))

status.data <- lookup_statuses(641766384918093824)
status.data$retweet_count
status.data$favorite_count

user <- "hadleywickham"
tw.data <- get_timeline(user, n = 100)
msn <- tw.data$mentions_screen_name
mention.net = matrix(nrow = 0, ncol = 2)
for (i in 1:nrow(tw.data)) {
 if (!is.na(msn[i])) {
  targets <- unlist(strsplit(msn[[i]], split = " "))
  for (j in 1:length(targets)) {
   mention.net <- rbind(mention.net, c(user, targets[j]))
  }
 }
}
mention.igraph <- graph_from_data_frame(mention.net)
plot(mention.igraph)

rtweets <- search_tweets("#rstats", n = 50)
msn2 <- rtweets$mentions_screen_name
edges <- matrix(nrow = 0, ncol = 2)
for (i in 1:nrow(rtweets)) {
 if (!is.na(msn2[i])) {
  targets <- unlist(strsplit(msn2[[i]], split = " "))
  for (j in 1:length(targets)) {
   edges <-
    rbind(edges, c(rtweets$screen_name[i], targets[j]))
  }
 }
}
plot(graph_from_edgelist(edges), vertex.size = 5)

#9.3
#twitteR‚Ìê‡
ego <- getUser("RdeManabu")
ego.TL <- twListToDF(userTimeline(ego))
ego.TL$text <- iconv(ego.TL$text, "UTF-8", "SHIFT_JIS")
tw.text <- ego.TL$text[order(ego.TL$created)]

#rtweet‚Ìê‡
ego <- "RdeManabu"
ego.TL <- get_timeline(ego)
ego.TL$text <- iconv(ego.TL$text, "UTF-8", "SHIFT_JIS")
tw.text <- ego.TL$text[order(ego.TL$created_at)]

#install.packages("RMeCab", repos = "http://rmecab.jp/R")
library(RMeCab)
tw.list <- RMeCabDF(as.data.frame(tw.text), 1)
tw.list[[1]]
noun.list <- lapply(tw.list, function(x){x[which(names(x) == "–¼ŽŒ")]})
(words <- unique(unlist(noun.list)))
words <- setdiff(words, c("‚½‚¿", "‚à‚Ì", "‚»‚±", "‚±‚ê‚ç", "‚»‚ê‚ç"))

word.occurrence <- sapply(noun.list, function(x){is.element(words,x)})
rownames(word.occurrence) <- words
word.adj <- word.occurrence %*% t(word.occurrence)

sort(diag(word.adj), decreasing = TRUE)[1:10]

library(sna)
gplot(word.adj, gmode = "graph", displaylabels = TRUE, label.pos = 5,
 vertex.cex = 0,edge.col = rgb(0,0,0,0.08))

#9.4
library(igraph)
data.url <- "http://networkdata.ics.uci.edu/data/lesmis/lesmis.gml"
lesmis <- read_graph(data.url, format = "gml")
plot(lesmis, vertex.size = 0, vertex.label.cex = 1,
 vertex.label.color = "black")

person <- list()
person[[1]] <- "•‘ "
person[[2]] <- "‚¨’Ê"
person[[3]] <- "–””ª"
person[[4]] <- "‘òˆÁ"
person[[5]] <- "ŽéŽÀ"
person[[6]] <- "‚¨b"
person[[7]] <- "‚¨‹á"
person[[8]] <- "‚¨™"
person[[9]] <- "–³“ñÖ"
person[[10]] <- "“T”n"
person[[11]] <- "‹P­"
person[[12]] <- c("Œ ˜Z","Œ f•ƒ")
person[[13]] <- c("’O¶","’O¶‰q–å","é™")
person.name <- sapply(person, function(x){x[1]})

data <- scan("02chino_maki.txt", what = character())
subtitle <- c(grep("’†Œ©o‚µ", data), grep("’ê–{Fu‹{–{•‘ iˆêjv", data))
(nsec <- length(subtitle) - 1)

data.sec <- rep("", nsec)
for (i in 1:nsec) {
 for (j in (subtitle[i] + 1):(subtitle[i + 1] - 1)) {
  data.sec[i] <- paste(data.sec[i], data[j])
 }
}

nper <- length(person)
occurrence <- matrix(0, nrow = nsec, ncol = nper)
for (i in 1:nper) {
 for (j in 1:length(person[[i]])) {
  occurrence[grep(person[[i]][j], data.sec),i] <- 1
 }
}
colnames(occurrence) <- person.name

head(occurrence)

library(sna)
par(mar = c(0,0,0,0))
gplot(occurrence, gmode = "twomode", usearrows = FALSE,
 vertex.cex = c(rep(1.5, nsec), rep(2.2, nper)),
 vertex.col = c(cm.colors(nsec), rep("white", nper)),
 vertex.sides = 50, edge.col = rgb(0,0,0,0.2), 
 displaylabels = TRUE, label = c(1:nsec, person.name),
 label.pos = 5, label.cex = c(rep(0.7, nsec), rep(0.6, nper)))

(prsn.adj <- t(occurrence) %*% occurrence)

gplot(prsn.adj, gmode = "graph", thresh = 3, 
 vertex.col = "white", vertex.cex = 0.5 + log(diag(prsn.adj)) / 3, 
 edge.col = rgb(0,0,0,0.2), edge.lwd = 2 * log(prsn.adj),
 displaylabels = TRUE, label.pos = 5, label.cex = 0.7)