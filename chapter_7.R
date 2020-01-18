#7.2
a <- matrix(c(
0,1,0,0,0,
1,0,1,0,0,
0,1,0,1,1,
0,0,1,0,1,
0,0,1,1,0),
nrow = 5, ncol = 5)
b <- matrix(c(
0,1,0,0,1,
1,0,1,0,0,
0,1,0,1,1,
0,0,1,0,1,
1,0,1,1,0),
nrow = 5, ncol = 5)
library(sna)
gcor(a, b, mode = "graph")

result <- c()
for (i in 1:5) {
 for (j in (1:5)[-i]) {
  for (k in (1:5)[-c(i,j)]) {
   for (l in (1:5)[-c(i,j,k)]) {
    m <- (1:5)[-c(i,j,k,l)]
    result <- c(result, 
     gcor(a,b[c(i,j,k,l,m),c(i,j,k,l,m)], mode = "graph"))
   }
  }
 }
}
 
table(result)
table(round(result,4)) / 120
barplot(table(round(result,4)))

gstack <- array(dim = c(2,5,5))
gstack[1,,] <- a
gstack[2,,] <- b
qap <- qaptest(gstack, gcor, g1 = 1, g2 = 2)
summary(qap)

#7.3
library(sna)
interaction <- matrix(0, 20, 20)
observation <- c(2,10,4,5,5,9,7,4,3,3,7,3,2,5,1,4,1,0,1,
5,1,3,1,4,2,6,2,5,4,3,2,2,6,3,1,1,1,8,9,5,11,7,8,8,14,
17,9,11,11,5,9,4,6,5,4,0,3,4,2,3,5,3,11,4,7,0,4,3,3,0,
3,5,7,4,3,5,6,3,4,4,1,2,1,3,3,5,2,3,2,2,4,4,3,1,1,2,0,
1,2,5,4,6,3,9,5,5,4,2,6,3,2,2,3,0,3,4,2,1,3,0,1,1,1,0,
1,3,2,4,5,4,3,4,1,3,2,4,7,5,5,7,2,2,3,3,2,9,3,4,4,2,4,
2,3,1,7,7,8,3,7,2,4,3,8,11,3,8,2,5,3,8,1,5,4,4,1,2,5,2,
2,1,6,1,0,1,4,3,3,2,1,6)
interaction[lower.tri(interaction)] <- observation
interaction <- symmetrize(interaction, rule = "lower")
rownames(interaction) <- colnames (interaction) <-
 paste(1:20,c(rep("M",5),rep("F",15)), sep = "")

gplot(interaction, gmode = "graph", displaylabels = TRUE,
 label.pos = 5, vertex.col = "white",
 vertex.cex = c(rep(2.5,5),rep(2,15)),
 vertex.sides = c(rep(4,5),rep(50,15)))

make.pat.matrix3 <- function(x) {
 pat.matrix <- matrix(1, length(x), length(x))
 pat.matrix[which(x == 0), which(x == 0)] <- 0
 diag(pat.matrix) <- 0
 return(pat.matrix)
}

male.core <- make.pat.matrix3(c(rep(1,5),rep(0,15)))

rep.result <- 1:1000
n <- nrow(interaction)
for (i in 1:1000) {
 perm <- sample(1:n, n)
 rep.result[i] <- gcor(male.core, interaction[perm, perm])
}
sum(rep.result >= gcor(male.core, interaction)) / 1000

hist(rep.result)
abline(v = gcor(male.core, interaction), col = "red")

gstack2 <- array(dim = c(2,20,20))
gstack2[1,,] <- make.pat.matrix3(c(rep(1,5),rep(0,15)))
gstack2[2,,] <- interaction
qap2 <- qaptest(gstack2, gcor, g1 = 1, g2 = 2)
summary(qap2)

plot(qap2)

#7.4
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

library(sna)
gcor(ADVICE, FRIEND)

sum(ADVICE)
gden(ADVICE)
sum(FRIEND)
gden(FRIEND)

rep.cor <- 1:1000
for (i in 1:1000) {
 g1 <- rgraph(21, tprob = gden(ADVICE))
 g2 <- rgraph(21, tprob = gden(FRIEND))
 rep.cor[i] <- gcor(g1, g2)
}
sum(rep.cor >= gcor(ADVICE, FRIEND)) / 1000

hist(rep.cor)
abline(v = gcor(ADVICE, FRIEND), col = "red")

gstack3 <- array(dim = c(2,21,21)) 
gstack3[1,,] <- ADVICE
gstack3[2,,] <- FRIEND
cug.gcor <- cugtest(gstack3, gcor)
summary(cug.gcor)

plot(cug.gcor)

gtrans(FRIEND)

rep.trans <- 1:1000
n <- nrow(FRIEND)
for (i in 1:1000) {
 rep.trans[i] <- gtrans(rgraph(n, tprob = gden(FRIEND)))
}
sum(rep.trans >= gtrans(FRIEND)) / 1000
hist(rep.trans, xlim = c(0.1,0.5))
abline(v = gtrans(FRIEND), col = "red")

cug.gtrans <- cug.test(FRIEND, gtrans, cmode = "edges")
print(cug.gtrans)
plot(cug.gtrans)

rep.dens <- 1:1000
n <- nrow(FRIEND)
for (i in 1:1000) {
 rep.dens[i] <- gden(rgraph(n, tprob = 0.5))
}
sum(rep.dens <= gden(FRIEND)) / 1000
hist(rep.dens, xlim = c(0.2,0.6))
abline(v = gden(FRIEND), col = "red")

(cug.gden <- cug.test(FRIEND, gden, cmode = "size"))
plot(cug.gden)

(cug.gtrans2 <- cug.test(FRIEND, gtrans, cmode = "size"))
plot(cug.gtrans2)

dyad.census(FRIEND)

(cug.gtrans3 <- cug.test(FRIEND, gtrans, cmode = "dyad.census"))
plot(cug.gtrans3)

#7.5.3
i <- 0:20
theta <- c(0.25, 0.5, 1)
for (t in 1:3) {
 plot(i, 1 - (1 - exp(-1 * theta[t]))^i,
  type = "l", ylim = c(0,1), lty = t)
 par(new = TRUE)
}
legend("center", legend = theta, lty = 1:3)

#7.5.4
library(statnet)
data(florentine)
flomarriage
class(flomarriage)
flomarriage %v% "vertex.names"
flomarriage %v% "wealth"

plot(flomarriage, vertex.cex = (flomarriage %v% "wealth")/25,
displaylabels = TRUE)

flomarriage.model.0 <- ergm(flomarriage ~ edges)
summary(flomarriage.model.0)
exp(flomarriage.model.0$coef) / (1 + exp(flomarriage.model.0$coef))

flomarriage.model.1 <- ergm(flomarriage ~ edges + triangle)
summary(flomarriage.model.1)

flomarriage.model.2 <- ergm(flomarriage ~ edges + nodecov("wealth"))
summary(flomarriage.model.2)

flomarriage.model.0$mle.lik
flomarriage.model.1$mle.lik
flomarriage.model.2$mle.lik

data(sampson)
samplike

plot(samplike, vertex.cex = 3, vertex.col = "white",
arrowhead.cex = 1.5, displaylabels = TRUE, label.pos = 5,
label = 1:18)

samplike.model.1 <- ergm(samplike ~ edges + sender + receiver)
summary(samplike.model.1)

samplike.model.2 <- ergm(samplike ~ edges + receiver + mutual)
summary(samplike.model.2)

gof.samplike.model.2 <- gof(samplike.model.2)

windows()
par(mfrow = c(2,3), mar = c(4,4,3,1))
plot(gof.samplike.model.2)

data(faux.mesa.high)
(fmh <- faux.mesa.high)

table(fmh %v% "Grade")
table(fmh %v% "Sex")

windows()
plot(fmh, vertex.col = "Grade",
vertex.side = ((fmh %v% "Sex"== "F")*17 + 3))
legend("topright", legend = 7:12, fill = 7:12, title  ="Grade")
legend("bottomright", legend = c("Female", "Male"),
pch = 1:2, title = "Sex")

mixingmatrix(fmh, "Grade")
mixingmatrix(fmh, "Sex")

fmh.model.1 <- 
ergm(fmh ~ edges + nodefactor("Grade") + nodefactor("Sex")) 
summary(fmh.model.1)

fmh.model.2 <-
ergm(fmh ~ edges + nodefactor("Grade") + nodefactor("Sex") +
nodematch("Grade", diff = TRUE) + nodematch("Sex")) 
summary(fmh.model.2)

fmh.model.3 <- ergm(fmh ~ edges + gwesp(0.25, fixed = TRUE))
summary(fmh.model.3)

gof.fmh.model.3 <- gof(fmh.model.3, 
GOF = ~ degree + espartners + distance - model)

windows(width = 15)
par(mfrow = c(1,4))
plot(gof.fmh.model.3)

data(coleman)
fall <- coleman[1,,]
spring <- coleman[2,,]
sum(spring * fall) / sum(fall)

coleman.model.1 <- ergm(spring ~ edges + edgecov(fall))
summary(coleman.model.1)
exp(sum(coleman.model.1$coef)) / (1 + exp(sum(coleman.model.1$coef)))

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
advice <- as.network(ADVICE)

age <- c(33,42,40,33,32,59,55,34,62,37,46,
34,48,43,40,27,30,33,32,38,36)
tenure <- c(9.333,19.583,12.75,7.5,3.333,
28,30,11.333,5.417,9.25,27,8.917,0.25,10.417,
8.417,4.667,12.417,9.083,4.833,11.667,12.5)
dpt <- c(4,4,2,4,2,1,0,1,2,3,3,1,2,2,2,4,1,3,2,2,1)
level <- c(3,2,3,3,3,3,1,3,3,3,3,3,3,2,3,3,3,2,3,3,2)
advice %v% "age" <- age
advice %v% "tenure" <- tenure
advice %v% "dpt" <- dpt
advice %v% "level" <- level

diff.age <- abs(sweep(matrix(age, nrow = 21, ncol = 21), 2, age))
diff.tenure <-
 sweep(matrix(tenure, nrow = 21, ncol = 21, byrow = TRUE), 1, tenure)
diff.level <- sweep(matrix(level, nrow = 21, ncol = 21), 2, level)

advice.model.1 <- ergm(advice ~ edges + edgecov(diff.age) +
 edgecov(diff.tenure) + edgecov(diff.level) +  nodefactor("dpt") +
 nodematch("dpt") + mutual)
summary(advice.model.1)

degree(advice, cmode = "indegree")
median(degree(advice, cmode = "indegree"))

advice.model.2 <- ergm(advice ~ edges + edgecov(diff.age) +
 edgecov(diff.tenure) + edgecov(diff.level) +  nodefactor("dpt") +
 nodematch("dpt") + receiver(base = 13) + mutual)
summary(advice.model.2)

gof.advice.model.2 <- gof(advice.model.2)


windows()
par(mfrow = c(2,3), mar = c(4,4,3,1))
plot(gof.advice.model.2)

#7.6.1
install.packages("RSiena", repos = "http:/R-Forge.R-project.org")
#上記コードが作動しない場合は、下の例のようにCRANからインストールする
#install.packages("RSiena")

library(RSiena)
?s50

library(sna)
windows(width = 12,height = 4)
par(mfrow = c(1,3), mar = c(0.5,0.5,3,0.5))
gplot(s501, main = "s501") -> coord1
gplot(s502, main = "s502", coord = coord1)
gplot(s503, main = "s503", coord = coord1)

friendshipData <- array(c(s501, s502, s503), dim = c(50,50,3))
friendship <- sienaDependent(friendshipData)
alcohol <- varCovar(s50a)
smoke1 <- coCovar(s50s[,1])

mydata <- sienaDataCreate(friendship, alcohol, smoke1)
mydata

myeff <- getEffects(mydata)
effectsDocumentation(myeff)
myeff
myeff <- includeEffects(myeff, egoX, interaction1 = "alcohol")
myeff <- includeEffects(myeff, altX, egoXaltX, interaction1 = "alcohol")
myeff <- includeEffects(myeff, simX, interaction1 = "smoke1" )
myeff

myalgorithm <- sienaAlgorithmCreate(projname = "s50")
ans <- siena07(myalgorithm, data = mydata, effects = myeff)
summary(ans)
(t_statistic <- ans$theta / ans$se)
(p_value <- 2 * pnorm(-abs(t_statistic)))
round(p_value, 3)
siena.table(ans, type = "html", sig = TRUE)

ans <- siena07(myalgorithm, data = mydata, effects = myeff,
 returnDeps = TRUE)

gofi <- sienaGOF(ans, IndegreeDistribution, varName = "friendship",
 cumulative = FALSE, join = TRUE)
windows()
plot(gofi)
table(c(colSums(s502), colSums(s503)))

friendshipData <- array(c(s501, s502, s503), dim = c(50,50,3))
friendship <- sienaDependent(friendshipData)

drinkingbeh <- sienaDependent(s50a, type = "behavior")
myCoEvolutionData <- sienaDataCreate(friendship, drinkingbeh)
myCoEvolutionEff <- getEffects(myCoEvolutionData)
myCoEvolutionEff

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, egoXaltX,
                    interaction1 = "drinkingbeh")
CoEvAlgorithm <- sienaAlgorithmCreate(projname = 's50CoEv')
CoEvAns <- siena07(CoEvAlgorithm, data = myCoEvolutionData,
        effects = myCoEvolutionEff)
summary(CoEvAns)
round(2 * pnorm(-abs(CoEvAns$theta / CoEvAns$se)), 3)
siena.table(CoEvAns, type = "html", sig = TRUE)

myCoEvolutionEff <- includeEffects(myCoEvolutionEff,
                    name = "drinkingbeh", totSim,
                    interaction1 = "friendship")

CoEvAns2 <- siena07(CoEvAlgorithm, data = myCoEvolutionData,
        effects = myCoEvolutionEff)
summary(CoEvAns2)
round(2 * pnorm(-abs(CoEvAns2$theta / CoEvAns2$se)), 3)
siena.table(CoEvAns2, type = "html", sig = TRUE)