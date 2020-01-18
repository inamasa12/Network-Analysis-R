#8.1.2
library(foreign)
gss85 <- read.spss("GSS1985.sav", use.value.labels = TRUE,
 to.data.frame=TRUE, max.value.labels = 5)
attach(gss85)

net.size <- table(NUMGIVEN)
addmargins(net.size)
round(100 * prop.table(net.size), 1)

numgiven2 <- NUMGIVEN
numgiven2[numgiven2 == 6] <- 6.5
mean(numgiven2, na.rm = TRUE)

kin <- 2 - cbind(
 SPOUSE1, SPOUSE2, SPOUSE3, SPOUSE4, SPOUSE5,
 PARENT1, PARENT2, PARENT3, PARENT4, PARENT5,
 SIBLING1, SIBLING2, SIBLING3, SIBLING4, SIBLING5,
 CHILD1, CHILD2, CHILD3, CHILD4, CHILD5,
 OTHFAM1, OTHFAM2, OTHFAM3, OTHFAM4, OTHFAM5)
kin.sum <- rowSums(kin, na.rm = TRUE)
kin.sum[is.na(NUMGIVEN)] <- NA
kin.size <- table(kin.sum)
addmargins(kin.size)
round(100 * prop.table(kin.size), 1)
mean(kin.sum, na.rm = TRUE)

close <- cbind(CLOSE12, CLOSE13, CLOSE14, CLOSE15, CLOSE23,
               CLOSE24, CLOSE25, CLOSE34, CLOSE35, CLOSE45)
close[close == 2] <- 0.5
close[close == 3] <- 0
density <- apply(close, 1, mean, na.rm = TRUE)

density.class <- rep(NA, nrow(gss85))
density.class[which(density < 0.25)] <- 1
density.class[which((density >= 0.25) & (density < 0.5))] <- 2 
density.class[which((density >= 0.5) & (density < 0.75))] <- 3
density.class[which(density >= 0.75)] <- 4
density.class <- factor(density.class, level = 1:4,
 labels = c("<0.25", "0.25-0.49", "0.50-0.74", ">0.74"))
density.tab <- table(density.class)
addmargins(density.tab)
round(100 * prop.table(density.tab), 1)
mean(density, na.rm = TRUE)

age.net <- cbind(AGE1, AGE2, AGE3, AGE4, AGE5)
age.sd <- apply(age.net, 1, sd, na.rm = TRUE)
mean(age.sd, na.rm = TRUE)
age.sd.class <- rep(NA, nrow(gss85))
age.sd.class[which(age.sd < 5)] <- 1
age.sd.class[which((age.sd >= 5) & (age.sd < 10))] <- 2
age.sd.class[which((age.sd >= 10) & (age.sd < 15))] <- 3
age.sd.class[which(age.sd >= 15)] <- 4
age.sd.class <- factor(age.sd.class, level = 1:4,
 labels = c("<5", "5-<10", "10-<15", "15+"))
age.htr <- table(age.sd.class)
addmargins(age.htr)
round(100 * prop.table(age.htr), 1)

sex.net <- cbind(SEX1, SEX2, SEX3, SEX4, SEX5)
sex.iqv <- rep(NA, nrow(gss85))
for (i in 1:length(sex.iqv)) {
 sex.iqv[i] <- (1 - sum(prop.table(table(sex.net[i,]))^2)) * 2
}
sex.iqv[is.na(NUMGIVEN)] <- NA
sex.iqv[NUMGIVEN < 2] <- NA
sex.iqv.tab <- table(sex.iqv)
addmargins(sex.iqv.tab)
round(100 * prop.table(sex.iqv.tab), 1)

same.sex <- rep(NA, nrow(gss85))
for (i in 1:length(same.sex)) {
 same.sex[i] <- 
 sum(as.integer(SEX[i])==sex.net[i,],na.rm=TRUE)/min(NUMGIVEN[i],5)
}
mean(same.sex, na.rm = TRUE)
same.sex.tab <- table(same.sex)
addmargins(same.sex.tab)
round(100 * prop.table(same.sex.tab), 1)

plot(SEX, same.sex)
t.test(same.sex ~ SEX)

plot(RACE, same.sex)
pairwise.t.test(same.sex, RACE, p.adj = "bonf")
plot(MARITAL, same.sex, cex.axis = 0.7)
pairwise.t.test(same.sex, MARITAL, p.adj = "bonf")

cor.test(AGE, same.sex)
cor.test(NUMGIVEN, same.sex)

model.1 <- lm(same.sex ~ AGE + SEX + RACE + MARITAL + NUMGIVEN)
summary(model.1)

spouse <- cbind(SPOUSE1, SPOUSE2, SPOUSE3, SPOUSE4, SPOUSE5)
spouseNet <- rowSums(spouse == 1, na.rm = TRUE)
spouseNet[is.na(NUMGIVEN)] <- NA
spouseNet[NUMGIVEN == 0] <- NA
spouseNet <- as.logical(spouseNet)

model.2 <- 
 lm(same.sex ~ AGE + SEX + RACE + MARITAL + NUMGIVEN + spouseNet)
summary(model.2)

xtabs(~ MARITAL + spouseNet)

install.packages("DAAG")
library(DAAG)
vif(model.2)

#8.2.3
cg <- array(dim = c(3,2,2))
a1 <- matrix(c(
0,1,
1,0),
nrow = 2)
a2 <- matrix(c(
0,0,
0,0),
nrow = 2)
a3 <- matrix(c(
0,1,
1,0),
nrow = 2)
cg[1,,] <- a1
cg[2,,] <- a2
cg[3,,] <- a3

prior.prob <- matrix(c(
  0, 0.5,
0.5,   0),
nrow = 2)

eplus <- c(0.2,0.1,0.3)
eminus <- c(0.3,0.2,0.1)

library(sna)
(b1 <- bbnam(cg, model = "fixed", outmode = "posterior",
 nprior = prior.prob, ep = eplus, em = eminus))

(b2 <- bbnam(cg, model = "fixed", outmode = "draws",
 nprior = prior.prob, ep = eplus, em = eminus))