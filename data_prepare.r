library(dplyr)
library(stringr)
library(hoopR)
library(parallel)
library(nortest)
library(pheatmap)
library(xtable)
library(mclust)
library(rgl)
library(MVN)

set.seed(1)

### Initial data collection and preprocessing ###

# Kaggle dataset
df <- read.csv("data/Seasons_Stats.csv")
df$X <- df$blanl <- df$blank2 <- NULL # vacuous columns
df$GS <- NULL # It is not very important. Keeping it introduces a lot of NA, which requires to drop many more rows.

# basic counters that can be safely added up
feats_cumulative <- c(
  "G", "MP", "X2P", "X2PA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "AST", "STL", "BLK", "TOV", "PF"
)
# features that we know how to compute from the cumulative ones:
feats_compute <- \(X) {
  # The formulas were checked against the glossary: https://www.basketball-reference.com/about/glossary.html
  X$FG = X$X2P + X$X3P
  X$FGA = X$X2PA + X$X3PA
  X$PTS = X$FT + 2 * X$X2P + 3 * X$X3P
  X$TRB = X$ORB + X$DRB
  ### fractional stuff:
  # this one is not among the orig. features, but is a component of some of them
  TSA = X$FGA + 0.44 * X$FTA
  # this rule is questionable. x/0 should be NA, but we'll make it 0 if x=0.
  # we will likely impose some lower bounds on some stats, so this problem will go away
  divide = \(a, b) ifelse(a == 0 & b == 0, 0, a / b)
  X$FT. = divide(X$FT, X$FTA)
  X$TS. = divide(X$PTS, 2 * TSA)
  X$X3PAr = divide(X$X3PA, X$FGA)
  X$FTr = divide(X$FTA, X$FGA)
  X$FG. = divide(X$FG, X$FGA)
  X$eFG. = divide(X$FG + 0.5 * X$X3P, X$FGA)
  X$X2P. = divide(X$X2P, X$X2PA)
  X$X3P. = divide(X$X3P, X$X3PA)
  X$TOV. = divide(X$TOV, TSA + X$TOV)
  X
}

df <- do.call(rbind, mclapply(unique(df$Player), \(pname) {
  X <- subset(df, Player == pname)
  # exclude if sum(X$MP) is 0 or NA
  if (!identical(T, sum(X$MP) > 0)) return(data.frame())
  Y <- head(X["Player"], 1)
  # introduce a new variable: seasons played
  Y$S <- nrow(X)
  # most played position (by minutes played)
  Y$Pos <- arrange(aggregate(MP ~ Pos, X, sum), desc(MP))[1, 1]
  # most played team (by minutes played)
  Y$Tm <- arrange(aggregate(MP ~ Tm, X, sum), desc(MP))[1, 1]
  Y <- cbind(Y, t(colSums(X[feats_cumulative])))
  # For non-cumulative features that cannot be computed from cumulative ones, we apply weighted averaging,
  # with weight as minutes played:
  cbind(Y, t(X$MP / sum(X$MP)) %*% as.matrix(X[c(
    "Year", "Age",
    # Apply weighted average to the below, because we can't recompute them: they depend on season-specific
    # info that we don't have.
    "PER", "ORB.", "DRB.", "TRB.", "AST.", "STL.", "BLK.", "USG.",
    "OWS", "DWS", "WS", "WS.48", "OBPM", "DBPM", "BPM", "VORP"
  )]))
}))
nrow(df)
# after rolling up, drop rows with missing values, and recompute what is recomputable
df <- feats_compute(df[rowSums(is.na(df)) == 0,])
nrow(df)

# A function to simplify names for the purpose of matching them with the names in the official NBA stats database,
# from which I pulled the awards info. The names there are not exactly the same. Simplifying names by removing
# special characters and making everything uppercase improves the consistency of names
normalize_name <- \(x) str_trim(str_replace_all(str_to_upper(x), "[^[A-Z ]+]", ""))

# sometimes fails, because NBA API has a traffic rate limit
df_nba_pi = as.data.frame(nba_playerindex()$PlayerIndex)
df_personal = data.frame(
  Id = df_nba_pi$PERSON_ID,
  Player = normalize_name(paste(df_nba_pi$PLAYER_FIRST_NAME, df_nba_pi$PLAYER_LAST_NAME)),
  Height = 30.48 * as.numeric(str_replace(df_nba_pi$HEIGHT, "-", ".")),
  Weight = 0.45 * as.numeric(df_nba_pi$WEIGHT)
)

# drop ambiguous names (that normalize to the same name)
df_personal = subset(df_personal, !Player %in% Player[duplicated(Player)])
# some heights/weights come as NA from the API
df_personal = subset(df_personal, !is.na(Height) & !is.na(Weight))

df$Player <- normalize_name(df$Player)
# drop ambiguous names (that normalize to the same name)
df = subset(df, !Player %in% Player[duplicated(Player)])
nrow(df)

# add height/weight
df <- inner_join(df_personal, df, by = "Player")
nrow(df)

# pre-fetched award info from the NBA API
df_awd = read.csv("data/awards.csv")
awd_enum = sort(unique(df_awd$DESCRIPTION))
writeLines("Award kinds:")
awd_enum
awd_class1 = c(
  "Hall of Fame Inductee", "NBA Most Valuable Player", "All-NBA1", "All-Defensive Team1",
  "NBA Defensive Player of the Year", "All-NBA2", "All-Defensive Team2", "All-NBA3",
  # These seem to be significantly less prestigious. Renato?
  "NBA Clutch Player of the Year", "NBA Sixth Man of the Year"
)
# add the award info
df = left_join(df, aggregate(. ~ Id, data.frame(Id = as.character(df_awd$PERSON_ID), AWD = df_awd$DESCRIPTION),
                             \(awards) 1 * any(awards %in% awd_class1)))
df$AWD[is.na(df$AWD)] = 0
mean(df$AWD)
df$AWD = factor(df$AWD, ordered = T)
df$Pos = factor(df$Pos)
# tale only the primary position if a player has a secondary one
df$Pos.simple <- factor(sapply(as.character(df$Pos), \(xi) first(strsplit(xi, "-")[[1]]), USE.NAMES = F))
df$Tm <- factor(df$Tm)
df <- arrange(df, desc(Year))
write.csv(df, "nba_raw.csv")
nrow(df)

### Main dataset ###

# drop players that have extremely low total career play time
df0 = subset(df, MP >= 60 & G >= 10)
# Category 1 features
feats1 = c("X2P", "X2PA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "AST", "STL", "BLK", "TOV", "PF")
# add 1 in case we want take a logarithm, divide by MP
df = (1 + df0[feats1]) / df0$MP
df$MPG = df0$MP / df0$G
df$MPS = df0$MP / df0$S
df = cbind(df, df0[c("Height", "Weight")])
df$AWD = df0$AWD
df$Pos = df0$Pos.simple
write.csv(df, "nba.csv", row.names = F)
nrow(df)

### Transformed dataset ###

# the transform for all features except Height and Weight
f = \(X, cn, k) k * (X[, cn] - 1) + log(X[, cn])
feats2 = c(feats1, "MPG", "MPS")
# find the optimal parameter for the transform for each of the features, that minimises the AD test statistic
k = sapply(feats2, \(cn) optim(
  0, \(k) ad.test(f(df, cn, k))$statistic,
  method = "Brent", lower = 0, upper = 1e5
)$par)
k = setNames(k, feats2)
# apply transforms
for (cn in feats2) df[, cn] = f(df, cn, k[cn])
for (cn in c("Height", "Weight")) df[, cn] = log(df[, cn] / mean(df[, cn]))
write.csv(df, "nba_transformed.csv", row.names = F)

### Transformed dataset + PCA ###

PCA = prcomp(df[c(feats2, "Height", "Weight")], scale. = T)
summary(PCA)
# take first 12 PCs
df = cbind(PCA$x[, 1:12], df[c("AWD", "Pos")])
write.csv(df, "nba_transformed_pca.csv", row.names = F)

### Figures and tables for the report ###
par(mar = rep(2, 4))

### Table 1
X = read.csv("nba.csv")[1:17]
round(cbind("Std. Dev." = sqrt(diag(var(X))), t(do.call(cbind, lapply(X, summary)))), 3)

### Figure 1
par.old = par(mfrow = c(6, 3))
for (cn in colnames(X)) hist(X[, cn], 100, main = cn, xlab = "", ylab = "")
par(par.old)

### Figure 2
pheatmap(cor(X), cluster_rows = F, cluster_cols = F, display_numbers = TRUE, breaks = seq(-1, 1, length.out = 1e2))

### Figure 3-9
# divide by std. deviations
Y = t(t(X) / sqrt(diag(var(X))))
for (i in 0:1) boxplot(Y[df$AWD == i,], main = paste0("AWD = ", i))
for (i in sort(unique(df$Pos))) boxplot(Y[df$Pos == i,], main = paste0("Pos = ", i))

### Figure 10
Xt = read.csv("nba_transformed.csv")[1:17]
par.old = par(mfrow = c(6, 3))
for (cn in colnames(X)) hist(Xt[, cn], 100, main = cn, xlab = "", ylab = "")
par(par.old)
# multivariate normality check
mvn(Xt)

### Figure 11
pheatmap(cor(Xt) - cor(X), cluster_rows = F, cluster_cols = F, display_numbers = TRUE,
         breaks = seq(-1, 1, length.out = 1e2))

### Table 2
f_tmp = \(X) {
  PCA = prcomp(X, scale. = T)
  cumsum(100 * PCA$sdev^2 / sum(PCA$sdev^2))
}

round(data.frame(before = f_tmp(X), after = f_tmp(Xt)), 2)

### Figure 12
plot(cumsum(100 * PCA$sdev^2 / sum(PCA$sdev^2)), type = "lines", xlab = "", ylab = "")
abline(v = 12)

### Figure 13
par.old = par(mfrow = c(4, 3))
Xt_pca = PCA$x[, 1:12]
for (cn in colnames(Xt_pca)) hist(Xt_pca[, cn], 100, main = cn, xlab = "", ylab = "")
par(par.old)

# multivariate normality check
mvn(Xt_pca)

### Figure 14
pheatmap(PCA$rotation[, 1:12], cluster_rows = F, cluster_cols = F, display_numbers = TRUE)

### Figure 15
Xt_pca3 = PCA$x[, 1:3]
gmm <- Mclust(Xt_pca3, G = 1:20, modelNames = "VVI")
gmm$G
plot3d(Xt_pca3, col = gmm$classification, size = 20)
# multivariate normality check
mvn(Xt_pca3)
# multivariate normality check for each cluster
for (i in unique(gmm$classification)) {
  writeLines(paste0("### cluster: ", i, " ###"))
  show(mvn(Xt_pca3[gmm$classification == i,]))
}
