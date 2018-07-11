library(DAAG)
epl.15 <- read.csv("1516.csv")
epl.16 <- read.csv("1617.csv")
epl.17 <- read.csv("1718.csv")
epl.16 <- epl.16[1:380,]
Hwin.1 <- ifelse(epl.16[4] == "H", 1, 0)
Hwin.2 <- ifelse(epl.17[4] == "H", 1, 0)
epl.16 <- cbind(Hwin.1, epl.16)
epl.17 <- cbind(Hwin.2, epl.17)
colnames(epl.15) <- colnames(epl.16)
colnames(epl.15)[1] <- "Hwin"
colnames(epl.16)[1] <- "Hwin"
colnames(epl.17)[1] <- "Hwin"

epl.1516 <- rbind(epl.15, epl.16)
epl.total <- rbind(epl.1516, epl.17)

str(epl.15)
str(epl.16)
str(epl.17)

table(epl.15$FTR)
table(epl.16$FTR)
table(epl.17$FTR)

goal.check <- function(x) {
  G.c <- cut(x$FTHG.FTAG, unique(quantile(FTHG.FTAG, probs = c(0, 0.25, 0.5, 0.75, 1)), na.rm = T))
  mosaicplot(~G.c + x$FTR, color = c("white", "gray", "red"))
}

goal.check(epl.15)
goal.check(epl.16)
goal.check(epl.17)

cutting <- function(x) {
  cut(x, unique(quantile(x, prob = c(0, 0.25, 0.5, 0.75, 1)), na.rm = T))
}

grim <- function(y, a) { mosaicplot(~ cutting(y) + FTR, color = c("white", "gray", "red"), main = a ) }

attach(epl.15)
for (i in 6:24) {
  grim(epl.15[, i], colnames(epl.15[i]))
  pause()
}

attach(epl.16)
for (i in 6:24) {
  grim(epl.16[, i], colnames(epl.15[i]))
  pause()
}

attach(epl.17)
for (i in 6:24) {
  grim(epl.17[, i], colnames(epl.15[i]))
  pause()
}
# epl season 15/16
set.seed(44)
rn <- sample(1:380, 253)
epl.15.s1 <- epl.15[c(rn), ]
epl.15.s2 <- epl.15[-c(rn), ]

attach(epl.15.s1)
logistic <- glm(FTR ~ HS.AS + HST.AST + HF.AF + HC.AC + HY.AY + HR.AR + Hova.Aova
                + Hatt.Aatt + Hmid.Amid + Hdef.Adef + Hbpeed.Abpeed + Hbdribble.Abdribble +
                  Hbpass.Abpass + Hcppass.Acppass + Hccross.Across + Hcshoot.Acshoot +
                  Hdpressure.Adpressure + Hdaggression.Adaggression + Hdwidth.Adwidth,
               family = "binomial"  )

fitted <- predict(logistic, epl.15.s2[, 6:24])
table(epl.15.s1$FTR)
predicted <- ifelse(rank(fitted) <= table(epl.15.s1$FTR)[1], "away", 
                    ifelse(rank(fitted) <= (table(epl.15.s1$FTR)[1] + table(epl.15.s1$FTR)[2])
                           & rank(fitted) >= table(epl.15.s1$FTR)[3], "Draw", "home"))
addmargins(table(predicted, epl.15.s2$FTR))


# epl season 16/17
set.seed(44)
rn <- sample(1:380, 253)
epl.16.s1 <- epl.16[c(rn), ]
epl.16.s2 <- epl.16[-c(rn), ]

attach(epl.16.s1)
logistic <- glm(FTR ~ HS.AS + HST.AST + HF.AF + HC.AC + HY.AY + HR.AR + Hova.Aova
                + Hatt.Aatt + Hmid.Amid + Hdef.Adef + Hbpeed.Abpeed + Hbdribble.Abdribble +
                  Hbpass.Abpass + Hcppass.Acppass + Hccross.Across + Hcshoot.Acshoot +
                  Hdpressure.Adpressure + Hdaggression.Adaggression + Hdwidth.Adwidth,
                family = "binomial")

fitted <- predict(logistic, epl.16.s2[, 6:24])
table(epl.16.s1$FTR)
predicted <- ifelse(rank(fitted) <= table(epl.16.s1$FTR)[1], "away", 
                    ifelse(rank(fitted) <= (table(epl.16.s1$FTR)[1] + table(epl.16.s1$FTR)[2])
                           & rank(fitted) >= table(epl.16.s1$FTR)[3], "Draw", "home"))
addmargins(table(predicted, epl.16.s2$FTR))

# epl season 17/18
set.seed(44)
rn <- sample(1:140, 93)
epl.17.s1 <- epl.17[c(rn), ]
epl.17.s2 <- epl.17[-c(rn), ]

attach(epl.17.s1)
logistic <- glm(FTR ~ HS.AS + HST.AST + HF.AF + HC.AC + HY.AY + HR.AR + Hova.Aova
                + Hatt.Aatt + Hmid.Amid + Hdef.Adef + Hbpeed.Abpeed + Hbdribble.Abdribble +
                  Hbpass.Abpass + Hcppass.Acppass + Hccross.Across + Hcshoot.Acshoot +
                  Hdpressure.Adpressure + Hdaggression.Adaggression + Hdwidth.Adwidth,
                family = "binomial")

fitted <- predict(logistic, epl.17.s2[, 6:24])
table(epl.17.s1$FTR)
predicted <- ifelse(rank(fitted) <= table(epl.17.s1$FTR)[1], "away", 
                    ifelse(rank(fitted) <= (table(epl.17.s1$FTR)[1] + table(epl.17.s1$FTR)[2])
                           & rank(fitted) >= table(epl.17.s1$FTR)[3], "Draw", "home"))
addmargins(table(predicted, epl.17.s2$FTR))

#epl.total
epl.totals <- read.csv("Total-difference.csv", sep = " ")
set.seed(44)
rn <- sample(1:140 )


#svm for 15, 16, 17 
library(e1071)
set.seed(44)
rn <- sample(1:380, 253)
epl.15.s3 <- epl.15[c(rn), ]
epl.15.s4 <- epl.15[-c(rn), ]

m1 <- svm(FTR ~., data = epl.15.s3,  kernel = "linear")
m2 <- svm(FTR ~., data = epl.15.s3, kernel = "sigmoid")
m3 <- svm(FTR ~., data = epl.15.s3, kernel = "radial")

z <- subset(epl.15.s4, select = -FTR)
z.1 <- epl.15.s4$FTR
pred1 <- predict(m1, z)
pred2 <- predict(m2, z)
pred3 <- predict(m3, z)
table(z.1, pred1)
table(z.1, pred2)
table(z.1, pred3)

# goals 

str(epl.1516)
epl.156 <- read.csv("1516or.csv")
epl.167 <- read.csv("1617or.csv")
Hwin.0 <- ifelse(epl.156[6] == "H", 1, 0)
epl.156[1] <- Hwin.0
epl.167 <- cbind(Hwin.1, epl.167)
colnames(epl.167)[1] <- "Hwin"
colnames(epl.156) <- colnames(epl.167)
epl.total <- rbind(epl.156, epl.167)
qqplot(epl.total$FTHG, dpois(x = c(0:6), lambda = mean(epl.total$FTHG)))
hist(epl.total$FTHG, breaks = seq(-2, 8, 1), freq = FALSE)
x <- seq(0, 8, length = 10)
plot(x, dpois(round(x), lambda = mean(epl.total$FTHG)), type = "l")


linear <- with(epl.total, lm(FTHG ~ HS + HST + HF + HC + HY + HR + H.ova
                             + H.att + H.mid + H.def + Hbpeed + Hbdribble
                             + Hbpass + Hcppass + Hccross + Hcshoot + Hdpressure
                             + Hdaggression + Hdwidth))
summary(linear)
plot(epl.total$FTHG ~ linear$fit, xlab = "fitted")
cor(epl.total$FTHG, linear$fit)

epl.total.1 <- cbind(epl.total[, 7], epl.total[, 9], epl.total[, 11], epl.total[, 13],
                     epl.total[, 15], epl.total[, 17], epl.total[, 19:31])
important <- rep(1, 19)
names(important) <- c("HS", "HST", "HF", "HC", "HY", "HR", "H.ova", "H.att", "H.mid", "H.def", "Hbpeed", "Hbdribble",
                      "Hbpass", "Hcappass", "Hccross", "Hcshoot", "Hdpressure", "Hdaggression", "Hdwidth")
for (j in 1:19) {
  lowess.fit <- lowess(epl.total.1[, j], epl.total$FTHG)$y
  important[j] <- max(lowess.fit) - min(lowess.fit)
}


with(epl.total, plot(HST, FTHG + rnorm(20, 0, 0.25)))
lines(lowess(epl.total$HST, epl.total$FTHG), col = "red")
loess.m <- with(epl.total, loess(FTHG ~ HST + Hccross + HS))
fit <- predict(loess.m)
with(epl.total,plot(FTHG ~ fit, xlab = "fitted", ylab = "HG", main = "loess"))
cor(epl.total$FTHG, loess.m$fit)

fset.seed(44)
rn <- sample(1:380, 253)
