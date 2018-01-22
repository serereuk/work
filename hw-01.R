#R data handling
#1.1
rm(list = ls())
cow <- read.csv("cow_data (1).csv", header = T)
cow <- na.omit(cow)
library(dplyr)
edible <- function(x) {
  x %>% mutate(is_edible = ifelse(age >= 50 & (grade == "3" | grade == "등외"),
                                  "폐기용", "식용"))
}
cow <- edible(cow)
head(cow, 6)
#1.2
library(stringr)
cow <- cow %>% mutate("도/시/군" = ifelse(substr(address,4,4) == " ", substr(address, 1, 7),
                                       ifelse(substr(address, 6, 6) == " ", substr(address, 1, 10), substr(address, 1, 8))))

cow.0 <- subset(cow, substr(cow$`도/시/군`, 3, 4) != "특별")
cow.1 <- subset(cow.0, grade == "1++")
tab <- table(cow.1$"도/시/군")
tab.1 <- sort(tab, decreasing = T)
count <- head(tab.1, 3)

#1.3
region.name <- names(count)
cow.re <- cow[cow$`도/시/군` %in% region.name, ]
cow.re <- cow.re %>% mutate(price2 = gsub(",", "",cow.re$price))
cow.re1 <- subset(cow.re, cow.re$"도/시/군" %in% region.name[1])
cow.re2 <- subset(cow.re, cow.re$"도/시/군" %in% region.name[2])
cow.re3 <- subset(cow.re, cow.re$"도/시/군" %in% region.name[3])
list(region.name[1], cow.re1 %>%  group_by(grade) %>% summarise(mean(as.numeric(price2))))
list(region.name[2], cow.re2 %>%  group_by(grade) %>% summarise(mean(as.numeric(price2))))
list(region.name[3], cow.re3 %>%  group_by(grade) %>% summarise(mean(as.numeric(price2))))

#1.4
#install.packages("extrafont")
library(extrafont)
font_import()
table(cow.re$"도/시/군")
cow.re <- cow.re %>% mutate(month = substr(slaughter_date, 1, 6))
cow.month <- subset(cow.re, select = c("month", "도/시/군"))
tab.month <- table(cow.month)
par(family = "AppleGothic")
plot(names(tab.month[, 1]), tab.month[, 1], type = "l", 
     xlab = "Month", ylab = "slaughter", main = "월별 도축된 수")
lines(names(tab.month[, 2]), tab.month[, 2], col = "red")
lines(names(tab.month[, 3]), tab.month[, 3], col = "blue")
legend(x= 201409, y = 270, 
       legend = unique(cow.month$"도/시/군"), col = c("black", "red", "blue"),
       lwd = 1, cex = 1)

#1.5
p <- 3; importance <- rep(0, p)
for (i in 1:p) {
  lowess.fit <- lowess(as.numeric(cow[, 7 + i]), (as.numeric(gsub(",", "", cow[, 11]))),
                       delta = 0.01 * diff(range(as.numeric(cow[ ,7 + i]))))$y
  importance[i] <- max(lowess.fit) - min(lowess.fit)
}
print(importance)

# R Algorithm

#1번
#1.1
rm(list = ls())
n.trial <- 1000
rp <- NULL
rolling <- function(){
  temp <- sum(sample(1:6, 2, replace = T))
  temp.1 <- 0
  if (temp == 7 | temp == 11) {
    result <- 1
  }
  if (temp == 2 | temp == 3 | temp == 12) {
    result <- 0
  }
  if (temp %in% c(4, 5, 6, 8, 9, 10)) {
    repeat {
      temp.1 <- sum(sample(1:6, 2, replace = T))
      if (temp.1 == 7) {
        result <- 0
        break
      }
      if (temp.1 != 7 & temp == temp.1) {
        result <- 1
        break
      }
      next
    } 
  }
  print(result)
}

for (i in 1:n.trial) {
  set.seed(i)
  res <- replicate(100, rolling())
  p <- sum(res)/100
  rp[i] <- p
}
P <- mean(unlist(rp))

#1.2
N <- 21
i <- init <- 12
q <- 1 - P
value <- NULL
for (iter in 1:n.trial)
{
  set.seed(iter)
  i <- init
  j <- N-i
  while (i > 0 & j > 0)
  {
    result <- rbinom(1, 1, P)
    if (result == 1) i <- i+1
    else i <- i-1
    j <- N-i
    
  }
  if (i == 0) {
    value[iter] <- 1
  } else if (j == 0) value[iter] <- 0
}
prob <- 1 - mean(value)
cat("ky: 12, sy : 9 solution:", prob) 

N <- 29
i <- init <- 20
q <- 1 - P
value <- NULL
for (iter in 1:n.trial)
{
  set.seed(iter)
  i <- init
  j <- N-i
  while (i > 0 & j > 0)
  {
    result <- rbinom(1, 1, P)
    if (result == 1) i <- i+1
    else i <- i-1
    j <- N-i
    
  }
  if (i == 0) {
    value[iter] <- 1
  } else if (j == 0) value[iter] <- 0
}
prob2 <- 1 - mean(value)
cat("ky: 20, sy : 9 solution:", prob2) 


#2번
rm(list = ls()) 
Mat1 <- matrix(c(1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE)
Mat2 <- matrix(c(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE)
large <- function(x) {
  la <- function(y) {ifelse(identical(as.vector(y), rep(1, length(y))), length(y), 0)}
  la(x)
  temp <- vector()
  for (i in 1:(dim(x)[1] - 1)) {
    for (j in 0:i) {
      for (k in 0:i){
        temp.1 <- la(x[(1 + j):(dim(x)[2] +j - i), (1 + k):(dim(x)[2] +k - i)])
        temp <- c(temp, temp.1)
        if (temp.1 > 0) break
      }
    }  
  }
  print(max(temp))
}

large(Mat1)
large(Mat2)

#3번 시도
rm(list = ls())
temp <- NULL
chopchop4 <- function(x) {
  n <- length(x)
  if(n == 3) return(prod(x))
  if(n == 4) return(min(chopchop4(x[-2]) + chopchop4(x[-4]), chopchop4(x[-1]) + chopchop4(x[-3])))
  if(n > 4) {
    for (i in 1:length(x[c(-1, -length(x))])) {
      for (j in 3:(ifelse((length(x)-i) < 3, 3, (length(x)-i)))) {
        cv <- x[i:(i+j-1)] 
        dv <- x[(length(x)-i+1):(length(x)-i-j+2)]
        temp.1 <- chopchop4(cv) + chopchop4(x[-c((i+1):(i+j-2))])
        temp.2 <- chopchop4(dv) + chopchop4(x[-c((length(x)-i):(length(x)-i-j+3))])
        temp <- c(temp, temp.1, temp.2)
      }
    }
    return(min(temp))
  }
}
chopchop4(c(30,35,15,5,10,20,25,40,45))

#4번
rm(list = ls())
set.seed(1234)
e <- matrix(sample(1:100, 25), 5, 5)
solveEquation <- function(a, x = NULL, y = NULL){
  temp <- NULL
  for(i in 1:ncol(a)) {
    for(j in 1:nrow(a)) {
      temp.1 <- (-1)^(i+j) * det(a[-j,-i])
      temp <- c(temp, temp.1)
    }
  }
  if (is.null(x)) {
    temp.mat <- (1/det(a)) * matrix(temp, 5, 5, byrow = TRUE) 
    return(temp.mat %*% matrix(y, length(y), 1))
  }
  if (is.null(y)) {
    return(a %*% matrix(x, length(x), 1))
  }
  return(ifelse(all.equal((a %*% matrix(x, length(x), 1)), matrix(y, length(y), 1))
                , "Correct", "Incorrect"))
  
}
a <- solveEquation(e, ,rep(1, 5))
b <- solveEquation(e,rep(1, 5), )  
solveEquation(e, a, matrix(rep(1,5), 5, 1))

#4.5번
solveEquation2 <- function(a, x = NULL, y = NULL) {
  A <- diag(dim(a)[1])
  A.1 <- cbind(a, A)
  A.2 <-  apply(A.1, 2, "/", A.1[, 1])
  A.3 <- t(apply(A.2, 1, "-", A.2[1, ]))
  A.4 <- rbind(A.2[1,], A.3[2:nrow(A.3),])
  return(A.4)
}  
solveEquation2(e)
sc <- matrix(c(1,7,0,0,0,9,0,1,3),3, 3)
sb(sc)
rowchange <- function(a) {
  if (length(diag(a)) == 2) {
    if(any(diag(a) == 0)) {
      move <- a[1, ]
      a[1, ] <- a[2, ]
      a[2, ] <- move
    }
  }
  return(a)
}
## 실습용