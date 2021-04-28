# 사칙연산과 응용
2+3
10-3
3*4
8/2
2^3
10%%3
10%/%3

# 크기 비교
x<-3
x<10
x<-3
x>10
x<-3
x==10
x<-3
x!=10
x<-4

# 벡터의 원소
x <- c(1,2,3,4,5)
x
x[2]
x[c(1,3,5)]
x[-c(2,4)]
x[x>2]
x[x>=2 & x<=4]
x[2] <-20
x[c(3,4)] <- 15
x[x<=15]<-10

# 벡터 값에 대한 함수의 활용 예
x <- seq(1:10)

mean(x)
var(x)
sd(x)
sqrt(x)
length(x)
abs(x)


# 벡터 생성
subject_name <- c("John","Dunpy","Steve")
temperature <- c(32.2, 33.0, 37.9)
flu_status <- c(FALSE, FALSE, TRUE)

# 팩터 생성
gender <- factor(c("MALE","FEMALE","MALE"))
gender

blood <- factor(c("O","AB","A"),
                levels = c("A","B","AB","O"))
blood

# 데이터프레임 생성
people <- data.frame(subject_name, temperature, flu_status, gender, blood,
                     stringsAsFactors = FALSE)
people

# 데이터프레임 실습
people$subject_name
people[c("temperature", "flu_status")]
people[2:3]
people[1,2]
people[c(1,3), c(2,4)]
people[1, ]
people[ ,1]
people[ , ]
people[-2, c(-3,-5)]
View(people[-2, c(-3,-5)])

m1 <- matrix(c('a','b','c','d'),nrow = 2)
m2 <- matrix(c('a','b','c','d'), ncol = 2)
m1
m2
m3 <- matrix(c('a','b','c','d','e','f'), nrow = 3)
m4 <- matrix(c('a','b','c','d','e','f'), ncol = 3)
m3
m4
m4[1,1]
m4[2,3]
m3[1,]
m3[,1]

# RData 저장
save(m1,m2,m3,m4, file="mydata.RData")
load("mydata.Rdata")

# 데이터프레임 저장 불러오기
write.csv(people, file = "./datasets/first.csv")
first <- read.csv(file="./datasets/first.csv")
first <- read.csv("./datasets/first.csv", stringsAsFactors = FALSE)
first1 <- read.csv("./datasets/first.csv",stringsAsFactors = FALSE, header = TRUE)
first2 <- read.csv("./datasets/first.csv", stringsAsFactors = FALSE, header = FALSE)

# 데이터 속성 이해
usedcars <- read.csv("./datasets/usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)
summary(usedcars)
summary(usedcars$year)
summary(usedcars[c("price","mileage")])

# 평균과 중앙값
(36000 + 44000 + 56000)/3
mean(c(36000, 44000, 56000))
median(c(36000, 44000, 56000))

# 사분위수
range(usedcars$price)
diff(range(usedcars$price))
IQR(usedcars$price)
quantile(usedcars$price)
quantile(usedcars$price, probs = c(0.01, 0.99))
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# 시각화
boxplot(usedcars$price,
        main = "Boxplot of Used Car Prices",
        ylab = "Price ($)")
boxplot(usedcars$mileage,
        main = "Boxplot of Used Car Mileage",
        ylab = "Odometer (mi.)")
hist(usedcars$price,
     main = "Histogram of Used Car Prices",
     xlab = "Price ($)")
hist(usedcars$mileage,
     main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")

# 분산과 표준편차
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

# 일원배치
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

mt <- table(usedcars$model)
prop.table(mt)

rmt <- prop.table(mt)
rmt <- prop.table(mt) * 100
round(rmt, digits = 1)

# 산포도
plot(x = usedcars$mileage,
     y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")

# 패키지 설치
install.packages("gmodels")
library(gmodels)

# 이원배치표 생성
usedcars$conservative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)
CrossTable(x = usedcars$model,
           y = usedcars$conservative)

# function()
getCircleArea <- function(r){
        area = 3.14*r^2
        return(area)
}
getCircleArea(3)

