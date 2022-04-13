# 2조 R 조별과제

################# 1번 #################
### 1-1번 ###
div_count <- function(x) {
    count = 0
    for (i in 1:x) {
        if (x %% i == 0)
            count <- count + 1
        
    }
    return(count) }

### 1-2번 ###
prime_count <- function(x) {
    return(
        sum(
            sapply(1:x, function(x) {
                div_count(x) == 2
            }
            )
        ))
}

prime_count(4)


################# 2번 #################
str(mtcars)
summary(mtcars)
mtcars[mtcars$gear == 4,]
mtcars[mtcars$gear == 4, c('mpg', 'cyl', 'hp', 'wt')]
mtcars[mtcars$gear == 4,]
mtcars[mtcars$mpg == min(mtcars$mpg), ]
mtcars[mtcars$mpg == min(mtcars$mpg), c('mpg', 'cyl', 'hp', 'wt')]

## 연비의 평균, 분산, 표준편차
mean(mtcars$mpg)
var(mtcars$mpg)
sd(mtcars$mpg) 

## 연비가 중앙값보다 큰 모델들의 평균, 표준편차
mean(mtcars$mpg > median(mtcars$mpg))
sd(mtcars$mpg > median(mtcars$mpg))

## 그래프
hist(mtcars$mpg, xlab='mtcars', ylab='mpg')
barplot(mtcars$gear ~ rownames(mtcars), xlab='car model', ylab='number of gear')
plot(x = mtcars$mpg, y = mtcars$wt, xlab='mpg', ylab='wt')



################# 3번 #################
summary(st)

##확률분포 시각화
#도전1
hist(st$Murder, prob=T)
curve(dnorm(x, mean(st$Murder), sd(st$Murder)),0,16,
      add = T, col='red')

#도전2
x <- seq(0, 16, length=200)
y <- dnorm(x, mean(st$Murder), sd(st$Murder))
plot(x,y, type = 'l')

#각 변수 간 관계 시각화
install.packages('corrplot')
library(corrplot)

plot(st) #산점도
corrplot(cor(st))
#결과
#Murder와 Life Exp는 반비례 관계(-0.78)
#Murder와 Illiteracy는 정비례 관계(0.70)




################# 4번 #################

## 5장 연습문제 8번
pbinom(q=(3-1), size = 5, prob = 0.05, lower.tail = T)


## 5장 연습문제 16번 
### 치명적인 자동차 사고의 55%가 음주운전에 의한 것이다.
### 5건의 치명적인 자동차 사고가날 때 음주운전에 의해
### 사고가 발생한 횟수 x에 대하여 다음 확률을 구하여라

x <-  55 / 100
x

# 1) 다섯 번 모두 사고가 날 확률
# 이항분포의 확률밀도함수를 이용한다.
dbinom(5, size = 5, prob = x)
답 : 0.0503

# 2) 꼭 3번 사고가 날 확률
dbinom(3, size = 5, prob = x)
답 : 0.33

# 3) 적어도 1번 이상 사고가 날 확률
# 적어도 1번의 치명적 사고가 일어날 확률은 모두 치명적 사고가 일어날 사건의 여사건이다.
# 따라서 1 - 모두 치명적 사고가 일어날 확률을 해주면 된다.

1 - 0.0503

or 

1 - dbinom(1, size = 5, prob = x)

답 : 0.9497



## 5장 연습문제 25번
### 평소에 세 번 전화를 걸면 두 번 통화가 되는 친구에게 5번째 통화에서 전화가될 확률

# 매번의 시행은 독립된 것이기에 기하분포 함수를 이용한다.

x <- 2 / 3
x

dgeom(4, x)    #(사건발생횟수, 사건발생확률)

답 : 0.0082


## 6장 연습문제 9번
#min = 25 ~ max = 300 인 균등분포
##1. 보험료로 지출의 평균, 표준편차 
m <- (300+25)/2
s <- sqrt((300-25)^2/12)
##2. 150이상 지출할 확률
punif(150, min = 25, max = 300, lower.tail = F)
##3. 50이상 150이하 (= 150이하 - 50미만)
#150이하
b <- punif(150,min = 25, max = 300, lower.tail = T)
#50미만(1-50이상)
a <- 1-punif(50, min = 25, max = 300, lower.tail = F)
b-a


## 6장 연습문제 19번
#평균10, 표준편차1.5인 정규분포 
#19.(1) 12이상
pnorm(12, 10, 1.5, lower.tail = F)
#19.(2) 9분 미만 (1- 9이상)
1-pnorm(9, 10, 1.5, lower.tail = F)
#19.(3) 7분 이상 11분 미만
b <- 1-pnorm(11, 10, 1.5, lower.tail = F)
a <- 1-pnorm(7,10,1.5, lower.tail = F)
b-a



################# 5번 #################

## 7장 연습문제 17번 
### 이종격투기 선수들의 평균 악력은 90kg이고 표준편차는 9kg이다.

# 1) 36명을 임의로 선정했을 때, 선수들의 평균 악력이 87~93 사이일 근사 확률
# 정규 모집단으로부터의 표본평균에 대한 확률분포 문제다. 
# 최대치와 최소치의 누적분포함수를 구한 뒤 빼주면 된다.
u <- 90
s <- 9
x1 <- 87
x2 <- 93
n <- 36

pnorm(x2, u, s / sqrt(n)) - pnorm(x1, u, s / sqrt(n))

# 답 : 95.44%

# 2) 64명의 선수를 임의로 선정했을 때, 1)의 확률
# n만 수정하여 그대로 계산
n <- 64

pnorm(x2, u, s / sqrt(n)) - pnorm(x1, u, s / sqrt(n))

# 답 : 99.23%




## 8장 연습문제 7번 
### 한 회사에서 생산하는 비누의 무게는 분산이 5.4인 정규분포를 따른다.
### 50개의 비누를 임의로 추출하였을 때 그 평균 무게의 값은 95.1이다.
### 이 회사에서 생산하는 비누의 평균 무게에 대한 95% 신뢰구간을 구하여라

n <- 50
u <- 95.1
s <- sqrt(5.4)
error <- qnorm(0.975, 0, 1)*s / sqrt(n)

upper <- u+error;round(upper, s)
95.74

lower <- u-error;round(lower, s)
94.46

# 따라서 신뢰구간은 94.46 ~ 95.74 이다.


##8 연습문제 9번
#95% 신뢰구간 구하기
l <- c(95,21,54,127,109,51,65,30,98,107,
       68,99,69,101,73,82,100,63,45,76,
       72,85,121,76,117,67,126,112,83,95)
avg <- mean(l)
s <- 25
n <- length(l)

low <- avg - 1.96 * s / sqrt(n)
high <- avg + 1.96 * s / sqrt(n)

low
high