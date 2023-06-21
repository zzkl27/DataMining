library(ggplot2)
library(dplyr)

#### ======================== < 분포 소개 > ========================== ####
# d : 확률 질량(밀도) 함수 값
# p : 누적 분포 함수 값
# q : 변위치(quantile) 값 - p의 역함수
# r : 난수 생성

# ---- [1] 이항 분포(binormial)[이산(Discrete)] ---- 
# 베르누이 실험 : 결과가 2가지 경우(성공/실패)만 존재하는 실험
# 성공의 확률이 p인 베르누이 실행을 n번 반복했을 때 성공 횟수에 대한 분포

# [모수] 
# size : 반복 횟수
# prob : 성공할 확률

# [참고]
#  x값의 범위 : [0,size]

# 분포 그래프
data <- data.frame(x=seq(from=0,to=10,by=1), y=dbinom(seq(from=0,to=10,by=1), size=10, prob=0.5))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,10,1))

# 누적 분포 그래프
data <- data.frame(x=seq(from=0,to=10,by=1), y=pbinom(seq(from=0,to=10,by=1), size=10, prob=0.5))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,10,1))

# 변위치 그래프
data <- data.frame(x=seq(from=0,to=1,by=0.001), y=qbinom(seq(from=0,to=1,by=0.001), size=10, prob=0.5))
ggplot(data) + geom_line(aes(x=x, y=y), color='red') + 
  scale_x_continuous(breaks = seq(from=0,to=1,by=0.1)) + scale_y_continuous(breaks = seq(0,10,1))

# 난수 히스토그램 그래프
random_data <- rbinom(n=50000, size=10, prob=0.5) %>% factor()
data <-data.frame(x=random_data) %>% group_by(x) %>% summarize(count=n()/length(random_data)) # 개수를 세아려 확률 계산
ggplot(data=data) + geom_col(aes(x=x, y=count), fill='red')





# ---- [2] 기하 분포(Geometric)[이산(Discrete)] ---- 
# 성공의 확률이 p인 베르누이 실험을 최소 성공이 나타날때 까지 반복한 횟수에 대한 분포

# [모수]
# prob : 성공할 확률

# [참고]
# 원래 기하 분포에서 x값의 범위는 [1,]이나 r에서는 [0,]

# 분포 그래프
data <- data.frame(x=seq(from=0,to=10,by=1), y=dgeom(seq(from=0,to=10,by=1), prob=0.5))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,10,1))
sum(data$y)

# 누적 분포 그래프
data <- data.frame(x=seq(from=0,to=10,by=1), y=pgeom(seq(from=0,to=10,by=1), prob=0.5))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,10,1))

# 변위치 그래프
data <- data.frame(x=seq(from=0,to=1,by=0.001), y=qgeom(seq(from=0,to=1,by=0.001), prob=0.5))
ggplot(data) + geom_line(aes(x=x, y=y), color='red') + 
  scale_x_continuous(breaks = seq(from=0,to=1,by=0.1)) + scale_y_continuous(breaks = seq(0,10,1))

# 난수 히스토그램 그래프
random_data <- rgeom(n=50000, prob=0.5) %>% factor()
data <-data.frame(x=random_data) %>% group_by(x) %>% summarize(count=n()/length(random_data)) # 개수를 세아려 확률 계산
ggplot(data=data) + geom_col(aes(x=x, y=count), fill='red')





# ---- [3] 초기하 분포(Hypergeometric)[이산(Discrete)] ---- 
# 실험의 결과가 2가지(성공/실패)일 때 반복할 수 있는 최대 횟수 N에서 성공이 N1, 실패가 N2일 경우
# n번 실험(비복원 추출)했을 때 성공의 횟수에 대한 분포

# [모수]
# m : 성공의 횟수
# n : 실패의 횟수
# k : 반복 횟수(최대 m+n)

# [참고]
# x값의 범위 : [max(0,k-n),k]

# 분포 그래프
data <- data.frame(x=seq(from=0,to=10,by=1), y=dhyper(seq(from=0,to=10,by=1), m=10, n=17, k=10))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,10,1))


# 누적 분포 그래프
data <- data.frame(x=seq(from=0,to=10,by=1), y=phyper(seq(from=0,to=10,by=1), m=10, n=17, k=10))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,10,1))

# 변위치 그래프
data <- data.frame(x=seq(from=0,to=1,by=0.001), y=qhyper(seq(from=0,to=1,by=0.001), m=10, n=17, k=10))
ggplot(data) + geom_line(aes(x=x, y=y), color='red') + 
  scale_x_continuous(breaks = seq(from=0,to=1,by=0.1)) + scale_y_continuous(breaks = seq(0,10,1))

# 난수 히스토그램 그래프
random_data <- rhyper(nn=50000, m=10, n=17, k=10) %>% factor()
data <-data.frame(x=random_data) %>% group_by(x) %>% summarize(count=n()/length(random_data)) # 개수를 세아려 확률 계산
ggplot(data=data) + geom_col(aes(x=x, y=count), fill='red')






# ---- [4] 음이항 분포(Negative Binomial)[이산(Discrete)] ---- 
# 성공 확률이 p인 베르누이 실험을 성공의 횟수가 n일 때 까지 반복한 횟수에 대한 분포
# r에서는 n번 성공할때 까지의 실패 횟수에 대한 분포로 정의

# [모수]
# size : 성공 횟수
# p : 성공할 확률

# [참고]
# 일반적인 음이항 분포에서 x값의 범위는 [size, ]이나 r에서는 [0, ]

# 분포 그래프
data <- data.frame(x=seq(from=0,to=20,by=1), y=dnbinom(seq(from=0,to=20,by=1), size=5, prob=0.5))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,20,1))


# 누적 분포 그래프
data <- data.frame(x=seq(from=0,to=20,by=1), y=pnbinom(seq(from=0,to=20,by=1),  size=5, prob=0.5))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,20,1))

# 변위치 그래프
data <- data.frame(x=seq(from=0,to=1,by=0.001), y=qnbinom(seq(from=0,to=1,by=0.001), size=5, prob=0.5))
ggplot(data) + geom_line(aes(x=x, y=y), color='red') + 
  scale_x_continuous(breaks = seq(from=0,to=1,by=0.1)) + scale_y_continuous(breaks = seq(0,10,1))

# 난수 히스토그램 그래프
random_data <- rnbinom(n=50000, size=5, prob=0.5) %>% factor()
data <-data.frame(x=random_data) %>% group_by(x) %>% summarize(count=n()/length(random_data)) # 개수를 세아려 확률 계산
ggplot(data=data) + geom_col(aes(x=x, y=count), fill='red')





# ---- [5] 푸아송 분포(Poisson)[이산(Discrete)] ---- 
# 연속인 단위 구간에서 사건 발생의 평균이 lambda 일때
# 단위 구간에서 사건의 발생 횟수에 대한 분포

# [모수]
# lambda : 평균 발생 횟수

# [참고]
# x값의 범위 : [0, ]

# 분포 그래프
data <- data.frame(x=seq(from=0,to=40,by=1), y=dpois(seq(from=0,to=40,by=1), lambda=20))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,40,4))


# 누적 분포 그래프
data <- data.frame(x=seq(from=0,to=40,by=1), y=ppois(seq(from=0,to=40,by=1), lambda=20))
ggplot(data) + geom_col(aes(x=x, y=y), fill='red') + scale_x_continuous(breaks = seq(0,40,4))

# 변위치 그래프
data <- data.frame(x=seq(from=0,to=1,by=0.001), y=qpois(seq(from=0,to=1,by=0.001),lambda=20))
ggplot(data) + geom_line(aes(x=x, y=y), color='red') + 
  scale_x_continuous(breaks = seq(from=0,to=1,by=0.1)) + scale_y_continuous(breaks = seq(0,40,4))

# 난수 히스토그램 그래프
random_data <- rpois(n=50000, lambda=20) %>% factor()
data <-data.frame(x=random_data) %>% group_by(x) %>% summarize(count=n()/length(random_data)) # 개수를 세아려 확률 계산
ggplot(data=data) + geom_col(aes(x=x, y=count), fill='red')





# ---- [6] 다항 분포(Multinomial)[이산(Discrete)] ---- 
# 이항 분포의 확장 분포
# 2차원 이상으로 나타나기도 함

# ---- [7] 균일 분포(Uniform)[이산(Discrete), 연속(Continuous)] ---- 

# ---- [8] 정규 분포(Normal)[연속(Continuous)] ---- 

# ---- [9] t 분포(Student's)[연속(Continuous)] ---- 

# ---- [10] 카이제곱 분포(Chi-square)[연속(Continuous)] ---- 

# ---- [11] 지수 분포(Exponential)[연속(Continuous)] ---- 

# ---- [12] 감마 분포(Gamma)[연속(Continuous)] ---- 

# ---- [13] F 분포(F)[연속(Continuous)] ---- 

# ---- [13] 베타 분포(Beta)[연속(Continuous)] ---- 

#### ======================== < 분포 소개 > ========================== ####
