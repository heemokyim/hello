rule<-read.csv(file.choose(), header =T) # 위에 있는 항목 이름을 표시할때 header = T 
 #rule <- readLines(file.choose()) 
 head(rule) 
 attach(rule) 
 a1 
 table(a1) 
 table(Gender) # 데이터가 없는 곳은 subset에서 제외한 후 다시 진행 
 test <- subset(rule, select=c("a37","a38","a41","a42",   
                                                               "a43","a44","a45","a46","a47","a48","a49","a50", 
                                                               "a51","a53","a54","a55")) 
 head(test) 
 ## 요인분석 
 fit <- factanal(test, factors=5, rostation="varimax",scores = "regression") 
 print(fit, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
 e_value<- eigen(cor(test)) 
 e_value # 결과값 맨 위에 $values 부분에서 1.0밑으로 내려가기전까지만 묶어서 한다. 
 # a40과 같이 2개 이상의 요소에 걸릴 경우 한가지라도 0.6이상이라면 그것을 지키고 
 # 둘다 0.6이 안넘는다면 지워야 한다. --> 설명력을 높이기 위해서 
 fit$scores
 #종속변수
 #1 41,42,43,44,45,46,48,49 (동호회 활동)
 #2 53,54,55 ( 여행)
 #3 35,36 (모험형 레저)
 #4 47, 50,51 (컴퓨터 게임)
 #5 37,38 (구기운동)
 
 
 
 
 
 
 
 
 
 
 
 

 test2 <- subset(rule, select=c("a18","a20","a21","a22",   
                                                                 "a23","a24","a25","a26","a27","a28","a29","a30", 
                                                                 "a31","a33","a34")) 
 head(test2) 
 ## 요인분석 
 fit1 <- factanal(test2, factors=2, rostation="varimax") 
 print(fit1, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
 e_value<- eigen(cor(test2)) 
 e_value 
 

 

 test3 <- subset(rule, select=c("a1", "a3","a6",   
                                                                "a7","a8","a10","a12","a13","a14")) 
 head(test3) 
 ## 요인분석 
 fit2 <- factanal(test3, factors=5, rostation="varimax") 
 print(fit2, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
 e_value<- eigen(cor(test3)) 
 e_value 

 
 
 
 
 
test4 <- subset(rule, select=c("a18","a20","a21","a22",   
                                "a23","a24","a25","a26","a27","a28","a29","a30", 
                                "a31","a33","a34","a1", "a3","a6",   
                                "a7","a8","a10","a12","a13","a14"))
 head(test4) 
 ## 요인분석 
 fit3 <- factanal(test4, factors=5, rostation="varimax", scores = "regression") 
 print(fit3, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
 e_value<- eigen(cor(test3)) 
 e_value  
 
 fit3$scores

 
 
 
 #설문 사람당 독립변수에 대한 평균
 IND1<-(a23+a24+a25+a29)/4
 IND2<-(a20+a21+a22+a28+a30)/5
 IND3<-(a6+a7+a13)/3
 IND4<-(a26+a27)/2
 IND5<-(a17+a18)/2
 DP1<-(a41+a42+a43+a44+a45+a46+a48+a49)/8
 DP2<-(a53+a54+a55)/3
 DP3<-(a35+a36)/2
 DP4<-(a47+a50+a51)/3
 DP5<-(a37+a38)/2 
 
 DT <- cbind(rule, IND1,IND2,IND3,IND4,IND5,DP1,DP2,DP3,DP4,DP5)

 INSC1 <- fit3$scores[,1]
 INSC2 <- fit3$scores[,2]
 INSC3 <- fit3$scores[,3]
 INSC4 <- fit3$scores[,4]
 INSC5 <- fit3$scores[,5]
 
 DNSC1 <- fit$scores[,1]
 DNSC2 <- fit$scores[,2]
 DNSC3 <- fit$scores[,3]
 DNSC4 <- fit$scores[,4]
 DNSC5 <- fit$scores[,5]
 #
 DT <- cbind(DT, INSC1,INSC2,INSC3,INSC4,INSC5,DNSC1,DNSC2,DNSC3,DNSC4,DNSC5)
head(DT) 
sen <- (a1+a2+a3+a4+a5)/5 
DT<-cbind(DT, sen) 
DT 
attach(DT) # 컬럼을 가져올 경우 Data$ 와 같은 수식을 없앤다.
#min~1st / 1st~median / median~3rd / 3rd~max
summary(sen) 
# 중앙값으로 범위를 나눈다.
DT$sen_G[sen<2.800] <-1
DT$sen_G[sen>=2.800 &sen<3.200] <-2
DT$sen_G[sen>=3.200 &sen<3.600] <-3
DT$sen_G[sen>=3.600] <- 4
DT$sen_G
DT
attach(DT)
table(sen_G)


##교차분석

Cros_T<-table(Gender, sen_G) # 마진을 시키기 위해서 테이블을 하나의 변수로 지정해야 한다.
margin.table(Cros_T, 1) #row summ
margin.table(Cros_T, 2) #colum summ
margin.table(Cros_T) #전체 summ
# 테이블에 나오는 수들 : 관측빈도
# x^2= 시그마(관측빈도-기대빈도)^2 ==> 관측과 기대빈도간에 차이가 크면 카이제곱이 커진다.
chisq.test(Cros_T) 

#조금더 편리한 교차분석

install.packages("gmodels")
library("gmodels")
CrossTable(Cros_T, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

#자유도(df) =(row-1)(colum-1)



## 독립표본 t 검정 / ->등분산성 검정[var test]을 먼저해야 한다.

# DP 종속변수, INND 독립변수
attach(DT)
var.test(DP1~Gender)
var.test(DP2~Gender)
var.test(DP3~Gender)
var.test(DP4~Gender)
var.test(DP5~Gender)



t.test(DP1~Gender, alt="two.sided", var.eq=F) # 등분산성 검사로 분산이 차이가 난 것을 확인하고 차이나면 F , 아니면 T 
t.test(DP2~Gender, alt="two.sided", var.eq=F)
t.test(DP3~Gender, alt="two.sided", var.eq=F)
t.test(DP4~Gender, alt="two.sided", var.eq=T)
t.test(DP5~Gender, alt="two.sided", var.eq=F)

# p-value 가 0.05보다 작은 것들은 남녀간에 차이가 있는 것이다.
#결과 DP1 -  x / DP2-여 / DP3-X/DP4-남/DP-5남

var.test(IND1~Gender)
var.test(IND2~Gender)
var.test(IND3~Gender)
var.test(IND4~Gender)
var.test(IND5~Gender)

t.test(IND1~Gender, alt="two.sided", var.eq=T)
t.test(IND2~Gender, alt="two.sided", var.eq=T)
t.test(IND3~Gender, alt="two.sided", var.eq=F)
t.test(IND4~Gender, alt="two.sided", var.eq=F)
t.test(IND5~Gender, alt="two.sided", var.eq=T)

# paired t test

t.test(a1,a2, alt="two.sided")


#

install.packages("psych")
library("psych")
describeBy(IND1, Gender)
describeBy(IND2, Gender)
describeBy(IND3, Gender)
describeBy(IND4, Gender)
describeBy(IND5, Gender)

describeBy(DP1, sen_G)
describeBy(DP2, sen_G)
describeBy(DP3, sen_G)
describeBy(DP4, sen_G)
describeBy(DP5, sen_G)

# one-way ANOVA

FV1 <- aov(DP1~sen_G, data=DT)
summary(FV1)

FV2 <- aov(DP2~sen_G, data=DT)
summary(FV2)

FV3 <- aov(DP3~sen_G, data=DT)
summary(FV3)

FV4 <- aov(DP4~sen_G, data=DT)
summary(FV4)

FV5 <- aov(DP5~sen_G, data=DT)
summary(FV5)


## 사후 검정툴 : 세페 / 터키 /던칸 - 아노바에서 유의미하다고 나왔어도 두 그룹으로 구성된 모든 경우가 만족한지 안한지


## Scheffc's Multiple Comparison

install.packages("doBy")
install.packages("agricolae")
library("doBy")
library("agricolae")

scheffe.test(FV3, "sen_G", alpha=0.05, console=TRUE)
# 알파벳 다른 것 끼리만 차이가 난다.
scheffe.test(FV4, "sen_G", alpha=0.05, console=TRUE)
duncan.test(FV4, "sen_G", alpha=0.05, console=TRUE)
# 던칸이 더 민감해서 FV4에 대해서 결과가 다르다.

scheffe.test(FV5, "sen_G", alpha=0.05, console=TRUE)
duncan.test(FV5, "sen_G", alpha=0.05, console=TRUE)
# 던칸이 더 민감함을 알 수 있다.



## Two-way ANOVA / 성별 2 과 집단(sen_G) 4이 있을 때 두개 볼 때 주효과 / 8개를 섞어서 볼때 상호작용 효과

FFV1 <- aov(DP1~Gender+sen_G)
summary(FFV1)

#주효과 각각 1개씩만 나타낸다.

FFV1 <- aov(DP1~Gender+sen_G+Gender:sen_G)
summary(FFV1)

#상호작용 효과

FFV2 <- aov(DP2~Gender+sen_G+Gender*sen_G)
summary(FFV2)

FFV3 <- aov(DP2~Gender+sen_G+Gender:sen_G)
summary(FFV3)







describeBy(DP5, Gender*sen_G)



# 상관관계 분석

cor(DP1, DP2)

COR_T <- (subset(DT, select=c(IND1, IND2, IND3, IND4, IND5, DP1, DP2, DP3, DP4, DP5)))
cor(COR_T, method="pearson")
# 2개 이상일 떈 subset을 통해 선택하고 분석을 한다.


#Scatter plot - 종속변수간 -> 의미 없다.

plot(DP1, DP2)
plot(DP2, DP3)

#Scatter plot - 종속변수와 독립변수간 

plot(IND1, DP2, pch=1) #숫자에 따라서 다르다.
plot(DP2, DP3)

# MUltiple Regression

reg1<- lm(DP1~IND1+IND2+IND3+IND4+IND5, data=DT)
summary(reg1)
# t와 estimate std(잔차)와 차이가 많이 날수록 PR 즉 차이가 크다.
reg2<- lm(DP1~IND1+IND2+IND3+IND4+IND5, data=DT)
summary(reg2)
head(DT)
# INSC 1~5 / DNSC 1~5
regSC1<- lm(DNSC1~INSC1+INSC2+INSC3+INSC4+INSC5, data=DT)
summary(regSC1)
# 설문 분석시 이것이 더 의미가 있다. -> 팩터를 평가한 것을 쓰기 때문



# 잔차 분석

par(mfrow=c(2,2)) #2행2열의 그래프
plot(reg2)
# 오른쪽 위 그래프 정규성에 거의 일치하지만 끝과 시작이 벗어나있다. Q_Q그래프 중요
# 빨간선이 직선 그리고 그 위에 점들이 따라가야 한다.

resid<-residuals(reg2) # 잔차값 -> 차이가 많이 나면 정확한 값이 맞는지 확인해야 한다. / 데이터 패턴에 따른 모형을 만들어야한다.
shapiro.test(resid) # 잔차가 정규선에 따르는 지 보는 것. 0.2보다 커야 따르는 것이다.

# 다중 공선성
#정교선 #잔차 등분산 qq plot 확인 /먼저 회귀분석 / 잔차의 공선성 확인(vif) / 잔차가 큰것에 대한 확인(residuals)
install.packages("car")
library(car)
vif(reg2) # 10이 넘어가면 안된다. 다중공선성 다중회귀에서 0.7이상이거나 vif 값이 10이상이면 변수를 뺀다.












