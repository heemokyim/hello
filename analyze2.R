rule<-read.csv(file.choose(), header =T) # ���� �ִ� �׸� �̸��� ǥ���Ҷ� header = T 
 #rule <- readLines(file.choose()) 
 head(rule) 
 attach(rule) 
 a1 
 table(a1) 
 table(Gender) # �����Ͱ� ���� ���� subset���� ������ �� �ٽ� ���� 
 test <- subset(rule, select=c("a37","a38","a41","a42",   
                                                               "a43","a44","a45","a46","a47","a48","a49","a50", 
                                                               "a51","a53","a54","a55")) 
 head(test) 
 ## ���κм� 
 fit <- factanal(test, factors=5, rostation="varimax",scores = "regression") 
 print(fit, cutoff=0.4, digit=3) # 0.4�̻� / 3�ڸ������� 
 e_value<- eigen(cor(test)) 
 e_value # ����� �� ���� $values �κп��� 1.0������ ���������������� ��� �Ѵ�. 
 # a40�� ���� 2�� �̻��� ��ҿ� �ɸ� ��� �Ѱ����� 0.6�̻��̶�� �װ��� ��Ű�� 
 # �Ѵ� 0.6�� �ȳѴ´ٸ� ������ �Ѵ�. --> �������� ���̱� ���ؼ� 
 fit$scores
 #���Ӻ���
 #1 41,42,43,44,45,46,48,49 (��ȣȸ Ȱ��)
 #2 53,54,55 ( ����)
 #3 35,36 (������ ����)
 #4 47, 50,51 (��ǻ�� ����)
 #5 37,38 (����)
 
 
 
 
 
 
 
 
 
 
 
 

 test2 <- subset(rule, select=c("a18","a20","a21","a22",   
                                                                 "a23","a24","a25","a26","a27","a28","a29","a30", 
                                                                 "a31","a33","a34")) 
 head(test2) 
 ## ���κм� 
 fit1 <- factanal(test2, factors=2, rostation="varimax") 
 print(fit1, cutoff=0.4, digit=3) # 0.4�̻� / 3�ڸ������� 
 e_value<- eigen(cor(test2)) 
 e_value 
 

 

 test3 <- subset(rule, select=c("a1", "a3","a6",   
                                                                "a7","a8","a10","a12","a13","a14")) 
 head(test3) 
 ## ���κм� 
 fit2 <- factanal(test3, factors=5, rostation="varimax") 
 print(fit2, cutoff=0.4, digit=3) # 0.4�̻� / 3�ڸ������� 
 e_value<- eigen(cor(test3)) 
 e_value 

 
 
 
 
 
test4 <- subset(rule, select=c("a18","a20","a21","a22",   
                                "a23","a24","a25","a26","a27","a28","a29","a30", 
                                "a31","a33","a34","a1", "a3","a6",   
                                "a7","a8","a10","a12","a13","a14"))
 head(test4) 
 ## ���κм� 
 fit3 <- factanal(test4, factors=5, rostation="varimax", scores = "regression") 
 print(fit3, cutoff=0.4, digit=3) # 0.4�̻� / 3�ڸ������� 
 e_value<- eigen(cor(test3)) 
 e_value  
 
 fit3$scores

 
 
 
 #���� ����� ���������� ���� ���
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
attach(DT) # �÷��� ������ ��� Data$ �� ���� ������ ���ش�.
#min~1st / 1st~median / median~3rd / 3rd~max
summary(sen) 
# �߾Ӱ����� ������ ������.
DT$sen_G[sen<2.800] <-1
DT$sen_G[sen>=2.800 &sen<3.200] <-2
DT$sen_G[sen>=3.200 &sen<3.600] <-3
DT$sen_G[sen>=3.600] <- 4
DT$sen_G
DT
attach(DT)
table(sen_G)


##�����м�

Cros_T<-table(Gender, sen_G) # ������ ��Ű�� ���ؼ� ���̺��� �ϳ��� ������ �����ؾ� �Ѵ�.
margin.table(Cros_T, 1) #row summ
margin.table(Cros_T, 2) #colum summ
margin.table(Cros_T) #��ü summ
# ���̺��� ������ ���� : ������
# x^2= �ñ׸�(������-����)^2 ==> ������ ���󵵰��� ���̰� ũ�� ī�������� Ŀ����.
chisq.test(Cros_T) 

#���ݴ� ������ �����м�

install.packages("gmodels")
library("gmodels")
CrossTable(Cros_T, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

#������(df) =(row-1)(colum-1)



## ����ǥ�� t ���� / ->��л꼺 ����[var test]�� �����ؾ� �Ѵ�.

# DP ���Ӻ���, INND ��������
attach(DT)
var.test(DP1~Gender)
var.test(DP2~Gender)
var.test(DP3~Gender)
var.test(DP4~Gender)
var.test(DP5~Gender)



t.test(DP1~Gender, alt="two.sided", var.eq=F) # ��л꼺 �˻�� �л��� ���̰� �� ���� Ȯ���ϰ� ���̳��� F , �ƴϸ� T 
t.test(DP2~Gender, alt="two.sided", var.eq=F)
t.test(DP3~Gender, alt="two.sided", var.eq=F)
t.test(DP4~Gender, alt="two.sided", var.eq=T)
t.test(DP5~Gender, alt="two.sided", var.eq=F)

# p-value �� 0.05���� ���� �͵��� ���ణ�� ���̰� �ִ� ���̴�.
#��� DP1 -  x / DP2-�� / DP3-X/DP4-��/DP-5��

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


## ���� ������ : ���� / ��Ű /��ĭ - �Ƴ�ٿ��� ���ǹ��ϴٰ� ���Ծ �� �׷����� ������ ��� ��찡 �������� ������


## Scheffc's Multiple Comparison

install.packages("doBy")
install.packages("agricolae")
library("doBy")
library("agricolae")

scheffe.test(FV3, "sen_G", alpha=0.05, console=TRUE)
# ���ĺ� �ٸ� �� ������ ���̰� ����.
scheffe.test(FV4, "sen_G", alpha=0.05, console=TRUE)
duncan.test(FV4, "sen_G", alpha=0.05, console=TRUE)
# ��ĭ�� �� �ΰ��ؼ� FV4�� ���ؼ� ����� �ٸ���.

scheffe.test(FV5, "sen_G", alpha=0.05, console=TRUE)
duncan.test(FV5, "sen_G", alpha=0.05, console=TRUE)
# ��ĭ�� �� �ΰ����� �� �� �ִ�.



## Two-way ANOVA / ���� 2 �� ����(sen_G) 4�� ���� �� �ΰ� �� �� ��ȿ�� / 8���� ��� ���� ��ȣ�ۿ� ȿ��

FFV1 <- aov(DP1~Gender+sen_G)
summary(FFV1)

#��ȿ�� ���� 1������ ��Ÿ����.

FFV1 <- aov(DP1~Gender+sen_G+Gender:sen_G)
summary(FFV1)

#��ȣ�ۿ� ȿ��

FFV2 <- aov(DP2~Gender+sen_G+Gender*sen_G)
summary(FFV2)

FFV3 <- aov(DP2~Gender+sen_G+Gender:sen_G)
summary(FFV3)







describeBy(DP5, Gender*sen_G)



# ������� �м�

cor(DP1, DP2)















 