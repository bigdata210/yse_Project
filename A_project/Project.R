setwd("C:/Rdata")
getwd()
library(readxl)
library(stringr)
library(ggplot2)
install.packages("plotrix")
library(plotrix)
tb <- read_xlsx("온실가스분야별배출량.xlsx")
tb <- as.data.frame(tb)
str(tb)
pie(tb$"2020",labels=tb$...1,
      col=c("#58D883","#CEDB59","#A746CE","#4865D6"), explode=0.1,
      labelcex=0.5, labelrad=0.5)


tb1 <- read.csv("국가온실가스분야별배출량.csv", fileEncoding ="CP949")
head(tb1)
class(tb1)
names(tb1)

xat=seq(1990,2020,by=1) 
yat=seq(0,700,by=20)

ggplot(data=tb1, aes(x=구분, y=배출량))+
  geom_line(aes(y=총배출량), col="#F25E60", size=3)+
  geom_line(aes(y=에너지),col="#34E200", size=2)+
  geom_line(aes(y=산업공정),col="#ED9E00", size=2)+
  geom_line(aes(y=농업), col="#BD45C1", size=2)+
  geom_line(aes(y=폐기물),col="#007AED", size=2)+
  scale_y_continuous(limits=c(0,800), breaks=seq(0,800, by=100))+
  scale_x_continuous(limits=c(1990,2020), breaks=seq(1990,2020, by=1))+
  xlab("")+
  ylab("")


library(RColorBrewer)
tb2 <- read.csv("세계온실가스배출량.csv", fileEncoding ="CP949")
head(tb2)
ggplot(data=tb2, aes(x=년도))+
  geom_line(aes(y=대한민국), col = "blue", size=2)+
  geom_line(aes(y=중국),col="red",size=2)+
  geom_line(aes(y=인도),col="orange",size=2)+
  geom_line(aes(y=일본), col="green",size=2)+
  geom_line(aes(y=캐나다), col="aquamarine",size=2)+
  geom_line(aes(y=미국), col="purple",size=2)+
  geom_line(aes(y=독일), col="pink1",size=2)+
  geom_line(aes(y=러시아), col="brown",size=2)+
  geom_line(aes(y=영국), col="darkgreen",size=2)+
  scale_y_continuous(limits=c(0,11000), breaks=seq(0,11000, by=1000))+
  scale_x_continuous(limits=c(1990,2020), breaks=seq(1990,2020, by=1))+
  xlab("")+
  ylab("")

tb3 <- read.csv("온실가스농도.csv", fileEncoding ="CP949")
tb3

ggplot(data=tb3, aes(x=년도))+
  geom_line(aes(y=한국), col="#272DC6",size=2)+
  geom_line(aes(y=세계), col="#ED2FB1",size=2)+
  scale_y_continuous(limits=c(450,550), breaks=seq(450,550, by=20))+
  scale_x_continuous(limits=c(2011,2020), breaks=seq(2011,2020, by=1))+
  ylab("")

library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)

k6 <- read.csv("산림흡수량.csv", fileEncoding = "EUC-KR")
k6

k1 <- read.csv("GDP 대비1인당 온실가스 배출량.csv", fileEncoding = "EUC-KR")
k1

ggplot(k1, aes(x=년도))+
  geom_line(aes(y=GDP), col=2, size=2)+
  geom_line(aes(y=X1인당), col=3, size=2)+
  scale_x_continuous(limits=c(1990, 2018), breaks=seq(1990,2018,by=1))+
  scale_y_continuous(limits=c(2,15), breaks=seq(2,15,by=2))+
  xlab("")+
  ylab("")

k_lulu <- k6 %>% select(연도=구분, LULUCF)
k_lulu

ggplot(k_lulu, aes(x=연도))+
  geom_line(aes(y=abs(LULUCF)), col="#1BC643",size=2)+
  scale_x_continuous(limits=c(1990, 2020), breaks=seq(1990,2020,by=1))+
  scale_y_continuous(limits=c(20,70), breaks=seq(20,70,by=5))+
  xlab("")+
  ylab("")


cs <- read.csv("분석용.csv", fileEncoding = "CP949")
head(cs)
out <- lm(CO2~., data=cs)
summary(out)

library(leaps)
leaps <- regsubsets(CO2~.,data=cs, nbest=10)
summary(leaps)
plot(leaps)

out_bic <- lm(CO2~SF6+열대야일수, data=cs)
summary(out_bic)

plot(leaps,scale="adjr2")
out_adj <- lm(CO2~평균기온+강수일수+열대야일수+HFCs+SF6, data=cs)
summary(out_adj)
out_agj1 <- lm(평균기온~., data=cs)
summary(out_agj1)


cc <- read.csv("연기후_온실가스데이터.csv", fileEncoding = "CP949")

cc1 <- lm(총배출량~.,data=cc)
summary(cc1)
cc2 <- lm(연평균기온~.,data=cc)
summary(cc2)
leaps <- regsubsets(연평균기온~+0,data=cc, nbest=7)
summary(leaps)

plot(leaps)
plot(leaps, scale="adjr2")
out_adj2 <- lm(연평균기온~연도+강수일수+폭염일수+한파일수+총배출량+LULUCF,data=cc)
summary(out_adj2)
head(cc)
ccc <- cc[,c('총배출량','연도','연강수량','한파일수','LULUCF')]
plot(ccc)
summary(out_adj2)
cor(cc)
head(cc)
plot(leaps, scale="adjr2")
out_adj3 <- lm(연평균기온~.,data=cc)
summary(out_adj3)
cor(cc)
out_adj3 <- lm(연평균기온~연도+연강수량+강수일수+한파일수+총배출량+LULUCF+0,data=cc)
summary(out_adj3)
plot(out_adj3)

out_adj5 <- lm(연평균기온~한파일수+총배출량,data=cc)
summary(out_adj5)
cor(cc)
out_adj4 <- lm(총배출량~연도+0,data=cc)
summary(out_adj4)

cor(cc)

step(lm(총배출량~.,data=cc), direction="both", trace=FALSE)


oc <- read.csv("연해양_온실가스데이터.csv", fileEncoding = "CP949")
oc1 <- na.omit(oc)
oc1 <- lm(연해양평균기온~.,data=oc1)
summary(oc1) #결정계수 :1
cor(oc)

bm <- read.csv("온실가스배출량(부문별).csv", fileEncoding ="CP949")
bm1 <- lm(총배출량~.,data=bm)
summary(bm1)  #결정계수 :1
x <- (13.02+0.000463-0.104574+0.017138+0.013602)/0.003113
x
x1 <- (13.52+0.000463-0.104574+0.017138+0.013602)/0.003113
x1
y <- -0.000463+0.104574-0.017138+0.003113*x-0.013602
y
b=137
a <- 0.27705*b
b <- a/0.27705
a <- a2-a1
a1=4158.891
a2=4319.508
b1 <- a1/0.27705
b2 <- a2/0.27705
b1
b2

a1 <- (1.967e+03)+(6.873e-02)*b1
a2 <- (1.967e+03)+(6.873e-02)*b2
a1
a2
c <- 15591-15011

c(2,4,6,8)+c(3,5,7,9,11)

10+5+25+5+20+20+15
0/0


mx=matrix(c(1,2,3,4,5,6), ncol=2,byrow=T)
mx
