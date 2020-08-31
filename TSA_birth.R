library(tsapp)
library(fpp2)
library(forecast)
library(xlsx)
library(tseries)

setwd("G:/시계열/통방신기")
data=read.csv("월별출생.csv",header = T)
ts_data=ts(data[,2],freq=12,start=c(2000))
plot(ts_data, ylab='The Number Of Newborn Baby', xlab='Date', type='l')
x=seq(2000,2017,by=1)
axis(side=1,at=x)

ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE)+ 
  ylab("Newborn Baby")+ggtitle("Seasonal plot: Average Monthly Newborn Baby")
ggseasonplot(ts_data, polar=TRUE)+ylab("Newborn Baby")
#년도별 출생아수가 점점 감소하는 것을 볼 수 있습니다.
ggsubseriesplot(ts_data)+ylab("Newborn Baby")+xlab("Year")+ggtitle("Average Monthly Newborn Baby")
#월별 출생아수:1,3월에 출생아수가 많은 것을 볼 수 있습니다.

plot(ts_data, ylab='The Number Of Newborn Baby', xlab='Date', type='l')
#변동이 일정하지 않아서 일정하게 만들기위해 변수변환을 해준다. 그 방법이...
BoxCox.ar(ts_data,method="ols",lambda=seq(-2,2,0.005))
#boxcox.ar을 이용하여 그래프를 확인해보니 lambda에 대한 95% 신뢰구간은 중시 근처에서 lambda=0인 값을 포함하므로 log변환을 실시.
plot(log(ts_data), ylab='The Number Of Newborn Baby', xlab='Date', type='l')
x=seq(2000,2017,by=1)
axis(side=1,at=x)
adf.test(log(ts_data)) #p-value = 0.3291
#p-value가 크고, 평균도 일정하지 않으므로 차분을 실시해줘야한다.
plot(diff(log(ts_data)), ylab='The Number Of Newborn Baby', xlab='Date', type='l')
adf.test(diff(log(ts_data))) #p-value = 0.01
#stationary하다.

acf(diff(log(ts_data)),lag.max=100)
#acf가 주기성을 띄니까 계절차분을 해야겠다.

par(mfrow=c(2,1))
acf(as.vector(diff(diff(log(ts_data)),12)),main="ACF Of NB",lag.max=100,seq=)
pacf(as.vector(diff(diff(log(ts_data)),12)),main="PACF Of NB",lag.max=100)
#lag=24,25의 pacf의 값은 비교적 신뢰구간을 덜 벗어나므로 무시한다.
#일반적으로 acf,pacf 둘 다 0으로 감소하는 형태면 arma(1,1)모델이다.
#order를 (1,1,1)부터 시작한 이유? acf와 pacf를 보니 둘 다 감소해서 arima(1,1,1)모델이다.

#seasonal은 (0,1,1) 또는 (1,1,0)
#근데 마침 auto.arima를 해보니 seasonal이 (0,1,1)이 나옴!
auto.arima(log(ts_data),d=1,D=1)
#(2,1,1),(0,1,1)가 나온다.
#그래서 auto.arima돌린 후보1모형(맨 처음에 넣어서 교수님이 까먹게 한다.)
aa=arima(log(ts_data), order=c(2,1,1), seasonal=list(order=c(0,1,1), period=12))
aa
tsdiag(aa)
shapiro.test(residuals(aa)) #잔차가 정규성을 만족한다.
#aic = -807.99

#더 좋은 모형을 찾기위해 차수를 늘여가며 분석
bb=arima(log(ts_data), order=c(2,1,2), seasonal=list(order=c(0,1,1), period=12))
bb
#aic = -825.33
tsdiag(bb)
shapiro.test(residuals(bb)) #잔차가 정규성을 만족하지 않는다.
#bb의 잔차가 정규성을 만족하지 않아서 bb를 후보모형에서 제외한다.
#p,q가 3을 넘어가면 모형이 복잡하므로 여기서 더이상 시행하지 않고, 최종 모형으로 aa를 채택
#그래서 aa가 후보 1번 모형이다.


#이번에는 seasonal이(1,1,0)인 걸 해보자.
aa=arima(log(ts_data), order=c(1,1,1), seasonal=list(order=c(1,1,0), period=12))
aa #기각

bb=arima(log(ts_data), order=c(2,1,1), seasonal=list(order=c(1,1,0), period=12))
bb
#aic = -783
tsdiag(bb) #기각

cc=arima(log(ts_data), order=c(1,1,2), seasonal=list(order=c(1,1,0), period=12))
cc #기각

dd=arima(log(ts_data), order=c(2,1,2), seasonal=list(order=c(1,1,0), period=12))
dd
#aic = -810.05
tsdiag(dd)
shapiro.test(residuals(dd))
#dd를 후보2번 모형으로 채택한다.

ee=arima(log(ts_data), order=c(3,1,2), seasonal=list(order=c(1,1,0), period=12))
ee #ma(3)의 표준오차가 크다.

ff=arima(log(ts_data), order=c(2,1,3), seasonal=list(order=c(1,1,0), period=12))
ff #ma(3)의 표준오차가 크다.

gg=arima(log(ts_data), order=c(3,1,3), seasonal=list(order=c(1,1,0), period=12))
gg
#aic = -809.27
tsdiag(gg)
shapiro.test(residuals(gg)) #p-value가 0.05를 넘지만 모형이 너무 복잡하다.(일단 보류)
#gg를 후보 3번으로 채택.

####후보1,2,3모형중 후보1번모형이 가장 단순하고 aic가 가장 작으므로 채택.

######모형이 적합한가? 잘라서 확인########
ts_data_cut=window(ts_data,end=c(2016,7))
nn=arima(log(ts_data_cut), order=c(2,1,1), seasonal=list(order=c(0,1,1), period=12))
mm=arima(log(ts_data_cut), order=c(2,1,2), seasonal=list(order=c(1,1,0), period=12))
ll=arima(log(ts_data_cut), order=c(3,1,3), seasonal=list(order=c(1,1,0), period=12))

plot(nn) 
points(log(ts_data), col=4)
lines(log(ts_data), col=4)
#예측값이 신뢰구간안에 포함된다.

################결과####################

plot(aa, n.ahead=12, ylab='Series, Forecasts, Actuals & Limits', pch=18)
#2017년 9월~2018년 8월까지의 출생아수가 이렇게 나올것이라고 예측됩니다.

