library(quadprogXT)
library(quantmod)
library(xts)

rm(list=ls())

today=Sys.Date()
start.date <- as.Date("2013-01-01")
end.date   <- as.Date("2017-12-31")

#ss_stock=getSymbols('005930.KS',from=today-365*1,to=today,auto.assign = F)

#LJ 化学
#LJ 电子
#现代汽车
#乐天精密化学
#CJCGV


ljc_stock=getSymbols('051910.KS',from=start.date,to=end.date, auto.assign = F)
lje_stock=getSymbols('066570.KS',from=start.date,to=end.date, auto.assign = F)
kse_stock=getSymbols('005380.KS',from=start.date,to=end.date, auto.assign = F)
bl_stock=getSymbols('004000.KS',from=start.date,to=end.date, auto.assign = F)
cgv_stock=getSymbols('079160.KS',from=start.date,to=end.date, auto.assign = F)

colnames(ljc_stock) <- c('open','high','low','close','volume','adjusted')
colnames(lje_stock) <- c('open','high','low','close','volume','adjusted')
colnames(kse_stock) <- c('open','high','low','close','volume','adjusted')
colnames(bl_stock) <- c('open','high','low','close','volume','adjusted')
colnames(cgv_stock) <- c('open','high','low','close','volume','adjusted')

#
ljc_rtn <- log(ljc_stock$adjusted) - log(lag(ljc_stock$adjusted))
lje_rtn <- log(lje_stock$adjusted) - log(lag(lje_stock$adjusted))
kse_rtn <- log(kse_stock$adjusted) - log(lag(kse_stock$adjusted))
bl_rtn <- log(bl_stock$adjusted) - log(lag(bl_stock$adjusted))
cgv_rtn <- log(cgv_stock$adjusted) - log(lag(cgv_stock$adjusted))
write.zoo(ljc_rtn,file = 'L:/mydata/ljc_stock.csv',sep=',')
write.zoo(lje_rtn,file = 'L:/mydata/lje_stock.csv',sep=',')
write.zoo(kse_rtn,file = 'L:/mydata/kse_stock.csv',sep=',')
write.zoo(bl_rtn,file = 'L:/mydata/bl_stock.csv',sep=',')
write.zoo(cgv_rtn,file = 'L:/mydata/cgv_stock.csv',sep=',')

plot(ljc_rtn)

nvar <- 5
m.rtn <- as.matrix(cbind(ljc_rtn,lje_rtn,kse_rtn,bl_rtn,cgv_rtn))
colnames(m.rtn) <- c('ljc','lje','kse','bl','cgv')


# annualized return and covariance

mu    <- colMeans(m.rtn,na.rm = TRUE)*252
cov   <- cov(m.rtn,use="complete.obs")*252

# portfolio optimization

rset <- seq(min(mu)+0.0000001,max(mu)-0.0000001,length=100)

port1.ret <- rset
port1.std <- rset*0
port1.wgt <- matrix(0,100,nvar)

for (i in 1:100)
{
  Dmat <- 2*cov
  dvec <- rep(0,nvar) #c(0,0)
  Amat <- t(rbind(t(rep(1,nvar)),t(mu),diag(nvar)))
  bvec <- c(1,rset[i],rep(0,nvar))
  m<-solveQPXT(Dmat,dvec,Amat,bvec,meq=2,factorized=FALSE)
  
  port1.std[i] <- sqrt(m$value)
  port1.wgt[i,] <- t(m$solution)
}

#plot(sqrt(diag(cov)),mu,xlim=c(0,1.2*max(port1.std)),col = "blue", lwd = 8)
#lines(port1.std,port1.ret,col = "red", lwd = 4)

par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
plot(sqrt(diag(cov)),mu,xlim=c(0,1.2*max(port1.std)),col = "blue", lwd = 8)
lines(port1.std,port1.ret,col = "red", lwd = 4)

barplot(t(port1.wgt),col=rainbow(nvar))
#pal <- colorRampPalette(colors = c("lightblue", "blue"))(nvar)
#barplot(t(port1.wgt),col = pal) 
#barplot(t(port1.wgt),col=grey.colors(nvar))

kospi_stock<-getSymbols('KRX：KOSPI200',from=start.date,to=end.date, auto.assign = F)
start.date1<-as.Date("2018-01-01")
end.date1<-as.Date("2019-01-01")
m$solution

ljc_out_stock=getSymbols('051910.KS',from=start.date1,to=end.date1, auto.assign = F)
lje_out_stock=getSymbols('066570.KS',from=start.date1,to=end.date1, auto.assign = F)
kse_out_stock=getSymbols('005380.KS',from=start.date1,to=end.date1, auto.assign = F)
bl_out_stock=getSymbols('004000.KS',from=start.date1,to=end.date1, auto.assign = F)
cgv_out_stock=getSymbols('079160.KS',from=start.date1,to=end.date1, auto.assign = F)

colnames(ljc_out_stock) <- c('open','high','low','close','volume','adjusted')
colnames(lje_out_stock) <- c('open','high','low','close','volume','adjusted')
colnames(kse_out_stock) <- c('open','high','low','close','volume','adjusted')
colnames(bl_out_stock) <- c('open','high','low','close','volume','adjusted')
colnames(cgv_out_stock) <- c('open','high','low','close','volume','adjusted')

#
ljc_out_rtn <- log(ljc_out_stock$adjusted) - log(lag(ljc_out_stock$adjusted))
lje_out_rtn <- log(lje_out_stock$adjusted) - log(lag(lje_out_stock$adjusted))
kse_out_rtn <- log(kse_out_stock$adjusted) - log(lag(kse_out_stock$adjusted))
bl_out_rtn <- log(bl_out_stock$adjusted) - log(lag(bl_out_stock$adjusted))
cgv_out_rtn <- log(cgv_out_stock$adjusted) - log(lag(cgv_out_stock$adjusted))
combine_rtn=m$solution[1]*ljc_out_rtn+m$solution[2]*lje_out_rtn+m$solution[3]*kse_out_rtn+m$solution[4]*bl_out_rtn+m$solution[5]*cgv_out_rtn
combine.cov   <- cov(combine_rtn,use="complete.obs")*252

library(dplyr)

library(PerformanceAnalytics)
SharpeRatio(combine_rtn,0.01)

kospi_data<-read.csv('K:/Edownload/KOSPI Historical Data.csv')

kospi_data$锘緿ate<- as.Date(kospi_data$锘緿ate)
kospi_data$Change <- as.numeric(kospi_data$Change)

row.names(kospi_data) <- kospi_data[[1]]
kospi_data<- as.xts(kospi_data)


as.numeric(kospi_data$Change[1])
i=0
for (i in length(kospi_data$Change)){
  kospi_data$Change[i] <- as.numeric(kospi_data$Change[i])
}
kos1<- as.data.frame(kospi_data$Change)
names(kos1) <- 'Change'
row.names(kos1) <- kospi_data$锘緿ate
kos2<- as.xts(kos1)
cov(kos1$`kospi_data$Change`) 
SharpeRatio.annualized(kos2,0.01)
