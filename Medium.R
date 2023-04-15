rm(list=ls())
# Install these packages
library(zoo); library(rugarch); library(rmgarch)
library(PerformanceAnalytics)
setwd("~/")   # Working directory

# Read the data set: BTC
BTC=read.csv("bit.csv",header = TRUE)
BTC[,1]=as.Date(BTC[,1],format="%B-%d-%Y")
BTC=zoo(BTC[,2:3],order.by=BTC[,1])
BTC=BTC$Close; 

# Read the data set: SP500
SP=read.csv("sp.csv",header = TRUE);
SP[,1]=as.Date(SP[,1],format="%d-%b-%y")
SP=zoo(SP[,2],order.by=SP[,1])
SP=(SP);

# Merge the data set
P=merge(SP,BTC,all=FALSE)
# Log return in %
R=100*diff(log(P))

# Time plots
par(mfrow=c(1,2))
chart.TimeSeries(P,legend.loc = "topleft", main="Price")
chart.TimeSeries(P$SP,main="SP index")
par(mfrow=c(1,1))
chart.TimeSeries(R,main="Logarithmic Returns in Percentage",legend.loc = "bottomright")

# Autocorrelation function for returns
par(mfrow=c(1,2))
chart.ACF(R$SP,main="SP")
chart.ACF(R$BTC,main="BTC")

# Autocorrelation function for |returns|
chart.ACF(abs(R$SP),main="SP")
chart.ACF(abs(R$BTC),main="BTC")

# Histograms
chart.Histogram(R$SP,methods = c("add.rug"),element.color = "blue",
                colorset = "#00008F",main="SP")
chart.Histogram(R$BTC,methods = "add.rug",element.color = "blue",
                colorset = "#00008F",main="BTC")

# Q-Q plots
chart.QQPlot(R$SP,main="SP",ylim=c(-40,20))
chart.QQPlot(R$BTC,main="BTC",ylim=c(-40,20)) 

# Boxplots
par(mfrow=c(1,1))
chart.Boxplot(R)

#par(mar = c(2, 3, 2, 2)) # Set the margin on all sides to 2
# GARCH(1,1) model
model=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)), 
                 mean.model=list(armaOrder=c(0,0)))

# For SP500 return
fit=ugarchfit(spec=model,data=R$SP)
# Extracting conditonal standard deviation 
sd.SP=zoo(fit@fit$sigma,order.by = index(R))

# For BTC return
fit=ugarchfit(spec=model,data=R$BTC)
# Extracting conditonal standard deviation
sd.BTC=zoo(fit@fit$sigma,order.by = index(R))
             

# plotting Conditional standard deviation from GARCH(1,1)
par(mfrow=c(1,1))
plot(sd.BTC,ylim=c(0,20),col="blue",lwd=2, ylab="volatility",
     main="Conditional standard deviation from GARCH(1,1)",
     cex.lab=0.7,cex.axis=0.7)
lines(sd.SP,col="red",lwd=2)
legend("topleft",legend=c("BTC", "SP"),
       col=c("blue", "red"), lty=1, cex=0.8,lwd=2)




# GJR-GARCH(1,1) model
model=ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)), 
                 mean.model=list(armaOrder=c(0,0)))

# New impact curve from the plot function
fit=ugarchfit(spec=model,data=R$SP)
plot(fit)
fit=ugarchfit(spec=model,data=R$BTC)
plot(fit)

# Correlations
par(mfrow=c(1,1))

# DCC model estimation for correlation 
model1=ugarchspec(mean.model = list(armaOrder=c(0,0)),
                  variance.model = list(garchOrder=c(1,1),model="sGARCH"),
                  distribution.model = "norm")
modelspec=dccspec(uspec = multispec(replicate(2,model1)),dccOrder = c(1,1),
                  distribution = "mvnorm")
modelfit=dccfit(modelspec,data = data.frame(R))

# Extracting correlation values
rho.est.line <- list()
rho.est.line = rcor(modelfit, type="R") # plot(rho.est.line[1,2,])
outputcorr <- matrix((rho.est.line[1,2,]), nrow = nrow(R), byrow = TRUE)
rho.est = data.frame((outputcorr)) 
rho.est.zoo = zoo(rho.est, order.by=index(R)) 

# Historical Correlation
wlength=220    # window length
index=1:wlength
tem=numeric()
while ( index[length(index)] <= nrow(R) ) {
tem=c(tem,cor(R$SP[index],R$BTC[index]))
index=index+1}
tem=zoo(tem, order.by = index(R)[(wlength+1):nrow(R)])

# plotting correlations
plot(rho.est.zoo,main="Correlation: BTC vs. SP",ylab="correlation",
     col="blue",lwd=2,ylim=c(-0.25,0.6),cex.axis=0.8)
abline(h=0)
lines(tem,lwd=2,col="red")
legend("topleft",legend=c("Conditional", "Historical"),
       col=c("blue", "red"), lty=1, cex=0.8,lwd=2)


# Standardizing the returns
sharp=cbind(R$SP/sd.SP,R$BTC/sd.BTC)
colnames(sharp)=c("SP","BTC")
chart.TimeSeries(sharp,legend.loc = "bottomright")
chart.Boxplot(sharp)
summary(sharp)

# Calculating and plotting the returns 
# With holdng period set in hold
hold=660
index=1:hold
tem=numeric()
while ( index[length(index)] <= nrow(R) ) {
  tem=rbind(tem,colMeans(R[index]))
  index=index+1}
tem=zoo(tem, order.by = index(R)[(hold+1):nrow(R)])

# Plotting
plot(tem[,2],main="Unstandardized Return",ylab="",
     col="blue",lwd=2)
abline(h=0)
lines(tem[,1],lwd=2,col="red")
legend("topright",legend=c("Conditional", "Historical"),
       col=c("blue", "red"), lty=1, cex=0.8,lwd=2)

# Calculating and plotting the standardized returns 
# With holdng period set in hold
index=1:hold
tem=numeric()
while ( index[length(index)] <= nrow(sharp) ) {
  tem=rbind(tem,colMeans(sharp[index]))
  index=index+1}
tem=zoo(tem, order.by = index(sharp)[(hold+1):nrow(R)])

# Plotting
plot(tem[,2],main="Standardized Return",ylab="",
     col="blue",cex.axis=0.8,lwd=2)
abline(h=0)
lines(tem[,1],lwd=2,col="red")
legend("topleft",legend=c("BTC", "SP"),
       col=c("blue", "red"), lty=1, cex=0.8,lwd=2)





