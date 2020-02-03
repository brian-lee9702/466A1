fileRead <- read.csv("Bond Data T.csv")
BondData <- subset(fileRead, select = -c(Name, Coupon))
Coupons <- fileRead$Coupon
coup <- as.list(Coupons)
coup <- as.numeric(coup)
TTM <- read.csv("6 Month Periods to Maturity.csv")
maturity <- read.csv("Maturiy Dates.csv")
mat <- as.Date(maturity[, 1])
dates <- read.csv("Dates.csv")
dates <- as.Date(dates$Dates)

ByDates <- c()
for (i in colnames(BondData)){
  ByDates <- c(ByDates, BondData[i])
}

ByDates2 <- c()
for (i in colnames(TTM)){
  ByDates2 <- c(ByDates2, TTM[i])
}

ph <- rep(1, 10)

CspotRates <- data.frame(ph)

#Continuous SpotRates
for (j in c(1:10)){
  spotRates <- c()
  sRates <- c()
  sumRates <- c()
  
  workingData <- ByDates[[j]]
  workingTimes <- ByDates2[[j]]
  
  b1 <- -log(workingData[1]/100)
  s1 <- b1/workingTimes[1]
  e1 <- exp(-b1)
  
  spotRates <- c(spotRates, s1)
  sRates <- c(sRates, b1)
  sumRates <- c(sumRates, e1)
  weights <- c(100)
  ct = 0
  raw <- c()
  cop <- coup/2
  
  for (i in c(1:9)){
    j = length(spotRates)
    a <- (workingData[j+1] - sum(cop[j+1]*weights*sumRates))/(100+100*cop[j+1])
    b <- -log(a)
    s <- b/workingTimes[j+1]
    e <- exp(-b)
    raw <- c(raw, a)
    weights <- c(weights, 100)
    spotRates <- c(spotRates, s)
    sRates <- c(sRates, b)
    sumRates <- c(sumRates, e)
  }
  CspotRates <- data.frame(CspotRates, spotRates)
}
CspotRates <- subset(CspotRates, select=-c(ph))
##PLOT
plot_col <- c("blue", "slategray", "gray0", "brown",
              "cyan", "deeppink", "darkolivegreen", "magenta", "mediumaquamarine",
              "orange")
setEPS()
postscript("Spot Curve.eps", width=10, height=6)
plot(workingTimes, 1000*CspotRates[,1], type="o", col=plot_col[1], xlab="Time to Maturity (Months)", ylab="Spot Rates (%)")
for (i in c(2:10)){
  lines(workingTimes, 1000*CspotRates[,i], type="o", col=plot_col[i])
}
legend(50, 2, legend=dates, col=plot_col, lty=1, cex=0.8)
title("5 Year Spot Curve")
dev.off()



#Discrete Spot Rate
dSpotRates <-c()

for (i in c(1:10)){
  dSpotRates[i] <- sumRates[i]^(1/i)-1
}

##Yield to Maturity
bondv <- function(i, cf,t=seq(along = cf)){
  sum(cf / (1 + i)^t)
}

ytm <- function(cf) {
  uniroot(bondv, c(0, 1), cf = cf)$root
}

cf1 <- c(-98.41, 0.375, 100.375)
cf2 <- c(-98.89, 0.375, 0.375, 100.375)
cf3 <- c(-98.41, 0.375, 0.375, 0.375, 100.375)
cf4 <- c(-97.57, 0.25, 0.25, 0.25, 0.25, 100.025)
cf5 <- c(-102.53, 1.375, 1.375, 1.375, 1.375, 1.375, 101.375)
cf6 <- c(-100.31, 0.875, 0.875, 0.875, 0.875, 0.875, 0.875, 100.875)
cf7 <- c(-99.48, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 100.75)
cf8 <- c(-102.52, 1.125, 1.125, 1.125, 1.125, 1.125, 1.125, 1.125, 1.125, 101.125)
cf9 <- c(-98.72, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 100.75)
cf10 <- c(-98.24, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 0.625, 100.625)

ytm1 <- ytm(cf1)
ytm2 <- ytm(cf2)
ytm3 <- ytm(cf3)
ytm4 <- ytm(cf4)
ytm5 <- ytm(cf5)
ytm6 <- ytm(cf6)
ytm7 <- ytm(cf7)
ytm8 <- ytm(cf8)
ytm9 <- ytm(cf9)
ytm10 <- ytm(cf10)

##Yield Calculation

cmat <- rep(mat, 10)

##PLOT

setEPS()
postscript("Yield Curve.eps", width=10, height=6)
plot(cmat[c(1:10)], 100*hold[c(1:10)], type="o", lty=1, col=plot_col[1], xlab="Maturity Date", ylab="Yield")

for (i in c(2:10)){
  lines(cmat[c((10*i-9):(10*i))], 100*hold[c((10*i-9):(10*i))], type="o", col=plot_col[i])
}
legend(as.Date("2023-05-01"), 1.8, legend=dates, col=plot_col, lty=1, cex=0.8)
title("5 Year Yield Curve")
dev.off()

##Forward Rate Computation
EO <- c()
for (i in c(1:5)){
  EO[i] <- 2*(i-1)+2
}
EOSpot <- 10*CspotRates[EO,]

f <- c(1:4)
fwdRates <- data.frame(f,f,f,f,f,f,f,f,f,f)

for (i in c(1:10)){
  fwdRates[1,i] <- ((1+EOSpot[2,i])^2)*(1+EOSpot[1,i])^(-1)-1
}
for (i in c(1:10)){
  fwdRates[2,i] <- ((1+EOSpot[3,i])^3)*(1+EOSpot[1,i])^(-1)-1
}
for (i in c(1:10)){
  fwdRates[3,i] <- ((1+EOSpot[4,i])^4)*(1+EOSpot[1,i])^(-1)-1
}
for (i in c(1:10)){
  fwdRates[4,i] <- ((1+EOSpot[5,i])^5)*(1+EOSpot[1,i])^(-1)-1
}

setEPS()
postscript("Forward Curve.eps", width=9.5, height=5)
plot(c(1,2,3,4), 100*fwdRates[, 1], type="o", xlab="1 Year - X Year", ylab = "Forward Rate (%)", col=plot_col[1], lwd=1.1)
for (i in c(2:10)){
  lines(c(1,2,3,4), 100*fwdRates[,i], type="o", col=plot_col[i])
}
legend(3.5, 3.5, legend=dates, col=plot_col, lty=1, cex=0.8)
title("Forward Curve")
dev.off()

##Covariance of log-return of yield
every_other <- c()
for (i in c(1:50)){
  every_other[i] <- 2*(i-1)+2
}
holdEO <- hold[every_other]
holdEO <- 100*holdEO
logR <- c()


for (i in c(1:9)) {
  logR[i]<-log(holdEO[5*i+1]/holdEO[5*i-4])
}

logRet <- data.frame(logR) 

for (j in c(2:5)) {
  for (i in c(1:9)) {
    logR[i]<-log(holdEO[5*i+j]/holdEO[5*i-(5-j)])
  }
  logRet <- data.frame(logRet, logR)
}

cov_mat <- cov(logRet)
View(cov_mat)

yeigen <- eigen(cov_mat)
yeval <- yeigen$values
yevec <- yeigen$vectors

#Covariance of log-return of fwd rates
logRf <- c()

fwd <- data.frame(t(fwdRates))
for (i in c(1:9)){
  logRf[i]<- log(fwd[i+1,1]/fwd[i,1])
}

logRetf <- data.frame(logRf)

for (j in c(2:4)){
  for (i in c(1:9)){
    logRf[i] <- log(fwd[i+1,j]/fwd[i,j])
  }
  logRetf <- data.frame(logRetf, logRf)
}

fwdcov <- cov(logRetf)
View(fwdcov)
feigen <- eigen(fwdcov)
feval <- feigen$values
fevec <- feigen$vectors
