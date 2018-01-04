library(fOptions)
library(sde)
library(quantmod)
library(timeSeries)
source("Confidence_Bounds.R")
options(scipen = 999)
symbols <- c("")
getSymbols(c("NFLX","TSLA","RHT","FIZZ"),src = "google" , from = as.Date("2016-04-03"), to = as.Date("2017-04-03")) 
pM <- cbind(NFLX[,4],TSLA[,4],RHT[,4],FIZZ[,4])
returnsMatrix <-  apply(pM,2,function(x) as.numeric(periodReturn(x,period = "weekly",type = "log")))
initialPriceV <- round(as.numeric(c(NFLX[,4][length(NFLX[,1])],TSLA[,4][length(NFLX[,1])],RHT[,4][length(NFLX[,1])],FIZZ[,4][length(NFLX[,1])])),2)
# the initial prices for the simulation are the end of the data that is used to calculate 
# the historical sigma and drift 
# So the simulation provides a confidence bound for the price of the stock in 3 months

meanV <- apply(returnsMatrix, 2, mean)
sigmaV <- apply(returnsMatrix,2,var)


T = 2/12 #Daily 252 trading days //52 weeks in a year
nt = 2^(10) # number of simulations
n = 2^(10) # number of increments 

###### Generate nt trajectories
dt = T/n 
t = seq(0,T,by = dt)
X1 = matrix(rep(0,length(t)*nt),nrow = nt)
X2 = matrix(rep(0,length(t)*nt),nrow = nt)
X3 = matrix(rep(0,length(t)*nt),nrow = nt)
X4 = matrix(rep(0,length(t)*nt),nrow = nt)
# X5 = matrix(rep(0,length(t)*nt),nrow = nt)


# Simulating paths of the stock price
for (j in 1:nt) {
  X1[j,] = GBM(x = initialPriceV[1],r = meanV[1],sigma = sigmaV[1],T = T,N = n)
  X2[j,] = GBM(x = initialPriceV[2],r = meanV[2],sigma = sigmaV[2],T = T,N = n)
  X3[j,] = GBM(x = initialPriceV[3],r = meanV[3],sigma = sigmaV[3],T = T,N = n)
  X4[j,] = GBM(x = initialPriceV[4],r = meanV[4],sigma = sigmaV[4],T = T,N = n)
  # X5[j,] = GBM(x = initialPriceV[5],r = meanV[5],sigma = sigmaV[5],T = T,N = n)
}

y1max = max(X1); y1min = min(X1) #bounds for simulated prices
y2max = max(X2); y2min = min(X2)
y3max = max(X3); y3min = min(X3)
y4max = max(X4); y4min = min(X4)
# y5max = max(X5); y5min = min(X5)


# par(mfrow = c(2,2))
# --------------------------------------------------------------------
#   plot(t,X1[1,], type = "l",ylim = c(y1min,y1max), col = 1,main = "S&P 500", ylab = "Price P(t)",xlab = "time t")
# for (i in 1:nt) {
#   lines(t,X1[i,], t = "l",ylim = c(y1min,y1max), col = i)
# }
# 
# # par(mfrow = c(2,1))
# #--------------------------------------------------------------------
# plot(t,X2[1,], type = "l",ylim = c(y2min,y2max), col = 1,main = "Netflix", ylab = "Price P(t)",xlab = "time t")
# for (i in 1:nt) {
#   lines(t,X2[i,], t = "l",ylim = c(y2min,y2max), col = i)
# }
# #--------------------------------------------------------------------
# plot(t,X3[1,], type = "l",ylim = c(y3min,y3max), col = 1,main = "Tesla", ylab = "Price P(t)",xlab = "time t")
# for (i in 1:nt) {
#   lines(t,X3[i,], t = "l",ylim = c(y3min,y3max), col = i)
# }
# #--------------------------------------------------------------------
# plot(t,X4[1,], type = "l",ylim = c(y4min,y4max), col = 1,main = "Red Hat, Inc.", ylab = "Price P(t)",xlab = "time t")
# for (i in 1:nt) {
#   lines(t,X4[i,], t = "l",ylim = c(y4min,y4max), col = i)
# }
# #--------------------------------------------------------------------
# plot(t,X5[1,], type = "l",ylim = c(y5min,y5max), col = 1,main = "National Beverage Corp", ylab = "Price P(t)",xlab = "time t")
# for (i in 1:nt) {
#   lines(t,X5[i,], t = "l",ylim = c(y5min,y5max), col = i)
# }
#--------------------------------------------------------------------

# matrices for holding the last value of the simulated price paths
Y1 <- matrix(ncol = 1,nrow = nt)
Y2 <- matrix(ncol = 1,nrow = nt)
Y3 <- matrix(ncol = 1,nrow = nt)
Y4 <- matrix(ncol = 1,nrow = nt)
# Y5 <- matrix(ncol = 1,nrow = nt)


# Putting the last value of the Simulated stock prices into seperate Matrices
for (i in 1:nt) {
  Y1[i,] <- X1[i,length(X1[1,])]
  Y2[i,] <- X2[i,length(X2[1,])]
  Y3[i,] <- X3[i,length(X3[1,])]
  Y4[i,] <- X4[i,length(X4[1,])]
  # Y5[i,] <- X5[i,length(X5[1,])]
}

# #confidence Bounds for Netflix
# NFpart1 <- ((meanV[2]) - (((1/2)*sigmaV[2])*T)) 
# NFpart2 <- ((1.96)*(sqrt(sigmaV[2]))*(sqrt(T)))
# NFLowerBound <- (initialPriceV[2]) * exp((NFpart1 - NFpart2))
# NFUpperBound <- (initialPriceV[2]) * exp((NFpart1 + NFpart2))
# 
# 
# #Confidence Bounds for National Beverage simulations at 2^5
# NBpart1 <- ((meanV[5]) - (((1/2)*sigmaV[5])*T)) 
# NBpart2 <- ((1.96)*(sqrt(sigmaV[5]))*(sqrt(T)))
# NBLowerBound <- (initialPriceV[5]) * exp((NBpart1 - NBpart2))
# NBUpperBound <- (initialPriceV[5]) * exp((NBpart1 + NBpart2))
# 
# #Simulations at 2^10
# NB1part1 <- ((meanV[5]) - (((1/2)*sigmaV[5])*T)) 
# NB1part2 <- ((1.96)*(sqrt(sigmaV[5]))*(sqrt(T)))
# NB1LowerBound <- (initialPriceV[5]) * exp((NB1part1 - NB1part2))
# NB1UpperBound <- (initialPriceV[5]) * exp((NB1part1 + NB1part2))


# GBSOption(TypeFlag, S, X, Time, r, b, sigma,title = NULL, description = NULL)
# plot(density(Y2))

NFLX_Bounds <- Confidence_Bounds(returnsMatrix[,1],T,initialPriceV[1])
NFLX_Bounds

dataframe <- data.frame(rep(0,4),row.names = c("NFLX","TSLA","RHT","FIZZ"))
# dataframe <- c("LowerBound","UpperBound")
data <- c()
for (i in 1:length(returnsMatrix[1,])) {
  data <- Confidence_Bounds(returnsMatrix[,i],T,initialPriceV[i])
}
