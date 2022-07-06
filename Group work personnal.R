###########################################
##     Created by Mary Omotosho   #########
##########################################
####    Hult International Business School
##########################################
##   Client Renaissance Technology  ####
########################################
#####       July 2 2022          #######

#creating a vector that contains the top Renaissance Technology investment portfolio

library(quantmod)
#step 1: pulling in pricing data:
## Getting 
renaissance.portfolio <- c("NVO","TSLA","KR","GILD","VRSN","AAPL","AMD","TEAM","ZM","AMZN","META","HSY","MRNA","FNV","GOOGL")

stock1 <- getSymbols("NVO",auto.assign = FALSE)
stock2 <- getSymbols("TSLA",auto.assign = FALSE)
stock3 <- getSymbols("KR",auto.assign = FALSE)
stock4 <- getSymbols("GILD",auto.assign = FALSE)
stock5 <- getSymbols("VRSN",auto.assign = FALSE)
stock6 <- getSymbols("AAPL",auto.assign = FALSE)
stock7 <- getSymbols("AMD",auto.assign = FALSE)
stock8 <- getSymbols("TEAM",auto.assign = FALSE)
stock9 <- getSymbols("ZM",auto.assign = FALSE)
stock10 <- getSymbols("AMZN",auto.assign = FALSE)
stock11 <- getSymbols("META",auto.assign = FALSE)
stock12 <- getSymbols("HSY",auto.assign = FALSE)
stock13 <- getSymbols("MRNA",auto.assign = FALSE)
stock14 <- getSymbols("FNV",auto.assign = FALSE)
stock15 <- getSymbols("GOOGL",auto.assign = FALSE)

## step 2: combining all data frame together
joined_prices <- merge.xts(stock1, stock2, stock3,stock4, stock5, stock6, stock7,stock8, stock9, stock10, stock11, stock12, stock13, stock14, stock15)

### step 3: pulling in adjusted prices
joined_prices_only <- joined_prices[,c(6,12,18,24,30,36,42,48,54,60,66,72,78,84,90)]

## step 4: Expert version
#version A:1

joined_returns_loop <- as.data.frame(joined_prices_only)

joined_returns_loop$log_ret <- c()
joined_returns_loop$log_ret[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$log_ret[i] <- log(joined_returns_loop$NVO.Adjusted[i]/joined_returns_loop$NVO.Adjusted[i-1])
}

# UDC to add time window:
window_returns <- function(x, t){
  compounded <- rep(NA, each=(t-1))
  for(i in t:length(x)){
    compounded[i] <- log(x[i]/x[i-t+1])
  }
  return(compounded)
}#closing the window_returns UDF


# calling the UDF:
window_returns(x=joined_returns_loop$NVO.Adjusted, t=25)
window_returns(x=joined_returns_loop$TSLA.Adjusted, t=25)
window_returns(x=joined_returns_loop$KR.Adjusted, t=25)
window_returns(x=joined_returns_loop$GILD.Adjusted, t=25)
window_returns(x=joined_returns_loop$VRSN.Adjusted, t=25)
window_returns(x=joined_returns_loop$AAPL.Adjusted, t=25)
window_returns(x=joined_returns_loop$AMD.Adjusted, t=25)
window_returns(x=joined_returns_loop$TEAM.Adjusted, t=25)
window_returns(x=joined_returns_loop$ZM.Adjusted, t=25)
window_returns(x=joined_returns_loop$AMZN.Adjusted, t=25)
window_returns(x=joined_returns_loop$META.Adjusted, t=25)
window_returns(x=joined_returns_loop$HSY.Adjusted, t=25)
window_returns(x=joined_returns_loop$MRNA.Adjusted, t=25)
window_returns(x=joined_returns_loop$FNV.Adjusted, t=25)
window_returns(x=joined_returns_loop$GOOGL.Adjusted, t=25)

## step 4 version B(Black box)
stock1_returns <- monthlyReturn(getSymbols("NVO",auto.assign = FALSE))
stock2_returns <- monthlyReturn(getSymbols("TSLA",auto.assign = FALSE))
stock3_returns <- monthlyReturn(getSymbols("KR",auto.assign = FALSE))
stock4_returns <- monthlyReturn(getSymbols("GILD",auto.assign = FALSE))
stock5_returns <- monthlyReturn(getSymbols("VRSN",auto.assign = FALSE))
stock6_returns <- monthlyReturn(getSymbols("AAPL",auto.assign = FALSE))
stock7_returns <- monthlyReturn(getSymbols("AMD",auto.assign = FALSE))
stock8_returns <- monthlyReturn(getSymbols("TEAM",auto.assign = FALSE))
stock9_returns <- monthlyReturn(getSymbols("ZM",auto.assign = FALSE))
stock10_returns <- monthlyReturn(getSymbols("AMZN",auto.assign = FALSE))
stock11_returns <- monthlyReturn(getSymbols("META",auto.assign = FALSE))
stock12_returns <- monthlyReturn(getSymbols("HSY",auto.assign = FALSE))
stock13_returns <- monthlyReturn(getSymbols("MRNA",auto.assign = FALSE))
stock14_returns <- monthlyReturn(getSymbols("FNV",auto.assign = FALSE))
stock15_returns <- monthlyReturn(getSymbols("GOOGL",auto.assign = FALSE))

### step 5: combining return into 1 data frame
joined_monthlyreturns <- merge.xts(stock1_returns,stock2_returns,stock3_returns,stock4_returns,stock5_returns,stock6_returns,stock7_returns,stock8_returns,stock9_returns,stock10_returns,stock11_returns,stock12_returns,stock13_returns,stock14_returns,stock15_returns)

# step 6:last
# calculating portfolio returns

NVO_alloc <- 0.006
TSLA_alloc <- 0.007
KR_alloc <- 0.007
GILD_alloc <-0.008
VRSN_alloc <- 0.006
AAPL_alloc <- 0.007
AMD_alloc <- 0.006
TEAM_alloc <- 0.008
ZM_alloc <- 0.006
AMZN_alloc <- 0.006
META_alloc <- 0.006
HSY_alloc <- 0.006
MRNA_alloc <- 0.006
FNV_alloc <- 0.008
GOOGL_alloc <- 0.007

# creating a new variable to host portfolio returns

joined_monthlyreturns$portfolio <- joined_monthlyreturns$monthly.returns*NVO_alloc+joined_monthlyreturns$monthly.returns.1*TSLA_alloc+joined_monthlyreturns$monthly.returns.2*KR_alloc+joined_monthlyreturns$monthly.returns.3*GILD_alloc+joined_monthlyreturns$monthly.returns.4*VRSN_alloc+joined_monthlyreturns$monthly.returns.5*AAPL_alloc+joined_monthlyreturns$monthly.returns.6*AMD_alloc+joined_monthlyreturns$monthly.returns.7*TEAM_alloc+joined_monthlyreturns$monthly.returns.8*ZM_alloc+joined_monthlyreturns$monthly.returns.9*AMZN_alloc+joined_monthlyreturns$monthly.returns.10*META_alloc+joined_monthlyreturns$monthly.returns.11*HSY_alloc+joined_monthlyreturns$monthly.returns.12*MRNA_alloc+joined_monthlyreturns$monthly.returns.13*FNV_alloc+joined_monthlyreturns$monthly.returns.14*GOOGL_alloc                              

#moving to investment risk
#calculating sigma to show total risk for assets and portfolio

time_index <- nrow(joined_monthlyreturns)

joined_monthlyreturns <- as.data.frame(joined_monthlyreturns)

NVO_sigma <- sd(joined_monthlyreturns$monthly.returns[(time_index-11):time_index])

NVO_sigma <- sd(joined_monthlyreturns$monthly.returns[(time_index-11):time_index])*sqrt(12)

TSLA_sigma <- sd(joined_monthlyreturns$monthly.returns.1[(time_index-11):time_index])*sqrt(12)

KR_sigma <- sd(joined_monthlyreturns$monthly.returns.2[(time_index-11):time_index])*sqrt(12)

GILD_sigma <- sd(joined_monthlyreturns$monthly.returns.3[(time_index-11):time_index])

VRSN_sigma <- sd(joined_monthlyreturns$monthly.returns.4[(time_index-11):time_index])*sqrt(12)

AAPL_sigma <- sd(joined_monthlyreturns$monthly.returns.5[(time_index-11):time_index])*sqrt(12)

AMD_sigma <- sd(joined_monthlyreturns$monthly.returns.6[(time_index-11):time_index])*sqrt(12)

TEAM_sigma <- sd(joined_monthlyreturns$monthly.returns.7[(time_index-11):time_index])

ZM_sigma <- sd(joined_monthlyreturns$monthly.returns.8[(time_index-11):time_index])*sqrt(12)

AMZN_sigma <- sd(joined_monthlyreturns$monthly.returns.9[(time_index-11):time_index])*sqrt(12)

META_sigma <- sd(joined_monthlyreturns$monthly.returns.10[(time_index-11):time_index])*sqrt(12)

HSY_sigma <- sd(joined_monthlyreturns$monthly.returns.11[(time_index-11):time_index])

MRNA_sigma <- sd(joined_monthlyreturns$monthly.returns.12[(time_index-11):time_index])*sqrt(12)

FNV_sigma <- sd(joined_monthlyreturns$monthly.returns.13[(time_index-11):time_index])*sqrt(12)

GOOGL_sigma <- sd(joined_monthlyreturns$monthly.returns.14[(time_index-11):time_index])*sqrt(12)

portfolio_sigma <- sd(joined_monthlyreturns$portfolio[(time_index-11):time_index])*sqrt(12)


















