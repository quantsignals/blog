# before running this source the file PAMR.R
# source('PAMR.R')

# create the antithetic pair of stocks and publish it into the environment
# generate sequence of dates
times <- seq(as.POSIXct(strptime('2011-01-1 16:00:00', '%Y-%m-%d %H:%M:%S')),
             as.POSIXct(strptime('2011-12-1 16:00:00', '%Y-%m-%d %H:%M:%S')),
             by="1 months")

# generate and store dummy price series = 200,100,200,100,200 ...
prices.xts <- xts(rep(c(200,100),length(times)/2), order.by=as.POSIXct(times))
stk <- 'STK1'
colnames(prices.xts) <- paste(stk,'.Adjusted',sep="")
assign(x=stk, value=prices.xts, envir=global.env )

# generate and store dummy price series = 100,200,100,200,100 ...
prices.xts <- xts(rep(c(100,200),length(times)/2), order.by=as.POSIXct(times))
stk <- 'STK2'
colnames(prices.xts) <- paste(stk,'.Adjusted',sep="")
assign(x=stk, value=prices.xts, envir=global.env )

# run the strategy and create a plot
strategy(portfolio= c("STK1","STK2"),threshold=.9,doplot=TRUE,title="Artificial Portfolio Example")
