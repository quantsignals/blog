# before running this source the file PAMR.R
# source('PAMR.R')

portfolio.DJI <- getIndexComposition('^DJI')
getSymbols(portfolio.DJI,env=global.env)
strategy(portfolio.DJI,.9,TRUE,"2011-01-01::2012-11-01",title="Portfolio DJI, Jan-2011 - Nov 2012")
strategy(c("TRV","XOM","CVX","PG","JPM"),.9,TRUE,"2011-01-01::2012-11-01",title="Portfolio 5 out of DJI, Jan-2011 - Nov 2012")
