require(tawny)
require(quantmod)
require(ggplot2)

# global market data environment
global.env <- new.env()

# search anti correlated stocks
# also return daily variance and maximum one day variance
portfolio.screen <- function(portfolio, daterange="")
{
  ac <- matrix()
  for(stk in portfolio)
  {
    p.xts <- Ad(get(stk,global.env)[daterange])
    dr.xts <- dailyReturn(p.xts)
    dr.maxvar <- max( (dr.xts-mean(dr.xts) )^2)
    dr.var <- coredata(var(dr.xts))
    dr.var.ratio <- dr.maxvar/(dr.var)
    dr.ac <-  coredata( acf(dr.xts)$acf[1:5] )
    dr.ac.res <- c(stk, dr.var.ratio, dr.maxvar, dr.var, dr.ac)
    
    if(length(ac)==1)
    {
      ac <- dr.ac.res
    }
    else
    {   
      ac <- rbind(ac, dr.ac.res)
    }
  }
  # sort by decreasing auto-correlations
  ac_sort <- (ac[order(as.numeric(ac[,6]),decreasing='FALSE'),])
  # return sorted matrix of stocks and correlations
  ac_sort
}

#############################################################################
# portfolio weight vector b
# price relative return vector x
# threshold parameter e, usually e <= 1 for

# returns intrinsic value of call on portfolio return with strike e
strategy.loss <- function(b, x, e)
{
  max(b%*%x-e,0)
}

strategy.gearing <- function(b, x, e)
{
  loss <- strategy.loss(b,x,e)
  varx <- var(x)
  if(varx>0) { -loss/varx }
  else { 0}
}

# return a vector w so that w is close to v
# by minimizing the distance (w-v)^2
# with the condition that w is on the simplex
# sum(w)=1
strategy.simplex <- function(v,z)
{   
  # initialize projection to zero
  w <- rep(0, length(v) )
  # Sort v into u in descending order
  
  idx <- order(v,decreasing=TRUE)
  u <- v[idx]
  
  #index of number of non zero elements in w
  rho <-max(index(u)[index(u)*u  >= cumsum(u) -z])
  if(rho>0)
  {
    theta <-(sum(u[1:rho])-z)/rho
    w[idx[1:rho]] <- v[idx[1:rho]] - theta;
  }
  w
}

strategy.rebalance <- function(b,x,e)
{
  dbdx <- strategy.gearing <- strategy.gearing(b, x, e)
  b_new <- b + dbdx*(x-mean(x))
  b_new <- strategy.simplex(b_new,1)
}

############################################
###################################
# Simulate the portfolio trading strategy
# Arguments:
# portfolio : portfolio of stocks f
# threshold: mean reversion loss parameter ( should be in the order but smaller than 1)
# doplot: create an optional plot
# daterange: range of dates to test
#
# returns the accumulated gain of the strategy
#
strategy <- function(portfolio, threshold=1, doplot=FALSE,daterange="",title="" )
{
  portfolio.prices.xts <- xts()
  # raw portfolio components price time series
  for( stk in portfolio)
  {
    portfolio.prices.xts <- cbind(portfolio.prices.xts, Ad(get(stk,envir = global.env)[daterange]))
  }
  # downfill missing closing prices (last observation carried forward)
  
  portfolio.prices.xts <- na.locf(portfolio.prices.xts)
  
  # not backfill missing history backwards, for example for IPO assets that did
  # not exist, we set these to the constant initial 'IPO' price , resembling a cash asset
  
  portfolio.prices.xts <- na.locf(portfolio.prices.xts,fromLast=TRUE)
  times <- index(portfolio.prices.xts) 
  nprices <- NROW(portfolio.prices.xts)
  
  # relative prices S(t)/S(t-1)
  portfolio.price.relatives.xts <- (portfolio.prices.xts/lag(portfolio.prices.xts,1))[2:nprices,]
  
  # initialize portfolio weights time series
  portfolio.weights.xts <- xts(matrix(0,ncol=length(portfolio),nrow=nprices-1),
                               order.by=as.POSIXct(times[2:nprices]))
  
  colnames(portfolio.weights.xts) = portfolio
  
  # initialize strategy with equal weights
  portfolio.weights.xts[1,] = rep(1/length(portfolio),length(portfolio))
  
  # run strategy:
  for( i in 1:(nprices-2))
  {
    #portfolio weights at opening of the day
    b_open <- portfolio.weights.xts[i,]
    
    #price relative return at close x = S(t)/S(t-1)
    x_close <- portfolio.price.relatives.xts[i,]
    
    # compute end of day rebalancing portfolio with strategy
    b_new <-  strategy.rebalance(as.vector(b_open),as.vector(x_close),threshold)
    
    # assign portfolio composition for next day opening
    portfolio.weights.xts[i+1,] <- b_new
  }
  
  #aggregate portfolio returns
  
  portfolio.total.relatives.xts <- xts( rowSums(portfolio.weights.xts * portfolio.price.relatives.xts), order.by=as.POSIXct(times[2:nprices]))
  portfolio.cum.return.xts <- cumprod( portfolio.total.relatives.xts)
  
  # cummulative wealth gained by portfolio
  
  if(doplot==TRUE)
  {
    Times <- times
    df<-data.frame(Date=Times,
                   
                   Performance=c(1,coredata(portfolio.cum.return.xts)), pane='Portfolio')
    myplot <- ggplot()+theme_bw()+
      facet_grid (pane ~ ., scale='free_y')+
      labs(title=title) +
      ylab("Cumulative Performance ") +
      geom_line(data = df,  aes(x = Date, y = Performance), color="blue")
    
    for(i in 1:length(portfolio))
    {
      df1<-data.frame(Date=Times, Performance=c(1,coredata(cumprod(portfolio.price.relatives.xts[,i] ) ) ),pane='Components')
      names(df1)[2] <- "Performance"
      myplot <- myplot +
        geom_line(data = df1, aes_string(x = "Date", y = "Performance"),color=i)
      
      df3<-data.frame(Date=Times, Performance=c(1,coredata(cumprod(portfolio.price.relatives.xts[,i] ) ) ), pane='Components')
      names(df1)[2] <- "Performance"
      myplot <- myplot +
        geom_line(data = df1, aes_string(x = "Date", y = "Performance"),color=i)
      
      df2<-data.frame(Date=Times[1:NROW(Times)-1],
                      Weights=coredata(portfolio.weights.xts[,i]) , pane='Weights')
      
      names(df2)[2] <- "Weights"
      
      myplot <- myplot + geom_line(data = df2, aes_string(x = "Date", y = "Weights"),color=i)
    }
    print(myplot)
  } # end of plotting
  
  # return performance
  last(portfolio.cum.return.xts)
}
