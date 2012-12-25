require(tawny)
require(quantmod)
require(ggplot2)
require(ape)
#################################
# load list of symbols via getSymbols()
# enclose fetching symbols within try clause in case of faulty/non-existing tickers
# disregard symbols without a complete history
# return list of successful loaded names
####################################l
symbol.exists <- function(stk)
{
  tryCatch( typeof(get(stk,env=global.env))=="double", error = function(e) FALSE )
}
symbols.load <- function(indx.master)
{
  indx <- c() # list of loaded tickers
  for(stk in indx.master)
  {
    cc <- 0
    while( symbol.exists(stk) == FALSE & cc < 2 )
    {
      print(stk)
      cc <- cc +1   
      tryCatch( getSymbols(stk, from="2011-01-03", to="2011-12-30",env=global.env), error = function(e) Sys.sleep(1) )
      if(exists(stk)==TRUE){
        if(NROW(get(stk,env=global.env)['2011-01-03::2011-12-30'])==252){
          indx<-c(indx,stk)  }}
    }
  }
}

#########################
# check master symbol list
# return clean list of symbols that are successfully loaded
#########################
symbols.clean <- function(indx.master)
{
  indx <- c()
  for(stk in indx.master){ if(symbol.exists(stk)==TRUE) {
    if(NROW(get(stk,env=global.env)['2011-01-03::2011-12-30'])==252){
      indx<-c(indx,stk) }}}
  indx
}

##################
# compute list of sharp ratio for each name in the indx list
##################
sharp.ratios <-function(indx)
{
  results<-matrix(nrow=length(indx),ncol=4)
  stks.dailyret.ts <- xts()
  cc <-1
  for(stk in indx){
    stk.ts <- Ad(get(stk,env=global.env)['2011-01-03::2011-12-30'])
    stk.cumret.ts <- stk.ts/coredata(stk.ts[1])[1]
    stk.dailyret.ts <- tail(stk.cumret.ts/lag(stk.cumret.ts,k=1)-1,-1)
    stks.dailyret.ts <- merge(stks.dailyret.ts, stk.dailyret.ts)
    stk.mean <- mean(stk.dailyret.ts)
    stk.var <- var(stk.dailyret.ts)
    stk.sharp <- sqrt(252)*stk.mean/sqrt(stk.var)
    results[cc,] <-  c(stk, stk.mean, sqrt(stk.var), stk.sharp)
    cc <- cc+1
  }
  results
}


######################
# compute sharp ratio and optimum combination for names in stks
######################
require(Rsolnp)
sharp.ratio <- function(stks)
{
  stks.dailyret.ts <- xts()
  for(stk in stks ){
    stk.ts <- Ad(get(stk,global.env)['2011-01-03::2011-12-30'])
    stk.cumret.ts <- stk.ts/coredata(stk.ts[1])[1]
    stk.dailyret.ts <- tail(stk.cumret.ts/lag(stk.cumret.ts,k=1)-1,-1)
    stks.dailyret.ts <- merge(stks.dailyret.ts, stk.dailyret.ts)
  }
  dcount <- sqrt(252)
  mu<-colMeans(stks.dailyret.ts)
  V <- cov(stks.dailyret.ts)
  res <- solnp(
    rep(1/length(mu), length(mu)),
    function(w) - dcount*t(w) %*% mu / sqrt( t(w) %*% V %*% w ),
    eqfun = function(w) sum(w),
    eqB   = 1,
    LB = rep(0, length(mu))
  )
  res$mu <-  t(res$pars) %*% mu
  res$std <-  sqrt( t(res$pars) %*% V %*% res$pars )
  res$sharp <- -tail( res$values,1)
  res
}

sharp.simple.ratio <- function(stks)
{
  stks.cumret.ts <- xts()
  for(stk in stks ){
    stk.ts <- Ad(get(stk,global.env)['2011-01-03::2011-12-30'])
    stk.cumret.ts <- stk.ts/coredata(stk.ts[1])[1]
    stks.cumret.ts <- merge(stks.cumret.ts, stk.cumret.ts)
  }
  dcount <- sqrt(252)
  res <- solnp(
    rep(1/length(stks), length(stks)),
    function(w) {
      stks.dailyret.ts <-  stks.cumret.ts %*% w
      n <- length(stks.dailyret.ts)
      stks.dailyret.ts <- stks.dailyret.ts[2:n]/stks.dailyret.ts[1:n-1]-1
      - dcount*  mean(stks.dailyret.ts)/  sqrt(var(stks.dailyret.ts))
    },
    eqfun = function(w) sum(w),
    eqB   = 1,
    LB = rep(0, length(stks))
  )
  stks.dailyret.ts <-  stks.cumret.ts %*% res$par
  n <- length(stks.dailyret.ts)
  stks.dailyret.ts <- stks.dailyret.ts[2:n]/stks.dailyret.ts[1:n-1]-1
  res$mu <- mean(stks.dailyret.ts)
  res$std <-  sqrt(var(stks.dailyret.ts))
  res$sharp <- -tail( res$values,1)
  res
}


# scan universe for best stk to add to portfolio
portfolio.add <- function(portfolio, universe)
{
  n<-NROW(universe)
  # check home many of the portfolio stocks are also part of the universe
  noverlap <- sum(sapply(1:length(portfolio), function(i) any(portfolio[i]==universe)))
  # allocate result matrix
  results<-matrix(nrow=n-noverlap,ncol=length(portfolio)+4)
  cc <- 1
  for (i in 1:n)   if(any(portfolio==universe[i])==FALSE)
  {
    stks<-c(portfolio,universe[i])
    #sr<-sharp.simple.ratio(stks)
    sr<-sharp.ratio(stks)
    res <- c(sprintf("%s",stks), sr$mu, sr$std, sr$sharp )
    results[cc,] = res
    cc <- cc+1
  }
  results.ordered <- results[order(as.numeric(results[,ncol(results)]),decreasing=TRUE),]
  results.ordered
}
node.id <- function(portfolio)
{
  ratio <- sprintf("%2.2f",as.numeric(sharp.ratio(portfolio)$sharp))
  paste(portfolio[1],portfolio[2],portfolio[3],portfolio[4],ratio,sep="_")
}

portfolio.visited <- function(portfolio, visited)
{
  any(portfolio[1]==visited[,1] & portfolio[2]==visited[,2] & portfolio[3]==visited[,3] )
}
portfolio.visited4 <- function(portfolio, visited)
{
  any(portfolio[1]==visited[,1] & portfolio[2]==visited[,2] & portfolio[3]==visited[,3], portfolio[4]==visited[,4] )
}
