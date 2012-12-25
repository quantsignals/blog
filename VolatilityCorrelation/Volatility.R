require(tawny)
indx <- getIndexComposition('^DJI')
for(stk in indx) { getSymbols(stk) }

widthNrBusDays <- 90
dayBasis <- 260
indx.ts <- xts()
var0.ts <- xts()
var1.ts <- xts()
divisor <- 0.13213
for(stk in indx) 
{
  stk.ts<-Cl(get(stk)['2008-01::'])
  var.ts=apply.rolling(diff(log(stk.ts)),width=widthNrBusDays,FUN="var")*dayBasis
  if(length(indx.ts) == 0)
  {
    indx.ts <- stk.ts/divisor
    var0.ts <- var.ts*(stk.ts/divisor)^2
    var1.ts <- sqrt(var.ts)*stk.ts/divisor
  } else {
    indx.ts = indx.ts + stk.ts/divisor
    var0.ts = var0.ts + var.ts*(stk.ts/divisor)^2
    var1.ts = var1.ts + sqrt(var.ts)*stk.ts/divisor
  }
}
var0.ts = var0.ts/(indx.ts*indx.ts)
var1.ts = (var1.ts/indx.ts)^2

indx.var.ts <- apply.rolling(diff(log(indx.ts)),width=widthNrBusDays,FUN="var")*dayBasis

indx.corr.ts <- (indx.var.ts - var0.ts)/(var1.ts - var0.ts)
indx.vol.ts <-  sqrt(indx.var.ts)

par(mfrow=c(2,1))
chart_Series(indx.corr.ts, name='DJI 30 day implied correlation')
chart_Series(indx.vol.ts, name='DJI 30 day realized volatility')
