# before running this code source PortfolioFunctions.R

global.env <- new.env()
indx.master <- getIndexComposition('^DJI')
symbols.load(indx.master)
universe <- symbols.clean(indx.master)
portfolio <- c("JPM", "HPQ", "AA", "BAC")

mytree <- (read.tree(text = paste("(:1,:1,:1,",node.id(portfolio),":1):1;")))
mytree$node.label <- "ROOT"

visited <- c()
sharp.max <- -999999
search.results <- c()
cont <- TRUE
while(cont == TRUE)
{
  # create combinations with of subportfolios with 3 names
  portfolios <- list( c(portfolio[2],portfolio[3],portfolio[4]),
                      c(portfolio[1],portfolio[3],portfolio[4]),
                      c(portfolio[1],portfolio[2],portfolio[4]),
                      c(portfolio[1],portfolio[2],portfolio[3]) )
  # order each subportfolio alphabetical
  portfolios <- lapply(1:4, function(i) {
    portfolios[[i]] [ order( portfolios[[i]]     ) ]  }  )
  
  i.portfolio.best <- 0
  my.portfolio.best <- c()
  mytree.txt <- "("
  visited4 <- c()
  for(i in 1:4)
  {
    if(portfolio.visited(portfolios[[i]],visited)==FALSE)
    {
      visited <-  rbind(visited,portfolios[[i]])
      my.results  <- portfolio.add(portfolios[[i]], universe)    
      my.results.best <- my.results[1,1:7]
      my.sharp <- my.results[1,7]
      search.results <- rbind( search.results , my.results.best)
      
      # check if we got this node already
      my.results.best.ordered <-  my.results.best[1:4] [ order( my.results.best[1:4]  ) ]
      if(portfolio.visited(my.results.best.ordered,visited4)==FALSE){
        visited4 <-  rbind(visited4, my.results.best.ordered)
        if(i>1) { mytree.txt <- paste(mytree.txt,",") }
        mytree.txt <- paste(mytree.txt, node.id(my.results.best[1:4]),":1")
        
        if(my.sharp > sharp.max){
          sharp.max <- my.sharp
          i.portfolio.best <- i
          my.portfolio.best <- my.results[1,1:4]}
      }
    }
  }
  
  mytree.txt <- paste(mytree.txt,"):1;")
  if( i.portfolio.best > 0 )
  {
    mytree.sub <-  read.tree(text = mytree.txt)
    mytree.sub$node.label <- node.id( portfolio)
    mytree <- bind.tree(mytree, mytree.sub,
                        where=which(mytree$tip.label == node.id(portfolio) ), position=0)
    
    plot(mytree,cex=1,srt=0,direction="u")
    nodelabels(mytree$node.label,frame="r",cex=1,srt=0)
    
    portfolio <- my.portfolio.best
    print(portfolio)
    flush.console()
  }
  
  if(i.portfolio.best == 0 )
  {
    cont <- FALSE
    print(search.results)
    flush.console()
  }
  
}
mytree.drop <- drop.tip(mytree,1:2)
plot(mytree.drop,cex=1,srt=0,direction="u",root.edge=TRUE)
nodelabels(mytree.drop$node.label,frame="r",cex=1,srt=0)
