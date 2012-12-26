require(ape)
tree.top <- (read.tree(text = "(TIP1:1,TIP2:2,TIP3:3):1;"))
tree.top$node.label <- "ROOT"
par(mfrow=c(2,1))
plot(tree.top,cex=1,srt=0,direction="r",root.edge=TRUE)
nodelabels(tree.top$node.label,frame="r",cex=1)

tree.child <- (read.tree(text = "(TIP4:1,TIP5:2):1;"))
tree.child$node.label <- "CHILD"
plot(tree.child,cex=1,srt=0,direction="r",root.edge=TRUE)
nodelabels(tree.child$node.label,frame="r",cex=1)
