par(mfrow=c(3,1))

tree.child$node.label <- "TIP1"
tree.combined <- bind.tree(tree.top, tree.child, where=which(tree.top$tip.label == tree.child$node.label ), position=0)
plot(tree.combined,cex=1,srt=0,direction="r",root.edge=TRUE)
nodelabels(tree.combined$node.label,frame="r",cex=1)

tree.child$node.label <- "TIP2"
tree.combined <- bind.tree(tree.top, tree.child, where=which(tree.top$tip.label == tree.child$node.label ), position=0)
plot(tree.combined,cex=1,srt=0,direction="r",root.edge=TRUE)
nodelabels(tree.combined$node.label,frame="r",cex=1)

tree.child$node.label <- "TIP3"
tree.combined <- bind.tree(tree.top, tree.child, where=which(tree.top$tip.label == tree.child$node.label ),position=0)
plot(tree.combined,cex=1,srt=0,direction="r",root.edge=TRUE)
nodelabels(tree.combined$node.label,frame="r",cex=1)
