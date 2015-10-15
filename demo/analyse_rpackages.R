
### Example for KernSmooth
path.to.package.src <- system.file('examples','packages', 'KernSmooth', package='functionMap')

edge.list <- edgelist.from.rpackage(path.to.package.src)

n.kernsmooth <- network.from.edgelist(edge.list)

# by default , we use the out degree (number of called functions as the value of the node)
plot(eForce(n.kernsmooth, use.network.attr=TRUE))

pr <- page.rank(n.kernsmooth)
network.vertex.names(n.kernsmooth) [ rank(pr) > 30 ]
n.kernsmooth %v% 'value' <- as.vector(pr) / max(pr) * 20
# now use page.rank
plot(eForce(n.kernsmooth, use.network.attr=TRUE))

el.merged <- interconnected(edge.list)
n.kernsmooth.merged <- network.from.edgelist(el.merged)

# here, because we have only a small number of nodes, we have to increase gravity to make them closer to each other
plot(eForce(n.kernsmooth.merged, use.network.attr=TRUE, gravity=1.1)) 


### Example for latticeExtra
path.to.package.src <- system.file('examples','packages', 'LatticeExtra', package='functionMap')

el2 <- edgelist.from.rpackage(path.to.package.src)
el3 <- interconnected(el2)
net2 <- network.from.edgelist(el2)
net3 <- network.from.edgelist(el3)

ha <- hits.rank(net2[,])
network.vertex.names(net2) [ rank(ha[,1])>420 ]
network.vertex.names(net2) [ rank(ha[,2])>420 ]

plot(eForce(net2, use.network.attr=TRUE))
plot(eForce(net3, use.network.attr=TRUE, gravity=2))

(ind<-which('panel.ablineq' == network.vertex.names(net2)))
(L<-dfs.matrix.travel(net2[,], ind, dir='forward'))

# now we can create sub-network on panel.ablineq
net.sub <- net2 %s% L

plot(eForce(net.sub, use.network.attr=T))


## Example for psych
path.to.package.src <- system.file('examples','packages', 'psych', package='functionMap')
el.psych <- edgelist.from.rpackage(path.to.package.src)
barplot(sort(table(el.psych$rfile),decreasing=T))
sort(table(el.psych$rfile),decreasing=T)[1:5]


net.psych <- network.from.edgelist(subset(el.psych, rfile=='tetrachor.R'))

plot(eForce(net.psych, use.network.attr=T))

## Example for xpose4data
path.to.package.src <- system.file('examples','packages', 'xpose4data', package='functionMap')
el.xpose <- edgelist.from.rpackage(path.to.package.src)
net.xpose <- network.from.edgelist(el.xpose)
pr.xpose <- page.rank(net.xpose)
network.vertex.names(net.xpose)[ rank(pr.xpose) > 200 ]

(ind <- which("read.vpctab"==network.vertex.names(net.xpose)))
net.xpose.sub <- net.xpose %s% dfs.matrix.travel(net.xpose[,], 149, 'forward')
plot(eForce( net.xpose.sub, use.network.attr=T))

## Example for evaluate

path.to.package.src <- system.file('examples','packages', 'evaluate', package='functionMap')
el.evaluate <- edgelist.from.rpackage(path.to.package.src)

net.evaluate <- network.from.edgelist(el.evaluate)

plot(eForce(net.evaluate, use.network.attr=TRUE))

## Example for sp

path.to.package.src <- system.file('examples','packages', 'sp', package='functionMap')
el.sp <- edgelist.from.rpackage(path.to.package.src)

net.sp <- network.from.edgelist(el.sp)

plot(eForce(net.sp, use.network.attr=TRUE))

## we can explore those function which will eventually call a native function
vs <- unique(unlist(subset(el.sp, category=='native.call', 'tails')))
(ind <- which(network.vertex.names(net.sp) %in% vs))
(L <- unique(unlist(sapply(ind, function(i) dfs.matrix.travel(net.sp[,], i)))))

net.1 <- net.sp %s% L

plot(eForce(net.1, use.network.attr=TRUE))

