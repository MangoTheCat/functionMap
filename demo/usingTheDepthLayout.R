# The Echart force directed graph has a feature of locking one coordinate but free the other one.
# This feature allows us to layout those nodes (vertices) by fix one coordinate to a so called depth and free another one
# to the equilibrium of repulsive and attractive force.

## Tree depth

data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
cpus.ltr$frame
nodes = cpus.ltr$frame
nms = sort(as.integer(rownames(nodes)))
nodes$name = ifelse( nodes$var=='<leaf>', paste('Pred', round(nodes$yval,3), sep='='), paste(nodes$var, nodes$splits[,1]) )

edge.list = NULL
for(i in nms) {
    p = nodes[ as.character(i), 'name' ]
    if ( (2*i) %in% nms ) {
        q = nodes[ as.character(2*i), 'name' ]
        edge.list = rbind(edge.list, data.frame(tails=p, head=q, category='left child'))
    }
    if ( (2*i + 1) %in% nms ) {
        q = nodes[ as.character(2*i + 1), 'name' ]
        edge.list = rbind(edge.list, data.frame(tails=p, head=q, category='right child'))
    }
}

net = network.from.edgelist(edge.list)

cate = ifelse(grepl('^Pred',network.vertex.names(net) ), '<leaf>', 'split')
# demo how to use a factor as category, then we can control the order of levels shown
cate = factor(cate, labels=c('split','<leaf>')) 
# network dont support assigning a factor to category
# net %v% 'category' = cate
# But following trick works
net %v% 'category' = as.list(cate)
net %v% 'category'

# we can now use eForce, but the display is not a tree style anymore
plot(eForce(net, use.network.attr=T, gravity = 1.2))

# Now we use Depth layout, which by default do a topological sort and arrange them
plot(eForce(net, use.network.attr=T, do.depth.layout=T , gravity = 0.5))

# Or we want a horizontal layout , them using following
plot(eForce(net, use.network.attr=T, do.depth.layout=T , gravity = 0.5, depth.layout='fixY'))

# One pitfall to be noted here is the left/right children information is lost in the eForce plot


## Layout functions on page.rank

### Example for KernSmooth
path.to.package.src <- system.file('examples','packages', 'KernSmooth', package='functionMap')

edge.list <- edgelist.from.rpackage(path.to.package.src)

n.kernsmooth <- network.from.edgelist(edge.list)

pr <- page.rank(n.kernsmooth)
network.vertex.names(n.kernsmooth) [ rank(pr) > 30 ]
n.kernsmooth %v% 'value' <- as.vector(pr) / max(pr) * 20
depth. = 10 - as.integer(factor(pr,sort(unique(pr))))
n.kernsmooth %v% 'depth' <- depth.
# now use page.rank
plot(eForce(n.kernsmooth, use.network.attr=TRUE, do.depth.layout=T, gravity=1.1))

# or you may jitter them a little
plot(eForce(n.kernsmooth, use.network.attr=TRUE, do.depth.layout=T, gravity=1.1, do.jitter = T))
