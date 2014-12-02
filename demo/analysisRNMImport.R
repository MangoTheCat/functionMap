
# Demostration of using functionMap to analysis RNMImport code
# RNMImport pacakge have S4 methods. 
# The functions in the package  will call each other and the inter relation are very complicated.

# you may change this to the path of the pacakge you want to analysis
path.to.package.src <- '/experiment/RNONMEM/importing/RNMImport'

edge.list <- edgelist.from.rpackage(path.to.package.src)

edge.list[1:5, ]

# tails means caller
# heads means callee, and in the graph we will draw an arrow from tail pointing to head
# category, current can be 
#   - normal, means an ordinary function call
#   - do.call, invoked using do.call mechanism
#   - native.call, a native C or FORTRAN function, invoked by .C, .Fortran, .Call or .External
#   - eval.call, invoked by eval(), typically might be a variable or expression, rather than a existing function
# weights , how many times the callee appearing in the caller
# rfile   , where this relation is parsed from

# we can check all but the ordinary function call

subset(edge.list, category!='normal')

# To visualize, it's useful to convert it to a network(graph) object
(net <- network.from.edgelist(edge.list))

# It's a directed graph with 420 nodes
# store vertex names

v.names <- net %v% 'vertex.names'

# net[,] results a adjacency matrix without considering the weight
# dfs.matrix.travel can list all the connected components
(CP <- dfs.matrix.travel(net[,], direction='bidirection'))

# only 2 components, second one have only 2 elements
v.names[ CP[[2]] ]

# make a force directed graph of the whole net
plot(eForce(net, use.network.attr = TRUE))
# you can observe mutliple categories the nodes(not edges) belongs to, the meaning is self-evident
# On the right top corner, you can switch between force (another layout) and chord plot, or restore to original
# Or save the current graph to a file
# When mouse is on the nodes the value of nodes(by default is how many functions it called) is displayed
# if mouse is floating on edge, the weights is displayed.

# You can click the categories on the top left corner, to exclude all the nodes (and edges linking to them).
# For example, you can click "other" category to hide all the functions not defined in this package.

# The mouse wheel can be used to zoom out or zoom in the graph.

# The title is by default the deparsed object name and shown on the right bottom corner.





# use interconnected to convert the edge.list to smaller one, by summarying all out of pacakge callees
el2 <- interconnected(edge.list)
# list of the functions only calling out of pacakge functions
subset(el2, heads == out_packages)

# The names of out of pacakge callees can be get from new attributes
attr(el2,'vertex.other.call')[1:10]

# create the network, the 'details' attributes are passed from above one
net2 <- network.from.edgelist(el2) 
(net2 %v% 'details')[1:10]
# visualize it using the eForce
plot(eForce(net2, use.network.attr=TRUE))


### It's straight forward to page.rank 
pr <- page.rank( net )
### nodes with highest 30 page rank
v.names[ rank(pr) > length(pr) - 30 ]

### set to value of nodes
net %v% 'value' <- as.vector(pr)

### Now the value is shown in the force directed graph
plot(eForce(net, use.network.attr=TRUE))



### hubness and authorities
ha <- hits.rank(net)

v.names[rank(ha[,'h']) > 390]

v.names[rank(ha[,'a']) > 390]

