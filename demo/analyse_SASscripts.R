
path.to.package.src <- system.file('examples','SASscripts', package='functionMap')

(edge.list <- edgelist.from.SASfolder(path.to.package.src))

(net <- network.from.edgelist(edge.list))

plot(eForce(net, use.network.attr=TRUE, gravity=1.1))

plot(eForce(net, use.network.attr=TRUE, gravity=1.1, do.depth.layout=TRUE))


## Those data Not available in the package, but they belong to extenal test data
path.to.package.src <- '/experiment/functionMap/trunk/tests/testdata/SAScode'

(edge.list <- edgelist.from.SASfolder(path.to.package.src))

(net <- network.from.edgelist(edge.list))

plot(eForce(net, use.network.attr=TRUE))

plot(eForce(net, use.network.attr=TRUE, do.depth.layout=TRUE, gravity=0.1))


