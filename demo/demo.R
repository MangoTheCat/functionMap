

library(functionMap)

lfun <- parseRfolder(system.file("examples", "MSToolkit", "R", package = "functionMap"))

# plot function map
n1 <- createNetwork(lfun)
n2 <- createNetwork(lfun, rootfunc="mstoolkitUnitTests")



plotFunctionMap(n1)
plotFunctionMap(n2)


# Analyze the funtion relationships
n3 <- createNetwork(lfun, returnmatrix = TRUE)

netPlot <- eForce(n3)
newplot <- netPlot + option(size=c(1024,768), title="functionMap network output", showLabel=T)
plot(newplot, Local = T)

newplot <- netPlot + option(size=c(640,480), title="functionMap network output", showLabel=F)
plot(newplot, Local = T)

n3[3, , drop = FALSE] # this row means the function 'analyzeData' calls 'analyzeRep' and so on.

res1 <- apply(n3, 1, sum) # this result means how many functions are invoked directly by each function.
res2 <- apply(n3, 2, sum) # this result means how many time each function is invoked.

allfunc <- rownames(n3)
res3 <- rep(0, length(allfunc))
names(res3) <- allfunc

res3 <- sapply(rownames(n3), FUN = function(X) # this result means how many functions are used by each function.
		{
			tmp <- ncol(createNetwork(lfun, returnmatrix = TRUE, rootfunc = X)); 
			ifelse(is.null(tmp), 0, tmp)
		}
)
