

library(functionMap)

lfun <- parseRfolder(system.file("examples", "MSToolkit", "R", package = "functionMap"))

n1 <- createNetwork(lfun)
n2 <- createNetwork(lfun, rootfunc="mstoolkitUnitTests")
n3 <- createNetwork(lfun, returnmatrix = TRUE)

plotFunctionMap(n1)

plotFunctionMap(n2)
