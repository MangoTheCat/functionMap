
test.createNetwork <- function(){
    n0 <- parseRfolder(system.file('examples/R', package='functionMap'))
    # normal call
    n1 <- createNetwork(n0)
    checkEquals(
        dfs.matrix.travel(as.matrix(n1),direction='bidirection'),
        list(1L, 2:3, 4L, 5L, 6L, 7L))
    # with non existing function
    checkException(n2 <- createNetwork(n0, rootfunc='nonexist'))
    # with existing function
    # note it's directed, although add, fib are in same strongly connected component, only fib->add no add->fib
    n3 <- createNetwork(n0, rootfunc='fib')
    n4 <- createNetwork(n0, rootfunc='add')
    checkEquals( as.matrix(n3), structure(c(0, 0, 1, 0), .Dim = c(2L, 2L), .Dimnames = list(c("fib", "add"), c("fib", "add"))))
    checkEquals( as.matrix(n4), structure(0, .Dim = c(1L, 1L), .Dimnames = list("add", "add")) )
    # other
    n5 <- createNetwork(n0, '1$', returnmatrix=TRUE)
    checkEquals( n5, structure(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(6L, 6L), .Dimnames = list(c("add", "fib", "play", "play.default", "play.Instrument", "play.Stringed"), c("add", "fib", "play", "play.default", "play.Instrument", "play.Stringed"))) )

}


test.createNetwork.duplicated.names <- function() {
    L <- list( c('x','y','z'), c('x','z'),c('y','y'), c('z'))
    # we deliberately set duplicated names 
    names(L) <- c('z','x','x','y')
    net <- createNetwork(L)

    # should be equivalent to 
    L1 <- list( z=c('x','y','z'), x=c('x','y','z'), y='z')
    net1 <- createNetwork(L1)

    # net and net1 should be equivalent
    checkTrue(all ( net1[c('x','y','z'),c('x','y','z')] == net[c('x','y','z'),c('x','y','z')] ))
}


