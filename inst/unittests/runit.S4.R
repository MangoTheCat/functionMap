test.dumpS4Generic <- function(){
   for(i in list.files(system.file("examples", "R", package = "functionMap"),full=TRUE, pattern='*.R')) source(i)
   S4.funs <- dumpS4Generic()
   checkEquals( S4.funs, 
        c("`play2[Stringed]` <- ", "function (object) ", "{", "    print(\"I am a Stringed\")", 
        "}", "\n", "`show[Instrument]` <- ", "function (object) ", "print(object@tune)", "\n"))
}

test.parseS4fromNs <- function(){
    for(i in list.files(system.file("examples", "R", package = "functionMap"),full=TRUE, pattern='*.R')) source(i)

    checkEquals(  parseS4fromNs(), structure(list(`play2[Stringed]` = "print", `show[Instrument]` = "print"), .Names = c("play2[Stringed]", "show[Instrument]"))  )
}

test.createDirectedNetwork <- function(){

    ordinary.functions <- parse_r_folder(system.file("examples", "R", package = "functionMap"))
    txt <- extract.S4.defn( path = system.file("examples", "R", package = "functionMap"))
    eval(parse(text=txt))
    S4.funs <- parseS4fromNs()
    n1 <- createDirectedNetwork(ordinary.functions, S4.funs)

    checkEquals( n1[,],
        structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0), .Dim = c(9L, 9L), .Dimnames = list(c("add1", "add", "fib", 
        "play", "play.default", "play.Instrument", "play.Stringed", "play2[Stringed]", 
        "show[Instrument]"), c("add1", "add", "fib", "play", "play.default", 
        "play.Instrument", "play.Stringed", "play2[Stringed]", "show[Instrument]"))) )

}

test.extract.S4.defn <- function() {
	txt <- extract.S4.defn(path = system.file("examples", "R", package = "functionMap"))
	checkTrue(txt ==
'setClass("Instrument", representation("VIRTUAL", tune = "character"))
setClass("Stringed", representation("Instrument"))
setClass("Wind", representation("Instrument"))
setClass("Brass", contains = "Wind")
setClass("Woodwind", representation(tune = "character"))
setGeneric("play2")
setMethod("show", "Instrument", function(object) print(object@tune))
setMethod("play2", signature(obj = "Stringed"), play2.Stringed)'
    )
}

