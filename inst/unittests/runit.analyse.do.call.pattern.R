test.analyse.do.call.pattern <- function() {

    checkEquals( analyse.do.call.pattern( quote({
        do.call('sin', list(1:10) )
    })), 'sin' )

    checkEquals( analyse.do.call.pattern( quote({
        do.call(myfunction, list(1:10) )
    })), list(quote(myfunction)) )

    checkEquals( convertToCharacter(analyse.do.call.pattern( quote({
        do.call(fun2, list(1:10) )
    }))), 'fun2' )
    
    checkEquals( analyse.do.call.pattern(body(glm)), 'glm.control' )
}
