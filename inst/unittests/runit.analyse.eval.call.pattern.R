
test.analyse.eval.call.pattern <- function(){

    checkEquals( c(analyse.eval.call.pattern(lm)),
        "stats::model.frame" )
    
    checkEquals( c(analyse.eval.call.pattern(glm)),
        c("if (is.function(method)) \"method\" else method", "stats::model.frame", 
        "if (is.function(method)) \"method\" else method") )

    checkEquals( c(analyse.eval.call.pattern(anova.mlm)),
            "anova.mlmlist" )

    checkEquals( c(analyse.eval.call.pattern( quote({
            mf <- match.call()
            mf[[1]] <- 'new.fun'
            eval(mf)
    }) ) ) , 'new.fun' )

    checkEquals( c(analyse.eval.call.pattern( quote({
            nested <- function(l) {
                mf <- match.call()
                mf[[1]] <- 'new.fun'
                eval(mf)
            }
    }) ) ) , 'new.fun' )

    checkEquals( c(analyse.eval.call.pattern( quote({
            mf <- match.call()
            mf[[1]] <- 'old.fun'
            nested <- function(l) {
                mf <- match.call()
                mf[[1]] <- 'new.fun'
            }
            eval(mf)
    }) ) ) , 'old.fun' )

    # this is tricky, actually, not able to determine static from code
    checkEquals( c(analyse.eval.call.pattern( quote({
            mf <- match.call()
            mf[[1]] <- 'old.fun'
            nested <- function(l) {
                eval(mf)
                mf <- match.call()
                mf[[1]] <<- 'new.fun'
            }
            eval(mf)
    }) ) ) , c('old.fun', 'new.fun') )

    checkEquals( c(analyse.eval.call.pattern( quote({
            mf <- match.call()
            mf[[1]] <- 'old.fun'
            nested <- function(l) {
                eval(mf)
                mf <- match.call()
                mf[[1]] <- 'new.fun'
            }
            eval(mf)
    }) ) ) , c('old.fun', 'old.fun') )

    checkEquals( c(analyse.eval.call.pattern( quote({
            mf <- match.call()
            mf[[1]] <- 'old.fun'
            if ( eval(mf) > 0)  {
                mf[[1]] <- 'new.fun'
            }
            eval(mf)
    }) ) ) , c('old.fun', 'new.fun') )
}
