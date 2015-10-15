test.assess.code <- function(){
    checkTrue(assess.code( c('lm', 'shapiro.test'))['statistical test']>0)
}

test.assessRfolder <- function(){
    re <- assessRfolder(system.file('examples/R', package='functionMap'))
    # no statistical function used in the folder
    checkTrue( length(re)==0)
}
