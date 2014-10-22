test.analyse.external.call.pattern <- function() {
    # .Call
    checkEquals(analyse.external.call.pattern( lm.fit ), "EXTERNAL_C_Cdqrls" )
    # no prefix
    options(add.prefix.for.external.call=FALSE)
    checkEquals(convertToCharacter(analyse.external.call.pattern(lm.fit)), "C_Cdqrls" )
    options(add.prefix.for.external.call=TRUE)
    # Fortran
    checkEquals(analyse.external.call.pattern(hclust), c("FORTRAN_C_hclust","FORTRAN_C_hcass2") )
    # Fortran and C
    checkEquals(analyse.external.call.pattern(kmeans), c("FORTRAN_C_kmns", "C_C_kmeans_Lloyd", "C_C_kmeans_MacQueen"))
    # .External
    checkEquals(analyse.external.call.pattern(read.table), c("EXTERNAL_C_readtablehead", "EXTERNAL_C_readtablehead"))
}
