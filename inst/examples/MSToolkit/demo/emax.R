
# MSToolkit will create ReplicateData, MicroData and MacroData directories 
# under the current working directory

library(MSToolkit)

generateData( replicateN = 5, subjects = 100, treatDoses = c(0, 5, 10, 50, 100), 
  genParNames = "E0,ED50,EMAX", genParMean = c(2,50,10), genParVCov = c(.5,30,10), 
  respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 2, 
  interimSubj = ".3,.7" 
  )

demo(emaxfit,ask=F)

emaxCode <- function(data){
    uniDoses <- sort( unique(data$DOSE)) 
	obsMean <- tapply(data$RESP, list(data$DOSE), mean)
	obsSD <- tapply(data$RESP, list(data$DOSE), sd)                                                                   
    eFit <- emax.fit( data$RESP, data$DOSE )
    outDf <- data.frame( DOSE = uniDoses, 
      MEAN = eFit$fitpred, 
      SE = eFit$sdpred,
	  SDDIF = eFit$sddif)
    outDf$LOWER <- outDf$MEAN - 1.96*outDf$SE
    outDf$UPPER <- outDf$MEAN + 1.96*outDf$SE
    outDf$N     <- table(data$DOSE)
	outDf$OBSMEAN <- obsMean
	outDf$OBSSD <- obsSD
    outDf 
}                                                                                                                   
             
macroCode <- function(data) {
  # Is effect at highest dose significant?
  success <- data$LOWER[data$INTERIM==max(data$INTERIM) & data$DOSE==max(data$DOSE)] > 7
  data.frame( SUCCESS = success )
}
  
interimCode <- function( data ){
  # DROP any doses where the lower bound of the difference from placebo is negative
  dropdose  <- with( data , DOSE [ LOWER < 0 & DOSE != 0] )
  outList <- list()
  if( length(dropdose) > 0 ) outList$DROP <- dropdose
  outList$STOP <- length(dropdose) == nrow(data)-1
  outList
}
   
analyzeData(analysisCode = emaxCode, macroCode = macroCode, 
  interimCode = interimCode )