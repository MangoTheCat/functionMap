# basically we need an knowledge database on which kind of statistical analysis supported by which kind of
# common R functions and R packages


# One simplest and flexible data structure is a free Graph

# We may possible use text mining to extract data from R doc

# and generate database we need

# you may consider db <- Rd_db('stats') 

# titles <- lapply(db, tools:::.Rd_get_metadata, 'title')
# descriptions <- lapply(db, tools:::.Rd_get_metadata, 'description')
# details <- lapply(db, tools:::.Rd_get_metadata, 'details')

# however that give too many noises

# It's better to handle it manually

# The principle is to include those "typical" function which may indicate or be a hint for what the code is aiming
# too general function like coef(), cor, cov will be omitted, as they are similar to stop words in text mining

.db.stats <- list(
list(name='acf', keywords=c('autocovariance','autocorrelation')),
list(name='acf2AR', keywords=c('AR process','autocovariance','autocorrelation')),
list(name='AIC', keywords=c('goodness-of-fit','model selection','Akaike')),
list(name='BIC', keywords=c('goodness-of-fit','model selection','Bayesian')),
list(name='anova', keywords=c('anova tables','analysis of variance')),
list(name='anova.glm', keywords=c('anova','analysis of deviance', 'glm')),
list(name='anova.mlm', keywords=c('anova','model comparation', 'multivariate linear model')),
list(name='ansari.test', keywords=c('statistical test','hypothesis test','Ansari-Bradley', 'scale')),
list(name='aov', keywords=c('fit anova model','linear model')),
list(name='ar', keywords=c('autoregressive','time series', 'AR')),
list(name='ar.burg', keywords=c('autoregressive','time series', 'AR')),
list(name='ar.burg.default', keywords=c('autoregressive','time series', 'AR')),
list(name='ar.mle', keywords=c('autoregressive','time series', 'AR')),
list(name='ar.ols', keywords=c('autoregressive','time series', 'ols', 'AR')),
list(name='ar.yw', keywords=c('autoregressive','time series', 'AR')),
list(name='ar.yw.default', keywords=c('autoregressive','time series', 'AR')),
list(name='arima', keywords=c('time series', 'ARIMA')),
list(name='arima.sim', keywords=c('time series', 'simulation', 'ARIMA')),
list(name='arima0', keywords=c('time series')),
list(name='ARMAacf', keywords=c('ARMA','acf')),
list(name='ARMAtoMA', keywords=c('MA','acf')),
list(name='ARMAtoMA', keywords=c('MA','acf')),
list(name='bartlett.test', keywords=c('bartlett','statistical test','hypothesis test', 'homogeneity of variance')),
list(name='binom.test', keywords=c('binomial','statistical test','hypothesis test', 'probability of success')),
list(name='Box.test',  keywords = c("Box-Pierce" ,"Ljung-Box", "statistical test",'hypothesis test', 'independence', 'time series', 'portmanteau') ),                                     
list(name='chisq.test', keywords= c("Pearson's Chi-squared",'chi-squared','count data', 'goodness-of-fit', 'gof', 'contingency table' )),
list(name='cmdscale', keywords = c("multidimensional scaling", 'multiscale')),
list(name='convolve', keywords = c("convolution",  "FFT" )),
list(name='cophenetic', keywords = c("cophenetic",  "distance", "hierarchical clustering")),
list(name='cor.test', keywords = c('association', 'correlation', 'paired samples', 'Pearson', 'Spearman', 'Kendall')),
list(name='decompose',  keywords = c("moving average" , 'time series')), 
list(name='factanal',  keywords = c("factor analysis"   )),                                                  
list(name='fft',       keywords = c('Fourier', 'FFT')),
list(name='filter',  keywords = c('time series')),                                   
list(name='fisher.test', keywords=c("Fisher", 'statistical test','hypothesis test',  'count data', 'contingency table', 'independence')),                                  
list(name='fligner.test', keywords = c("Fligner-Killeen", 'statistical test','hypothesis test',  'homogeneity of variance')),                   
list(name='friedman.test', keywords=c("Friedman", "rank sum", 'statistical test','hypothesis test')), 
list(name='glm', keywords = c("generalized linear models", 'GLM' )),                              
list(name='hclust', keywords=c("Hierarchical Clustering")),
list(name='HoltWinters', keywords=c("Holt-Winters Filtering" , 'time series')),
list(name='isoreg' , keywords = c("isotonic",  "monotone", "regression", 'piecewise constant')),
list(name='KalmanLike', keywords=c("Kalman filtering", 'log-likelihood')), 
list(name='KalmanRun', keywords=c("Kalman filtering", 'fitting', 'residual')), 
list(name='KalmanForecast', keywords=c("Kalman filtering", 'forecastling')), 
list(name='KalmanSmooth', keywords=c("Kalman filtering", 'smoothing')), 
list(name='kmeans',  keywords = c('k-means',  'clustering')),
list(name='kruskal.test', keywords = c("Kruskal-Wallis",  'rank sum', 'statistical test','hypothesis test', 'location')),                                        
list(name='ks.test', keywords = c("Kolmogorov-Smirnov", "statistical test",'hypothesis test', 'distribution' )),
list(name='loess', keywords = c('local fitting', 'smoothing')),
list(name='loglin', keywords = c("log-linear",'contingency table')),
list(name='lsfit', keywords = c("least squares",  "fit")),
list(name='lm', keywords = c('linear','model', "least squares",  "fit")),
list(name='manova', keywords = c("anova", 'multivariate')),
list(name='mantelhaen.test', keywords = c("Cochran-Mantel-Haenszel",  "Chi-Squared", 'statistical test','hypothesis test',  'count data', 'conditional independence')),
list(name='mauchly.test', keywords = c("Mauchly", "Sphericity", 'covariance', 'Wishart', 'statistical test', 'hypothesis test')),
list(name='mcnemar.test', keywords = c("McNemar", "Chi-squared",  "count data", 'statistical test', 'hypothesis test')),
list(name='mood.test', keywords = c("Mood",  "two-sample",  "scale", 'statistical test', 'hypothesis test')),
list(name='nlm', keywords = c("nonlinear",  "minimization", 'optimization')),
list(name='nlminb', keywords = c("optimization",  'nonlinear', 'PORT' )), 
list(name='nls', keywords = c("nonlinear",  "least squares", 'model' )),
list(name='oneway.test', keywords = c("equal means",  'one-way',  'statistical test', 'hypothesis test', 'normal')),
list(name='optim', keywords = c('optimization')),
list(name='optimization', keywords = c('optimization', 'one-dimensional')),
list(name='p.adjust', keywords = c('statistical test', 'multiple test', "multiple comparisons" ,'bonferroni', 'holm','hochberg','hommel','BH','FDR','BY', 'FWER')),
list(name='pairwise.prop.test', keywords = c('statistical test', 'multiple test', 'pairwise', "proportions")),
list(name='pairwise.t.test', keywords = c('statistical test', 'multiple test', 'pairwise', 'T test')),                                                    
list(name='pairwise.wilcox.test', keywords = c('pairwise',  'Wilcoxon',  'rank sum', 'statistical test', 'multiple test' )),                                    
list(name='poisson.test', keywords = c("Poisson", 'statistical test', 'hypothesis test' )),
list(name='power.anova.test', keywords = c("power calculation", 'balanced',  'one-way', 'anova')),
list(name='power.prop.test',keywords = c("power calculation", 'two sample',  'proportion')),              
list(name='power.t.test', keywords = c("power calculation",  'one sample', 'two sample',  'T test' )),
list(name='PP.test', keywords = c("Phillips-Perron",  'unit root', 'time series', 'Newey-West', 'cointegration', 'hypothesis test', 'statistical test')), 
list(name='ppr', keywords = c("projection pursuit",  "regression" )),                                      
list(name='prcomp', keywords=c("principal components analysis"  , 'PCA')),
list(name='princomp', keywords=c("principal components analysis"  , 'PCA')),
list(name='prop.test', keywords = c('proportion', 'statistical test', 'hypothesis test', 'count data')),                                  
list(name='prop.trend.test', keywords = c("trend in proportion", 'trend', 'proportion', 'statistical test', 'hypothesis test')),
list(name='quade.test', keywords = c("Quade", 'hypothesis test', 'statistical test', 'normality', 'blocked data')),
list(name='shapiro.test', keywords = c("Shapiro-Wilk",  'normality',  "hypothesis test", 'statistical test')),
list(name='spec.ar', keywords = c("spectral density",  'time series',  'AR')),
list(name='spec.pgram', keywords = c("spectral density",  'time series', "periodogram")),
list(name='spectrum', keywords = c("spectral density",  'time series' )),
list(name='step', keywords = c('model selection', 'AIC',  'stepwise algorithm')),
list(name='stl', keywords = c("seasonal decomposition",  'time series',  'loess')),                     
list(name='StructTS', keywords = c("structural",  "time series" )),
list(name='t.test', keywords = c("Student's t-Test" , 'statistical test', 'hypothesis test', 'T test', 'Student test')),                                                   
list(name='TukeyHSD', keywords = c("Tukey Honest Significant Differences", 'HSD', 'Tukey', 'TukeyHSD', 'multiple comparation')),
list(name='var.test', keywords = c("F test", 'statistical test', 'hypothesis test', 'variance', 'normal')), 
list(name='wilcox.test', keywords = c("Wilcoxon",  'rank sum',  'signed Rank', 'statistical test', 'hypothesis test', 'symmetric', 'Mann-Whitney'))
)


#' assess.code
#' 
#' assess the code by look for what kind of statistical routine used
#' @param v vector of functions used
#' @param db the database of knowledge we used
#' @return the external score of the function based directly on the statistical functions in db
#' @export
#' @examples \dontrun{
#'    code0 <- 'ff <- function() {mod1 <- lm(Species ~ . , data=iris); summary(mod1)}' 
#'    cat(code0, file=(f0<-tempfile()))
#'    ret <- parseRscript(f0)
#'      assess.code( ret[['ff']] )
#' }
assess.code <- function(v, db = .db.stats) {
    nm <- sapply(db, el ,'name')
    ind <- unlist(sapply(unique(v), function(x) which(x == nm), USE.NAMES=FALSE))
    if (length(ind)>0) {
        weights <- table(unlist( sapply(ind, function(x) db[[x]]$keywords, USE.NAMES=FALSE)))
        weights <- weights[order(weights, decreasing=TRUE)]
        return(weights)
    }
    NA
}

#' assessRfolder
#'
#' analysis an folder of r source files using the \code{parseRscript} and \code{assess.code}
#' @param filepath target path to be analysed
#' @param pattern file pattern to be analysed
#' @param db reference database , see \code{\link{assess.code}}
#' @return list of functions with non-zero scores on statistical functions in \code{db}
assessRfolder <- function(filepath, pattern = '\\.[R|r|S|s|Q|q]', db = .db.stats) {
    fun.attr <- parseRfolder(filepath, pattern)
    l <- sapply(fun.attr, assess.code, db = db)
    l <- l[ sapply(l, function(x) !is.na(x[1])) ]
    nms <- unlist(sapply(l, names), use.names=FALSE)
    cnt <- unlist(l, use.names=FALSE)
    re <- tapply(cnt, nms, sum)
    re <- re[order(as.numeric(re), decreasing=TRUE)]
    attr(l, 'score') <- re
    l
}
