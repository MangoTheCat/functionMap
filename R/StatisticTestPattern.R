## Basically we need a knowledge database on which kind of statistical
## analysis supported by which kind of common R functions and R packages.
##
## One simple and flexible data structure is a free Graph.
##
## We may possibly use text mining to extract data from the R manuals,
## and generate the database we need. E.g.
##
##  db <- Rd_db('stats')
## titles <- lapply(db, tools:::.Rd_get_metadata, 'title')
## descriptions <- lapply(db, tools:::.Rd_get_metadata, 'description')
## details <- lapply(db, tools:::.Rd_get_metadata, 'details')
##
## However this gives noisy results. It's better to handle it manually.
##
## The principle is to include those "typical" function which may indicate
## or be a hint for what the code is aiming too general function like
## coef(), cor, cov will be omitted, as they are similar to stop words in
## text mining.

annotation_db <- list()

annotation_db[["stats"]] <- list(
  'acf' = c(
    'autocovariance',
    'autocorrelation'
  ),
  'acf2AR' = c(
    'AR process',
    'autocovariance',
    'autocorrelation'
  ),
  'AIC' = c(
    'goodness-of-fit',
    'model selection',
    'Akaike'
  ),
  'BIC' = c(
    'goodness-of-fit',
    'model selection',
    'Bayesian'
  ),
  'anova' = c(
    'anova tables',
    'analysis of variance'
  ),
  'anova.glm' = c(
    'anova',
    'analysis of deviance',
    'glm'
  ),
  'anova.mlm' = c(
    'anova',
    'model comparation',
    'multivariate linear model'
  ),
  'ansari.test' = c(
    'statistical test',
    'hypothesis test',
    'Ansari-Bradley',
    'scale'
  ),
  'aov' = c(
    'fit anova model',
    'linear model'
  ),
  'ar' = c(
    'autoregressive',
    'time series',
    'AR'
  ),
  'ar.burg' = c(
    'autoregressive',
    'time series',
    'AR'
  ),
  'ar.burg.default' = c(
    'autoregressive',
    'time series',
    'AR'
  ),
  'ar.mle' = c(
    'autoregressive',
    'time series',
    'AR'
  ),
  'ar.ols' = c(
    'autoregressive',
    'time series',
    'ols',
    'AR'
  ),
  'ar.yw' = c(
    'autoregressive',
    'time series',
    'AR'
  ),
  'ar.yw.default' = c(
    'autoregressive',
    'time series',
    'AR'
  ),
  'arima' = c(
    'time series',
    'ARIMA'
  ),
  'arima.sim' = c(
    'time series',
    'simulation',
    'ARIMA'
  ),
  'arima0' = c(
    'time series'
  ),
  'ARMAacf' = c(
    'ARMA',
    'acf'
  ),
  'ARMAtoMA' = c(
    'MA',
    'acf'
  ),
  'ARMAtoMA' = c(
    'MA',
    'acf'
  ),
  'bartlett.test' = c(
    'bartlett',
    'statistical test',
    'hypothesis test',
    'homogeneity of variance'
  ),
  'binom.test' = c(
    'binomial',
    'statistical test',
    'hypothesis test',
    'probability of success'
  ),
  'Box.test' = c(
    'Box-Pierce' ,
    'Ljung-Box',
    'statistical test',
    'hypothesis test',
    'independence',
    'time series',
    'portmanteau'
  ),
  'chisq.test' =  c(
    'Pearson\'s Chi-squared',
    'chi-squared',
    'count data',
    'goodness-of-fit',
    'gof',
    'contingency table'
  ),
  'cmdscale' = c(
    'multidimensional scaling',
    'multiscale'
  ),
  'convolve' = c(
    'convolution',
    'FFT'
  ),
  'cophenetic' = c(
    'cophenetic',
    'distance',
    'hierarchical clustering'
  ),
  'cor.test' = c(
    'association',
    'correlation',
    'paired samples',
    'Pearson',
    'Spearman',
    'Kendall'
  ),
  'decompose' = c(
    'moving average' ,
    'time series'
  ),
  'factanal' = c(
    'factor analysis'
  ),
  'fft' = c(
    'Fourier',
    'FFT'
  ),
  'filter' = c(
    'time series'
  ),
  'fisher.test' = c(
    'Fisher',
    'statistical test',
    'hypothesis test',
    'count data',
    'contingency table',
    'independence'
  ),
  'fligner.test' = c(
    'Fligner-Killeen',
    'statistical test',
    'hypothesis test',
    'homogeneity of variance'
  ),
  'friedman.test' = c(
    'Friedman',
    'rank sum',
    'statistical test',
    'hypothesis test'
  ),
  'glm' = c(
    'generalized linear models',
    'GLM'
  ),
  'hclust' = c(
    'Hierarchical Clustering'
  ),
  'HoltWinters' = c(
    'Holt-Winters Filtering' ,
    'time series'
  ),
  'isoreg'  = c(
    'isotonic',
    'monotone',
    'regression',
    'piecewise constant'
  ),
  'KalmanLike' = c(
    'Kalman filtering',
    'log-likelihood'
  ),
  'KalmanRun' = c(
    'Kalman filtering',
    'fitting',
    'residual'
  ),
  'KalmanForecast' = c(
    'Kalman filtering',
    'forecastling'
  ),
  'KalmanSmooth' = c(
    'Kalman filtering',
    'smoothing'
  ),
  'kmeans' = c(
    'k-means',
    'clustering'
  ),
  'kruskal.test' = c(
    'Kruskal-Wallis',
    'rank sum',
    'statistical test',
    'hypothesis test',
    'location'
  ),
  'ks.test' = c(
    'Kolmogorov-Smirnov',
    'statistical test',
    'hypothesis test',
    'distribution'
  ),
  'loess' = c(
    'local fitting',
    'smoothing'
  ),
  'loglin' = c(
    'log-linear',
    'contingency table'
  ),
  'lsfit' = c(
    'least squares',
    'fit'
  ),
  'lm' = c(
    'linear',
    'model',
    'least squares',
    'fit'
  ),
  'manova' = c(
    'anova',
    'multivariate'
  ),
  'mantelhaen.test' = c(
    'Cochran-Mantel-Haenszel',
    'Chi-Squared',
    'statistical test',
    'hypothesis test',
    'count data',
    'conditional independence'
  ),
  'mauchly.test' = c(
    'Mauchly',
    'Sphericity',
    'covariance',
    'Wishart',
    'statistical test',
    'hypothesis test'
  ),
  'mcnemar.test' = c(
    'McNemar',
    'Chi-squared',
    'count data',
    'statistical test',
    'hypothesis test'
  ),
  'mood.test' = c(
    'Mood',
    'two-sample',
    'scale',
    'statistical test',
    'hypothesis test'
  ),
  'nlm' = c(
    'nonlinear',
    'minimization',
    'optimization'
  ),
  'nlminb' = c(
    'optimization',
    'nonlinear',
    'PORT'
  ),
  'nls' = c(
    'nonlinear',
    'least squares',
    'model'
  ),
  'oneway.test' = c(
    'equal means',
    'one-way',
    'statistical test',
    'hypothesis test',
    'normal'
  ),
  'optim' = c(
    'optimization'
  ),
  'p.adjust' = c(
    'statistical test',
    'multiple test',
    'multiple comparisons' ,
    'bonferroni',
    'holm',
    'hochberg',
    'hommel',
    'BH',
    'FDR',
    'BY',
    'FWER'
  ),
  'pairwise.prop.test' = c(
    'statistical test',
    'multiple test',
    'pairwise',
    'proportions'
  ),
  'pairwise.t.test' = c(
    'statistical test',
    'multiple test',
    'pairwise',
    'T test'
  ),
  'pairwise.wilcox.test' = c(
    'pairwise',
    'Wilcoxon',
    'rank sum',
    'statistical test',
    'multiple test'
  ),
  'poisson.test' = c(
    'Poisson',
    'statistical test',
    'hypothesis test'
  ),
  'power.anova.test' = c(
    'power calculation',
    'balanced',
    'one-way',
    'anova'
  ),
  'power.prop.test' = c(
    'power calculation',
    'two sample',
    'proportion'
  ),
  'power.t.test' = c(
    'power calculation',
    'one sample',
    'two sample',
    'T test'
  ),
  'PP.test' = c(
    'Phillips-Perron',
    'unit root',
    'time series',
    'Newey-West',
    'cointegration',
    'hypothesis test',
    'statistical test'
  ),
  'ppr' = c(
    'projection pursuit',
    'regression'
  ),
  'prcomp' = c(
    'principal components analysis'  ,
    'PCA'
  ),
  'princomp' = c(
    'principal components analysis'  ,
    'PCA'
  ),
  'prop.test' = c(
    'proportion',
    'statistical test',
    'hypothesis test',
    'count data'
  ),
  'prop.trend.test' = c(
    'trend in proportion',
    'trend',
    'proportion',
    'statistical test',
    'hypothesis test'
  ),
  'quade.test' = c(
    'Quade',
    'hypothesis test',
    'statistical test',
    'normality',
    'blocked data'
  ),
  'shapiro.test' = c(
    'Shapiro-Wilk',
    'normality',
    'hypothesis test',
    'statistical test'
  ),
  'spec.ar' = c(
    'spectral density',
    'time series',
    'AR'
  ),
  'spec.pgram' = c(
    'spectral density',
    'time series',
    'periodogram'
  ),
  'spectrum' = c(
    'spectral density',
    'time series'
  ),
  'step' = c(
    'model selection',
    'AIC',
    'stepwise algorithm'
  ),
  'stl' = c(
    'seasonal decomposition',
    'time series',
    'loess'
  ),
  'StructTS' = c(
    'structural',
    'time series'
  ),
  't.test' = c(
    'Student\'s t-Test' ,
    'statistical test',
    'hypothesis test',
    'T test',
    'Student test'
  ),
  'TukeyHSD' = c(
    'Tukey Honest Significant Differences',
    'HSD',
    'Tukey',
    'TukeyHSD',
    'multiple comparation'
  ),
  'var.test' = c(
    'F test',
    'statistical test',
    'hypothesis test',
    'variance',
    'normal'
  ),
  'wilcox.test' = c(
    'Wilcoxon',
    'rank sum',
    'signed Rank',
    'statistical test',
    'hypothesis test',
    'symmetric',
    'Mann-Whitney'
  )
)

#' Annotate code with keywords
#'
#' Assess the code by finding out what kind of statistical methods
#' it uses.
#'
#' @param funcs A character vector of function names to annotate.
#' @param paths A character vector of R files or folders with R files
#'    to annotate.
#' @return A table object (named integer vector) of all terms.
#'
#' @export
#' @examples
#' annotate("lm")
#'
#' f <- function() {
#'   lm(Sepal.Width ~ Sepal.Length, data = iris)
#' }
#' annotate("f")
#'
#' ## Multiplicity of terms, 'nonlinear' appears twice
#' annotate(c("nlm", "nls"))

annotate <- function(funcs = NULL, paths = NULL) {
  funcs <- as.character(funcs)
  paths <- as.character(paths)

  db <- annotation_db[["stats"]]

  calls <- c(
    unlist(lapply(funcs, annotate_functions)),
    unlist(lapply(paths, annotate_paths))
  )

  res <- table(unlist(drop_null(db[c(funcs, calls)])))
  dimnames(res) <- unname(dimnames(res))
  res
}

#' Annotate functions
#'
#' @inheritParams annotate
#' @return A character vector of terms.

annotate_functions <- function(funcs) {
  objs <- mget(funcs, inherits = TRUE)

  unlist(lapply(
    objs,
    get_global_calls,
    include_base = FALSE,
    multiples = TRUE
  ))
}

#' Annotate all functions in R scripts or folders of scripts
#'
#' @inheritParams annotate
#' @return A character vector of terms.

annotate_paths <- function(paths) {

  res <- lapply(paths, function(p) {
    if (!file.exists(p)) {
      warning("File '", p, "' does not exist")
    } else {
      if (file.info(p)$isdir) {
        parse_r_folder(p)
      } else {
        parse_r_script(p)
      }
    }
  })

  unlist(res)
}
