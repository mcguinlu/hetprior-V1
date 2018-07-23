#'Informative bayesian prior lookup for heterogenity in meta-analyses
#'
#'\code{hetprior} returns an informative Bayesian prior based on the conditions
#'specified
#'
#'This function uses data from five original studies to provide a repository of
#'informative priors for the heterogeneity observed in meta-analysis of studies
#'in particular areas of health care.
#'
#'@usage hetprior(a,b,c,d,e,f,g,h, graph, quiet)
#'
#'@param a heterogeity statisitc, allowable options are: Tau2 | I2
#'@param b data type, allowable options are: Binary | Continuous | Other
#'@param graph plots density function for the specified prior distribution,
#'  default = FALSE
#'@param quiet outputs variables describing prior distribution without printing
#'  anything to console, default = FALSE
#'
#'
#'@examples
#'General examples
#'  hetprior("Tau2", "Binary","Log odds ratio","Log normal","General","General","Cancer","<50")
#'  hetprior("Tau2", "Binary","Log odds ratio","Log normal","General","General","Cancer",">50",quiet=TRUE, graph=TRUE)
#'  hetprior("Tau2", "Binary","Log odds ratio","Log normal","Non-pharmacological vs any","All-cause mortality","Cancer","<50", graph=TRUE)
#'
#'
#'@return \code{hetprior} generates new varaibles which describe the informative
#'prior distribution for the conditions specified in the arguments
#'
#'If distribution form is X, Y or Z, the variables generate represent the mean
#'and standard deviation of that prior distrubtion.
#'
#'If the distribution form specified is W, the varaibels return describe the
#'shape and scale of that prior distribution
#'
#'@seealso \url{TBC} for interative web-app
#'
#'@references Turner, Rebecca M., Jonathan Davey, Mike J. Clarke, Simon G.
#'  Thompson, and Julian PT Higgins. "Predicting the extent of heterogeneity in
#'  meta-analysis, using empirical data from the Cochrane Database of Systematic
#'  Reviews." International journal of epidemiology 41, no. 3 (2012): 818-827.
#'
#'  Turner, Rebecca M., Dan Jackson, Yinghui Wei, Simon G. Thompson, and Julian
#'  PT Higgins. "Predictive distributions for between?study heterogeneity and
#'  simple methods for their application in Bayesian meta?analysis." Statistics
#'  in medicine 34, no. 6 (2015): 984-998.
#'
#'  Rhodes, Kirsty M., Rebecca M. Turner, Ian R. White, Dan Jackson, David J.
#'  Spiegelhalter, and Julian PT Higgins. "Implementing informative priors for
#'  heterogeneity in meta?analysis using meta?regression and pseudo
#'  data." Statistics in medicine 35, no. 29 (2016): 5495-5511.
#'
#'  Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. "Predictive
#'  distributions were developed for the extent of heterogeneity in
#'  meta-analyses of continuous outcome data." Journal of Clinical
#'  Epidemiology 68, no. 1 (2015): 52-60.
#'
#'  Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. "Empirical
#'  evidence about inconsistency among studies in a pair?wise
#'  meta?analysis." Research synthesis methods 7, no. 4 (2016): 346-370.|
#'
#'
#'@author Luke A McGuinness \email{luke.mcguinness@@bristol.ac.uk}
#'
#'@export




hetprior <- function(a,b,c,d,e,f,g,h,graph=FALSE,quiet=FALSE){

# Input ----
# Display input to allow user to double check if input is correct.

  if(quiet==FALSE){

    cat("Input:","\n")
    cat("   Heterogeneity statistic: ", a, "\n")
    cat("   Data type:               ", b, "\n")
    cat("   Effect measure:          ", c, "\n")
    cat("   Distribution form:       ", d, "\n")
    cat("   Type of Intervention:    ", e, "\n")
    cat("   Nature of outcome:       ", f, "\n")

    if(missing(g)){stop("\n", "Missing argument - please fullly specify prior conditions")}else{cat("   Medical area:            ", g, "\n")}

    if(missing(h)){stop("\n", "Missing argument - please fullly specify prior conditions")}else{cat("   Average sample size:     ", h, "\n", "\n")}

  }else{

    if(missing(g)){stop("\n", "Missing argument - please fullly specify prior conditions")}
    if(missing(h)){stop("\n", "Missing argument - please fullly specify prior conditions")}

  }

# Import data ----


# Subset data based on specifications ----

  data.x <- subset(data,  Heterogeneity.statistic==a &
                          Data.Type==b &
                          Effect.Measure==c &
                          Distribution.form==d &
                          Type.of.Intervention==e &
                          Nature.of.Outcome==f,
                          select = c(Medical.area:Average.Sample.Size))

# Prior available? ----
# Check if prior available for specified parameters

    prioravailable <- data.x[1,1]

    if(is.na(prioravailable)==TRUE) {

      stop('No prior available for this specification')

    }

# Extract pior ----
# Check to see if medical area or average sample size is required, and subset data based on specified conditions

    if(quiet==FALSE){cat("Checking if details on Medical area & Average Sample Size are required to specifiy this prior. . . . .", "\n")}

    data.check <- subset(data, Heterogeneity.statistic==a &
                          Data.Type==b &
                          Effect.Measure==c &
                          Distribution.form==d &
                          Type.of.Intervention==e &
                          Nature.of.Outcome==f,
                          select = c(Medical.area:Average.Sample.Size))

    requiremedicalarea<-data.check[1,1]
    requireaveragess<-data.check[1,2]

    if(requiremedicalarea == 0){

      prior.data <- subset(data, Heterogeneity.statistic==a &
                                 Data.Type==b &
                                 Effect.Measure==c &
                                 Distribution.form==d &
                                 Type.of.Intervention==e &
                                 Nature.of.Outcome==f,
                                 select = c(Mean:Reference))

      if(quiet==FALSE){
      cat("   Medical Area not required", "\n")
      cat("   Average Sample Size not required", "\n")
      }
    } else {
        cat("   Medical area required", "\n")
        if(requireaveragess == 0){

          if(quiet==FALSE){cat("   Average Sample Size not required", "\n")}

          prior.data <- subset(data, Heterogeneity.statistic==a &
                                  Data.Type==b &
                                  Effect.Measure==c &
                                  Distribution.form==d &
                                  Type.of.Intervention==e &
                                  Nature.of.Outcome==f &
                                  Medical.area==g,
                                  select = c(Mean:Reference))
        } else {

          if(quiet==FALSE){cat("   Average Sample Size required", "\n")}

          prior.data <<- subset(data, Heterogeneity.statistic==a &
                                   Data.Type==b &
                                   Effect.Measure==c &
                                   Distribution.form==d &
                                   Type.of.Intervention==e &
                                   Nature.of.Outcome==f &
                                   Medical.area==g &
                                   Average.Sample.Size==h,
                                   select = c(Mean:Reference))
        }
    }

# Results ----
# Display results of look-up and generate new variables, specifying the priors characteristics

    if(quiet==FALSE){
      cat("\n")
      cat("Prior look-up results (variable names in brackets):", "\n")
    }

    if(d=="Inverse gamma"){

      priorshape<<-prior.data[1, 1]
      priorscale<<-prior.data[1, 2]

      if(quiet==FALSE){
        cat("   Prior distrubtion        =      ", d, "\n")
        cat("   Prior shape (priorshape) =      ", priorshape, "\n")
        cat("   Prior scale (priorscale) =      ", priorscale, "\n")
      }

    } else {

    priormean<<-prior.data[1, 1]
    priorsd<<-prior.data[1, 2]
    priorvar<<-priorsd*priorsd
    priormedian<<-prior.data[1,4]
    #priorlow95ci<<-prior.data[1,5]
    #priorhigh95ci<<-prior.data[1,6]

    if(quiet==FALSE){
      cat("   Prior distrubtion          = ", d, "\n")
      cat("   Prior Mean (priormean)     = ", priormean, "\n")
      cat("   Prior SD (priorsd)         = ", priorsd, "\n")
      cat("   Prior variance (priorvar)  = ", priorvar, "\n")
      #cat("   Prior Median (priormedian) = ", priormedian, "\n")
      #cat("   Low 95% CI (priorlow95ci) = ", priorlow95ci, "\n")
      #cat("   High 95% CI (priorhigh95ci) = ", priorhigh95ci, "\n", "\n")
    }
    }

  if(quiet==FALSE){
  Notes<-as.character(prior.data[1,7])
  Notes2<-as.character(prior.data[1,8])
  cat("\n", "Notes", "\n")
  cat("   1.", Notes, "\n")
  cat("   2.", Notes2, "\n", "\n")

  Reference<-as.character(prior.data[1,9])
  cat("Reference:", "\n")
  cat(Reference, "\n")
  }

# Probability Density Plots ----
# Draw plot for result

  if(graph == TRUE){

     if(d=="Inverse gamma"){

      is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
      if(is.installed("invgamma")<=FALSE){
        install.packages("invgamma")
        library(invgamma)
      }

      xv <- seq(0,10,.001)
      plot(xv, dinvgamma(xv, shape=priorshape, scale=priorscale), type='l', xlab=a, ylab="Density",main="Prior density distribution")

     }

    if(d=="Log normal"){
      xv <- seq(0,5,.001)
      plot(xv, dlnorm(xv, priormean, priorvar), type='l', xlab=a, ylab="Density",main="Prior density distribution")
    }

    if(d=="Log t5"){
      xv <- seq(0,5,.001)
      plot(xv, dinvgamma(xv, shape=priormean, scale=priorsd), type='l', xlab=a, ylab="Density",main="Prior density distribution")
    }

    if(d=="t5 for logit(I2)"){
      xv <- seq(0,5,.001)
      plot(xv, dinvgamma(xv, shape=priormean, scale=priorsd), type='l', xlab=a, ylab="Density",main="Prior density distribution")
    }
  }
}
