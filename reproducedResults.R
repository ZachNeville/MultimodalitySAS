# Reproduction of the SAS results in the R environment

# Due to differences in the way random numbers are generated in R vs SAS, we have provided
# the data sets from the SAS environment to ensure complete reproducibility
# NOTE: These file paths will need to be updated to reflect the location of the data files on your machine

# An easy way to update the file paths is to set the working directory. A general example is provided below.
setwd("C:\\Users\\Username\\Documents")

onenorm <- read.csv("onenorm.csv", sep="")
twoNorms1 <- read.csv("twonorms1.csv", sep="")
twoNorms2 <- read.csv("twonorms2.csv", sep="")
threenorms <- read.csv("threenorms.csv", sep="")
irisPW <- read.csv("irispw.csv", sep="")


# Dip Test results
library(diptest)
dip.test(onenorm$x)
dip.test(twoNorms1$x)
dip.test(twoNorms2$x)
dip.test(threenorms$x)
dip.test(irisPW$PetalWidth)


# Silverman Test - define the function using the same code as silverman.sas
sas_silverman <- function(silvdata, k = 1, m = 999, adjust = 0, digits = 6, setseed = NULL, showseed = 0, outseed = NULL, out = NULL, completecase = 0, include = NULL) {

  # Verify data is numeric
  if(!is.numeric(as.matrix(silvdata))) {
    cat("ERROR: Your data was input in SAS as numeric, but R did not recognize it as numeric. This may be due to the use of SAS formats, such as dates, times, or custom formats. If you decide that removing a format is appropriate, then documentation with directions is available at the following link:\n http://support.sas.com/documentation/cdl/en/lrdict/64316/HTML/default/viewer.htm#a000178212.htm")
  } else {
    cat(paste("------------------------------------\n", "Silverman's Critical Bandwidth Test\n", "------------------------------------\n", sep = ""))
    # From the silvermantest package cited in the paper, we concatenated the three main functions necessary to perform
    # the test. We also modified so that k = 1 is the default, and the returned
    # value of the silverman.test is a list containing the p-value and saved seed.
    # The method used in the bootstrap is the one described in Silverman 1981, which is slightly different
    # from the method used in the original silvermantest package
    # Originally obtained from: https://www.mathematik.uni-marburg.de/~stochastik/R_packages/

    silverman.test <-
      function(x,k=1,M=999,adjust=FALSE,digits=6){
        # x: data
        # k: number of modes to be tested
        # M: number of bootstrap replications

        #check if seed is available (as done in boot package)
        #if so save it
        seedAvailable = exists(x=".Random.seed",envir=.GlobalEnv,inherits=FALSE)
        if(seedAvailable)
          saved_seed = .Random.seed
        else{
          rnorm(1)
          saved_seed = .Random.seed
        }

        # temp function for bootstrapping
        y.obs <- function(x,h,sig=sd(x)){
          (x+h*rnorm(length(x),0,1))/((1+h^2/sig^2)^(1/2))
        }

        # temp function for density calculation
        nor.kernel <- function(x,h){
          density(x,bw=h,kernel ="gaussian")$y
        }

        #start of the test
        h0 <- h.crit(x, k)
        n <- 0


        for (i in 1:M) {
          x.boot <- sort(y.obs(sample(x, replace=TRUE),h0))
          mod.temp <- nr.modes(nor.kernel(x.boot,h0))
          if (mod.temp > k){
            n <- n+1
          }
        }

        p <- n/M
        ptemp=p

        if(adjust==TRUE){
          if(k==1){
            cat("Using adjusted p-values. See Hall and York\n\n")
            #asymptotic levels of silvermantest by Hall/York
            x=c(0,0.005,0.010,0.020,0.030,0.040,0.050,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.25,0.30,0.35,0.40,0.50)
            y=c(0,0,0,0.002,0.004,0.006,0.010,0.012,0.016,0.021,0.025,0.032,0.038,0.043,0.050,0.057,0.062,0.07,0.079,0.088,0.094,0.102,0.149,0.202,0.252,0.308,0.423)
            sp = interpSpline(x,y)
            #adjusting the p-value
            if(p<0.005)
              p=0
            else{
              p = predict(sp,p)$y
              p = round(p,digits)
            }

          }
          else{
            cat("The option to adjust the p-value is valid only for k = 1\n\n")
          }
        }

        return(list(saved_seed = saved_seed, p_value = p, k = k, hcrit = h0))
      }

    nr.modes <-
      function(y){

        d1 <- diff(y)
        signs <- diff(d1/abs(d1))
        length(signs[signs==-2])

      }

    h.crit <-
      function(x,k,prec=6){

        #temp function
        nor.kernel <- function(x,h){
          density(x,bw=h,kernel ="gaussian")$y
        }

        digits=prec
        prec=10^(-prec)
        x <- sort(x)
        minh <- min(diff(x))		#minimal possible h
        maxh <- diff(range(x))/2	#maximal possible h
        a <- maxh
        b <- minh

        while (abs(b-a)>prec){
          m <- nr.modes(nor.kernel(x,a))

          b <- a
          if (m > k){
            minh <- a
            a <- (a + maxh)/2
          }
          else {
            maxh <- a
            a <- (a - minh)/2
          }
        }

        a=round(a,digits)


        if(nr.modes( nor.kernel(x,a) ) <= k){
          # subtract until more than k modes
          while(nr.modes( nor.kernel(x,a) ) <= k){
            a = a - prec
          }
          a=a+prec
        }

        if(nr.modes( nor.kernel(x,a) ) > k){
          # add until nr. of modes correct
          while(nr.modes( nor.kernel(x,a) ) > k){
            a = a + prec
          }
        }

        a
      }

    # Load necessary packages. Modified code from: https://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library
    # Splines is necessary only when adjusting p-values, so only install if necessary
    if(adjust) {
      # if package is installed locally, then load
      if("splines" %in% rownames(installed.packages())) {
        do.call("library", list("splines"))
      } else { # if package is not installed locally, then download and load
        cat("Installing splines package.\n\n")
        install.packages("splines")
        do.call("library", list("splines"))
      }
    }
    # Set RNG seed if one was provided
    if(!is.null(setseed)) {
      cat(paste("Seed set in R to:", setseed, "\n\n", sep = " "))
      set.seed(setseed)
    }

    # Perform the test and output results to window
    stresult <- silverman.test(as.matrix(silvdata), k, m, adjust, digits)

    if(stresult$k > 1) {
      cat(paste("Null Hypothesis: ", stresult$k, " or fewer modes", "\n",  sep=""))
      cat(paste("Alternative Hypothesis: more than ", stresult$k, " modes", "\n\n", sep = ""))
    } else {
      cat("Null Hypothesis: Unimodal\n")
      cat("Alternative Hypothesis: Non-unimodal, i.e. at least bimodal\n\n")
    }
    cat(paste("Critical Bandwidth:", stresult$hcrit, "\n", sep = " "))
    cat(paste("p-value:", prettyNum(stresult$p_value, digits), "\n", sep=" "))
    cat(paste("(obtained from", m, "bootstrap replications)\n", sep = " "))


    # Return RNG seed if requested
    if(showseed) {
      cat("\nSeed: \n\n")
      cat(.Random.seed)
    }

    # Create data frame from results to send back to SAS if the out parameter was specified
    resultdf <- data.frame(NullHypothesis = paste("Number of modes <=", stresult$k, sep = " "),  k = stresult$k, PValue = stresult$p_value)
    outseed <- as.data.frame(.Random.seed)
  }
}

# Results from the unadjusted Silverman test
sas_silverman(onenorm$x, setseed = 1234)
sas_silverman(twoNorms1$x, setseed = 1234)
sas_silverman(twoNorms2$x, setseed = 1234)
sas_silverman(threenorms$x, setseed = 1234)

sas_silverman(irisPW$PetalWidth, setseed = 1234)

sas_silverman(twoNorms2$x, setseed = 1234, k = 2)
sas_silverman(threenorms$x, setseed = 1234, k = 2)

sas_silverman(irisPW$PetalWidth, setseed = 1234, k = 2)

sas_silverman(threenorms$x, setseed = 1234 , k = 3)


# Results from the adjusted Silverman test
sas_silverman(onenorm$x, setseed = 1234, adjust = 1)
sas_silverman(twoNorms1$x, setseed = 1234, adjust = 1)
sas_silverman(twoNorms2$x, setseed = 1234, adjust = 1)
sas_silverman(threenorms$x, setseed = 1234, adjust = 1)
sas_silverman(irisPW$PetalWidth, setseed = 1234, adjust = 1)
