
##################################################################################################################################
##  Function MM_Estimation  : Takes Sample data and Distribution name as input                                                  ##
##                            and prints out the estimated parameters values (empirical)                                        ##  
##                            based on Methods of Moments.                                                                      ##
##                                                                                                                              ##
##               ASSIGNMENT SUBMISSION BY NITIN REDDY KAROLLA                                                                   ##
##################################################################################################################################

library(MASS)

MM_Estimator <- function( data.sample, distribution){
  
  if (distribution == "Point Mass"){
    #######  Point Mass #######
    param <- 1
    #data.sample <- 10
    m1 <- data.sample/1
    print(paste("Distribution - Point Mass(a) ; Moment1 (m1) = ", m1, "; Parameter1 (a) = ", m1 ))
  }
  
  else if (distribution == "Bernoulli"){
    ####### Bernoulli #######
    param <- 1
    #data.sample <- bern.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    print(paste("Distribution - Bernoulli Distribution(p) ; Moment1 (m1) = ", m1, "; Estimated Parameter1 (p) = ", m1 ))
  }
  
  else if (distribution == "Binomial"){
    ####### Binomial #######
    param <- 2
    #data.sample <- binom.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    m2 <- 1/length(data.sample) * sum(data.sample^2)
    # We have np = m1 and np(1-p) = m2-m1^2
    p = 1 - ((m2 - (m1^2))/m1)
    n = m1/p
    print(paste("Distribution - Binomial Distribution(n,p) ; Moment1 (m1) = ", m1, "; Moment2 (m2) = ", m2,"; Estimated Parameter1 (n) = ", n
                , "; Estimated Parameter2 (p) = ", p))
  }
  
  else if (distribution == "Geometric"){
    ####### Geometric #######
    param <- 1
    #data.sample <- geom.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    #we have p = 1/mean
    p = 1/m1
    print(paste("Distribution - Geometric Distribution (p) ; Moment1 (m1) = ", m1, "; Estimated Parameter1 (p) = ", p ))
  }
  
  else if (distribution == "Poisson"){
    ####### Poisson #######
    param <- 1
    #data.sample <- pois.data
    m1 <- 1/length(data.sample) * sum(data.sample)#we have p = 1/mean
    lambda = m1
    print(paste("Distribution - Poisson Distribution (lambda) ; Moment1 (m1) = ", m1, "; Estimated Parameter1 (lambda) = ", lambda ))
  }
  
  else if (distribution == "Uniform"){
    ####### Uniform #######
    param <- 2
    #data.sample <- unif.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    m2 <- 1/length(data.sample) * sum(data.sample^2)
    a <- m1 - sqrt(3* (m2 - m1^2))
    b <- m1 + sqrt(3* (m2 - m1^2))
    print(paste("Distribution - Uniform Distribution(a,b) ; Moment1 (m1) = ", m1, "; Moment2 (m2) = ", m2,"; Estimated Parameter1 (a) = ", a
                , "; Estimated Parameter2 (b) = ", b))
  }
  
  else if (distribution == "Normal"){
    ####### Normal #######
    param <- 2
    #data.sample <- norm.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    m2 <- 1/length(data.sample) * sum(data.sample^2)
    mu <- m1
    sig <- sqrt(m2 - m1^2)
    print(paste("Distribution - Normal Distribution(mu,sigma) ; Moment1 (m1) = ", m1, "; Moment2 (m2) = ", m2,"; Estimated Parameter1 (mu) = ", mu
                , "; Estimated Parameter2 (sigma) = ", sig))
  }
  
  else if (distribution == "Exponential"){
    ####### Exponential #######
    param <- 1
    #data.sample <- expo.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    #we have p = 1/mean
    beta <- m1
    print(paste("Distribution - Exponential Distribution (beta) ; Moment1 (m1) = ", m1, "; Estimated Parameter1 (beta) = ", beta ))
  }
  
  else if (distribution == "Gamma"){
    ####### Gamma #######
    param <- 2
    #data.sample <- gamma.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    m2 <- 1/length(data.sample) * sum(data.sample^2)
    beta <- (m2 - m1^2)/m1
    alpha <- m1/ beta
    print(paste("Distribution - Gamma Distribution(alpha, beta) ; Moment1 (m1) = ", m1, "; Moment2 (m2) = ", m2,
                "; Estimated Parameter1 (alpha) = ", alpha
                , "; Estimated Parameter2 (beta) = ", beta))
  }
  
  else if (distribution == "Beta"){
    ####### Beta #######
    param <- 2
    #data.sample <- beta.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    m2 <- 1/length(data.sample) * sum(data.sample^2)
    alpha <- (m1^2 - (m1*m2))/(m2 - m1^2)
    beta <- (alpha/ m1) - alpha
    print(paste("Distribution - Beta Distribution(alpha, beta) ; Moment1 (m1) = ", m1, "; Moment2 (m2) = ", m2,
                "; Estimated Parameter1 (alpha) = ", alpha
                , "; Estimated Parameter2 (beta) = ", beta))
  }
  
  else if (distribution == "Student T"){
    ####### Student T #######
    param <- 1
    #data.sample <- studt.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    m2 <- 1/length(data.sample) * sum(data.sample^2)
    #we have p = 1/mean
    v <- 2*(m2 - m1^2)/(m2- m1^2 - 1)
    print(paste("Distribution - Student T (v) ; Moment1 (m1) = ", m1,"; Moment2 (m2) = ", m2, "; Estimated Parameter1 (v) = ", v, "(NOTE : This
                v value is from Second moment of Student T test, however, 2nd moment wont exist based on 
                assumptions of Methods of moments as Student T test has only 1 parameter" ))
  }
  
  else if (distribution == "Chi-Square"){
    ####### Chi - Square #######
    param <-1
    #data.sample <- chi.sqaure.data
    m1 <- 1/length(data.sample) * sum(data.sample)
    p <- m1
    print(paste("Distribution - Chi-sqaure (p) ; Moment1 (m1) = ", m1, "; Estimated Parameter1 (p) = ", p ))
  }
  
  else if (distribution == "Multinomial"){
    ####### Multinomial #######
 #   data.sample <- multi.nom.data
    m1 <- rowSums(data.sample)/ncol(data.sample)
    m2 <- data.sample %*% t(data.sample)/ncol(data.sample)
    #calculation just p1 to find n
    p1 <- 1 + m1[1] - (m2[1,1]/m1[1])
    n <- m1[1]/p1
    p <- m1/n
    cat("Distribution - Multinominal Distribution(n, prob) ; ",  "Estimated Parameter1 (n) = ", n, "; Estimated Parameter2 (prob) = ", p)
  }
  
  else if (distribution == "Multivariate Normal"){
    ####### Multivariate Normal #######
    #data.sample <- mv.norm.data
    m1 <- rowSums(data.sample)/ncol(data.sample)
    m2 <- data.sample %*% t(data.sample)/ncol(data.sample)
    sd <- sqrt(m2 - m1 %*% t(m1))
    cat("Distribution - Multivariate Normal Distribution(mu, sigma) ; ",  "Estimated Parameter1 (mu) = ", m1, "; Estimated Parameter2 (sigma) = ", sd)
  }
}


############ Data Generation for Distributions and Testing on Sample Data #####################

# Point Mass
point.mass.data <- 14
MM_Estimator(point.mass.data, "Point Mass")

# Bernoulli
bern.data = sample(c(0,1), replace=TRUE, size= 100)
MM_Estimator(bern.data, "Bernoulli")

# Binomial 
binom.data <- rbinom(n = 1000 , size= 10, prob = 0.6)
MM_Estimator(binom.data, "Binomial")

# Geometric
geom.data <- rgeom(1000,0.5)
MM_Estimator(geom.data, "Geometric")

# Poisson
pois.data <- rpois(10000, 10)
MM_Estimator(pois.data, "Poisson")

# Uniform
unif.data <- runif(1000, 10, 14)
MM_Estimator(unif.data, "Uniform")

# Normal
norm.data <- rnorm(1000, 20, 3)
MM_Estimator(norm.data, "Normal")

# Exponential
expo.data <- rexp(1000, 1.7)
MM_Estimator(expo.data, "Exponential")

# Gamma
expo.data <- rgamma(1000, shape = 10, scale = 20)
MM_Estimator(expo.data, "Gamma")

# Beta
beta.data <- rbeta(1000, shape1 = 10, shape2 = 20)
MM_Estimator(beta.data, "Beta")

# Student T (using 2nd moment to calculate the df)
studt.data <- rt(1000, 20)
MM_Estimator(studt.data, "Student T") #NOTE :This v value is from Second moment of Student T test, however, 
                                      # 2nd moment wont exist based on assumptions of Methods of moments as 
                                      # Student T test has only 1 parameter

# Chi-Square
chi.sqaure.data <- rchisq(1000, 12)
MM_Estimator(chi.sqaure.data, "Chi-Square")

# Multinomial
multi.nom.data <- rmultinom(1000, 9, c(0.2,0.3,0.1,0.4))
MM_Estimator(multi.nom.data, "Multinomial")

# Mutivariate Normal
mv.norm.data <- t(mvrnorm(1000, mu = c(5,10,15), Sigma = matrix(c(2,0.5,0.1,0.5,7,0.3,0.1,0.5,3), nrow = 3) ))
MM_Estimator(mv.norm.data, "Multivariate Normal")
