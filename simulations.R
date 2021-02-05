library(ggplot2)
library(mvtnorm)

#Simulation of a response interacted with a one kink linear spline covariate 

covariate_range <- seq(0,40)
kink <- 20


# First representation : a unique spline covariate term

# we have two gammas : 
# - one for the non interacted weather term
# - one for the weather*covariatespline with 

## Set up a random set of parameters for gammas
mu <- rnorm(2) #mean
vcv <- rWishart(1, 20, diag(c(.1, .001, .001), 2, 2))[,,1] #VCV

## Generate Monte Carlo draws of gammas
gamma.hat <- rmvnorm(1000, mu, vcv)

## Compute the response : pass covariate values to gammas (we assume weather value is equal to one)
# first column : vector of 1 / second column : covariatespline
# covariatespline == ifelse(covariate<=kink, covariate, kink)
covariate_values <- cbind(1, ifelse(covariate_range <= kink, covariate_range, kink))

## Compute quantiles over draws
yy <- covariate_values %*% t(gamma.hat)
quants <- apply(yy, 1, function(y) quantile(y, c(.025, .5, .975)))

## Plot it : we obtain a flat response after the kink
ggplot(data.frame(covariate_range, yy=quants[2,], cihi=quants[1,], cilo=quants[3,]), aes(covariate_range, yy)) +
    geom_line() + geom_ribbon(aes(ymin=cilo, ymax=cihi), alpha=.5) + theme_bw() +
    scale_x_continuous(expand=c(0, 0))


# Second representation : two terms for the covariate.

# we have two gammas : 
# - one for the non interacted weather term
# - one for weather*covariate
# - one for weather*covariatespline (different definition of covariatespline)

mu2 <- c(mu, -mu[2]) #add the third gamma, which is minus the second gamma
#construct the VCV based on mathematical formula of covariance
#Cov(X,-X)=-V(X) and V(-X)=V(X)
vcv2 <- rbind(cbind(vcv, -vcv[,2]),
                      c(-vcv[,2], vcv[2,2])) 

## Generate Monte Carlo draws of new set of gammas
gamma.hat2 <- rmvnorm(1000, mu2, vcv2)


## Compute the response : pass covariate values to gammas (we assume weather value is equal to one)
# first column : vector of 1 / second column : covariate values / third column : covariatespline
# covariatespline == kink-covariate if covariate>kink, 0 otherwise
covariate_values2 <- cbind(1, covariate_range, ifelse(covariate_range<=kink, 0, covariate_range-kink))


## Compute quantiles over draws
yy2 <- covariate_values2 %*% t(gamma.hat2)
quants2 <- apply(yy2, 1, function(y) quantile(y, c(.025, .5, .975)))

## Plot the comparison
ggplot(rbind(data.frame(covariate_range, yy=quants[2,], cihi=quants[1,], cilo=quants[3,], panel='original'),
             data.frame(covariate_range, yy=quants2[2,], cihi=quants2[1,], cilo=quants2[3,], panel='transformed')),
       aes(covariate_range, yy, linetype=panel)) +
    geom_line() + geom_ribbon(aes(ymin=cilo, ymax=cihi, color=panel), alpha=.5) + theme_bw() +
    scale_x_continuous(expand=c(0, 0))



# Third representation : second representation with coupled variances : Cov(X,-X)=1 and V(-X)=V(X)


vcv3 <- rbind(cbind(vcv, c(-vcv[1,2], -1)),
              c(c(-vcv[1,2], -1), vcv[2,2])) 
gamma.hat3 <- rmvnorm(1000, mu2, vcv3)
yy3 <- covariate_values2 %*% t(gamma.hat3)
quants3 <- apply(yy3, 1, function(y) quantile(y, c(.025, .5, .975)))

## Plot the comparison with representation 1 and representation 2
ggplot(rbind(data.frame(covariate_range, yy=quants[2,], cihi=quants[1,], cilo=quants[3,], panel='w/0 transformed'),
             data.frame(covariate_range, yy=quants2[2,], cihi=quants2[1,], cilo=quants2[3,], panel='w transformed, unconstrained covar'),
             data.frame(covariate_range, yy=quants3[2,], cihi=quants3[1,], cilo=quants3[3,], panel='w transformed, constrained covar')),
       aes(covariate_range, yy, linetype=panel, color=panel)) +
    geom_line() + geom_ribbon(aes(ymin=cilo, ymax=cihi, color=panel), alpha=.3) + theme_bw() +
    scale_x_continuous(expand=c(0, 0)) + theme(legend.position = "bottom")