library(here)
library(rcarbon)
library(nimbleCarbon)
DK <- read.csv(here('data','DK.csv'))
DK.caldates=calibrate(x=DK$C14Age,errors=DK$C14SD,calCurves='intcal20',verbose=FALSE) #calibration

a = 6500
b = 4500
r = 0.002
t = 0:(a-b)
pi = ((1+r)^t)/(sum((1+r)^t))

set.seed(123)
n = 300
calendar.dates = sample(a:b,size=n,prob=pi)
cra = round(uncalibrate(calendar.dates)$ccCRA) #back-calibrate in 14C ages
cra.error = rep(20,n) #assign error of 20 years

calibrated.dates = calibrate(cra,cra.error,verbose=FALSE)
summed.prob = spd(calibrated.dates,timeRange=c(6500,4500),verbose = FALSE)
summed.prob.grid = summed.prob$grid
fit <- nls(PrDens ~ exp(a + b * calBP), data = summed.prob.grid, start = list(a = 0, b = 0))
-coefficients(fit)[2] #Estimated growth
-confint(fit,level=0.95)[2,] #95% CI

data("intcal20") #load the IntCal20 calibration curve, Remier et al 2020
constants <- list(N=n,calBP=intcal20$CalBP,C14BP=intcal20$C14Age,C14err=intcal20$C14Age.sigma,start=a,end=b)

data <- list(X=cra,sigma=cra.error)
m.dates = medCal(calibrate(cra,cra.error,verbose = FALSE)) #compute the median calibrated for an initial estimate of theta
if(any(m.dates>a|m.dates<b)){m.dates[m.dates>a]=a;m.dates[m.dates<b]=b} #ensure that theta is within the time range of analysis
inits <- list(r=0.0004,theta=m.dates)
inits.function = function() list(r=rexp(1,1/0.0004),theta=m.dates)

library(parallel)
ncores <- 2
cl <- makeCluster(ncores)

# Generate a single wrapper function:
runFun <- function(seed, data, constants, m.dates) {
  library(nimbleCarbon)
  inits.function = function() list(r=rexp(1,1/0.0004),theta=m.dates)

  model <- nimbleCode({
    for (i in 1:N){
      theta[i] ~ dExponentialGrowth(a=start,b=end,r=r);
      mu[i] <- interpLin(z=theta[i], x=calBP[], y=C14BP[]);
      sigmaCurve[i] <- interpLin(z=theta[i], x=calBP[], y=C14err[]);
      sd[i] <- (sigma[i]^2+sigmaCurve[i]^2)^(1/2);
      X[i] ~ dnorm(mean=mu[i],sd=sd[i]);
    }
    r ~ dexp(1/0.0004);
  })
    results<- nimbleMCMC(code = model,constants = constants,data = data,niter = 10000, nchains = 1, thin=1, nburnin = 2000, progressBar = FALSE, monitors=c('r','theta'), inits=inits.function, samplesAsCodaMCMC=TRUE,setSeed=seed)

  return(results)
}

# Run the model in parallel:
seeds = c(123,456)
chain_output = parLapply(cl = cl, X = seeds, fun = runFun, data = data, constants=constants,m.dates=m.dates)
stopCluster(cl)

# Convert into a mcmc.list object for diagnostic (see below)
chain_output=coda::mcmc.list(chain_output)

save(chain_output,file=here('results','output.RData'))

