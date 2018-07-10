## Import libraries
library(knitr)
library(readr)
library(rjags)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

## Import data
cats <- read_csv("~/Desktop/cats.csv")
cats.df <- data.frame(cats)

## Set the seed
set.seed(464)

## Initialize variables for JAGS
Y <- cats.df$DeltaPCV
breed <- as.numeric(as.factor(cats.df$Breed))
domestic <- c(1,1,1,0,0)
nBreeds <- 5
nCats <- length(Y)

## Create objects for JAGS
dataList <- list("Y" = Y,
                 "Dose" = catsData$Dose,
                 "Type" = catsData$Type,
                 "Domestic" = domestic, 
                 "breed" = breed,
                 "nBreeds" = nBreeds,
                 "nCats" = nCats)

## List of parameters to be monitored  
parameters <- c("beta", "gamma", "sigma2_prec", "tau2_alpha_prec")

## Set initial values
initsValues <- list("beta" = rep(0, 3),
                    "gamma" = 0,
                    "sigma2_prec" = 1,
                    "tau2_alpha_prec" = 1)

## Number of iteration for "tuning" 
adaptSteps <- 5000 

## Number of iterations for "burn-in" 
burnInSteps <- 5000   

## Number of chains to run
nChains <- 2          

## Total number of iterations to save
numSavedSteps <- 5000           

## Thinning to keep every iteration
thinSteps <- 1                  

## Iterations per chain
ITER <- ceiling((numSavedSteps * thinSteps) / nChains)

## Fitting the JAGS model
jagsModel <- jags.model("catsmodel.txt", 
                        data = dataList, 
                        inits = initsValues, 
                        n.chains = nChains, 
                        n.adapt = adaptSteps)

## Burn-in the algorithm
update(jagsModel, n.iter = burnInSteps)

## Run algorithm to get interations for inference
codaSamples <- coda.samples(jagsModel, 
                            variable.names = parameters, 
                            n.iter = ITER, 
                            thin = thinSteps)

## Make a dataframe with the posterior samples
mcmcChainDF <- data.frame(as.matrix(codaSamples, 
                                    iters = T, 
                                    chains = T))

## Create a vector with the variable names
varNames <- names(mcmcChainDF)[3:(dim(mcmcChainDF)[2])]

## Number of variables
nVars <- length(varNames)
mcmcChainDF$CHAIN <- as.factor(mcmcChainDF$CHAIN)

## Construct trace plots
p <- list()
for(k in 1:nVars) {
  plot_frame <- mcmcChainDF
  plot_frame$dep_var <- mcmcChainDF[,varNames[k]]
  p[[k]] <- ggplot(plot_frame, aes(x = ITER, y = dep_var)) +
    geom_line(aes(color = CHAIN)) + 
    labs(y = varNames[k])
}

## Trace plots
do.call(grid.arrange, c(p, list("ncol" = 1)))