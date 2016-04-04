library(rjags)

set.seed(1)

N <- 100
p <- 10
X <- matrix(rnorm(N * p), N, p)
alpha <- -5
beta <- c(10, -5, 0, 0, 1, 4, 20, 15, -12, 0)
y <- alpha + X %*% beta + rnorm(N)

dat_jags <- list(y = y,
                 X = X,
                 p = p,
                 N = N)

jags <- function(file, data, params)
{
  model <- jags.model(file, data = dat_jags)
  samps <- coda.samples(model, params, n.iter = 5000)
  summary(samps)
}

params <- c('theta', 'Ind')
fe <- jags('FixedEffect.txt', dat_jags, params)
re <- jags('RandomEffect.txt', dat_jags, params)
kmfe <- jags('KaoMallickFE.txt', dat_jags, params)
kmre <- jags('KaoMallickRE.txt', dat_jags, params)
gvsfe <- jags('GVSFE.txt', dat_jags, params)
gvsre <- jags('GVSRE.txt', dat_jags, params)
ssvsfe <- jags('SSVSFE.txt', dat_jags, params)
ssvsre <- jags('SSVSRE.txt', dat_jags, params)
jeff <- jags('Jeffreys.txt', dat_jags, params)
lapfe <- jags('LaplacianFE.txt', dat_jags, params)
lapre <- jags('LaplacianRE.txt', dat_jags, params)

library(R2WinBUGS)

inits <- list(list(tau = 1))
rjmcmcfe <- bugs(dat_jags, inits, n.chains = 1,
                 parameters.to.save = 'k',
                 model.file = 'RJMCMCFE.txt', 
                 bugs.directory = 'C:/WinBUGS14')
