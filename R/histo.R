library(VASIDEA)
#VASIDEA::run_app()
#?`dearseq-package`
number of runs
nsims <- 2 #100
res <- numeric(nsims)
for(i in 1:nsims){
  n <- 1000 #number of genes
  nr=5 #number of measurements per subject (grouped data)
  ni=50 #number of subjects
  r <- nr*ni #number of measurements
  t <- matrix(rep(1:nr), ni, ncol=1, nrow=r) # the variable to be tested
  sigma <- 0.5
  b0 <- 1
  #under the null:
  b1 <- 0
  #create the matrix of gene expression
  y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
  y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
           matrix(rep(y.tilde, n), ncol=n, nrow=r))
  #no covariates
  x <- matrix(1, ncol=1, nrow=r)
  #run test
  #asymptotic test with preprocessed grouped data
  res_genes <- dear_seq(exprmat=y, covariates=x, variables2test=t,
                        sample_group=rep(1:ni, each=nr),
                        which_test='asymptotic',
                        which_weights='none', preprocessed=TRUE)
  #proportion of raw p-values>0.05
  mean(res_genes$pvals[, 'rawPval']>0.05)
  #quantiles of raw p-values
  quantile(res_genes$pvals[, 'rawPval'])
  #proportion of raw p-values<0.05 i.e. proportion of DE genes
  res[i] <- mean(res_genes$pvals[, 'rawPval']<0.05)
  message(i)
}
#library(VASIDEA)
#VASIDEA::run_app()
#library(VASIDEA)
#VASIDEA::run_app()
#VASIDEA::run_app()
#VASIDEA::run_app()
#library(VASIDEA)
devtools::load_all(".")
#library(VASIDEA)
#VASIDEA::run_app()
#library(VASIDEA)
#library(VASIDEA)
#VASIDEA::run_app()
#devtools::load_all(".")
#VASIDEA::run_app()
#library(VASIDEA)
#VASIDEA::run_app()
#?dear_seq
#VASIDEA::run_app()
res[[1]]
res
res_genes
summary(res_genes)
plot(res_genes)
a <- plot(res_genes)
class(a)
temp <- summary(res_genes)
temp$which_signif
temp$adj_pval
#dear_seq
#dgsa_seq

