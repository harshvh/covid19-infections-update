################################################################################
# Tracking the progress of the COVID-19 pandemic
################################################################################

# Prior Functions #
# India
# Wave 2 - 01.07.2020 to 31.10.2020

################################################################################
source(paste0(here::here(), "/0-config.R"))

# Set number to be sampled from each prior
nsamp <- 1e5
#---------------------------------------
# Define prior distributions
#---------------------------------------

# p1 := P(S|tested)
samp_p1 <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.7,b = 1,
                    shape1 = find_beta_shape_params(mu = 0.9, sd = (0.1)^2)$a,
                    shape2 = find_beta_shape_params(mu = 0.9, sd = (0.1)^2)$b)
}

# p2 := P(S|untested)
samp_p2 <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.001,b = 0.13,
                    shape1 = find_beta_shape_params(mu = 0.05, sd = (0.1)^2)$a,
                    shape2 = find_beta_shape_params(mu = 0.05, sd = (0.1)^2)$b)
}

# Z_s := adjustment factor for P(test +|S) among untested (alpha)
samp_zs <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.4,b = 0.7,
                    shape1 = find_beta_shape_params(mu = 0.475, sd = (0.1)^2)$a,
                    shape2 = find_beta_shape_params(mu = 0.475, sd = (0.1)^2)$b)
}

# Z_a := adjustment factor for P(test +|A) among untested (beta)
samp_za <- function(n){
  truncdist::rtrunc(n = n,spec = "beta",a = 0.025,b = 0.4,
                    shape1 = find_beta_shape_params(mu = 0.125, sd = (0.1)^2)$a,
                    shape2 = find_beta_shape_params(mu = 0.125, sd = (0.1)^2)$b)
}

# Sample the prior
sample_prior <- function(n){
  stopifnot(n>0)
  out <- matrix(data = NaN,nrow = n,ncol = 4,dimnames = list(NULL,c("p1","p2","zs","za")))
  out[,"p1"] <- samp_p1(n)
  out[,"p2"] <- samp_p2(n)
  out[,"zs"] <- samp_zs(n)
  out[,"za"] <- samp_za(n)
  return(out)
}
set.seed(123)
theta_samp <- sample_prior(n = nsamp)
theta_samp = as.data.frame(theta_samp)

#---------------------------------------
# Adding Se and Sp to each prior
#---------------------------------------
set.seed(123)
# distribution of sensitivity of test
dist_Se = truncdist::rtrunc(n = 100000,spec = "beta",a = 0.82,b = 1,
                            shape1 = find_beta_shape_params(mu = 0.975, sd = (0.1)^2)$a,
                            shape2 = find_beta_shape_params(mu = 0.975, sd = (0.1)^2)$b)

# distribution of specificity of test
dist_Sp = truncdist::rtrunc(n = 100000,spec = "beta",a = 0.9998,b = 1,
                            shape1 = find_beta_shape_params(mu = 0.99995, sd = (0.01)^2)$a,
                            shape2 = find_beta_shape_params(mu = 0.99995, sd = (0.01)^2)$b)

# Adding the above priors to the set of sampled priors
process_priors = function(priors, Se, Sp){
  
  simdata = as.data.frame(priors) %>%
    rename(
      P_S_tested = "P(S|tested)",
      P_S_untested = "P(S|untested)"
    ) %>%
    mutate(dist_Se = Se,
           dist_Sp = Sp)
  
  simdata$P_A_testpos = est_P_A_testpos(
    simdata$P_S_untested,
    simdata$Z_A,
    simdata$Z_S
  )
  
  return(simdata)
  
}

##############################################
# PDF of P(A|test+)
##############################################
p0 <- function(x){
  truncdist::dtrunc(x = x,spec = "beta",a = 0.15,b = 0.85,
                    shape1 = find_beta_shape_params(mu = 0.5, sd = (0.1)^2)$a,
                    shape2 = find_beta_shape_params(mu = 0.5, sd = (0.1)^2)$b)
}

#