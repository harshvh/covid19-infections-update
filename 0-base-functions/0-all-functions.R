################################################################################
# Tracking the progression of COVID-19
################################################################################
## Modeling the pandemic - base functions, bias correction functions, and 
## prior functions
################################################################################

##BASE FUNCTIONS

###############################################
# Documentation:  find_beta_shape_params
# Description:    Calculate a and b shape parameters for a beta 
#                 distribution from mean and standard deviation
#
# Returns: a list with elements a and b that include shape parameters
##############################################
find_beta_shape_params = function(mu, sd){
  var = sd ^ 2
  a = ((1 - mu) / var - 1 / mu) * mu ^ 2
  b = a * (1 / mu - 1)
  return (list(a = a, b = b))
}

##############################################
# Documentation:  gg_color_hue
# Description:    create default ggplot color palette
#                 for a given number of unique values
#
# Returns: color palette
##############################################
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

##############################################
# Documentation:  load_world_data
# Description:    read in data from COVID Tracking Project,
#                 merge in Census 2019 population projections,
#                 filter on date range
# Returns:        data from COVID Tracking Project restricted
#                 to dates of interest with state populations 
##############################################
load_world_data = function(country, min_date, max_date) {
  
  covid_world = read.csv(paste0(here::here(),"/1-data/covid-tracking-data/owid-world.csv"))
  
  covid_world <- covid_world %>% mutate(date = as.Date(date)) %>%
    filter(date >= as.Date(min_date) & date <= as.Date(max_date)) %>%
    filter(location==country)
           #| country=="United States" | country=="United Kingdom" | country=="Mexico") 
      
  covid_world
  
}

##############################################
# Documentation:  load_global_data                            --> rename to load_country_data
# Description:    read in data from COVID Tracking Project,
#                 merge in Census 2019 population projections,
#                 filter on date range
# Returns:        data from COVID Tracking Project restricted
#                 to dates of interest with state populations 
##############################################
load_global_data = function(country, min_date, max_date) {
  
  covid_world = read.csv(paste0(here::here(),"/1-data/covid-tracking-data/owid-world.csv"))
  
  covid_world <- covid_world %>% mutate(date = as.Date(date)) %>%
    filter(date >= as.Date(min_date) & date <= as.Date(max_date)) %>%
    filter(location == "India" | location=="United States" | location=="United Kingdom" | location=="Mexico") 
  
  covid_world
  
}
################################################################################

##BIAS CORRECTION FUNCTIONS

##############################################
# Documentation:  calc_A_star
# Usage:          calc_A_star(N, N_tested, N_pos_obs, P_testpos_est, P_S_tested,
#                 P_S_untested, P_A_testpos, Z_S, Z_A, Se, Sp)
# Description:    estimate the number of infections correcting for
#                 incomplete testing and imperfect test accuracy
#
#
# Args/Options:
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# P_S_tested:     prior value for P(S|tested)
# P_S_untested:   prior value for P(S|untested)
# P_A_testpos:    prior value for P(A|test +)
# Z_S:            prior value for alpha range
# Z_A:            prior value for beta range
# Se:             prior value for sensitivity
# Sp:             prior value for specificity

# Returns:        the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy, as a scalar
##############################################
calc_A_star = function(N, N_tested, N_pos_obs, est_cases, P_testpos_est, P_S_tested, P_S_untested, P_A_testpos, Z_S, Z_A, Se, Sp){
  
  #N_susceptible = N - N_pos_obs
  N_susceptible = N - est_cases
  
  #----- NS, NA among tested ----------------------
  #Number of individuals with severe/mild symptoms among those already tested positive
  Npos_tested_S = N_pos_obs * (1 - P_A_testpos)
  Npos_tested_A = N_pos_obs - Npos_tested_S
  
  #----- prob testpos among untested ----------------------
  P_testpos_S = P_testpos_est * Z_S
  P_testpos_A = P_testpos_est * Z_A
  
  # estimate number of positives among susceptible
  Npos_untested_S = P_S_untested * N_susceptible * P_testpos_S
  Npos_untested_A = (1 - P_S_untested) * N_susceptible * P_testpos_A
  
  
  A_star = Npos_tested_S   + Npos_tested_A +
    Npos_untested_S + Npos_untested_A
  
  # correct for imperfect sensitivity and specificity
  A = (A_star - ((1 - Sp) * N)) / (Se + Sp - 1)
  
  return(A)
  
}


##############################################
# Documentation:  correct_bias
# Usage:          correct_bias(N, N_tested, N_pos_obs, P_testpos_est, distributions)
# Description:    perform probabilistic bias analysis to estimate the
#                 number infections correcting for incomplete testing and
#                 imperfect test accuracy
#
# Args/Options:
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# distributions:  data frame with prior distributions for P_S_tested, P_S_untested,
#                 P_A_testpos, Z_S, Z_A, Se, Sp

# Returns:        data frame with the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy
##############################################
correct_bias = function(N, N_tested, N_pos_obs, est_cases, P_testpos_est, distributions){
  
  cat(".")

  # sample index to draw from distribution
  sample_ind = sample(1:nrow(distributions), size = 1, replace=TRUE)
  
  # randomly sample from each distribution
  samples = distributions[sample_ind,]
  
  # corrected case count
  Astar = calc_A_star(N = N,
                      N_tested = N_tested,
                      N_pos_obs = N_pos_obs,
                      est_cases = est_cases,
                      P_testpos_est = P_testpos_est,
                      P_S_tested = samples[which(names(samples) == "P_S_tested")],
                      P_S_untested = samples[which(names(samples) == "P_S_untested")],
                      P_A_testpos = samples[which(names(samples) == "P_A_testpos")],
                      Z_S = samples[which(names(samples) == "Z_S")],
                      Z_A = samples[which(names(samples) == "Z_A")],
                      Se = samples[which(names(samples) == "dist_Se")],
                      Sp = samples[which(names(samples) == "dist_Sp")]
  )
  
  names(Astar) = "exp_cases"
  
  out = data.frame(
    Astar = Astar,
    N = N,
    N_tested = N_tested,
    N_pos = N_pos_obs,
    est_cases = est_cases,
    P_S_tested = samples[which(names(samples) == "P_S_tested")],
    P_S_untested = samples[which(names(samples) == "P_S_untested")],
    Z_S = samples[which(names(samples) == "Z_S")],
    Z_A = samples[which(names(samples) == "Z_A")],
    P_A_testpos = samples[which(names(samples) == "P_A_testpos")],
    P_testpos_S = samples[which(names(samples) == "P_testpos_S")],
    P_testpos_A = samples[which(names(samples) == "P_testpos_A")],
    Se = samples[which(names(samples) == "dist_Se")],
    Sp = samples[which(names(samples) == "dist_Sp")]
  )
  
  return(out)
}

##############################################
# Documentation:  generate_corrected_sample
# Usage:          generate_corrected_sample(N, N_tested, N_pos_obs, distribution_list,
#                 num_reps, state, select_state=NULL)
# Description:    wrapper for correct_bias that subsets a list of prior distributions
#                 to those for a particular state, performs probabilistic bias
#                 analysis for that state, and formats output
#
# Args/Options:
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# distributions:  data frame with prior distributions for P_S_tested, P_S_untested,
#                 P_A_testpos, Z_S, Z_A, Se, Sp

# Returns:        data frame with the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy and prior values
##############################################
generate_corrected_sample = function(N, N_tested, N_pos_obs, est_cases, distribution_list,
                                     num_reps, select_state){
  
  #----------------------------------------
  # Obtain corrected case estimates
  #----------------------------------------
  reps = num_reps
  
  # subset to prior distributions for a given state
  distributions = distribution_list[which(names(distribution_list) == select_state)][[1]]
  
  set.seed(123)
  
  # perform probabilistic bias analysis
  result = replicate(reps, correct_bias(
    N = N,
    N_tested = N_tested,
    N_pos_obs = N_pos_obs,
    est_cases = est_cases,
    P_testpos_est = mean(distributions$est_testpos),
    distributions = distributions
  ))
  
  # format output of probabilistic bias analysis
  result_long = as.data.frame(matrix(result, nrow=reps, byrow=TRUE))
  colnames(result_long) = c(
    "exp_cases", "N", "N_tested", "N_pos", "est_cases",
    "P_S_tested", "P_S_untested",
    "Z_S", "Z_A", "P_A_testpos",
    "P_testpos_S", "P_testpos_A", "Se", "Sp"
  )
  
  for(i in 1:ncol(result_long)){
    result_long[,i] = unlist(result_long[,i])
  }
  
  return(result_long)
}

################################################################################

## BIAS CORRECTION FUNCTONS FOR UNDERTESTING

################################################################################
# Documentation:  calc_A_star_undertesting
# Usage:          calc_A_star_undertesting(N, N_tested, N_pos_obs, P_testpos_est, P_S_tested,  
#                 P_S_untested, P_A_testpos, Z_S, Z_A, Se, Sp)
# Description:    estimate the number infections correcting for
#                 incomplete testing and imperfect test accuracy and
#                 the proportion of cases under-estimated due to
#                 incomplete testing vs. imperfect test accuracy
#
# Args/Options:   
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# P_S_tested:     prior value for P(S|tested)
# P_S_untested:   prior value for P(S|untested)
# P_A_testpos:    prior value for P(A|test +)
# Z_S:            prior value for alpha range
# Z_A:            prior value for beta range
# Se:             prior value for sensitivity
# Sp:             prior value for specificity

# Returns:        the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy (A), 
#                 the number of estimated infections correcting for
#                 incomplete testing assuming perfect test accuracy (A_star), 
#                 the percentage of under-estimation attributable to 
#                 imperfect test accuracy (percent_acc), and the percentage of
#                 under-estimation due to incomplete testing (percent_und), as a list
##############################################
calc_A_star_undertesting = function(N, N_tested, N_pos_obs, est_cases, P_testpos_est, P_S_tested, P_S_untested, P_A_testpos, Z_S, Z_A, Se, Sp){
  
  N_susceptible = N - est_cases
  
  #----- NS, NA among tested ----------------------
  Npos_tested_S = N_pos_obs * (1 - P_A_testpos)
  Npos_tested_A = N_pos_obs - Npos_tested_S
  
  #----- prob testpos among untested ----------------------
  P_testpos_S = P_testpos_est * Z_S
  P_testpos_A = P_testpos_est * Z_A
  
  # estimate number of positives among susceptible
  Npos_untested_S = P_S_untested * N_susceptible * P_testpos_S 
  Npos_untested_A = (1 - P_S_untested) * N_susceptible * P_testpos_A
  
  A_star = Npos_tested_S   + Npos_tested_A +
    Npos_untested_S + Npos_untested_A
  
  # correct for imperfect sensitivity and specificity
  A = (A_star - ((1 - Sp) * N)) / (Se + Sp - 1)
  
  missed_infections = A - N_pos_obs
  percent_acc = (A - A_star) / missed_infections
  percent_und = 1 - percent_acc
  
  return(list(
    A = A,
    A_star = A_star,
    percent_acc = percent_acc,
    percent_und = percent_und))
  
}

##############################################
# Documentation:  correct_bias_undertesting
# Usage:          correct_bias_undertesting(N, N_tested, N_pos_obs, P_testpos_est, distributions)
# Description:    perform probabilistic bias analysis to estimate the 
#                 number infections correcting for incomplete testing and 
#                 imperfect test accuracy    
#
# Args/Options:   
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# distributions:  data frame with prior distributions for P_S_tested, P_S_untested, 
#                 P_A_testpos, Z_S, Z_A, Se, Sp

# Returns:        data frame with the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy (exp_cases), 
#                 the number of estimated infections correcting for
#                 incomplete testing assuming perfect test accuracy (exp_cases_perfSeSp), 
#                 the percentage of under-estimation attributable to 
#                 imperfect test accuracy (percent_acc), and the percentage of
#                 under-estimation due to incomplete testing (percent_und) and prior distributions
##############################################
correct_bias_undertesting = function(N, N_tested, N_pos_obs, est_cases, P_testpos_est, distributions){
  
  cat(".")
  
  # sample index to draw from distribution
  sample_ind = sample(1:nrow(distributions), size = 1, replace=TRUE)
  
  # randomly sample from each distribution 
  samples = distributions[sample_ind,]
  
  # corrected case count
  res = calc_A_star_undertesting(N = N,
                                 N_tested = N_tested,
                                 N_pos_obs = N_pos_obs,
                                 est_cases = est_cases,
                                 P_testpos_est = P_testpos_est,
                                 P_S_tested = samples[which(names(samples) == "P_S_tested")],
                                 P_S_untested = samples[which(names(samples) == "P_S_untested")],
                                 P_A_testpos = samples[which(names(samples) == "P_A_testpos")],
                                 Z_S = samples[which(names(samples) == "Z_S")],
                                 Z_A = samples[which(names(samples) == "Z_A")],
                                 Se = samples[which(names(samples) == "dist_Se")],
                                 Sp = samples[which(names(samples) == "dist_Sp")]
  )
  
  out = data.frame(
    exp_cases = res$A,
    exp_cases_perfSeSp = res$A_star,
    percent_acc = res$percent_acc,
    percent_und = res$percent_und,
    N = N,
    N_tested = N_tested,
    N_pos = N_pos_obs,
    est_cases = est_cases,
    P_S_tested = samples[which(names(samples) == "P_S_tested")],
    P_S_untested = samples[which(names(samples) == "P_S_untested")],
    Z_S = samples[which(names(samples) == "Z_S")],
    Z_A = samples[which(names(samples) == "Z_A")],
    P_A_testpos = samples[which(names(samples) == "P_A_testpos")],
    P_testpos_S = samples[which(names(samples) == "P_testpos_S")],
    P_testpos_A = samples[which(names(samples) == "P_testpos_A")],
    Se = samples[which(names(samples) == "dist_Se")],
    Sp = samples[which(names(samples) == "dist_Sp")]
  )
  
  return(out)
}

##############################################
# Documentation:  generate_corrected_sample
# Usage:          generate_corrected_sample(N, N_tested, N_pos_obs, distribution_list, 
#                 num_reps, state, select_state=NULL)
# Description:    wrapper for correct_bias_undertesting that subsets a list of prior distributions
#                 to those for a particular state, performs probabilistic bias 
#                 analysis for that state, and formats output
#
# Args/Options:   
# N:              population size
# N_tested:       number of people tested
# N_pos_obs:      number of confirmed COVID-19 cases
# P_testpos_est:  empirical estimate of P(test+|tested)
# distributions:  data frame with prior distributions for P_S_tested, P_S_untested, 
#                 P_A_testpos, Z_S, Z_A, Se, Sp

# Returns:        data frame with the number of estimated infections correcting for
#                 incomplete testing and imperfect test accuracy (exp_cases), 
#                 the number of estimated infections correcting for
#                 incomplete testing assuming perfect test accuracy (exp_cases_perfSeSp), 
#                 the percentage of under-estimation attributable to 
#                 imperfect test accuracy (percent_acc), and the percentage of
#                 under-estimation due to incomplete testing (percent_und) and prior values
##############################################
generate_corrected_sample_und = function(N, N_tested, N_pos_obs, est_cases, distribution_list, num_reps, select_state=NULL){

  #----------------------------------------
  # Obtain corrected case estimates
  #----------------------------------------
  reps = num_reps
  
  distributions = distribution_list[which(names(distribution_list) == select_state)][[1]]
  
  set.seed(123)
  
  result = replicate(reps, correct_bias_undertesting(
    N = N,
    N_tested = N_tested,
    N_pos_obs = N_pos_obs,
    est_cases = est_cases,
    P_testpos_est = mean(distributions$est_testpos),
    distributions = distributions
  ))
  
    result_long = as.data.frame(matrix(result, nrow=reps, byrow=TRUE))
    colnames(result_long) = c(
      "exp_cases", "exp_cases_perfSeSp",
      "percent_acc", "percent_und",
      "N", "N_tested", "N_pos", "est_cases",
      "P_S_tested", "P_S_untested",
      "Z_S", "Z_A", "P_A_testpos",
      "P_testpos_S", "P_testpos_A", "Se", "Sp"
    )
  
    for(i in 1:ncol(result_long)){
      result_long[,i] = unlist(result_long[,i])
    }
  
    return(result_long)
  }




################################################################################

# Prior Functions #

##############################################
# Documentation:  est_P_testpos_AS
# Usage:          est_P_testpos_AS(priors, est_testpos)
# Description:    calculate P(test+|A), P(test+|S)
#
# Args/Options:
# priors:         matrix or data frame containing columns for priors for Z_S (alpha range),
#                 Z_A (beta range)
# est_testpos:    empirical estimate of P(test+|tested)

# Returns:        data frame containing P(test+|S), P(test+|A),
#                 empirical estimate of P(test+|tested)
# Output:         none
##############################################
est_P_testpos_AS = function(priors, est_testpos){
  
  priors = as.data.frame(priors)
  
  P_testpos_S = priors$Z_S  * est_testpos
  P_testpos_A = priors$Z_A  * est_testpos
  
  priors_out = priors %>% mutate(
    P_testpos_S = P_testpos_S,
    P_testpos_A = P_testpos_A,
    est_testpos = est_testpos
  )
  
  return(priors_out)
}

##############################################
# Documentation:  est_P_A_testpos
# Usage:          est_P_A_testpos(P_S_untested, Z_A, Z_S)
# Description:    calculate P(A|test+)
#
# Args/Options:
# P_S_untested:   prior for P(S|untested), as a scalar
# Z_A:            prior for Z_A, as a scalar
# Z_S:            prior for Z_S, as a scalar

# Returns:        estimate of P(A|test+)
# Output:         none
##############################################
est_P_A_testpos = function(P_S_untested, Z_A, Z_S){
  
  Z_A * (1 - P_S_untested) / (( Z_A * (1 - P_S_untested)) + (Z_S * P_S_untested))
  
}
est_P_A_testpos <- Vectorize(est_P_A_testpos)

##############################################
# Documentation:  constrain_priors
# Usage:          constrain_priors(priors)
# Description:    constrain priors using Bayesian melding
#                 to ensure P(A|test+) is within defined range
#
# Args/Options:
# priors:         data frame with priors for P_S_untested, Z_S, Z_A

# Returns:        data frame with constrained priors for P_S_untested, Z_S, Z_A,
#                 and P(A|test+)
# Output:         none
##############################################
constrain_priors = function(priors){
  
  #---------------------------------------
  # Run the SIR algorithm to sample from
  # the induced "posterior" on theta
  #---------------------------------------
  
  phi <- est_P_A_testpos(
    P_S_untested = priors[,2],
    Z_S = priors[,3],
    Z_A = priors[,4]
  )
  
  phi_induced <- density(x = phi,n = nsamp,adjust = 2,kernel = "gaussian")
  phi_sampled_density <- unlist(parallel::mclapply(X = phi,FUN = function(p){
    phi_induced$y[which(phi_induced$x > p)[1]]
  }))
  
  weights <- parallel::mcmapply(FUN = function(p,phi_sampled_density,alpha){
    (phi_sampled_density/ p0(p))^(1-alpha)
  },p=phi,phi_sampled_density=phi_sampled_density,MoreArgs = list(alpha=0.5))
  
  # resample the posterior
  nsamp_post <- 1e5 # number of samples from the posterior
  post_samp_ind <-sample.int(n=nsamp, size=nsamp_post, prob=1/weights,replace=T)
  
  pi_samp <- matrix(
    data = NaN,
    nrow = nsamp_post,
    ncol = 5,
    dimnames = list(NULL,c("P(S|tested)","P(S|untested)","Z_S","Z_A","P(A|test+)"))
  )
  
  pi_samp[1:nsamp_post,1:4] <- as.matrix(theta_samp[post_samp_ind,])
  pi_samp[1:nsamp_post,5] <- phi[post_samp_ind]
  
  pi_samp = cbind(pi_samp, `P(S|test+)` = 1 - pi_samp[,5])
  
  return(pi_samp)
  
}





