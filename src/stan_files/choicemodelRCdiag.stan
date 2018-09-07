data {
    int<lower=2> C; // Number of alternatives (choices) in each scenario
    int<lower=1> R; // Number of respondents
    int<lower=1> V_fc; // Number of respondent-specific fixed covariate parameters
    int<lower=0> V_rc; // Number of respondent-specific random effects vectors
    int<lower=1> rc_dims[V_rc]; // Dimension of each random covariate
    int<lower=0> total_rc; /* total num. random effects sum(rc_dims) */ 
    /* or fixed cov var fixed real<lower=0> sig_fc */  
    int<lower=1> S[R]; // Number of questions per respondent
    int<lower=1> RS; // sum(S)
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of parameters
    int<lower=1> V_attribute[A]; // Number of parameters in each attribute
    int<lower=1,upper=C> Y[RS]; // choices
    matrix[C, V] X[RS]; // matrix of attributes for each obs
    matrix[R,V_fc] Xmat;  // design matrix of resp. characteristics fixed effects
    matrix[R,total_rc] Zmat;  // design matrix of resp. characteristics fixed effects  
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
}

parameters {
    vector<lower=0>[V] sigma;
    /* matrix<lower=0>[V_covariates, V] sig_fc; */
    vector<lower=0>[V] sig_fc; /* each fixed cov shares a variance param */
    matrix<lower=0>[V_rc, V] sig_rc;
    matrix[V_fc, V] resp_fixed_coef;
    cholesky_factor_corr[V] L_omega;
    matrix[V, R] standard_normal;
    matrix[total_rc, V] resp_rand_eff;
    /* real mu0[V]; */
}

transformed parameters {
    matrix[V, V] L_sigma;
    matrix[R, V] beta; /* partworths */

    L_sigma = diag_pre_multiply(sigma, L_omega);
    beta = Xmat * resp_fixed_coef + Zmat*resp_rand_eff + (L_sigma * standard_normal)';
}

model {
    int rs = 1;
    int start_idx; /* for updating random effects */

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    /* sigma ~ gamma(1.39435729464721, 0.39435729464721); */
    sigma ~ gamma(10, 10000);
    /* mu0 ~ normal(prior_mean, prior_sd); */
    /* sig_fc ~ gamma(1.39435729464721, 0.39435729464721); */
  sig_fc ~ gamma(10,10000);
    /* to_vector(sig_rc) ~ gamma(1.39435729464721, 0.39435729464721); */
  to_vector(sig_rc) ~ gamma(10,10000);
    /* to_vector(sig_theta) ~ cauchy(0,5); */

    for(j in 1:V_fc){
      to_vector(resp_fixed_coef[j]) ~ normal(0,sig_fc[j]);
    }
    for(i in 1:V){
        start_idx = 1;
        for(j in 1:V_rc){
          resp_rand_eff[start_idx:rc_dims[j],i] ~ normal(0, sig_rc[j,i]);
            start_idx += rc_dims[j];
        }    
    }
    /* theta[1] += mu0; */
    
  
    L_omega ~ lkj_corr_cholesky(4);

    /* to_vector(standard_normal) ~ normal(0, 1); */
    to_vector(standard_normal) ~ normal(0, .001);
  
    for (r in 1:R)
    {
        for (s in 1:S[r])
        {
            Y[rs] ~ categorical_logit(X[rs] * to_vector(beta[r]));
            rs += 1;
        }
    }
}

generated quantities {
    real log_likelihood = 0;
    int rs = 1;

    for (r in 1:R)
    {
        for (s in 1:S[r])
        {
            log_likelihood += categorical_logit_lpmf(Y[rs] | X[rs] * to_vector(beta[r]));
            rs += 1;
        }
    }
}
