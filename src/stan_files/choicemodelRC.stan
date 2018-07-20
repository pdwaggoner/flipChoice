data {
    int<lower=2> C; // Number of alternatives (choices) in each scenario
    int<lower=1> R; // Number of respondents
    int<lower=1> V_covariates; // Number of respondent-specific covariates
    int<lower=1> S[R]; // Number of questions per respondent
    int<lower=1> RS; // sum(S)
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of parameters
    int<lower=1> V_attribute[A]; // Number of parameters in each attribute
    int<lower=1,upper=C> Y[RS]; // choices
    matrix[C, V] X[RS]; // matrix of attributes for each obs
    matrix[R,V_covariates] covariates;  // matrix of respondent characteristics
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
}

parameters {
    vector<lower=0>[V] sigma;
    matrix<lower=0>[V_covariates, V] sig_theta;  
    matrix[V_covariates, V] theta;
    cholesky_factor_corr[V] L_omega;
    matrix[V, R] standard_normal;
    real mu0[V];
}

transformed parameters {
    matrix[V, V] L_sigma;
    matrix[R, V] beta;

    L_sigma = diag_pre_multiply(sigma, L_omega);
    beta = covariates * theta + (L_sigma * standard_normal)';
}

model {
    int rs = 1;

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    sigma ~ gamma(1.39435729464721, 0.39435729464721);

    mu0 ~ normal(prior_mean, prior_sd);

    to_vector(sig_theta) ~ gamma(1.39435729464721, 0.39435729464721);
    /* to_vector(sig_theta) ~ cauchy(0,5); */
    theta[1] ~ normal(mu0, sig_theta[1]);
    for(v in 2:V_covariates){
        theta[v] ~ normal(0, sig_theta[v]);
    }
    /* theta[1] += mu0; */
  
    L_omega ~ lkj_corr_cholesky(4);

    to_vector(standard_normal) ~ normal(0, 1);

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
