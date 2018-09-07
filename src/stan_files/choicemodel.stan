data {
    int<lower=2> C; // Number of alternatives (choices) in each question
    int<lower=1> R; // Number of respondents
    int<lower=1> S[R]; // Number of questions per respondent
    int<lower=1> RS; // sum(S)
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of parameters
    int<lower=1> V_attribute[A]; // Number of parameters in each attribute
    int<lower=1,upper=C> Y[RS]; // choices
    matrix[C, V] X[RS]; // matrix of attributes for each obs
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
}

parameters {
    vector<lower=0>[V] sigma;
    row_vector[V] theta;
    cholesky_factor_corr[V] L_omega;
    matrix[V, R] standard_normal;
}

transformed parameters {
    matrix[V, V] L_sigma;
    matrix[R, V] beta;

    L_sigma = diag_pre_multiply(sigma, L_omega);
    beta = rep_matrix(theta, R) + (L_sigma * standard_normal)';
}

model {
    int rs = 1;

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    /* sigma ~ gamma(1.39435729464721, 0.39435729464721); */
    sigma ~ gamma(10, 10000);

    theta ~ normal(prior_mean, prior_sd);
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
