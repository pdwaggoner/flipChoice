data {
    int<lower=2> C; // Number of alternatives (choices) in each question
    int<lower=1> R; // Number of respondents
    int<lower=1> S[R]; // Number of questions per respondent
    int<lower=0> S_out; // Number of holdout questions
    int<lower=1> RS; // sum(S)
    int<lower=1> RS_out; // R * S_out
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of parameters
    int<lower=1> V_attribute[A]; // Number of parameters in each attribute
    int<lower=1,upper=C> Y[RS]; // choices
    int<lower=1,upper=C> Y_out[RS_out]; // holdout choices
    matrix[C, V] X[RS]; // matrix of attributes for each obs
    matrix[C, V] X_out[RS_out]; // matrix of holdout attributes for each obs
    int<lower=1> U; // Number of standard deviation parameters
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
    real<lower=0> gamma_shape; // shape parameter for gamma prior for sigma
    real<lower=0> gamma_scale; // scale parameter for gamma prior for sigma
}

parameters {
    row_vector[V] theta;
    row_vector<lower=0>[U] sigma_unique;
    matrix[R, V] standard_normal;
}

transformed parameters {
    row_vector<lower=0>[V] sigma;
    matrix[R, V] beta;

    if (U == 1)
    {
        for (v in 1:V)
            sigma[v] = sigma_unique[1];
    }
    else
        sigma = sigma_unique;

    beta = rep_matrix(theta, R) + rep_matrix(sigma, R) .* standard_normal;
}

model {
    int rs = 1;
    vector[V * R] vector_normal;

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    /* sigma_unique ~ gamma(1.39435729464721, 0.39435729464721); */
    sigma_unique ~ gamma(gamma_shape, gamma_scale);

    theta ~ normal(prior_mean, prior_sd);

    vector_normal = to_vector(standard_normal);
    vector_normal ~ normal(0, 1);

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
    real log_likelihood_out = 0;
    vector[R] rlh;
    vector[R] rlh_out;
    real mean_rlh;
    real mean_rlh_out;

    // Add braces to exclude rs from exported values
    {
        int rs = 1;
        for (r in 1:R)
        {
            real resp_ll = 0;
            for (s in 1:S[r])
            {
                resp_ll += categorical_logit_lpmf(Y[rs] | X[rs] * to_vector(beta[r]));
                rs += 1;
            }
            log_likelihood += resp_ll;
            rlh[r] = exp(resp_ll / S[r]);
        }
        mean_rlh = exp(log_likelihood / sum(S));
    }

    if (S_out > 0)
    {
        int rs = 1;
        for (r in 1:R)
        {
            real resp_ll = 0;
            for (s in 1:S_out)
            {
                resp_ll += categorical_logit_lpmf(Y_out[rs] | X_out[rs] * to_vector(beta[r]));
                rs += 1;
            }
            log_likelihood_out += resp_ll;
            rlh_out[r] = exp(resp_ll / S_out);
        }
        mean_rlh_out = exp(log_likelihood_out / (R * S_out));
    }
}
