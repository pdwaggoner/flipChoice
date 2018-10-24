data {
    int<lower=2> C; // Number of alternatives (choices) in each question
    int<lower=1> R; // Number of respondents
    int<lower=1> S[R]; // Number of questions per respondent
    int<lower=0> S_out; // Number of holdout questions
    int<lower=1> RS; // sum(S)
    int<lower=1> RS_out; // R * S_out
    int<lower=1> P; // Number of classes
    int<lower=1> A; // Number of attributes
    int<lower=1> V; // Number of parameters
    int<lower=1> V_attribute[A]; // Number of parameters in each attribute
    int<lower=1,upper=C> Y[RS]; // choices
    int<lower=1,upper=C> Y_out[RS_out]; // holdout choices
    matrix[C, V] X[RS]; // matrix of attributes for each obs
    matrix[C, V] X_out[RS_out]; // matrix of holdout attributes for each obs
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
    real<lower=1> lkj_shape; // shape parameter for LKJ prior
    real<lower=0> gamma_shape; // shape parameter for gamma prior for sigma
    real<lower=0> gamma_scale; // scale parameter for gamma prior for sigma
}

parameters {
    vector<lower=0>[V] sigma[P];
    vector[V] theta[P];
    cholesky_factor_corr[V] L_omega[P];
    vector[V] standard_normal[R, P];
    simplex[P] class_weights;
}

transformed parameters {
    matrix[V, V] L_sigma[P];
    vector[V] class_beta[R, P];

    for (p in 1:P)
    {
        L_sigma[p] = diag_pre_multiply(sigma[p], L_omega[p]);

        for (r in 1:R)
            class_beta[r, p] = theta[p] + L_sigma[p] * standard_normal[r, p];
    }
}

model {
    vector[P] posterior_prob;
    int rs = 1;

    for (p in 1:P)
    {
        // gamma distribution with mode = 1 and p(x < 20) = 0.999
        /* sigma[p] ~ gamma(1.39435729464721, 0.39435729464721); */
        sigma[p] ~ gamma(gamma_shape, gamma_scale);

        theta[p] ~ normal(prior_mean, prior_sd);
        L_omega[p] ~ lkj_corr_cholesky(lkj_shape);
        for (r in 1:R)
            standard_normal[r, p] ~ normal(0, 1);
    }

    for (r in 1:R)
    {
        for (s in 1:S[r])
        {
            for (p in 1:P)
            {
                if (s == 1)
                    posterior_prob[p] = log(class_weights[p]);
                posterior_prob[p] += categorical_logit_lpmf(Y[rs] | X[rs] * class_beta[r, p]);
            }
            rs += 1;
        }
        target += log_sum_exp(posterior_prob);
    }
}

generated quantities {
    vector[V] beta[R];
    real log_likelihood = 0;
    real log_likelihood_out = 0;
    vector[R] rlh;
    vector[R] rlh_out;

    // Add braces to exclude rs from exported values
    {
        int rs = 1;
        for (r in 1:R)
        {
            vector[P] posterior_prob;
            real log_sum_exp_pp;
            vector[P] respondent_class_weights;

            for (p in 1:P)
                posterior_prob[p] = 0;

            for (s in 1:S[r])
            {
                for (p in 1:P)
                    posterior_prob[p] += categorical_logit_lpmf(Y[rs] | X[rs] * class_beta[r, p]);
                rs += 1;
            }

            for (p in 1:P)
                posterior_prob[p] += log(class_weights[p]);

            log_sum_exp_pp = log_sum_exp(posterior_prob);
            log_likelihood += log_sum_exp_pp;
            rlh[r] = exp(log_sum_exp_pp / S[r]);

            respondent_class_weights = exp(posterior_prob - log_sum_exp_pp);
            for (v in 1:V)
            {
                beta[r, v] = 0;
                for (p in 1:P)
                    beta[r, v] += class_beta[r, p, v] * respondent_class_weights[p];
            }
        }
    }

    if (S_out > 0)
    {
        int rs = 1;
        for (r in 1:R)
        {
            vector[P] posterior_prob;
            real log_sum_exp_pp;

            for (p in 1:P)
                posterior_prob[p] = 0;

            for (s in 1:S_out)
            {
                for (p in 1:P)
                    posterior_prob[p] += categorical_logit_lpmf(Y_out[rs] | X_out[rs] * class_beta[r, p]);
                rs += 1;
            }

            for (p in 1:P)
                posterior_prob[p] += log(class_weights[p]);

            log_sum_exp_pp = log_sum_exp(posterior_prob);
            log_likelihood_out += log_sum_exp_pp;
            rlh_out[r] = exp(log_sum_exp_pp / S_out);
        }
    }
}
