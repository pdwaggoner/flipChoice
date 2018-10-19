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
    int<lower=1> U; // Number of standard deviation parameters
    vector[V] prior_mean; // Prior mean for theta
    vector[V] prior_sd; // Prior sd for theta
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

    // gamma distribution with mode = 1 and p(x < 20) = 0.999
    sigma_unique ~ gamma(1.39435729464721, 0.39435729464721);

    theta ~ normal(prior_mean, prior_sd);

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
