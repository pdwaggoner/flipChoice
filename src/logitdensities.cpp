#include <Rcpp.h>

using namespace Rcpp;

// A numerically stable way of calculating log(sum(exp(x)))
// [[Rcpp::export]]
double logSumExp(NumericVector x)
{
    double max_log = max(x);
    double result = 0;
    for (int i = 0; i < x.size(); i++)
        result += exp(x[i] - max_log);
    result = log(result) + max_log;
    return result;
}

// [[Rcpp::export]]
NumericVector logDensitiesChoice(NumericVector b, NumericMatrix X,
                                 NumericVector weights, int n_alternatives,
                                 int n_parameters)
{
    int n_resp_questions = X.nrow();
    NumericVector result(n_resp_questions);
    for (int i = 0; i < n_resp_questions; i++)
    {
        NumericVector discriminants(n_alternatives);
        int c = 0;
        for (int j = 0; j < n_alternatives; j++)
        {
            for (int k = 0; k < n_parameters; k++)
            {
                discriminants[j] += X(i, c) * b[k];
                c++;
            }
        }
        result[i] = (discriminants[0] - logSumExp(discriminants)) * weights[i];
    }
    return result;
}

// [[Rcpp::export]]
double logDensityChoice(NumericVector b, NumericMatrix X,
                        NumericVector weights, int n_alternatives,
                        int n_parameters)
{
    return sum(logDensitiesChoice(b, X, weights, n_alternatives, n_parameters));
}

// [[Rcpp::export]]
NumericVector gradientChoice(NumericVector b, NumericMatrix X,
                             NumericVector weights, int n_alternatives,
                             int n_parameters)
{
    int n_resp_questions = X.nrow();
    NumericVector result(n_parameters);
    for (int i = 0; i < n_resp_questions; i++)
    {
        NumericVector exp_discriminants(n_alternatives);
        double sum_exp_discriminants = 0;
        int c = 0;
        for (int j = 0; j < n_alternatives; j++)
        {
            double discriminant = 0;
            for (int k = 0; k < n_parameters; k++)
            {
                discriminant += X(i, c) * b[k];
                c++;
            }
            exp_discriminants[j] = exp(discriminant);
            sum_exp_discriminants += exp_discriminants[j];
        }
        NumericVector shares = exp_discriminants / sum_exp_discriminants;
        for (int m = 0; m < n_parameters; m++)
        {
            double val = X(i, m);
            int c = m;
            for (int j = 0; j < n_alternatives; j++)
            {
                val -= X(i, c) * shares[j];
                c += n_parameters;
            }
            result[m] += val * weights[i];
        }
    }
    return result;
}
