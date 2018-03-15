#include <RcppEigen.h>

// [[Rcpp::export]]
double d0CriterionShortcut(Eigen::MatrixXd & question_design,
                               Eigen::MatrixXd & partial_info_matrix,
                               int alternatives_per_question)
{
    Eigen::MatrixXd sums = question_design.colwise().sum();
    return ((double)((partial_info_matrix +
                   question_design.transpose() * question_design -
                   sums.transpose() * sums / alternatives_per_question)).determinant()) /
                   pow(alternatives_per_question, partial_info_matrix.rows());
}

// [[Rcpp::export]]
Eigen::MatrixXd d0PartialInfoMatrix(Eigen::MatrixXd & coded_design,
                             int n_questions,
                             int question,
                             int alternatives_per_question)
{
    int n_parameters = coded_design.cols();
    Eigen::MatrixXd info_matrix = Eigen::MatrixXd::Zero(n_parameters,
                                                        n_parameters);
    for (int s = 0; s < n_questions; s++)
    {
        if (s != question - 1)
        {
            int start_row = s * alternatives_per_question;
            Eigen::MatrixXd question_design = coded_design.block(start_row, 0,
                                             alternatives_per_question,
                                             n_parameters);
            Eigen::MatrixXd sums = question_design.colwise().sum();
            info_matrix = info_matrix + question_design.transpose() * question_design -
                           sums.transpose() * sums / alternatives_per_question;
        }
    }

    return info_matrix;
}
