#include <RcppEigen.h>

// [[Rcpp::export]]
double d0CriterionShortcutRcpp(Eigen::MatrixXd & question_design,
                               Eigen::MatrixXd & partial_info_matrix,
                             int alternatives_per_question)
{
    Eigen::MatrixXd sums = question_design.colwise().sum();
    return ((double)((partial_info_matrix +
                   question_design.transpose() * question_design -
                   sums.transpose() * sums / alternatives_per_question)).determinant()) /
                   pow(alternatives_per_question, partial_info_matrix.rows());
}
