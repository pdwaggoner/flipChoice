#include <RcppEigen.h>

// [[Rcpp::export]]
double d0CriterionShortcut(Eigen::MatrixXd & question_design,
                               Eigen::MatrixXd & partial_info_matrix,
                               int alternatives_per_question)
{
    Eigen::MatrixXd sums = question_design.colwise().sum();
    return log(((double)((partial_info_matrix +
                   question_design.transpose() * question_design -
                   sums.transpose() * sums / alternatives_per_question)).determinant()) /
                   pow(alternatives_per_question, partial_info_matrix.rows()));
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

// [[Rcpp::export]]
Eigen::VectorXd choiceProbabilities(Eigen::MatrixXd & question_design,
                                    Eigen::VectorXd & prior)
{
    Eigen::ArrayXd exp_utilities = (question_design * prior).array().exp();
    return exp_utilities / exp_utilities.sum();
}

// [[Rcpp::export]]
double dPCriterionShortcut(Eigen::MatrixXd & question_design,
                           Eigen::VectorXd & prior,
                           Eigen::MatrixXd & partial_info_matrix,
                           int alternatives_per_question)
{
    Eigen::VectorXd choice_probs = choiceProbabilities(question_design, prior);
    return log((double)((partial_info_matrix + question_design.transpose() *
        (Eigen::MatrixXd(choice_probs.asDiagonal()) -
        choice_probs * choice_probs.transpose()) *
        question_design).determinant()));
}

// [[Rcpp::export]]
Eigen::MatrixXd dPPartialInfoMatrix(Eigen::MatrixXd & coded_design,
                           Eigen::VectorXd & prior,
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
            Eigen::VectorXd choice_probs = choiceProbabilities(question_design,
                                                               prior);
            info_matrix = info_matrix + question_design.transpose() *
                (Eigen::MatrixXd(choice_probs.asDiagonal()) - choice_probs *
                choice_probs.transpose()) * question_design;
        }
    }
    return info_matrix;
}

// [[Rcpp::export]]
Eigen::MatrixXd setLevel(Eigen::MatrixXd & question_design, int row_index,
                            int attribute_index, int lvl,
                            Eigen::VectorXi levels_per_attribute,
                            Eigen::VectorXi start_indices)
{
    int start_index = start_indices[attribute_index - 1] - 1;
    int n_levels = levels_per_attribute[attribute_index - 1] - 1;
    for (int i = 0; i < n_levels; i++)
    {
        if (lvl - 2 == i)
            question_design(row_index - 1, start_index + i) = 1;
        else
            question_design(row_index - 1, start_index + i) = 0;
    }
    return question_design;
}

// [[Rcpp::export]]
Eigen::MatrixXd setLevelAllRows(Eigen::MatrixXd & question_design,
                                int attribute_index, int lvl,
                                Eigen::VectorXi levels_per_attribute,
                                Eigen::VectorXi start_indices)
{
    int n_rows = question_design.rows();
    int start_index = start_indices[attribute_index - 1] - 1;
    int n_levels = levels_per_attribute[attribute_index - 1] - 1;
    for (int i = 0; i < n_levels; i++)
    {
        if (lvl - 2 == i)
        {
            for (int j = 0; j < n_rows; j++)
                question_design(j, start_index + i) = 1;
        }
        else
        {
            for (int j = 0; j < n_rows; j++)
                question_design(j, start_index + i) = 0;
        }
    }
    return question_design;
}

// [[Rcpp::export]]
int getLevel(Eigen::MatrixXd & question_design, int row_index,
             int attribute_index, Eigen::VectorXi levels_per_attribute,
             Eigen::VectorXi start_indices)
{
    int start_index = start_indices[attribute_index - 1] - 1;
    int n_levels = levels_per_attribute[attribute_index - 1] - 1;
    for (int i = 0; i < n_levels; i++)
    {
        if (question_design(row_index - 1, start_index + i) == 1)
            return i + 2;
    }
    return 1;
}
