% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification_models.R
\name{hd_model_test}
\alias{hd_model_test}
\title{Validate model on new data}
\usage{
hd_model_test(
  model_object,
  train_set,
  test_set,
  variable = "Disease",
  metadata_cols = NULL,
  case,
  control = NULL,
  balance_groups = TRUE,
  palette = NULL,
  seed = 123
)
}
\arguments{
\item{model_object}{An \code{hd_model} object coming from \code{hd_model_rreg()} and \code{hd_model_rf()} binary or multiclass classification.}

\item{train_set}{The training set as an HDAnalyzeR object or a dataset in wide format with sample ID as its first column and class column as its second column.}

\item{test_set}{The validation/test set as an HDAnalyzeR object or a dataset in wide format with sample ID as its first column and class column as its second column.}

\item{variable}{The name of the metadata variable containing the case and control groups. Default is "Disease".}

\item{metadata_cols}{The metadata variables to include in the analysis. Default is NULL.}

\item{case}{The case class.}

\item{control}{The control groups. If NULL, it will be set to all other unique values of the variable that are not the case. Default is NULL.}

\item{balance_groups}{Whether to balance the groups in the train set. It is only valid in binary classification settings. Default is TRUE.}

\item{palette}{The color palette for the classes. If it is a character, it should be one of the palettes from \code{hd_palettes()}. Default is NULL.}

\item{seed}{Seed for reproducibility. Default is 123.}
}
\value{
The model object containing the validation set, the metrics, the ROC curve, the probability plot, and the confusion matrix for the new data.
}
\description{
\code{hd_model_test()} validates the model on new data. It takes an already tuned model,
evaluates it on the validation (new test) set, calculates the metrics and plots the probability
and ROC curve based on the new data.
}
\details{
In order to run this function, the train and test sets should be in exactly
the same format meaning that they must have the same columns in the same order.
Some function arguments like the case/control, variable, and metadata_cols should
be also the same. If the data contain missing values, KNN (k=5) imputation
will be used to impute. If \code{case} is provided, the model will be a binary
classification model. If \code{case} is NULL, the model will be a multiclass classification model.

In multi-class models, the groups in the train set are not balanced and sensitivity and specificity
are calculated via macro-averaging. In case the model is run against a continuous variable,
the palette will be ignored.
}
\examples{
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Split the data for training and validation sets
dat <- hd_object$data
train_indices <- sample(seq_len(nrow(dat)), size = floor(0.8 * nrow(dat)))
train_data <- dat[train_indices, ]
validation_data <- dat[-train_indices, ]

hd_object_train <- hd_initialize(train_data, example_metadata, is_wide = TRUE)
hd_object_val <- hd_initialize(validation_data, example_metadata, is_wide = TRUE)

# Split the training set into training and inner test sets
hd_split <- hd_split_data(hd_object_train, variable = "Disease")

# Run the regularized regression model pipeline
model_object <- hd_model_rreg(hd_split,
                              variable = "Disease",
                              case = "AML",
                              grid_size = 5,
                              palette = "cancers12",
                              verbose = FALSE)

# Run the model evaluation pipeline
hd_model_test(model_object, hd_object_train, hd_object_val, case = "AML", palette = "cancers12")

# Run the pipeline against continuous variable
# Split the training set into training and inner test sets
hd_split <- hd_split_data(hd_object_train, variable = "Age")

# Run the regularized regression model pipeline
model_object <- hd_model_rreg(hd_split,
                              variable = "Age",
                              case = "AML",
                              grid_size = 2,
                              cv_sets = 2,
                              plot_title = NULL,
                              verbose = FALSE)

# Run the model evaluation pipeline
hd_model_test(model_object, hd_object_train, hd_object_val, variable = "Age", case = NULL)
}
