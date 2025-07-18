% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification_models.R
\name{evaluate_model}
\alias{evaluate_model}
\title{Finalize and evaluate the model}
\usage{
evaluate_model(
  dat,
  variable = "Disease",
  case,
  mixture = NULL,
  palette = NULL,
  verbose = TRUE,
  seed = 123
)
}
\arguments{
\item{dat}{An \code{hd_model} object coming from a tuning function.}

\item{variable}{The variable to predict. Default is "Disease".}

\item{case}{The case class.}

\item{mixture}{The mixture parameter for the elastic net. If NULL it will be tuned. Default is NULL.}

\item{palette}{The color palette for the classes. If it is a character, it should be one of the palettes from \code{hd_palettes()}. Default is NULL.}

\item{verbose}{Whether to print progress messages. Default is TRUE.}

\item{seed}{Seed for reproducibility. Default is 123.}
}
\value{
A model object containing the train and test data, the final model, the metrics, the ROC curve, and the mixture parameter.
}
\description{
\code{evaluate_model()} finalizes the model using the best hyperparameters and evaluates
the model using the test set. It calculates the accuracy, sensitivity, specificity,
AUC, and confusion matrix. It also plots the ROC curve.
}
\keyword{internal}
