% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step3_generateComparisons.R
\name{absFromDataframe}
\alias{absFromDataframe}
\title{Generate pairwise absolute differences from dataframes}
\usage{
absFromDataframe(pairsOfInterest, dtfName, varName, setToMinus1)
}
\arguments{
\item{pairsOfInterest}{dataframe with columns `hash1`, `hash2` referring to
vendor hashes for the comparison}

\item{dtfName}{dataframe containing the columns to be compared}

\item{varName}{vector of variable names that we want to compute differences
for (TODO: now only works if >1 col)}

\item{setToMinus1}{logical vector, same length as `varName`}
}
\value{
vector the length of `nrow(pairsOfInterest)`, containing the
  resulting similarity scores for each pair
}
\description{
Given a dataframe where each row corresponds to a vendor hash (can be
generated using the functions in `step2_anytime.R`), generate absolute
differences between columns. Columns need to be numeric variables. There is
also the option to set the result to -1 if the variable being compared has
value 0 for both accounts in the pair. This is useful for variables such as
the number of tokens in the profiles -- if both have no profile information
the similarity should be missing (-1) instead of 0.
}
