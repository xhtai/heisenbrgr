% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step1a_cleanData.R
\name{cleanUsers}
\alias{cleanUsers}
\title{Clean users data}
\usage{
cleanUsers(users, feedback)
}
\arguments{
\item{users}{name of dataframe with unique user accounts, as described above}

\item{feedback}{name of dataframe with feedback data containing `vendor_hash`}
}
\value{
dataframe with usable feedback data
}
\description{
Given users data, at least with columns `hash_str`, `marketplace`, `id`,
`diversity`, delete rows that have no match in feedback data
}
