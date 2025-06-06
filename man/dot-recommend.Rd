% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classdistr.R
\name{.recommend}
\alias{.recommend}
\title{Helper function to check for recommendations}
\usage{
.recommend(x, distr, outliers, classint_pref, nclasses)
}
\arguments{
\item{x}{The observations}

\item{outliers}{Does the data have IQR outliers}

\item{nclasses}{The number of desired classes for classInt}

\item{distr_with_outliers}{The likely distribution with outliers}

\item{distr_without_outliers}{The likely distribution without outliers}

\item{classInt_pref}{The preferred classInt style}

\item{potential_distrs}{The types of distributions to fit}
}
\description{
Helper function to check for recommendations
}
\keyword{internal}
