% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joint_fit_sample.R
\name{joint_fit_sample}
\alias{joint_fit_sample}
\title{Compute a baseline and periodic fit to data.}
\usage{
joint_fit_sample(data, formula)
}
\arguments{
\item{data}{a data frame with three columns: date (a POSIX date), time (the decimal time) and the value}

\item{formula}{The specific regression formula you want to use.}
}
\value{
A data frame of fitted values than we can use for plotting
}
\description{
\code{joint_fit_sample} Is an overarching analysis function that does a lot of heavy lifting:
\itemize{
  \item Computes a periodic and trigonometric fit through random thinning of the data sets(using periodic_fit_sample and trigonometric_fit_sample)
  \item Rolls up the results into a data frame for plotting.
}Returns a list of fitted and predicted values of periodically repeating data.  This works over a list of data sites efficiently processing multiple locations
}
\examples{

# This really is an overarching function that does the master fitting, since we are doing a lot of things repeatedly this seemed to be the easiest to avoid errors!

}
