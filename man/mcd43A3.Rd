% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcd43A3.R
\docType{data}
\name{mcd43A3}
\alias{mcd43A3}
\title{Black sky albedo for 3 MODIS pixels.}
\format{
A data frame with 5907 rows and 5 variables
}
\source{
Schaaf, C., Wang, Z. (2015). MCD43A3 MODIS/Terra+Aqua BRDF/Albedo Daily L3 Global - 500m V006. NASA EOSDIS Land Processes DAAC. doi: 10.5067/MODIS/MCD43A3.006. Accessed January 4, 2019.
}
\usage{
data(mcd43A3)
}
\description{
\itemize{
  \item site. Fluxnet site abbreviation
  \item date. POSIX date, but measurement started in 1998. We should have one measurement per day.
  \item time. The decimal representation of the date.
  \item product. "BS_Albedo" black sky albedo
  \item value. value of measurement in previous column
}
}
\keyword{datasets}
