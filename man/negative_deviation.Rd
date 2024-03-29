% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_deficit_assessment.R
\name{negative_deviation}
\alias{negative_deviation}
\title{Relative Negative Deviation from a reference}
\usage{
negative_deviation(data_vector, reference_vector)
}
\arguments{
\item{data_vector}{Numeric data vector}

\item{reference_vector}{Corresponding data of the reference}
}
\value{
Numeric value between 0 and 1
}
\description{
The cumulative sum of all negative deviations.
}
\details{
First the similarity the data vector and the reference vector is calculated.
Only complete pairs (no NA values) are used. For each data pair the
quotient between data and reference is calcutaled. If data > reference the
value is set to 1. All quotients  are cumulated (-> absolute similarity).
This can be maximum the number of data pairs. When deviding by the number of
data pairs, the relative similarity is obtained. One minus the relative
similarity is the negative deviation.
}
