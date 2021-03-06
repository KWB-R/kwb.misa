% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/continuousTimeIntervals.R
\name{continuousTimeIntervals}
\alias{continuousTimeIntervals}
\title{Force data into predefined time intervals}
\usage{
continuousTimeIntervals(
  time_vector,
  data_vector,
  res = 15,
  first_pointOfTime = min(time_vector, na.rm = T),
  last_pointOfTime = max(time_vector, na.rm = T)
)
}
\arguments{
\item{time_vector}{A POSIXct vector}

\item{data_vector}{A numeric vector, with data corresponding to the time_vector}

\item{res}{Temporal resolution in minutes}

\item{first_pointOfTime}{Starting point (POSIXct) of the newly defined time series.
By default the minimum of the time_vector}

\item{last_pointOfTime}{End point (POSIXct) of the newly defined time series.
By default the maximum of the time_vector}
}
\value{
Dataframe with POSIX column "t" and data column "d"
}
\description{
The measurements are fitted into timesteps defined be the first point of
time and a temporal resolution
}
\details{
In a first step a vactor is generated with continuous timesteps,
starting at first_pointOfTime by a defined time interval. Subsequently, the
measured data is forced into timesteps with a similar time interval. Here,
the measurements are assigned to the timestep that is closest to the actual
time of measurements. If more than one measurement are assigned to one
timestep, the average is used. If there is no measurement, NA is used.
}
