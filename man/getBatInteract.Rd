% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getBatInteract.R
\name{getBatInteract}
\alias{getBatInteract}
\title{getBatInteract}
\usage{
getBatInteract(filename, desiredtz)
}
\arguments{
\item{filename}{name of file where pdk-device-battery is stored (txt).}

\item{desiredtz}{timezone (character) in Europe/London format}
}
\value{
timestamps (POSIX) on which the battery was either being connected or unplugged.
}
\description{
getBatInteract
}
