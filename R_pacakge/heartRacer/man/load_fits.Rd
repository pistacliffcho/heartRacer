% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heartRacer.R
\name{load_fits}
\alias{load_fits}
\title{Load n most recent fit files}
\usage{
load_fits(filesLocation = "~/Documents/fitFiles", n = 5)
}
\arguments{
\item{filesLocation}{Folder contain location of files}

\item{n}{How many files to load

Loads in \code{n} most recent fit files at \code{filesLocation}. Assumes name is
is timestamp (i.e. saved by upload_fit)}
}
\description{
Load n most recent fit files
}
