% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bruceR_utils.R
\name{\%notin\%}
\alias{\%notin\%}
\title{The opposite of \code{\%in\%}.}
\usage{
x \%notin\% vector
}
\arguments{
\item{x}{Numeric or character vector.}

\item{vector}{Numeric or character vector.}
}
\value{
A vector of \code{TRUE} or \code{FALSE}.
}
\description{
The opposite of \code{\%in\%}.
}
\examples{
data = data.table(ID=1:10, X=sample(1:10, 10))
data
data[ID \%notin\% c(1, 3, 5, 7, 9)]

}
\seealso{
\code{\link[base:match]{\%in\%}}
}
