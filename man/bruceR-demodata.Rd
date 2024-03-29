% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bruceR-stats_3_manova.R
\name{bruceR-demodata}
\alias{bruceR-demodata}
\alias{between.1}
\alias{between.2}
\alias{between.3}
\alias{mixed.2_1b1w}
\alias{mixed.3_1b2w}
\alias{mixed.3_2b1w}
\alias{within.1}
\alias{within.2}
\alias{within.3}
\title{Demo data.}
\format{
\describe{
  \item{\strong{1. Between-Subjects Design}}{
    \itemize{
      \item \code{between.1} - A(4)
      \item \code{between.2} - A(2) * B(3)
      \item \code{between.3} - A(2) * B(2) * C(2)
    }
  }
  \item{\strong{2. Within-Subjects Design}}{
    \itemize{
      \item \code{within.1} - A(4)
      \item \code{within.2} - A(2) * B(3)
      \item \code{within.3} - A(2) * B(2) * C(2)
    }
  }
  \item{\strong{3. Mixed Design}}{
    \itemize{
      \item \code{mixed.2_1b1w} - A(2, between) * B(3, within)
      \item \code{mixed.3_1b2w} - A(2, between) * B(2, within) * C(2, within)
      \item \code{mixed.3_2b1w} - A(2, between) * B(2, within) * C(2, between)
    }
  }
}
}
\source{
\href{https://book.douban.com/subject/1195181/}{Multi-Factor Experimental Design in Psychology and Education}
}
\description{
Demo datasets of multi-factor ANOVA as examples to show how the functions
\code{\link{MANOVA}} and \code{\link{EMMEANS}} work.
}
\keyword{internal}
