% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bruceR-stats_4_regress.R
\name{HLM_summary}
\alias{HLM_summary}
\title{Tidy report of HLM (\code{lmer} and \code{glmer} models).}
\usage{
HLM_summary(model = NULL, test.rand = FALSE, digits = 3, ...)
}
\arguments{
\item{model}{A model fitted with \code{lmer} or \code{glmer} function using the \code{lmerTest} package.}

\item{test.rand}{[Only for \code{lmer} and \code{glmer}]
\code{TRUE} or \code{FALSE} (default).
Test random effects (i.e., variance components) by using the likelihood-ratio test (LRT),
which is asymptotically chi-square distributed.
For large datasets, it is much time-consuming.}

\item{digits}{Number of decimal places of output. Defaults to \code{3}.}

\item{...}{Other arguments. You may re-define \code{formula}, \code{data}, or \code{family}.}
}
\value{
No return value.
}
\description{
NOTE: \code{\link{model_summary}} is preferred.
}
\examples{
\donttest{library(lmerTest)

## Example 1: data from lme4::sleepstudy
# (1) 'Subject' is a grouping/clustering variable
# (2) 'Days' is a level-1 predictor nested within 'Subject'
# (3) No level-2 predictors
m1 = lmer(Reaction ~ (1 | Subject), data=sleepstudy)
m2 = lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
m3 = lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
HLM_summary(m1)
HLM_summary(m2)
HLM_summary(m3)

## Example 2: data from lmerTest::carrots
# (1) 'Consumer' is a grouping/clustering variable
# (2) 'Sweetness' is a level-1 predictor
# (3) 'Age' and 'Frequency' are level-2 predictors
hlm.1 = lmer(Preference ~ Sweetness + Age + Frequency +
               (1 | Consumer), data=carrots)
hlm.2 = lmer(Preference ~ Sweetness + Age + Frequency +
               (Sweetness | Consumer) + (1 | Product), data=carrots)
HLM_summary(hlm.1)
HLM_summary(hlm.2)
}
}
\references{
Hox, J. J. (2010).
\emph{Multilevel analysis: Techniques and applications} (2nd ed.).
New York, NY: Routledge.

Nakagawa, S., & Schielzeth, H. (2013).
A general and simple method for obtaining \emph{R}^2 from generalized linear mixed-effects models.
\emph{Methods in Ecology and Evolution, 4,} 133--142.

Xu, R. (2003).
Measuring explained variation in linear mixed effects models.
\emph{Statistics in Medicine, 22,} 3527--3541.
}
\seealso{
\code{\link{print_table}} (print simple table)

\code{\link{model_summary}} (highly suggested)

\code{\link{GLM_summary}}

\code{\link{regress}}
}
