\name{completed_LOCF}
\alias{completed_LOCF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Applies an extended LOCF transform to impute missing data
}
\description{
Given a vector, applies LOCF transform to this vector to impute missing data and also impute first or last potential missing observations with resp. the first obs. available or the last 
obs. available depending on the fromLast argument chosen in na.locf. 
}
\usage{
completed_LOCF(x,fromLast=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector with missing data you need to impute}

  \item{fromLast}{Boolean indicating whether we
want to apply locf from the first observation (\code{FALSE}) or from the last observation (\code{TRUE})}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector with the same length whose missing data have been imputed with the extended locf method.
}

\author{
Benjamin Schannes
}

\examples{
\dontrun{
xx <- c(NA,NA,2,3,NA,NA,2,3,NA)
xx <- completed_LOCF(xx,fromLast=F)
}
}

