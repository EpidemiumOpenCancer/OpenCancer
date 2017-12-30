\name{createcorlag}
\alias{createcorlag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transform dataframe by creating multiple lagged variables satisfying correlation constraints with a target}
\description{
Given a dataframe containing a target and covariates, \code{createcorlag} returns a dataframe containing all the variables and only the lagged variables whose absolute pairwise correlation with the target exceeds a fixed threshold. Lag function is applied by groups. Additional variables
that should not be lagged can be provided.
}
\usage{
createcorlag(df, refvar="incidence", groupingvar = c("sex","age","Country_Transco"), k = 1, cross_cor_threshold=0.2,
                      labelvar = c("year", "Zonier"))
}
\arguments{
  \item{df}{Dataframe whose variables must be lagged}
  \item{refvar}{Reference variable or target from which to compute the cross correlation with other lagged variables}
  \item{cross_cor_threshold}{Correlation threshold required to keep a lag}
  \item{groupingvar}{Variables used to define group}
  \item{k}{Maximum lag that must be computed. If k>1, lags 1:k are
  computed}
  \item{labelvar}{Variables that should not be lagged}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe with non lagged and lagged observations -whose absolute correlation with the target exceeds the selected threshold- by groups.
}

\author{
Benjamin Schannes
}
