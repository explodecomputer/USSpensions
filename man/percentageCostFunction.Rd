% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_2018.R
\name{percentageCostFunction}
\alias{percentageCostFunction}
\title{Cost function for percetage saved}
\usage{
percentageCostFunction(savedPerYear, targetTotal, income, prudence, fund)
}
\arguments{
\item{savedPerYear}{Percentage saved each year}

\item{targetTotal}{Target total savings amount}

\item{income}{Income earned in each year}

\item{prudence}{Parameter for \code{investment_returns}, 50 or 65}

\item{fund}{Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"}
}
\value{
Dollars needed to save per year.
}
\description{
This function is the cost function used for determining how much is needed 
to be saved as a percentage of income each year.
}
