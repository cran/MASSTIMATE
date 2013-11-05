\name{ppe}
\alias{ppe}
\title{Percent Prediction Error}
\description{This function calculates the percent prediction error in a sample}
\usage{ppe(true, pred, abs = TRUE)}
\arguments{
  \item{true}{a numeric vector representing the true value on which a prediction (pred) will be based}
  \item{pred}{a numeric vector with the predicted values, must be equal length to 'true'}
  \item{abs}{a logical value indicating whether to calculate absolute values for ppe, default is TRUE}
}
\details{
This function calculates the percent prediction error (ppe) as a scaled residual, so:

ppe=((true-pred)/pred)*100
}
\value{
Function returns a list of results, including the a list of all the precent prediction errors, along with the mean, 95 percent confidence intervals, range, and standard deviation.
}
\author{Nicolas E. Campione}
\references{
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
  
  Smith, R. J. (1980) Rethinking allometry. Journal of Theoretical Biology, 87, 97-111.
}