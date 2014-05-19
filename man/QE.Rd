\name{QE}
\alias{QE}
\title{Body Mass Estimates Using Campione and Evans 2012}
\description{This function presents the bivariate regression equations from Campione and Evans (2012) for esimating body mass in terretrial vertebartes using the combined humeral and femoral circumferences}
\usage{QE(HC, FC, equation = c("raw","phylocor"), data = NULL)}
\arguments{
  \item{HC}{a value or vector representing humeral circumference (in mm)}
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{equation}{desired estimation equation. Two possible choices (See Details)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{
The function includes two different equations. \code{equation = "raw"} applies the non-phylogenetically corrected regression equation - equation 1 of Campione and Evans (2012). \code{equation = "phylocor"} applies the phylogenetically corrected regression equation - equation 2 of Campione and Evans (2012).

If a data object is specified, the mass estiamtes and additional results will be added as columns to the data.frame or matrix within that object.
}
\value{
Four numeric values or columns are returned:
	\item{log.QE}{A numeric value or vector representing the mass estimate(s) in log10 grams}
	\item{QE}{A numeric value or vector representing the mass estimate(s) in grams}
	\item{upper.QE}{A numeric value or vector representing the upper prediction error based on that derived for the specific equation by Campione and Evans (2012)}
	\item{lower.QE}{A numeric value or vector representing the lower prediction error}
}
\author{Nicolas E. Campione}
\references{
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
}
\seealso{
  \code{\link{cQE}}
  \code{\link{quadrupeds}}
  \code{\link{MR}}
  \code{\link{AHR1985}}
  \code{\link{MCF2004}}
}
\examples{
##Dinosaur data from Campione and Evans (2012) for quadrupedal dinosaurs
data(dinos)

##Combined equation based on the raw regression
QE(dinos$HC, dinos$FC, equation = "raw", data = dinos)

##Combined equation based on the phylogenetically corrected regression, data not specified
QE(dinos$HC, dinos$FC, equation = "phylocor")
}