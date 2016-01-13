\name{QE}
\alias{QE}
\title{Body Mass Estimates Using Combined Humeral and Femoral Circumferences}
\description{This function is based on the bivariate regression equations from Campione and Evans (2012) for esimating body mass in terretrial vertebartes using the combined humeral and femoral circumferences}
\usage{QE(HFC = NULL, HC, FC, equation = "raw", quadratic = FALSE, data = NULL)}
\arguments{
  \item{HFC}{a value or vector representing the combined humeral and femoral circumferences}
  \item{HC}{a value or vector representing humeral circumference (in mm)}
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{equation}{desired estimation equation. Two possible choices (See Details)}
  \item{quadratic}{a logical indicating whether estimates based on the quadratic equation should be used (See Details)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{
The function includes two different equations. \code{equation = "raw"} (default) applies the non-phylogenetically corrected regression equation - equation 1 of Campione and Evans (2012). \code{equation = "phylocor"} applies the phylogenetically corrected regression equation - equation 2 of Campione and Evans (2012).

If HFC is specified, then HC and FC are ignored.

If \code{quadratic = TRUE}, then a second set of estimates will be returned based on a quadratic view of the Campione and Evans (2012) data set (Campione in review).

If a data object is specified, the mass estiamtes and additional results will be added as columns to the data.frame or matrix within that object.
}
\value{
Eight numeric values or columns are returned if \code{quadratic = TRUE} (identified by \code{q}):
	\item{log.QE}{A numeric value or vector representing the mass estimate(s) in log10 grams}
	\item{QE}{A numeric value or vector representing the mass estimate(s) in grams}
	\item{lower.QE}{A numeric value or vector representing the lower prediction error derived for the specific equation by Campione and Evans (2012)}
	\item{upper.QE}{A numeric value or vector representing the upper prediction error}
	\item{log.qQE}{A numeric value or vector representing the mass estimate(s) in log10 grams using a quadratic equation}
	\item{qQE}{A numeric value or vector representing the mass estimate(s) in grams}
	\item{lower.qQE}{A numeric value or vector representing the lower prediction error derived for the specific equation by Campione (in review)}
	\item{upper.qQE}{A numeric value or vector representing the upper prediction error}
}
\author{Nicolas E. Campione}
\references{
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
	
	Campione, N. E. (in review) Extrapolation of a universal scaling relationship for estimating body masses in extinct terrestrial vertebrates.
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
QE(HC = dinos$HC, FC = dinos$FC, quadratic = TRUE, data = dinos)

##Combined equation based on the phylogenetically corrected regression, data not specified
QE(HC = dinos$HC, FC = dinos$FC, equation = "phylocor")
}