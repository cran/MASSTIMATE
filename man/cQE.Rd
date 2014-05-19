\name{cQE}
\alias{cQE}
\title{Body Mass Estimates Using Campione et al. (in review)}
\description{This function presents equations from Campione et al. (in review) for esimating body mass in bipeds using minimum femoral circumference and based on a correction of the quadrupedal equations from Campione and Evans (2012)}
\usage{cQE(FC, equation = c("raw", "phylocor"), cor = 2, data = NULL)}
\arguments{
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{equation}{desired estimation equation. Two possible choices (See Details)}
  \item{cor}{correction factor to be used (\eqn{\alpha{^2}}{\alpha^2}). The default is 2, as per Campione et al. (in review)}
  \item{data}{an optional object of \code{class = "data.frame"} or \code{class = "matrix"}}
}
\details{
The function includes two different equations. \code{equation = "raw"} applies the correction factor derived in Campione et al. (in review) to the raw (non-phylogenetically corrected) bivariate regression equation from Campione and Evans (2012). \code{equation = "phylocor"} applies the same correction factor to the phylogenetically corrected equation presented in the same study.

\code{cor = 2} refers to the correction factor (\eqn{\alpha{^2}}{\alpha^2}) to be used. The default (\code{cor = 2}) refers the initial derivation in Campione et al. (in review), however, this value can be modified based on the level of eccentricity of the femur. A set of values for \code{cor} corresponding to set eccentricities can be found in table S2 of Campione et al. (in review).

If a data object is specified, the mass estiamtes and additional results will be added as columns to the data.frame or matrix within that object.
}
\value{
Four numeric values or columns are returned:
	\item{log.cQE}{A numeric value or vector representing the mass estimate(s) in log10 grams}
	\item{cQE}{A numeric value or vector representing the mass estimate(s) in grams}
	\item{upper.cQE}{A numeric value or vector representing the upper prediction error based on that derived for the specific equation in Campione and Evans (2012)}
	\item{lower.cQE}{A numeric value or vector representing the lower prediction error}
}
\author{Nicolas E. Campione}
\references{
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
  
	Campione, N. E., Evans, D. C., Brown, C. M. and Carrano, M. T. (in review) Body mass estimation in non-avian bipeds using a theoretical conversion to quadruped stylopodial proportions. Methods in Ecology and Evolution
}
\seealso{
  \code{\link{QE}}
  \code{\link{bipeds}}
  \code{\link{AHR1985}}
  \code{\link{CF2004}}
	\code{\link{CM1992}}
}
\examples{
##Bipedal dinosaurs
data(dinosbip)

#Estimates for Tyrannosaurus (FMNH PR 2081 "Sue")
sue<-which(dinosbip$Taxon=="TyrannosaurusFMNH2081")
cQE(dinosbip$FC[sue], equation = "raw", cor = 2) #default correction factor
cQE(dinosbip$FC[sue], equation = "raw", cor = 1.815) #based on eccentricity of the femur

##Estimates for the sample of bipedal dinosaurs using phylogeneteically corrected formula
cQE(dinosbip$FC, equation = "phylocor", cor = dinosbip$cor, data = dinosbip)
}