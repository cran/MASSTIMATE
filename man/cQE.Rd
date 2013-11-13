\name{cQE}
\alias{cQE}
\title{Body Mass Estimates Using Campione et al. (in prep)}
\description{This function presents equations from Campione et al. (in prep) for esimating body mass in bipeds using minimum femoral circumference}
\usage{cQE(FC, equation = c("raw","phylocor"), cor = 2, data = NULL)}
\arguments{
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{equation}{desired estimation equation. Two possible choices (See Details)}
  \item{cor}{correction factor to be used. The default is 2, as per Campione et al. (in prep)}
  \item{data}{an optional object of class = "data.frame" or "matrix"}
}
\details{
The function includes two different equations. equation = "raw" applies the correction factor derived in Campione et al. (in prep) to the raw (non-phylogenetically corrected) bivariate regression equation from Campione and Evans (2012). equation = "phylocor" applies the same correction factor to the phylogenetically corrected equation presented in the same study.

If a data object is specified, the mass estiamtes and additional results will be added as columns to the data.frame or matrix within that object.
}
\value{
Four numeric values or columns are returned:
	\item{log.masstimate}{A numeric value or vector representing the mass estimate(s) in log10 grams}
	\item{masstimate}{A numeric value or vector representing the mass estimate(s) in grams}
	\item{upper}{A numeric value or vector representing the upper prediction error based on that derived for the specific equation in Campione and Evans (2012)}
	\item{lower}{A numeric value or vector representing the lower prediction error}
}
\author{Nicolas E. Campione}
\references{
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
  
	Campione, N. E., Evans, D. C., Brown, C. M. and Carrano, M. T. (in prep) A mathematically-derived equation for estimating body mass in terrestrial bipedal tetrapods.
}
\seealso{
  \code{\link{QE}}
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
cQE(dinosbip$FC[sue], equation = "raw", cor = 1.815) #based on eccentricity of femur

##Estimates for the sample of bipedal dinosaurs using phylogeneteically corrected formula
dinos.phylocor<-cQE(dinosbip$FC, equation = "phylocor", cor = dinosbip$cor, data = dinosbip)
}