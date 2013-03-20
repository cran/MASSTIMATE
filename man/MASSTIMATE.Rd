\name{MASSTIMATE}
\alias{MASSTIMATE}
\title{Body Mass Estimates Using Campione and Evans 2012}
\description{This function presents equations from Campione and Evans (2012) and Campione et al (in prep) for esimating body mass in terretrial vertebartes using humeral and/or femoral circumferences}
\usage{MASSTIMATE(HC=NULL, FC, equation=c("quad.raw", "quad.phylocor", "mult.raw", "mult.phylocor", "bip.raw", "bip.phylocor"), data=NULL)}
\arguments{
  \item{HC}{an optional value or vector representing humeral circumference (in mm)}
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{equation}{desired estimation equation. Six possible choices (See Details)}
  \item{data}{an optional object of class = "data.frame" or "matrix"}
}
\details{
The function includes six different equations. equation = "quad.raw" is meant for quadrupeds and thus requires both HC and FC to be specified; it is based on the raw extant data from Campione and Evans (2012). equation = "quad.phylocor" includes a phylogenetic correction to the regression coefficients based on a phylogenetic generalized least squares regression model. equation = "mult.raw" or "mult.phylocor" follow the same explanations stated for "quad.raw" and "quad.phylocor" with the excpetion that HC and FC are treated separately in a multiple regresssion instead of combining them (see Campione and Evans 2012 for more details).

Two bipedal equations are included based on a mathematical correction of the combined humeral and femoral circumference quadrupedal equation. As explained above, these are expressed both in terms of the raw (equation = "bip.raw") and phylogenetically corrected (equation = "bip.phylocor") regression equations.

 If a data object is specified, the mass estiamtes and additional results will be added as columns to the data.frame or matrix within that object.
}
\value{
Four numeric values or columns are returned:
	\item{log.masstimate}{A numeric value or vector representing the mass estimate(s) in log10 grams}
	\item{masstimate}{A numeric value or vector representing the mass estimate(s) in grams}
	\item{upper}{A numeric value or vector representing the upper prediction error based on that derived for the specific equation by Campione and Evans (2012)}
	\item{lower}{A numeric value or vector representing the lower prediction error}
}
\author{Nicolas E. Campione}
\references{
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
	Campione, N. E., Evans, D. C., Brown, C. M. and Carrano, M. T. (in prep) A mathematical correction factor for estimating body mass in terrestrial bipedal tetrapods.
}
\examples{
##Dinosaur data from Campione and Evans (2012) for quadrupedal dinosaurs
data(dinos)

##Combined equation based on the raw regression
dinos.raw<-MASSTIMATE(dinos[,2], dinos[,3], equation = "quad.raw", data = dinos)

##Combined equation based on the phylogenetically corrected regression, data not specified
dinos.phylocor<-MASSTIMATE(dinos$HC, dinos$FC, equation = "quad.phylocor")
}