\name{CF2004}
\alias{CF2004}
\title{Body Mass Estimates Using Christiansen and Farina 2004}
\description{This function is meant for bipedal mass estimate (in particular theropod dinosaurs) and presently only estimates body mass based on the femoral circumference equation of Christiansen and Farina (2004) derived from volumetric reconstructions of theropod dinosaurs}
\usage{CF2004(FC, data = NULL)}
\arguments{
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{data}{an optional object of class = "data.frame" or "matrix"}
}
\details{If a data object is specified, the mass estiamtes will be added as a column to the data.frame or matrix within that object.}
\value{
	\item{estimate.CF2004}{A numeric value or vector representing the mass estimate(s) in grams}
}
\author{Nicolas E. Campione}
\references{
	Christiansen, P. and Farina, R. A. (2004) Mass prediction in theropod dinosaurs. Historical Biology, 16, 85-92.
}
\seealso{
	\code{\link{cQE}}
	\code{\link{AHR1985}}
  \code{\link{CM1992}}
}
\examples{
##Bipedal dinosaurs
data(dinosbip)
CF2004(dinosbip$FC, data = dinosbip)
}
