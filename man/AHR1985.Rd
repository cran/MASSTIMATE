\name{AHR1985}
\alias{AHR1985}
\title{Body Mass Estimates Using Anderson el at 1985}
\description{This function estimates body mass based on humeral and/or femoral circumferences using the quadrupedal and bipedal equations of Anderson et al. (1985)}
\usage{
AHR1985(HC=NULL, FC, equation=c("bip", "quad"), data=NULL)
}
\arguments{
  \item{HC}{an optional value or vector representing humeral circumference (in mm)}
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{equation}{desired estimation equation. Two possible choices: "quad", "bip"}
  \item{data}{an optional object of class = "data.frame" or "matrix"}
}
\details{If equation = "bip" then only femoral circumference is needed. Both humeral and femoral circumference are needed to estimate body mass of a quadruped (equation = "quad"). If a data object is specified, the mass estiamtes will be added as a column to the data.frame or matrix within that object.}
\value{
	\item{estimate.AHR1985}{A numeric value or vector representing the mass estimate(s) in grams}
}
\author{Nicolas E. Campione}
\references{
	Anderson, J. F., Hall-Martin, A. and Russell, D.A. (1985) Long-bone circumference and weight in mammals, birds and dinosaurs. Journal of the Zoological Society of London A, 207, 53-61.
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
}
\seealso{
	\code{\link{MASSTIMATE}}
	\code{\link{CM1992}}
}
\examples{
##Dinosaur data from Campione and Evans (2012) for quadrupedal dinosaurs
data(dinos)
dinos<-AHR1985(dinos[,2], dinos[,3], equation = "quad", data = dinos)
dinos
}