\name{MCF2004}
\alias{MCF2004}
\title{Body Mass Estimates Using Mazzetta et al 2004}
\description{This function is meant for quadruped mass estimate (in particularly sauropod dinosaurs) and presently only estimates body mass based on the femoral circumference equation of Mazzetta et al (2004) derived from volumetric reconstructions of sauropod dinosaurs}
\usage{MCF2004(FC, data = NULL)}
\arguments{
  \item{FC}{a value or vector representing femoral circumference (in mm)}
  \item{data}{an optional object of class = "data.frame" or "matrix"}
}
\details{If a data object is specified, the mass estiamtes will be added as a column to the data.frame or matrix within that object.}
\value{
	\item{estimate.MCF2004}{A numeric value or vector representing the mass estimate(s) in grams}
}
\author{Nicolas E. Campione}
\references{
	Mazzetta, G. V., Christiansen, P. and Farina, R. A. (2004) Giants and bizzares: body size of some southern South American Cretaceous dinosaurs. Historical Biology, 16, 71-83.
}
\seealso{
	\code{\link{QE}}
  \code{\link{MR}}
	\code{\link{AHR1985}}
}

\examples{
##No current examples
}