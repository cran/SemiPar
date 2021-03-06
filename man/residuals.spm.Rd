\name{residuals.spm}
\alias{residuals.spm}
\title{Residuals for semiparametric regression.}
\description{Extracts residuals from a semiparametric regression fit object.}
\usage{
\method{residuals}{spm}(object,...)
}
\arguments{ 
  
\item{object}{a fitted \code{spm} object as produced by \code{spm()}.}

\item{...}{other possible arguments.}

}
\details{Extracts residuals from a semiparametric regression fit object.
The residuals are defined to be the difference between the response
variable and the fitted values.}

\value{The vector of residuals.}

\references{
     Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}


     Ganguli, B. and Wand, M.P. (2005)\cr
     \emph{SemiPar 1.0 Users' Manual}.\cr
     \url{http://matt-wand.utsacademics.info/SPmanu.pdf}
}
\author{M.P. Wand 
\email{mwand@uow.edu.au}
(other contributors listed in SemiPar Users' Manual).}

\seealso{\code{\link{spm}}
\code{\link{plot.spm}} 
\code{\link{lines.spm}}
\code{\link{predict.spm}}
\code{\link{summary.spm}}
\code{\link{fitted.spm}}}

\examples{
library(SemiPar)
data(fossil)
attach(fossil)
fit <- spm(strontium.ratio~f(age))
plot(age,residuals(fit))
abline(0,0)
}
\keyword{models} \keyword{smooth} \keyword{regression} 


