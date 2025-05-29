\name{break_interaction}
\alias{break_interaction}

\title{
Additive Breakdowns of Variances
}

\description{
Computes three additive breakdown of variances for the interaction between two supplementary variables
}

\usage{
break_interaction(resmca, v1, v2)
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{v1}{factor. The first categorical supplementary variable.}
  \item{v2}{factor. The second categorical supplementary variable.}
}

\details{
This function reproduces the approach developed in Le Roux & Rouanet (2010) in section 4.4, in particular table 4.5.
}

\value{
A data frame
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}


\seealso{
\code{\link{ggadd_interaction}}, \code{\link{ggadd_partial}}
}

\examples{
# specific MCA of Taste example data set
data(Taste)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA",
          "Comedy.NA", "Crime.NA", "Animation.NA", "SciFi.NA", "Love.NA", 
          "Musical.NA")
mca <- speMCA(Taste[,1:11], excl = junk)
# breakdowns of variance 
# for the interaction between Gender and Age
break_interaction(mca, Taste$Gender, Taste$Age)
}

\keyword{ multivariate }
\keyword{ misc }
