\name{runDali}
\alias{runDali}
\title{Sets up directory structure for a new Borges site
With the followind structure: ...}
\usage{
runDali(dp, vizId, response = NULL, outputFile = NULL)
}
\arguments{
  \item{site}{path to the directory you want to set up}
}
\value{
logical TRUE if successful, FALSE if directory already
exists
}
\description{
Sets up the directory structure for a new static site
}
\examples{
\dontrun{
mysite <- "/home/david/github/mysite"
skeleton(mysite)
}
}

