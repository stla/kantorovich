\name{kantorovich-package}
\alias{kantorovich-package}
\docType{package}
\title{
Kantorovich Distance Between Probability Measures
}
\description{
Computes the Kantorovich distance between two probability measures on a
finite set, also known as the earth mover's distance. The Kantorovich
distance is not a "unique" distance: it is defined by a given distance on
the two finite sets (generally equal). Note that the default distance is the
0-1 distance and with this choice the Kantorovich computation is totally
useless (see the vignette). Computing the Kantoroich distance is a linear
programming problem, and several methods are provided in the package. In
particular there is an exact method available when the probability weights
are rational numbers and when the distances are rational numbers as well.
A benchmark suggests that the faster methods are those using the 'CVXR'
package.

To learn more, start with the vignettes:
\code{browseVignettes(package="kantorovich")}.

If you encounter a bug, or if you have a suggestion to improve the package, please file an issue on the Github repo \url{https://github.com/stla/kantorovich}.
}
\details{
\tabular{ll}{
Package: \tab kantorovich\cr
Type: \tab Package\cr
Version: \tab 3.0.2\cr
Date: \tab 2023-08-22\cr
License: \tab GPL-3\cr
}
}

\author{
Stéphane Laurent
}
