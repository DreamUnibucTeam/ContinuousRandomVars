% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RepartiiClasice.R
\name{repbeta}
\alias{repbeta}
\title{Repartiția Beta}
\usage{
repbeta(a, b)
}
\arguments{
\item{a}{Primul parametru de formă}

\item{b}{Al doilea parametru de formă}
}
\value{
Returnează un obiect de tip \code{ContRV}.
}
\description{
Funcție care generează o variabilă aleatoare continuă ce are repartiție Beta
}
\details{
Repartiția Beta are următoarele caracteristici:
\itemize{
    \item Parametri: \code{a > 0, b > 0}
    \item Domeniul de valori: \code{(0, 1)}
    \item Densitate: \eqn{ f(x) = (x^(a - 1) * (1 - x)^(b - 1))/beta(a, b)}
    \item Aplicatie: Repartiția Beta este una potrivită pentru reprezentea comportamentului aleator a procentajelor și proporțiilor.
}
Funcția returnează o eroare în cazul în care \eqn{a <= 0} sau \eqn{b <= 0}
}
\examples{
var1 <- repbeta(2, 5)
# var2 <- repbeta(-2, 5) # Eroare: -2 <= 0
# var2 <- repbeta(2, -5) # Eroare: -5 <= 0
}
\references{
Beta distribution: https://en.wikipedia.org/wiki/Beta_distribution
}
