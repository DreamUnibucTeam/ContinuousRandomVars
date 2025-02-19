% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ContRV.R
\name{plot_RV,ContRV,numeric,numeric,character-method}
\alias{plot_RV,ContRV,numeric,numeric,character-method}
\title{Generarea graficelor funcției densitate de probabilitate și funcției de repartiție a unei variabile
aleatoare continue}
\usage{
\S4method{plot_RV}{ContRV,numeric,numeric,character}(crv, x_int, y_int, color)
}
\arguments{
\item{crv}{Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare}

\item{x_int}{Intervalul de valori pentru argumentul functiei}

\item{y_int}{Intervalul de valori pentru valorile functiei densitate și repartie}

\item{color}{Culoarea graficului}
}
\value{
Returnează graficele funcțiilor densitate de probabilitate și repartiție a unui obiect de tip ContRV.
}
\description{
Metodă a clasei \code{ContRV} care genereză graficele funcției densitate de probabilitate și
funcției de repartiție a unei variabile aleatoare continue
}
\examples{
plot_RV(uniform(1, 5), c(-5, 10), c(0, 1), "blue")
}
