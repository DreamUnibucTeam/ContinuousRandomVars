########## Constante ##########
#' Limita numerică maximă
#'
#' Setează limită numerică maximă constantă
#'
#' Această constantă este specifică acestui proiect
MAX_NUMERIC_LIMIT <- 10^20

#' Limita numerică minimă
#'
#' Setează limită numerică minimă constantă
#'
#' Această constantă este specifică acestui proiect
MIN_NUMERIC_LIMIT <- -10^20

#' Eroarea aboslută
#'
#' Setează o valoare constantă pentru eroarea abosolută
#'
#' Această constantă este specifică acestui proiect
EPSILON <- 10^-7

########## Cerinta 1 ##########
#' Constanta de normalizare a integralei
#'
#' Fiind dată o funcție și limita superioara și inferioară, află constanta de normalizare
#' (egala cu inversa integralei, daca exista si nu este nulă) din această funcție pe intervalul indicat.
#'
#' @param func Funcția pentru care trebuie să aflam constanta de normalizare
#' @param lowerBound Limita inferioară de integrare
#' @param upperBound Limita superioară de integrare
#'
#' @return Returnează un număr care reprezintă constanta de normalizare a funcției sau \code{NULL} în caz de eroare
#'
#' @details
#' Erori la calcularea constantei pot apărea în două cazuri:
#' \itemize{
#'     \item Valoarea integralei este 0 pe acel interval
#'     \item Integrala este divergentă pe acel interval
#' }
#'
#' @importFrom stats integrate
#' @export
#'
#' @examples
#' const <- normalizing_constant(function(x) { 1/2 * x }, -2, 10)
#' K_norm <- normalizing_constant(function(x) { x }, 0, 2)
normalizing_constant <- function(func, lowerBound, upperBound) {
  k_norm <- tryCatch({
    value <- integrate(Vectorize(func), lowerBound, upperBound)$value
    if (abs(value) <= EPSILON) {
      stop("valoarea integralei este 0\n")
    }
    return(1 / value)
  },
  error = function(err_msg) {
    message("Eroare: Constanta de normalizare nu poate fi calculata")
    message(err_msg)
  }
  )
}

#' Normalizarea unei funcții
#'
#' Fiind dată o funcție și limita superioara și inferioară,
#' calculează funcția normalizată (egala cu produsul dintre funcția inițială și
#' constanta de normalizare, dacă există) din această funcție pe intervalul indicat.
#'
#' @param func Funcția pentru care trebuie să aflam funcția normalizată
#' @param lowerBound Limita inferioară de integrare a funcției
#' @param upperBound Limita superioară de integrare a funcției
#'
#' @return Returnează o funcție care reprezintă funcția normalizată sau \code{NULL} în caz de eroare
#'
#' @details
#' Erori la calcularea funcției normalizate pot apărea în două cazuri, când nu putem calcula constanta de normalizare:
#' \itemize{
#'     \item Valoarea integralei este 0 pe acel interval
#'     \item Integrala este divergentă pe acel interval
#' }
#'
#' @export
#'
#' @examples
#' func <- normalize_function(function(x) { 1 / x^2 }, 1, 2)
#' fx <- normalize_function(function(x) { x }, 0, 1)
normalize_function <- function(func, lowerBound, upperBound) {
  k_norm <- normalizing_constant(func, lowerBound, upperBound)
  if (!is.null(k_norm)) {
    function(x) {
      k_norm * func(x)
    }
  }
}


########## Cerinta 2 ##########
#' Verificarea unei funcții dacă este funcție densitate de probabilitate
#'
#' Fiind dată o funcție și limita superioara și inferioară,
#' verifică dacă aceasta este o funcție densitate de probabilitate (pdf), verificând dacă
#' respectă proprietățile acesteia:
#' \itemize{
#'     \item Funcția este nenegativă pe intervalul \code{[lowerBound, upperBound]}
#'     \item Integrala funcției pe acest interval este egală cu 1.
#' }
#'
#'
#' @param func Funcția care trebuie să o verificăm dacă este pdf
#' @param lowerBound Limita inferioară a intervalului specificat (default: \code{-Inf})
#' @param upperBound Limita superioară a intervalului specificat (default: \code{Inf})
#'
#' @return Returnează \code{TRUE} dacă funcția introdusă este pdf, \code{FALSE} dacă nu este
#' și eroare dacă integrala este divergentă
#'
#' @details
#' Erori la verificarea funcției pot apărea în cazul în care integrala este divergentă pe acel interval
#'
#' @export
#' @importFrom stats optimize
#' @examples
#' fun <- function(x) { x }
#' check_if_pdf(fun, 0, 1)   # FALSE
#'
#' fun <- function(x) { 2 * exp(-2*x) }
#' check_if_pdf(fun, 0, Inf)   # TRUE
check_if_pdf <- function(func, lowerBound = -Inf, upperBound = Inf) {
  # Prima conditie - func sa fie pozitiva pe tot intervalul left right
  # Valoarea minima a functiei pe [lowerBound, upperBound] trebuie sa fie >= 0
  # Daca boundurile sunt Infinitul, atunci trebuie sa-i dam intervalului valorile
  # minime si maxime posibile pentru masina curenta

  lowerBoundNonInf <- ifelse(lowerBound == -Inf, MIN_NUMERIC_LIMIT, lowerBound)
  upperBoundNonInf <- ifelse(upperBound == Inf, MAX_NUMERIC_LIMIT, upperBound)
  min_val <- optimize(func, interval=c(lowerBoundNonInf, upperBoundNonInf), maximum = FALSE)$objective

  if (abs(min_val) <= EPSILON || min_val >= 0) {
    # A doua conditie - integrala de la left la right sa dea 1
    # Vectorize - sa fie de aceeasi lungime returnul (pentru a putea lucra cu functii constante)
    value <- tryCatch({
      integrate(Vectorize(func), lowerBound, upperBound)$value
    },
    error = function(err_msg) {
      message("Eroare:")
      message(err_msg)
      return(FALSE)
    }
    )
    return(abs(value - 1) <= EPSILON)
  } else {
    return(FALSE)
  }
}



