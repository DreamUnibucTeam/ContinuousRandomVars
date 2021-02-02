# Cateva exemple de repartitii clasice studiate

########### Cerinta 8 ##########
#' Repartitia uniforma
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție uniformă.
#'
#' @param a Limita inferioară
#' @param b Limita superioară
#'
#' @return Returnează un obiect de tip \code{ContRV} sau o eroare.
#'
#' @details
#' Repartiția uniformă are următoarele caracteristici:
#' \itemize{
#'     \item Parametri: \code{a < b}
#'     \item Domeniul de valori: \code{[a, b]}
#'     \item Densitate: f(x) = 1 / (b - a)
#'     \item Repartiția: F(x) = (x - a) / (b - a)
#'     \item Media: 1/2 * (a + b)
#'     \item Mediana: 1/2 * (a + b)
#'     \item Dispersia: 1/12 * (b - a)^2
#' }
#' Funcția returnează o eroare în cazul în care \code{a >= b}. \cr
#' Toate rezultatele din domeniul de valori au probabilitate egală. Repartiția uniformă
#' este utilizată destul de des, din motiv că probabilități pentru aceasta sunt ușor de calculat.
#' În practică, acestea sunt utilizate destul de des în următoarele aplicații: testarea
#' ipotezelor, finanțe, studii în fizică (de exemplu: emisiile de particule radioactive),
#' în economie (cerere și ofertă).
#'
#' @export
#'
#' @references
#' Continuous uniform distribution: https://en.wikipedia.org/wiki/Continuous_uniform_distribution \cr
#' Uniform Distributions: https://online.stat.psu.edu/stat414/lesson/14/14.6
#'
#' @examples
#' var1 <- uniform(2, 5)
#' # var2 <- uniform(5, 2) # Eroare: 5 > 2
uniform <- function(a, b) {
  if (a >= b) {
    stop("Valoarea lui a nu poate fi mai mare sau egala cu b")
  }

  func <- function(x) {
    rep(1 / (b - a), length(x))
  }

  var <- ContRV(pdf = func, lowerBound = a, upperBound = b)
  return(var)
}


#' Repartitia exponențială
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție exponențială.
#'
#' @param lambda Parametrul repartiției exponențiale (rata)
#'
#' @return Returnează un obiect de tip \code{ContRV} sau o eroare.
#'
#' @details
#' Repartiția exponențială are următoarele caracteristici:
#' \itemize{
#'     \item Parametru: \code{lambda > 0}
#'     \item Domeniul de valori: \code{[0, Inf]}
#'     \item Densitate: f(x) = lambda * exp(-lambda * x)
#'     \item Repartiția: F(x) = 1 - exp^(-(lambda * x))
#'     \item Mean: 1 / lambda
#'     \item Mediana: ln(2) / lambda
#'     \item Dispersia: 1 / lambda^2
#' }
#' Funcția returnează o eroare în cazul în care \code{lambda <= 0} \cr
#' Repartiția exponențială este deseori întâlnită în domenii precum: fizica
#' (timpul de descomunere radioactivă a particulelor), biologia (distanța dintre
#' mutații de ADN). De asemenea, se pot întâlni și în viața de zi cu zi: timpul până
#' la următorul apel telefonic.
#'
#' @export
#'
#' @references
#' Exponential distribution: https://en.wikipedia.org/wiki/Exponential_distribution
#'
#' @examples
#' var1 <- exponential(2)
#' # var2 <- exponential(-1) # Eroare: -1 <= 0
exponential <- function(lambda) {
  if (lambda <= 0) {
    stop("Valoarea lui lambda nu poate fi mai mica sau egala cu 0")
  }

  func <- function(x) {
    lambda * exp(-lambda * x)
  }

  var <- ContRV(pdf = func, lowerBound = 0, upperBound = Inf)
  return(var)
}


#' Repartitia normală
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție normală
#'
#' @param mean Media repartiției normale
#' @param disp Dispersia repartiției normale
#'
#' @return Returnează un obiect de tip \code{ContRV} sau o eroare.
#'
#' @details
#' Repartiția normală are următoarele caracteristici:
#' \itemize{
#'     \item Parametru: \code{mean, disp > 0}
#'     \item Domeniul de valori: \code{R}
#'     \item Densitate: f(x) = 1 / (sd * sqrt(2*pi)) * e^(-(x - mean)^2/(2*sd^2))
#'     \item Repartiția: F(x) = 1/2 * [1 + erf((x - mean) / (sd * sqrt(2))]
#'     \item Media: mean (din parametrul funcției)
#'     \item Mediana: mean (din parametrul funcției)
#'     \item Dispersia: disp (din parametrul funcției)
#' }
#' Funcția returnează o eroare în cazul în care \code{disp <= 0} (dispersia nu poate fi nepozitivă). \cr
#' Repartițiile normale se utilizează în diferite domenii, în special în fizică (de exemplu:
#' funcția de densitate de probabilitate într-un oscilator armonic liniar (cuantic), poziția unei
#' particule care este în proces de difuziune).
#'
#' @export
#'
#' @references
#' Normal distribution: https://en.wikipedia.org/wiki/Normal_distribution
#'
#' @examples
#' var1 <- normal(5, 2)
#' var2 <- normal(0, 1)
#' mean(var1) # 5
#' Var(var2) # 1
normal <- function(mean, disp) {
  if (disp <= 0) {
    stop("Dispersia nu poate fi mai mica sau egala cu 0")
  }

  # Deviatia standard
  sd <- sqrt(disp)
  func <- function(x) {
    1 / (sd * sqrt(2 * pi)) * exp(-(x - mean)^2 / (2 * disp))
  }

  var <- ContRV(pdf = func, lowerBound = -Inf, upperBound = Inf)
  return(var)
}


#' Repartiția Lognormală
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție log-normală.
#'
#' @param mean Media variabilei aleatoare
#' @param disp Dispersia varibilei aleatoare
#'
#' @return Returnează un obiect de tip \code{ContRV} sau o eroare.
#'
#' @details
#' Repartiția uniformă are următoarele caracteristici:
#' \itemize{
#'     \item Parametri: \code{mean, disp > 0}
#'     \item Domeniul de valori: \code{(o, Inf)}
#'     \item Densitate: \eqn{f(x) = 1 / (sd * x * sqrt(2 * pi)) * exp(-((log(x) - mean)^2)/( 2 * disp))}
#'     \item Aplicație: Dacă Variabila aleatoare \eqn{X} are repartiție log-normală, atunci \eqn{Y = ln(X)} este o repartiție normală. Repartiția log-normală se utilizează în simulări care se comportă ca și repartițiile normale, însă nu pot lua valori negative.
#' }
#' Funcția returnează o eroare în cazul în care \eqn{disp <= 0}
#' @export
#'
#' @references
#' Lognormal distribution: https://en.wikipedia.org/wiki/Log-normal_distribution
#'
#' @examples
#' var1 <- logNormal(0, 1)
#' # var2 <- logNormal(0, -100) # Eroare: -100 <= 0
logNormal <- function(mean, disp) {
  if(disp <= 0) {
    stop("Dispersia nu poate fi mai mica sau egala cu 0")
  }

  # Deviatia standard
  sd <- sqrt(disp)
  func <- function(x) {
    1 / (sd * x * sqrt(2 * pi)) * exp(-((log(x) - mean)^2)/( 2 * disp))
  }

  var <- ContRV(pdf = func, lowerBound = 0, upperBound = Inf)
  return(var)
}


#' Repartiția Weibull
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție Weibull.
#'
#' @param beta parametru de formă
#' @param gama parametru de scară
#' @param eta parametru de poziție sau al originii de timp
#'
#' @return Returnează un obiect de tip \code{ContRV}.
#'
#' @details
#' Repartiția Weibull are următoarele caracteristici:
#' \itemize{
#'     \item Parametri: \code{beta > 0, gama, eta > 0}
#'     \item Domeniul de valori: \code{[gama, Inf)}
#'     \item Densitate: \eqn{ f(x) = beta/eta * ((x - gama)/eta)^(beta - 1) * exp(-((x - gama)/eta)^beta)}
#'     \item Aplicatie: Repartiția Weibull este des utilizată în analiza fiabilității unui product.
#' }
#' Funcția returnează o eroare în cazul în care \eqn{beta <= 0} sau \eqn{eta <= 0}
#' @export
#'
#' @references
#' Weibull distribution: https://en.wikipedia.org/wiki/Weibull_distribution
#'
#' @examples
#' var1 <- weibull(3,0,200)
#' # var2 <- weibull(-3,40, 3) # Eroare: -3 <= 0
#' # var3 <- weibull(3,40, -4) # Eroare: -4 <= 0
weibull <- function(beta, gama, eta) {
  if(beta <= 0) {
    stop("Parametrul de forma beta nu poate fi mai mic sau egal cu 0")
  }

  if(eta <= 0) {
    stop("Parametrul de pozitie eta nu poate fi mai mic sau egal cu 0")
  }

  func <- function(x) {
    beta/eta * ((x - gama)/eta)^(beta - 1) * exp(-((x - gama)/eta)^beta)
  }

  var <- ContRV(pdf = func, lowerBound = gama, upperBound = Inf)
}


#' Repartiția Gamma
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție Gamma.
#'
#' @param eta parametru de formă
#' @param lambda repartiție exponențială
#'
#' @return Returnează un obiect de tip \code{ContRV}.
#'
#' @details
#' Repartiția Gamma are următoarele caracteristici:
#' \itemize{
#'     \item Parametri: \code{eta > 0, lambda > 0}
#'     \item Domeniul de valori: \code{(0, Inf)}
#'     \item Densitate: \eqn{ f(x) = (lambda^eta)/gamma(eta) * (x^(eta - 1)) * exp(-lambda * x)}
#'     \item Aplicatie: Repartiția Gamma reprezintă repartiția probabilității entropiei maxime.
#' }
#' Funcția returnează o eroare în cazul în care \eqn{eta <= 0} sau \eqn{lambda <= 0}
#' @export
#'
#' @references
#' Gamma distribution: https://en.wikipedia.org/wiki/Gamma_distribution
#'
#' @examples
#' var1 <- repgamma(9, 2)
#' # var2 <- repgamma(-100, 2) # Eroare: -100 <= 0
#' # var3 <- repgamma(9, -4) # Eroare: -4 <= 0
repgamma <- function(eta, lambda) {
  if(eta <= 0) {
    stop("Parametrul de forma eta nu poate fi mai mic sau egal cu 0")
  }
  if(lambda <= 0) {
    stop("Repartitia exponentiala lambda nu poate fi mai mic sau egal cu 0")
  }

  func <- function(x) {
    (lambda^eta)/gamma(eta) * (x^(eta - 1)) * exp(-lambda * x)
  }

  var <- ContRV(pdf = func, lowerBound = 0, upperBound = Inf)
}


#' Repartiția Student
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție Student.
#'
#' @param niu gradul de libertate
#'
#' @return Returnează un obiect de tip \code{ContRV}.
#'
#' @details
#' Repartiția Student are următoarele caracteristici:
#' \itemize{
#'     \item Parametri: \code{niu > 0}
#'     \item Domeniul de valori: \code{R}
#'     \item Densitate: \eqn{ f(x) = (gamma((niu + 1) / 2)) / (sqrt(pi * niu) * gamma(niu / 2)) * ((1 + (x^2) / niu)^(-(niu + 1)/2))}
#'     \item Aplicatie: Repartiția Student, la fel ca și distribuția normală, are formă de clopot, insă are capetele mai pronunțate, deci e mult mai folositoare pentru modelarea distribuțiilor a căror valori pot cădea mai departe de medie.
#' }
#' Funcția returnează o eroare în cazul în care \eqn{niu <= 0}
#' @export
#'
#' @references
#' Student distribution: https://en.wikipedia.org/wiki/Student%27s_t-distribution
#' @examples
#' var1 <- student(1)
#' # var2 <- student(-1) # Eroare: -1 <= 0
student <- function(niu) {
  if(niu <= 0) {
    stop("Gradul de libertate nu poate fi mai mic sau egal cu 0")
  }
  func <- function(x) {
    (gamma((niu + 1) / 2)) / (sqrt(pi * niu) * gamma(niu / 2)) * ((1 + (x^2) / niu)^(-(niu + 1)/2))
  }

  var <- ContRV(pdf = func, lowerBound = -Inf, upperBound = Inf)
}


#' Repartiția Beta
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție Beta
#'
#' @param a Primul parametru de formă
#' @param b Al doilea parametru de formă
#'
#' @return Returnează un obiect de tip \code{ContRV}.
#'
#' @details
#' Repartiția Beta are următoarele caracteristici:
#' \itemize{
#'     \item Parametri: \code{a > 0, b > 0}
#'     \item Domeniul de valori: \code{(0, 1)}
#'     \item Densitate: \eqn{ f(x) = (x^(a - 1) * (1 - x)^(b - 1))/beta(a, b)}
#'     \item Aplicatie: Repartiția Beta este una potrivită pentru reprezentea comportamentului aleator a procentajelor și proporțiilor.
#' }
#' Funcția returnează o eroare în cazul în care \eqn{a <= 0} sau \eqn{b <= 0}
#' @export
#'
#' @references
#' Beta distribution: https://en.wikipedia.org/wiki/Beta_distribution
#' @examples
#' var1 <- repbeta(2, 5)
#' # var2 <- repbeta(-2, 5) # Eroare: -2 <= 0
#' # var2 <- repbeta(2, -5) # Eroare: -5 <= 0
repbeta <- function(a, b) {
  if(a <= 0) {
    stop("Parametrul de forma a nu poate fi mai mic sau egal cu 0")
  }
  if(b <= 0) {
    stop("Parametrul de forma b nu poate fi mai mic sau egal cu 0")
  }
  func <- function(x) {
    (x^(a - 1) * (1 - x)^(b - 1))/beta(a, b)
  }

  var <- ContRV(pdf = func, lowerBound = 0, upperBound = 1)
}


#' Repartiția Chi
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție Chi
#'
#' @param k Gradul de libertate
#'
#' @return Returnează un obiect de tip \code{ContRV}.
#'
#' @details
#' Repartiția Chi are următoarele caracteristici:
#' \itemize{
#'     \item Parametri: \code{k > 0}
#'     \item Domeniul de valori: \code{[0, Inf)}
#'     \item Densitate: \eqn{ f(x) = (x^(k - 1) * exp( -(x^2) / 2)) / (2^((k / 2) - 1) * gamma(k / 2))}
#'     \item Aplicatie: Repartiția Chi poate reprezenta distribuția distanțelor Euleriene a unui set de puncte distribuite dupa repartiția normala.
#' }
#' Funcția returnează o eroare în cazul în care \eqn{k <= 0}
#' @export
#'
#' @references
#' Chi distribution: https://en.wikipedia.org/wiki/Chi_distribution
#' @examples
#' var1 <- chi(3)
#' # var2 <- chi(-1) # Eroare: -1 <= 0
chi <- function(k){
  if(k <= 0){
    stop("Gradul de libertate k nu poate fi mai mic sau egal cu 0")
  }
  func <- function(x) {
    (x^(k - 1) * exp( -(x^2) / 2)) / (2^((k / 2) - 1) * gamma(k / 2))
  }

  var <- ContRV(pdf = func, lowerBound = 0, upperBound = Inf)
}


#' Repartitia Fisher (F)
#'
#' Funcție care generează o variabilă aleatoare continuă ce are repartiție F
#'
#' @param d1 Gradul de liberate 1
#' @param d2 Gradul de libarate 2
#'
#' @return Returnează un obiect de tip \code{ContRV} sau o eroare.
#'
#' @details
#' Repartiția normală are următoarele caracteristici:
#' \itemize{
#'     \item Parametru: \code{mean, disp > 0}
#'     \item Domeniul de valori: \code{R}
#'     \item Densitate: f(x) = sqrt(((d1*x)^d1 * d2^d2) / ((d1*x + d2)^(d1 + d2))) / (x * beta(d1 / 2, d2 / 2))
#'     \item Media: d2 / (d2 - 2)
#'     \item Dispersia: 2 * d2^2 * (d1 + d2 - 2) / (d1 * (d2 - 2)^2 * (d2 - 4))
#' }
#' Funcția returnează o eroare în cazul în care \code{d1 <= 0} și \code{d2 <= 0}
#' (gradele de liberate nu pot fi nenegative). \cr
#' Un exemplu de utilizare a repartiției Fisher este distrubuția veniturilor. Pentru mai multe detalii,
#' vizitați linkurile din referința.
#'
#' @export
#'
#' @references
#' F-distribution: https://en.wikipedia.org/wiki/F-distribution \cr
#' When to use F-distribution: https://www.dummies.com/education/math/business-statistics/when-to-use-the-f-distribution/
#'
#' @examples
#' var1 <- repf(5, 2)
#' var2 <- repf(1, 1)
repf <- function(d1, d2) {
  if(d1%%1 > 0 || d1 <= 0){
    stop("Parametrul d1 trebuie sa fie Natural nenul")
  }
  if(d2%%1 > 0 || d1 <= 0){
    stop("Parametrul d2 trebuie sa fie Natural nenul")
  }

  func <- function(x) {
    sqrt(((d1*x)^d1 * d2^d2) / ((d1*x + d2)^(d1 + d2))) / (x * beta(d1 / 2, d2 / 2))
  }

  var <- ContRV(pdf = func, lowerBound = 0, upperBound = Inf)
}
