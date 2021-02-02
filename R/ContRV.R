########## Cerinta 3 ##########
# This is a class for the Continuous Random Variable object
#' Variabilă aleatoare continuă
#'
#' Clasă pentru variabile aleatoare continue. Pentru generarea unui obiect de tip
#' \code{ContRV}, e nevoie de o funcție densitate de probabilitate și limitele superioare și inferioare
#' ale funcției
#'
#' @slot pdf Funcție densitate de probabilitate
#' @slot lowerBound Limita inferioară a intervalului specificat
#' @slot upperBound Limita superioară a intervalului specificat
#'
#' @return Returnează un obiect de tip variabilă aleatoare continuă sau eroare.
#'
#' @details
#' Erori la crearea unui obiect pot apărea în cazul în care funcția introdusă nu este funcție densitate
#' de probabilitate
#'
#' @export ContRV
#' @exportClass ContRV
#' @import methods
#' @examples
#' var1 <- ContRV(pdf = function(x) { 1 / 3 }, lowerBound = 3, upperBound = 6)
#' crv <- ContRV(pdf = function(x) { 2 * exp(-2 * x) }, lowerBound = 0, upperBound = Inf)
ContRV <- setClass(
  "ContRV",
  slots = list(
    pdf = "function",
    lowerBound = "numeric",
    upperBound = "numeric"),

  validity = function(object) {
    if(check_if_pdf(object@pdf, object@lowerBound, object@upperBound))
      TRUE
    else {
      stop("Given function is not a valid pdf")
    }
  }
)


# Metode specifice pentru clasa ContRV
#' Funcția generică cdf
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#'
#' @return Returnează o funcție care reprezintă cdf-ul obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "cdf",
  def = function(crv) { standardGeneric("cdf") },
  valueClass = "function"
)

#' Generarea CDF-ului unei variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care genereză funcția de repartiție (funcția
#' de distribuție cumulativă, cdf) din funcția densitate de probabilitate a
#' variabilei aleatoare continue.
#'
#' @param crv Obiect de tip variabilă aleatoare continuă
#'
#' @return Returnează o funcție care reprezintă cdf-ul obiectului trimis ca parametru.
#'
#' @details
#' Pentru utilizare corectă în unele cazuri trebuie utilizată funcția Vectorize asupra funcției cdf obținute
#'
#' @export
#' @examples
#' var1 <- ContRV(pdf = function(x) { 1 / 3 }, lowerBound = 3, upperBound = 6)
#' var1_cdf <- cdf(var1)
#'
#' crv <- ContRV(pdf = function(x) { 2 * exp(-2 * x) }, lowerBound = 0, upperBound = Inf)
#' crv_cdf <- cdf(crv)
setMethod(
  f = "cdf",
  signature = c("ContRV"),
  definition = function(crv) {
    cdf <- function(x) {
      if (x <= crv@lowerBound) {
        return(0)
      } else if (x >= crv@upperBound) {
        return(1)
      } else {
        return(integrate(Vectorize(crv@pdf), crv@lowerBound, x)$value)
      }
    }
    return(cdf)
  }
)


########### Cerinta 4 ##########
# Metode specifice pentru clasa ContRV
#' Funcția generică plot.RV
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param x_int Intervalul de valori pentru argumentul functiei
#' @param y_int Intervalul de valori pentru valorile functiei densitate și repartie
#' @param color Culoarea graficului
#'
#' @return Returnează graficul funcției densitate de probabilitate și funcției de repartiție.
#'
#' @export
setGeneric(
  name = "plot_RV",
  def = function(crv, x_int, y_int, color) { standardGeneric("plot_RV") }
)

#' Generarea graficelor funcției densitate de probabilitate și funcției de repartiție a unei variabile
#' aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care genereză graficele funcției densitate de probabilitate și
#' funcției de repartiție a unei variabile aleatoare continue
#'
#' @param crv Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param x_int Intervalul de valori pentru argumentul functiei
#' @param y_int Intervalul de valori pentru valorile functiei densitate și repartie
#' @param color Culoarea graficului
#'
#' @return Returnează graficele funcțiilor densitate de probabilitate și repartiție a unui obiect de tip ContRV.
#'
#' @importFrom graphics par
#' @export
#' @examples
#' plot_RV(uniform(1, 5), c(-5, 10), c(0, 1), "blue")
setMethod(
  f = "plot_RV",
  signature = c("ContRV", "numeric", "numeric", "character"),
  definition = function(crv, x_int, y_int, color) {
    PDF <- Vectorize(crv@pdf)
    CDF <- Vectorize(cdf(crv))

    # Ploting the PDF and CDF
    par(mfrow=c(1,2))
    plot(PDF, xlim = x_int, ylim = y_int, col = c(color), main = "Functia densitate de probabilitate",
         xlab="x", ylab="PDF(x)")
    plot(CDF, xlim = x_int, ylim = y_int, col = c(color), main = "Functia de repartitie",
         xlab="x", ylab="CDF(x)")
    par(mfrow=c(1,1))
  }
)

#' Generarea graficelor funcției densitate de probabilitate și funcției de repartiție a unei repartiții
#' cunoscute
#'
#' Funcție care genereză graficele funcției densitate de probabilitate și
#' funcției de repartiție a unei repartiții cunoscute, trimise ca parametru.
#'
#' @param name Numele repartiției solicitate pentru afișarea graficului
#' @param x_int Intervalul de valori pentru argumentul repartiției
#' @param y_int Intervalul de valori pentru valorile functiei densitate și repartie
#' @param ... Parametri pentru repartiția solicitată
#'
#' @return Returnează graficele funcțiilor densitate de probabilitate și repartiție a unei repartiții cunoscute.
#'
#' @export
#' @examples
#' plot_distribution("uniform", x_int = c(0, 3), y_int = c(0, 2), a = 1, b = 2)
#' plot_distribution("exponential", x_int = c(0, 3), y_int = c(0, 2), lambda = 2)
#' plot_distribution("normal", x_int = c(-1, 10), y_int = c(0, 1), mean = 5, disp = 2)
#' plot_distribution("f", x_int = c(0, 5), y_int = c(0, 1), 5, 2)
plot_distribution <- function(name, x_int, y_int, ...) {
  crv <- NULL
  if (name == "uniform") {
    num_args <- length(list(...))
    if (num_args != 2)
      stop("Repartitia uniforma are nevoie de 2 argumente: a si b")
    crv <- uniform(...)
  } else if (name == "exponential") {
    num_args <- length(list(...))
    if (num_args != 1)
      stop("Repartitia exponentiala are nevoie de 1 argument: lambda")
    crv <- exponential(...)
  } else if (name == "normal") {
    num_args <- length(list(...))
    if (num_args != 2)
      stop("Repartitia normala are nevoie de 2 argumente: media si dispersia")
    crv <- normal(...)
  } else if (name == "lognormal") {
    num_args <- length(list(...))
    if (num_args != 2)
      stop("Repartitia lognormala are nevoie de 2 argumente: media si dispersia")
    crv <- logNormal(...)
  } else if (name == "weibull") {
    num_args <- length(list(...))
    if (num_args != 3)
      stop("Repartitia weibull are nevoie de 3 argumente: beta, gamma, eta")
    crv <- weibull(...)
  } else if (name == "gamma") {
    num_args <- length(list(...))
    if (num_args != 2)
      stop("Repartitia gamma are nevoie de 2 argumente: niu si lambda")
    crv <- repgamma(...)
  } else if (name == "beta") {
    num_args <- length(list(...))
    if (num_args != 2)
      stop("Repartitia beta are nevoie de 2 argumente: alfa si beta")
    crv <- repbeta(...)
  } else if (name == "f") {
    num_args <- length(list(...))
    if (num_args != 2)
      stop("Repartitia F are nevoie de 2 argumente: niu1 si niu2")
    crv <- repf(...)
  } else if (name == "student") {
    num_args <- length(list(...))
    if (num_args != 1)
      stop("Repartitia Student are nevoie de 1 argument: niu")
    crv <- student(...)
  } else {
    par(mfrow=c(1,1))
    stop("Nu exista o repartitie clasica cu aceasta denumire")
  }
  plot_RV(crv, x_int, y_int, "blue")
  par(mfrow=c(1,1))
}


########### Cerinta 5 ##########
#' Media unei variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează media variabilei aleatoare continue
#' primită ca parametru.
#'
#' @param x Obiect de tip variabilă aleatoare continuă
#'
#' @return Returnează o valoare numerică, ce reprezintă media variabilei aleatoare continue.
#'
#' @details
#' Utilizează integrarea pentru a calcula valoarea mediei variabilei aleatoare.
#'
#' @export
#' @examples
#' var1 <- ContRV(pdf = function(x) { 1 / 3 }, lowerBound = 3, upperBound = 6)
#' mean(var1)  # 4.5
#'
#' norm_5_4 = function(x) { 1 / (2 * sqrt(2 * pi)) * exp(-(x - 5)^2 / (2 * 4)) }
#' crv <- ContRV(pdf = norm_5_4, lowerBound = -Inf, upperBound = Inf)
#' mean(crv) # 5
setMethod(
  f = "mean",
  signature = c("ContRV"),
  definition = function(x) {
    # Parametrul trebuie sa fie x, sa corespunda cu definitia generica a functiei mean,
    # x in aceasta functie - obiectul
    x_func = function(new_x){
      new_x * x@pdf(new_x)
    }

    return(integrate(x_func, x@lowerBound, x@upperBound)$value)
  }
)

#' Funcția generică Var
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#'
#' @return Returnează o valoare care reprezintă varianța obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "Var",
  def = function(crv) { standardGeneric("Var") },
  valueClass = "numeric"
)

#' Dispersia unei variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează dispersia variabilei aleatoare continue
#' primită ca parametru.
#'
#' @param crv Obiect de tip variabilă aleatoare continuă
#'
#' @return Returnează o valoare numerică, ce reprezintă varianța variabilei aleatoare continue.
#'
#' @details
#' Utilizează integrarea pentru a calcula valoarea varianței variabilei aleatoare.
#'
#' @export
#' @examples
#' var1 <- ContRV(pdf = function(x) { 1 / 3 }, lowerBound = 3, upperBound = 6)
#' Var(var1)  # 0.75
#'
#' norm_5_4 = function(x) { 1 / (2 * sqrt(2 * pi)) * exp(-(x - 5)^2 / (2 * 4)) }
#' crv <- ContRV(pdf = norm_5_4, lowerBound = -Inf, upperBound = Inf)
#' Var(crv) # 4
setMethod(
  f = "Var",
  signature = c("ContRV"),
  definition = function(crv) {
    new_f = function(x){
      (x - mean(crv))^2 * crv@pdf(x)
    }
    return(integrate(new_f, crv@lowerBound, crv@upperBound)$value)
  }
)

#' Funcția generică moment_centrat
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param order Numarul de ordine al momentului centrat
#'
#' @return Returnează o valoare care reprezeintă momentul centrat de ordin order.
#'
#' @export
setGeneric(
  name = "moment_centrat",
  def = function(crv, order) { standardGeneric("moment_centrat") },
  valueClass = "numeric"
)

#' Momentul centrat al unei variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează momentul centrat
#' de ordin m al unei variabile aleatoare continue
#'
#' @param crv Obiect de tip variabilă aleatoare continuă
#' @param order Numarul de ordine al momentului centrat
#'
#' @return Returnează o valoare care reprezeintă momentul centrat de ordin order.
#'
#' @details
#' Utilizează integrarea pentru a calcula valoarea momentului centrat al variabilei aleatoare.
#'
#' @export
#' @examples
#' crv <- normal(5, 4)
#' moment_centrat(crv, 2) # 4 - Varianta
#' moment_centrat(crv, 4) # 48
setMethod(
  f = "moment_centrat",
  signature = c("ContRV", "numeric"),
  definition = function(crv, order) {
    if(order > 4 || order%%1 > 0 || order < 0){
      stop("Ordinul este mai mare ca 4 sau nu e un numar natural")
    }

    curr_mean <- mean(crv)

    var_function = function(x){
      ((x - curr_mean)^order) * crv@pdf(x)
    }
    value <- tryCatch({
        integrate(var_function, crv@lowerBound, crv@upperBound)$value
      },
      error = function(err_msg) {
        message("Eroare:")
        message("Momentul centrat de gradul introdus nu exista")
        return(NULL)
      }
    )
    return(value)
  }
)


#' Funcția generică moment_initial
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param order Numarul de ordine al momentului centrat
#'
#' @return Returnează o valoare care reprezeintă momentul initial de ordin order.
#'
#' @export
setGeneric(
  name = "moment_initial",
  def = function(crv, order) { standardGeneric("moment_initial") },
  valueClass = "numeric"
)

#' Momentul initial al unei variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează momentul initial
#' de ordin m al unei variabile aleatoare continue
#'
#' @param crv Obiect de tip variabilă aleatoare continuă
#' @param order Numarul de ordine al momentului centrat
#'
#' @return Returnează o valoare care reprezeintă momentul initial de ordin order.
#'
#' @details
#' Utilizează integrarea pentru a calcula valoarea momentului initial al variabilei aleatoare.
#'
#' @export
#' @examples
#' crv <- normal(5, 4)
#' moment_initial(crv, 2) # 29
#' moment_initial(crv, 4) # 1273
setMethod(
  f = "moment_initial",
  signature = c("ContRV", "numeric"),
  definition = function(crv, order) {
    if(order > 4 || order%%1 > 0 || order < 0){
      stop("Ordinul este mai mare ca 4 sau nu e un numar natural")
    }

    var_function <- function(x){
        (x^order) * crv@pdf(x)
    }

    value <- tryCatch({
        integrate(var_function, crv@lowerBound, crv@upperBound)$value
      },
      error = function(err_msg) {
        message("Eroare:")
        message("Momentul initial de gradul introdus nu exista")
        return(NULL)
      }
    )
    return(value)
  }
)

########## Cerinta 6 ##########
#' Funcție generică mean_crv_func
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv Obiect de tip variabilă aleatoare continuă
#' @param g Funcția continuă care primește ca argument variabila aleatoare continuă
#'
#' @return Returnează o valoare numerică, ce reprezintă media variabilei aleatoare continue obținute.
#'
#' @export
setGeneric(
  name = "mean_crv_func",
  def = function(crv, g) { standardGeneric("mean_crv_func") },
  valueClass = "numeric"
)


#' Media unei variabile aleatoare continue, asupra căreia este aplicată o funcție
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează media variabilei aleatoare continue,
#' asupra căreia este aplicată o funție continuă.
#'
#' @param crv Obiect de tip variabilă aleatoare continuă
#' @param g Funcția continuă care primește ca argument variabila aleatoare continuă
#'
#' @return Returnează o valoare numerică, ce reprezintă media variabilei aleatoare continue obținute.
#'
#' @details
#' Utilizează integrarea pentru a calcula valoarea mediei variabilei aleatoare obținute.
#'
#' @export
#' @examples
#' var1 <- ContRV(pdf = function(x) { 2 * exp(-2 * x) }, lowerBound = 0, upperBound = Inf)
#' mean_crv_func(var1, function(x){ x ^ 2 })
setMethod(
  f = "mean_crv_func",
  signature = c("ContRV", "function"),
  definition = function(crv, g) {

    g_func = function(x){
      crv@pdf(x) * g(x)
    }

    return(integrate(g_func, crv@lowerBound, crv@upperBound)$value)
  }
)

#' Funcția generică var_crv_func
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param g Funcția continuă care primește ca argument variabila aleatoare continuă
#'
#' @return Returnează o valoare care reprezintă varianța obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "var_crv_func",
  def = function(crv, g) { standardGeneric("var_crv_func") },
  valueClass = "numeric"
)


#' Dispersia unei variabile aleatoare continue, asupra căreia este aplicată o funcție
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează varianța variabilei aleatoare continue,
#' asupra căreia este aplicată o funție continuă primită ca parametru.
#'
#' @param crv Obiect de tip variabilă aleatoare continuă
#' @param g Funcția continuă care primește ca argument variabila aleatoare continuă
#'
#' @return Returnează o valoare numerică, ce reprezintă dispersia variabilei aleatoare continue obținute.
#'
#' @details
#' Utilizează formula varianței pentru a calcula valoarea dispersiei variabilei aleatoare obținute.
#'
#' @export
#' @examples
#' var1 <- ContRV(pdf = function(x) { 2 * exp(-2 * x) }, lowerBound = 0, upperBound = Inf)
#' var_crv_func(var1, function(x){ x ^ 2 })
setMethod(
  f = "var_crv_func",
  signature = c("ContRV", "function"),
  definition = function(crv, g) {
    g_2 = function(x){
      g(x) * g(x)
    }
    return(mean_crv_func(crv, g_2)- (mean_crv_func(crv, g))^2)
  }
)


########## Cerinta 9 ##########
#' Funcția generică generate_crv
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv Funcția de repartiție a unei variabile aleatoare continue
#' @param n Numarul de valori pe care dorim să le generăm
#'
#' @export
setGeneric(
  name = "generate_crv",
  def = function(crv, n) { standardGeneric("generate_crv") }
)

# Functia primeste ca parametrii repartitia unei variabile aleatoare continue si numarul valorilor care vor fi generate
#' Generarea valorilor dintr-o repartiție de variabile aleatoare continue
#'
#' Funcție care generează n valori dintr-o repartiție de variabile aleatoare continue, utilizând simularea.
#'
#' @param crv Obiectul de tip variabilă aleatoare continuă
#' @param n Numarul de valori pe care dorim să le generăm
#'
#' @return Returnează cele n valori generate.
#'
#' @details
#' Utilizează simularea pentru generarea valorilor.
#'
#' @importFrom GoFKernel inverse
#' @importFrom stats runif
#' @export
#' @examples
#' generate_crv(exponential(4), 10)
#' generate_crv(uniform(1, 5), 15)
#' generate_crv(normal(0, 1), 20)
setMethod(
  f = "generate_crv",
  signature = c("ContRV", "numeric"),
  definition = function(crv, n) {
    # Obtinem inversa functiei de repartie pe intervalul [lower, upper]
    cdf <- cdf(crv)
    cdf_inv<- inverse(cdf, lower = crv@lowerBound, upper = crv@upperBound)

    # Obtinem n valori din distributia uniforma si le aplicam inversa functiei de repartitie
    res <- sapply(runif(n), cdf_inv)
    return(res)
  }
)


########## Cerinta 10 ##########
#' Funcția generică Cov
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o valoare care reprezintă covarianța obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "Cov",
  def = function(crv1, crv2, dens) { standardGeneric("Cov") },
  valueClass = "numeric"
)

#' Covarianța a doua variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează covarianța a două variabile
#' aleatoare continue, utilizând funcția densitate comună acelor două.
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o valoare numerică, ce reprezintă covarianța variabilelor aleatoare continue.
#'
#' @details
#' Utilizează integrala dublă pentru calcularea covarianței celor două variabile aleatoare.
#'
#' @importFrom pracma integral2
#' @export
#' @examples
#' obj1 <- ContRV(pdf = function(x) { x }, lowerBound = 0, upperBound = sqrt(2))
#' obj2 <- ContRV(pdf = function(x) { 1 }, lowerBound = 0, upperBound = 1)
#' Cov(obj1, obj2, function(x, y) 4*x*y)
setMethod(
  f = "Cov",
  signature = c("ContRV", "ContRV", "function"),
  definition = function(crv1, crv2, dens) {
    x_y_dens <- function(x, y) {
      x * y * dens(x, y)
    }
    integrala <- integral2(x_y_dens, crv1@lowerBound, crv1@upperBound, crv2@lowerBound, crv2@upperBound)$Q
    return (integrala - (mean(crv1) * mean(crv2)))
  }
)

#' Funcția generică Cor
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o valoare care reprezintă varianța obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "Cor",
  def = function(crv1, crv2, dens) { standardGeneric("Cor") },
  valueClass = "numeric"
)

#' Coeficientul de corelație a doua variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează coeficientul de corelație a două variabile
#' aleatoare continue, utilizând funcția densitate comună acelor două.
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o valoare numerică, ce reprezintă coeficientul de corelație ale celor două
#' variabilelor aleatoare continue.
#'
#' @details
#' Utilizează covarinața celor două variabile pentru calcularea coeficientului de corelanță.
#'
#' @export
#' @examples
#' obj1 <- ContRV(pdf = function(x) { x }, lowerBound = 0, upperBound = sqrt(2))
#' obj2 <- ContRV(pdf = function(x) { 1 }, lowerBound = 0, upperBound = 1)
#' Cor(obj1, obj2, function(x, y) 4*x*y)
setMethod(
  f = "Cor",
  signature = c("ContRV", "ContRV", "function"),
  definition = function(crv1, crv2, dens) {
    cov <- Cov(crv1, crv2, dens)
    return (cov / (sqrt(Var(crv1)) * sqrt(Var(crv2))))
  }
)


########## Cerința 11 ##########
#' Funcția generică marginal_pdf_1
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o valoare care reprezintă varianța obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "marginal_pdf_1",
  def = function(crv1, crv2, dens) { standardGeneric("marginal_pdf_1") },
  valueClass = "function"
)

#' Densitatea marginală al lui x din doua variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează densitatea marginală al lui x,
#' din două variabile aleatoare continue, utilizând funcția densitate comună acelor două.
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o funcție, ce reprezintă densitatea marginală a lui x.
#'
#' @details
#' Utilizează integrarea dupa o singură variabilă a celor două variabile pentru calcularea acestei densități.
#'
#' @export
#' @examples
#' obj1 <- ContRV(pdf = function(x) { 1 }, lowerBound = 0, upperBound = 1)
#' obj2 <- ContRV(pdf = function(x) { 1 }, lowerBound = 1, upperBound = 2)
#' f1 <- marginal_pdf_1(obj1, obj2, function(x, y) 8/3 * x^3 *y)
#' f1(5) # 500
setMethod(
  f = "marginal_pdf_1",
  signature = c("ContRV", "ContRV", "function"),
  definition = function(crv1, crv2, dens) {
    f <-  function(x) {
      sapply(x, function(x) {
        integrate(function(y) dens(x, y), crv2@lowerBound, crv2@upperBound)$value
      })}
  }
)

#' Funcția generică marginal_pdf_2
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o valoare care reprezintă varianța obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "marginal_pdf_2",
  def = function(crv1, crv2, dens) { standardGeneric("marginal_pdf_2") },
  valueClass = "function"
)

#' Densitatea marginală al lui y din doua variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează densitatea marginală al lui y,
#' din două variabile aleatoare continue, utilizând funcția densitate comună acelor două.
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o funcție, ce reprezintă densitatea marginală a lui y.
#'
#' @details
#' Utilizează integrarea dupa o singură variabilă a celor două variabile pentru calcularea acestei densități.
#'
#' @export
#' @examples
#' obj1 <- ContRV(pdf = function(x) { 1 }, lowerBound = 0, upperBound = 1)
#' obj2 <- ContRV(pdf = function(x) { 1 }, lowerBound = 1, upperBound = 2)
#' f2 <- marginal_pdf_2(obj1, obj2, function(x, y) 8/3 * x^3 *y)
#' f2(3) # 2
setMethod(
  f = "marginal_pdf_2",
  signature = c("ContRV", "ContRV", "function"),
  definition = function(crv1, crv2, dens) {
    f <-  function(y) {
      sapply(y, function(y) {
        integrate(function(x) dens(x, y), crv1@lowerBound, crv1@upperBound)$value
      })}
  }
)

#' Funcția generică conditional_density_Y_X
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o valoare care reprezintă varianța obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "conditional_density_Y_X",
  def = function(crv1, crv2, dens) { standardGeneric("conditional_density_Y_X") },
  valueClass = "function"
)

#' Densitatea lui Y condiționată de X din doua variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează densitatea lui Y condiționată de X,
#' din două variabile aleatoare continue, utilizând funcția densitate comună acelor două.
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o funcție, ce reprezintă densitatea lui Y condiționată de X.
#'
#'
#' @export
#' @examples
#' obj1 <- ContRV(pdf = function(x) { 1 }, lowerBound = 0, upperBound = 1)
#' obj2 <- ContRV(pdf = function(x) { 1 }, lowerBound = 1, upperBound = 2)
#' f3 <- conditional_density_Y_X(obj1, obj2, function(x, y) 8/3 * x^3 *y)
#' f3(3, 4) # 2.6667
setMethod(
  f = "conditional_density_Y_X",
  signature = c("ContRV", "ContRV", "function"),
  definition = function(crv1, crv2, dens) {
    marg_x <- marginal_pdf_1(crv1, crv2, dens)
    f <-  function(x, y) {
      dens(x, y)/ marg_x(x)
    }
  }
)

#' Funcția generică conditional_density_X_Y
#'
#' Funcție generică disponibilă pentru overloading
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o valoare care reprezintă varianța obiectului trimis ca parametru.
#'
#' @export
setGeneric(
  name = "conditional_density_X_Y",
  def = function(crv1, crv2, dens) { standardGeneric("conditional_density_X_Y") },
  valueClass = "function"
)

#' Densitatea lui X condiționată de Y din doua variabile aleatoare continue
#'
#' Metodă a clasei \code{ContRV} care calculează și returnează densitatea lui X condiționată de Y,
#' din două variabile aleatoare continue, utilizând funcția densitate comună acelor două.
#'
#' @param crv1 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param crv2 Obiect de tip variabilă aleatoare continuă sau altul, în dependență de implementare
#' @param dens Funcția densitate comună a celor două variabile aleatoare
#'
#' @return Returnează o funcție, ce reprezintă densitatea lui X condiționată de Y.
#'
#'
#' @export
#' @examples
#' obj1 <- ContRV(pdf = function(x) { 1 }, lowerBound = 0, upperBound = 1)
#' obj2 <- ContRV(pdf = function(x) { 1 }, lowerBound = 1, upperBound = 2)
#' f4 <- conditional_density_Y_X(obj1, obj2, function(x, y) 8/3 * x^3 *y)
#' f4(2, 3) # 32
setMethod(
  f = "conditional_density_X_Y",
  signature = c("ContRV", "ContRV", "function"),
  definition = function(crv1, crv2, dens) {
    marg_y <- marginal_pdf_2(crv1, crv2, dens)
    f <-  function(x, y) {
      dens(x, y)/ marg_y(y)
    }
  }
)

########## Cerinta 12 ##########
# Putem face overload de operatori conform S3 sau S4
#' Adunarea a două variabile aleatoare continue independente
#'
#' Operație de adunare a două obiecte de tip \code{ContRV}. Adunarea se face
#' folosind formula de convoluție, ambele variabile primite ca parametru fiind variabilie
#' aleatoare continue independente.
#'
#' @param e1 Obiect de tip variabilă aleatoare continuă
#' @param e2 Obiect de tip variabilă aleatoare continuă
#'
#' @return Returnează un obiect de tip variabilă aleatoare continuă care reprezintă suma celor două
#' variabile inițiale.
#'
#' @details
#' Folosește formula de convoluție pentru adunarea celor două variabile.
#'
#' @export
#' @examples
#' fun1 <- normalize_function(function(x) { x^2 }, -2, 10)
#' var1 <- ContRV(pdf = fun1, lowerBound = -2, upperBound = 10)
#'
#' fun2 <- normalize_function(function(x) {x + 1}, 0, 10)
#' var2 <- ContRV(pdf = fun2, lowerBound = 0, upperBound = 10)
#'
#' var_sum <- var1 + var2
setMethod(
  f = "+",
  signature = c("ContRV", "ContRV"),
  definition = function(e1, e2) {
    lowerBoundAns <- max(e1@lowerBound, e2@lowerBound)
    upperBoundAns <- min(e1@upperBound, e2@upperBound)

    # Functia noastra finala (adunata)
    func <- function(y) {
      # Functia de integrat
      to_integrate <- function(x) {
        return(e1@pdf(y - x) * e2@pdf(x))
      }

      return(integrate(to_integrate, lowerBoundAns, upperBoundAns)$value)
    }

    crv <- ContRV(pdf = normalize_function(func, lowerBoundAns, upperBoundAns), lowerBound = lowerBoundAns, upperBound = upperBoundAns)
    # print(normalizing_constant(func, lowerBoundAns, upperBoundAns))
    return(crv)
  }
)

#' Scăderea a două variabile aleatoare continue independente
#'
#' Operație de scădere pentru a două obiecte de tip \code{ContRV}. Scăderea se face
#' folosind formula de convoluție, ambele variabile primite ca parametru fiind variabilie
#' aleatoare continue independente.
#'
#' @param e1 Obiect de tip variabilă aleatoare continuă
#' @param e2 Obiect de tip variabilă aleatoare continuă
#'
#' @return Returnează un obiect de tip variabilă aleatoare continuă care reprezintă diferența
#' dintre prima și a doua varibilă.
#'
#' @details
#' Folosește formula de convoluție pentru scăderea celor două variabile.
#'
#' @export
#' @examples
#' fun1 <- normalize_function(function(x) { x^2 }, -2, 10)
#' var1 <- ContRV(pdf = fun1, lowerBound = -2, upperBound = 10)
#'
#' fun2 <- normalize_function(function(x) {x + 1}, 0, 10)
#' var2 <- ContRV(pdf = fun2, lowerBound = 0, upperBound = 10)
#'
#' var_sum <- var1 - var2
setMethod(
  f = "-",
  signature = c("ContRV", "ContRV"),
  definition = function(e1, e2) {
    lowerBoundAns <- max(e1@lowerBound, e2@lowerBound)
    upperBoundAns <- min(e1@upperBound, e2@upperBound)

    # Transformam scaderea: X - Y in X + (-Y)
    neg_e2_pdf <- function(x) {
      return(-e2@pdf(x))
    }

    # Functia noastra finala (adunata)
    func <- function(y) {
      # Functia de integrat
      to_integrate <- function(x) {
        return(e1@pdf(y - x) * neg_e2_pdf(x))
      }

      return(integrate(to_integrate, lowerBoundAns, upperBoundAns)$value)
    }

    crv <- ContRV(pdf = normalize_function(func, lowerBoundAns, upperBoundAns), lowerBound = lowerBoundAns, upperBound = upperBoundAns)
    print(normalizing_constant(func, lowerBoundAns, upperBoundAns))
    return(crv)
  }
)





