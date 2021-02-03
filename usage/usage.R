# Cerinta 1
k_norm <- normalizing_constant(function(x) { x }, 0, 2)

# Cerinta 2
fun1 <- function(x) { x }
check_if_pdf(fun1, 0, 1)   # FALSE

fun2 <- function(x) { 2 * exp(-2*x) }
check_if_pdf(fun2, 0, Inf)  # TRUE


# Cerinta 3
var1 <- ContRV(pdf = function(x) { 1 / 3 }, lowerBound = 3, upperBound = 6)

var2 <- ContRV(pdf = function(x) { 1 / 4 }, lowerBound = 3, upperBound = 6) #nu poate fi creat

# Cerinta 4
plot_RV(var1, x_int = c(2, 6), y_int = c(0, 2), color = "magenta")
plot_distribution("normal", c(-2, 2), c(0,1), 0, 1)

# Cerinta 5
mean(var1)  # 4.5
Var(var1) # 0.75
moment_centrat(var1, 2) # 0.75
moment_centrat(var1, 3)
moment_centrat(var1, 4) 
moment_centrat(var1, 0) # 1
moment_centrat(var1, 5) # eroare

moment_initial(var1, 2) 
moment_initial(var1, 3)
moment_initial(var1, 4) 
moment_initial(var1, 0) # 1
moment_initial(var1, 5) # eroare

# Cerinta 6

var3 <- ContRV(pdf = function(x) { 2 * exp(-2 * x) }, lowerBound = 0, upperBound = Inf)
mean_crv_func(var3, function(x){ x ^ 2 }) # 0.5
var_crv_func(var3, function(x){ x ^ 2 }) # 1.25

# Cerinta 8
?exponential

# Cerinta 9
generate_crv(exponential(4), 10)

# Cerinta 10
obj1 <- ContRV(pdf = function(x) { x }, lowerBound = 0, upperBound = sqrt(2))
obj2 <- ContRV(pdf = function(x) { 1 }, lowerBound = 0, upperBound = 1)
Cov(obj1, obj2, function(x, y) 4*x*y)
Cor(obj1, obj2, function(x, y) 4*x*y)

# Cerinta 11
obj3<- ContRV(pdf = function(x) { 1 }, lowerBound = 0, upperBound = 1)
obj4 <- ContRV(pdf = function(x) { 1 }, lowerBound = 1, upperBound = 2)
f1 <- marginal_pdf_1(obj3, obj4, function(x, y) 8/3 * x^3 *y)
f1(5) # 500
f2 <- marginal_pdf_2(obj3, obj4, function(x, y) 8/3 * x^3 *y)
f2(3) # 2

f3 <- conditional_density_Y_X(obj3, obj4, function(x, y) 8/3 * x^3 *y)
f3(3, 4) # 2.6667

f4 <- conditional_density_X_Y(obj3, obj4, function(x, y) 8/3 * x^3 *y)
f4(2, 3) # 32

# Cerinta 12
obj5 <- normal(0, 1) + normal(0, 1)

fun3 <- normalize_function(function(x) { x^2 }, -2, 10)
var4 <- ContRV(pdf = fun3, lowerBound = -2, upperBound = 10)

fun4 <- normalize_function(function(x) {x + 1}, 0, 10)
var5 <- ContRV(pdf = fun4, lowerBound = 0, upperBound = 10)

var_diff1 <- var4 - var5
