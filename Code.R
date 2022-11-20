
# Instalando librerias

library(boot)
library(bootstrap)
library(MASS)

### Jackknife para estimar correlacion y error estandar del ESTIMADOR

set.seed(1) # Para que sea replicable
sigma = matrix(c(1,0.5,0.5,1), ncol = 2) # Matriz de varianzas y covarianzas
print(sigma) 

datos <- mvrnorm(100,c(0,0), Sigma = sigma) # Muestra (n=100) de X ~ N_2(mu = (0,0), Sigma)
head(datos, 3)
datos

# Estimacion puntual de la correlacion
cor(datos[,1], datos[,2])

# Implementando jack knife de manera manual en el coeficiente de correlacion muestral
hat.rho.menos.1 = c()
for(i in 1:100){
  hat.rho.menos.1[i] = cor(datos[-i,1],datos[-i,2])
}

# Graficamente
x11()
hist(hat.rho.menos.1, main='Estimaciones leave-one-out', xlab = expression(hat(rho)['-1']))

# Lo mismo pero usando la libreria bootstrap
theta.n <- function(x, xdata) { cor(xdata[x,1],xdata[x,2])} 
jack <- jackknife(1:100, theta.n, datos)

x11()
print(jack)
hist(jack$jack.values, main = 'Estimaciones leave-one-out', xlab = expression(hat(rho)['-1']))

mean(jack$jack.values) # Promedio de las estimaciones 
jack$jack.se # Error estandar jack
jack$jack.bias 

# Estimacion jack knife corregida
cor(datos[,1], datos[,2]) - jack$jack.bias

### Bootstrap 

data("mtcars")
head(mtcars,3)

reglin = lm(mpg~wt+disp, data = mtcars)
summary(reglin)

# Comienza la aplicacion 
n = dim(mtcars)[1]
B = 1000
R2a = c() # Guarda los valores de theta

set.seed(1)
for(i in 1:B){
  id = sample(n, n, replace = T) # Re muestreo con reemplazamiento
  reglin.boots = lm(mpg~wt+disp, data = mtcars, subset = id) # utilizo remuestras
  R2a[i] = summary(reglin.boots)$adj.r.squared  
  print(i)
}

x11()
hist(R2a, xlab = expression(R[aj]^2), main= 'Distribucion empirica del R^2 ajustado')

sd(R2a)




