#'Ejercicio básico de análisis de regresión simple. Tomado de Montgomery y modificado por https://github.com/eitt
#'La Resistencia al desprendimiEnto depende de la Longitud del alambre, la Altura de la matriz, la Altura del poste y la Altura del amarre 
#' Y ~ X1+X2+X3+X4 [Resist_ despren	~	Long_alambre+Alt_matriz+Alt_ poste+Alt_amarre]
#Carga de librerías
library(lattice)
library(ggplot2)
library(graphics)
library(corrplot)

#'#Carga del archivo
DATA <- read_delim("MONTGOMERY.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
#'Resumen descriptivo de las variables
summary(DATA)
#'Visualización de los datos
View(DATA)
#'Gráfica de sacatter de las combinaciones de variables predictoras
splom(DATA[2:5])
#'Gráfica de correlación
M<-cor(DATA[2:5])
corrplot(M, type="lower")
Y=DATA[,1]
X1=DATA[,2]
X2=DATA[,3]
X3=DATA[,4]
X4=DATA[,5]

#'Se realiza un análisis de regresión simple y múltiple para las posibles combinaciones de Xi con la variable Y
fitA <-lm(Y ~ X1,data = DATA)
summary(fitA)
fitB <-lm(Y ~ X2,data = DATA)
summary(fitB)
fitC <-lm(Y ~ X3,data = DATA)
summary(fitC)
fitD <-lm(Y ~ X4,data = DATA)
summary(fitD)
fitAB <-lm(Y ~ X1+X2,data = DATA)
summary(fitAB)
fitAC <-lm(Y ~ X1+X3,data = DATA)
summary(fitAC)
fitAD <-lm(Y ~ X1+X4,data = DATA)
summary(fitAD)
fitBC <-lm(Y ~ X2+X3,data = DATA)
summary(fitBC)
fitBD <-lm(Y ~ X2+X4,data = DATA)
summary(fitBD)
fitCD <-lm(Y ~ X3+X4,data = DATA)
summary(fitCD)
fitABC <-lm(Y ~ X1+X2+X3,data = DATA)
summary(fitABC)
fitABD <-lm(Y ~ X1+X2+X4,data = DATA)
summary(fitABD)
fitACD <-lm(Y ~ X1+X3+X4,data = DATA)
summary(fitACD)
fitBCD <-lm(Y ~ X2+X3+X4,data = DATA)
summary(fitBCD)
fitABCD <-lm(Y ~ X1+X2+X3+X4,data = DATA)
summary(fitABCD)

AB<-anova(fitA,fitB,fitC)
