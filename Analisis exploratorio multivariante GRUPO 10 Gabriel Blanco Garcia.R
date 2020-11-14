# Objetivo del trabajo --------------------------------------------------------

# Realizar un analisis exploratorio multidimensional
# (numerico y grafico) de un conjunto de variables cuantitativas 
# y una categorica, dentro del ambito de las finanzas

# Variables: Construir una cartera de activos 
# (nacionales, internacionales o mixtos) y elegir un indice de referencia 
# (IBEX35, etc.), para a continuacion obtener los rendimientos. 
# La variable categorica puede ser una variable dummy 
# que indique el inicio del COVID.

# Analisis: estudiar los rendimientos de forma aislada. 
# Estudiar las dependencias entre los rendimientos y el indice 
# por medio de las matrices de covarianzas y de correlaciones. 
# Obtener rendimientos antes y despues del COVID. Realizar analisis 
# graficos y numericos de todo lo anterior. 
# Obtener el rendimiento medio y la volatilidad de la cartera. 
# Se puede realizar un analisis de regresion simple tipo CAPM 
# para explicar los rendimientos de cada activo en funcion del 
# rendimiento del indice de referencia (IBEX35, etc).

# Paquetes --------------------------------------------------------------------

library(reshape2)
library(quantmod)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(moments)
library(ggrepel)

# Obtencion de los datos ------------------------------------------------------

descarga <- c("^IBEX", "CIE.MC", "MEL.MC", "SAN", "AMS.MC", "ITX.MC" )
getSymbols(descarga, from = "2019-09-24", to = "2020-09-24")

# En mayusculas, tal cual como lo deja el quantmod.

# En minusculas, dataframes solo de maximo, minimo y ajustado. Es mas 
# facil trabajar con Data Frames que con el formato de quant

ibex <- as.data.frame(IBEX[, c(2, 3, 6)])
cie <- as.data.frame(CIE.MC[, c(2, 3, 6)])
amadeus <- as.data.frame(AMS.MC[, c(2, 3, 6)])
inditex <- as.data.frame(ITX.MC[, c(2, 3, 6)])
melia <- as.data.frame(MEL.MC[, c(2, 3, 6)])
santander <- as.data.frame(SAN[, c(2, 3, 6)]) 


# Algunos retoques

nombres <- c("Maxino", "Minimo", "Ajustado")

colnames(ibex) <- nombres
colnames(cie) <- nombres
colnames(amadeus) <- nombres
colnames(melia) <- nombres
colnames(santander) <- nombres
colnames(inditex) <- nombres

rownames(ibex) <- NULL
rownames(cie) <- NULL
rownames(amadeus) <- NULL
rownames(melia) <- NULL
rownames(santander) <- NULL
rownames(inditex) <- NULL


# Agregacion del precio ajustado a los dataframes (para calcular sobre ellos 
# el rendimiento)

ajustado.cie <- cie$Ajustado # 253
ajustado.melia <- melia$Ajustado # 257
ajustado.santander <- santander$Ajustado # 253
ajustado.amadeus <- amadeus$Ajustado # 257
ajustado.inditex <- inditex$Ajustado # 257
ajustado.ibex <- ibex$Ajustado # 257


# Ajuste para que todos sean de la misma longitud: eliminamos las 
# 4 ultimas de melia, amadeus, inditex e ibex.

ajustado.melia <- melia$Ajustado[1:253]
ajustado.amadeus <- amadeus$Ajustado[1:253]
ajustado.inditex <- inditex$Ajustado[1:253]
ajustado.ibex <- ibex$Ajustado[1:253]
ajustado.cie <- cie$Ajustado[1:253]


# Rendimientos ----------------------------------------------------------------

# Calculo de los rendimientos para los 6 precios ajustados 
# Declaracion de una funcion para calcular los rendimientos diarios 

fun.rend <- function(activo) {
  n <- length(activo)
  activo[2:n] / activo [1:n - 1] -1
  }


# Aplicacion a cada serie de precios

r.amadeus <- fun.rend(ajustado.amadeus)
r.melia  <- fun.rend(ajustado.melia)
r.cie  <- fun.rend(ajustado.cie)
r.inditex  <- fun.rend(ajustado.inditex)
r.ibex  <- fun.rend(ajustado.ibex)
r.santander <- fun.rend(ajustado.santander)


# Finalmente, se crea un dataframe que contenga los rendimientos 
# diarios de cada activo.

Rendimientos <- data.frame(r.ibex, r.amadeus, r.cie, r.inditex,
                           r.melia, r.santander)

colnames(Rendimientos) <- c("Ibex", "Amadeus", "Cie", 
                            "Inditex", "Melia", "Santander")
Rendimientos


# Punto de referencia: 9 de marzo, 120 dias desde el inicio del peridodo ------

# Creacion del vector de 0's y 1's que actuara como variable dummy

p_break <- 120
covid <- c(rep(0, p_break), rep(1, nrow(Rendimientos) - p_break))
covid.r <- as.factor(covid) # para que lo reconozca como dummy

Rendimientos$covid <- covid.r


# Imputacion de las medias en los NA

Rendimientos$Inditex[209:210] <- mean(Rendimientos$Inditex, na.rm = TRUE)
Rendimientos$Cie[209:210] <- mean(Rendimientos$Cie, na.rm = TRUE)
Rendimientos$Melia[209:210] <- mean(Rendimientos$Melia, na.rm = TRUE)
Rendimientos$Ibex[66:67] <- mean(Rendimientos$Ibex, na.rm = TRUE) 


# Creacion de una columna de fechas en el dataframe de rendimientos.
# Para ello se toma una de las series originales y se extraen sus nombres
# de filas, que en quantmod son las fechas. 
# De este modo, podremos incluir las fechas exactas en los plots

aux <- as.data.frame(SAN)
fechas <- rownames(aux)
fechas <- fechas[1:252]
Rendimientos$Fechas <- as.Date(fechas)


# Estadisticos descriptivos ---------------------------------------------------

# Esta seccion recoge el calculo de algunos estadisticos descriptivos 
# de nuestros activos 

Varianza <- sapply(Rendimientos[, 1:6], var)
Desviacion_tipica <- sapply(Rendimientos[, 1:6], sd)
sapply(Rendimientos[, 1:6], fivenum) # Numeros de Tukey
Kurtosis <- (sapply(Rendimientos[, 1:6], kurtosis) -3) 
Asimetria <- sapply(Rendimientos[, 1:6], skewness)

Estadisticos_descriptivos <- as.data.frame(sapply(Rendimientos[, 1:6], summary))
Estadisticos_descriptivos <- rbind(Estadisticos_descriptivos, Desviacion_tipica)
Estadisticos_descriptivos <- rbind(Estadisticos_descriptivos, Varianza)
Estadisticos_descriptivos <- rbind(Estadisticos_descriptivos, Kurtosis)
Estadisticos_descriptivos <- rbind(Estadisticos_descriptivos, Asimetria)
# Esta tabla se crea para poder exportarla a csv e incluirla en la 
# presentacion 

# Recorrido semi-intercuartilico 
attach(Rendimientos)

recorrido.semi <- function(variable) {
  (quantile(variable, 0.75, names = F) - 
     quantile(variable, 0.25, names = F )) / 2
}
sapply(Rendimientos[, 1:6], recorrido.semi)
round(sapply(Rendimientos[, 1:6], recorrido.semi), 3) # recorrido semi-inter
# de todas las variables, 3 decimales

round(mean(sapply(Rendimientos[, 2:6], mean)), 3) # media de la cartera
round(mean(sapply(Rendimientos[, 2:6], sd)), 3) # sd de la cartera
round(mean(sapply(Rendimientos[, 2:6], var)), 3) # varianza de la cartera

# Contraste de hipotesis ------------------------------------------------------

  # Test de normalidad de Shapiro para todos los activos ----------------------

sapply(Rendimientos[, 1:6], shapiro.test)

# Se rechaza la normalidad para  los rendimientos de todos los activos y el
# indice.


  # Test para la varianza indice-activo -----------------------------------------

var.test(Ibex, Cie) # se rechaza 
var.test(Ibex, Inditex) # se rechaza
var.test(Ibex, Amadeus) # se rechaza
var.test(Ibex, Melia) # se rechaza
var.test(Ibex, Santander) # se rechaza
# Todas tienen varianza distinta al Ibex


  # Test para la varianza, antes y despues del COVID-19 -----------------------

var.test(Ibex ~ covid) # 0.09 no se rechaza 
var.test(Cie ~ covid) # se rechaza
var.test(Inditex ~ covid) # se rechaza
var.test(Amadeus ~ covid) # se rechaza
var.test(Melia ~ covid) # se rechaza 
var.test(Santander ~ covid) # no se rechaza 0.6

# La varianza de los rendimientos diarios cambia con el coronavirus en todos
# los activos, excepto en Santadner y en el Ibex.


  # Test para la media, antes y despues del COVID-19 --------------------------

t.test(Ibex ~ covid) # no se rechaza. Media es igual antes que despues 0.12
t.test(Santander ~ covid) # 0.09  no se rechaza
t.test(Cie ~ covid) # no se rechaza
t.test(Amadeus ~ covid) # no se rechaza
t.test(Melia ~ covid) # no se rechaza
t.test(Inditex ~ covid) # no se rechaza

# El coronavirus no ha tenido un impacto estadisticamente significativo
# en la media de los rendimientos diarios

  # Resultados de los contrastes: -----------------------------------------------

# Shapiro.test: se rechaza la normalidad para todos los activos.
# Los rendimientos analizados no se distribuyen de manera normal. Esta 
# caracteristica es comun en las series de rendimientos financieros, que
# suelen presentar colas pesadas en sus distribuciones.

# Var.test accion contra Ibex: se rechaza para todos los activos. Ningun activo
# presenta una varianza estadisticamente igual a la del IBEX. En otras
# palabras, los activos presentan mayor riesgo que el indice.

# Var.test antes y despues: se rechaza la igualdad de varianza antes y 
# despues del covid para todos los activos, excepto para Santander e  Ibex. 
# La no affeccion a la varianza de Santander se podria explicar por el declive
# que viene experimentando el sector bancario español desde hace años. Al 
# encontrarse ya en una situacion poco favorable, el covid no parece haber 
# impactado tanto en terminos de varianza. La resistencia en la varianza 
# del ibex podria tener su origen en la seguirdad del propio indice. Al ser
# un conjunto de muchos activos, es mas dificil que un hecho puntual tenga 
# la capacidad de afectar de manera significativa a su varianza, como sucede
# en el caso de los activos individuales.

# t.test, antes y despues: no se rechaza. El coronavirus no ha modificado 
# la media de los rendimientos diarios de manera significativa


# VISUALIZACIONES -------------------------------------------------------------

# Para ilustrar la caida de los mercados, se realiza una 
# visualizacion de la caida de los precios maximos. Las lineas 
# siguientes construyen, a partir de los datos de quantmod, distintos
# dataframe que se utilizaran para hace un grafico conjunto de la caida de 
# los precios

columnas <- c("Maximo", "Minimo")

maxmin.amadeus <- as.data.frame(AMS.MC[, 2:3])
colnames(maxmin.amadeus) <- columnas

maxmin.cie <- as.data.frame(CIE.MC[, 2:3])
colnames(maxmin.cie) <- columnas

maxmin.ibex <- as.data.frame(IBEX[, 2:3])
colnames(maxmin.ibex) <- columnas

maxmin.inditex <- as.data.frame(ITX.MC[, 2:3])
colnames(maxmin.inditex) <- columnas

maxmin.melia <- as.data.frame(MEL.MC[, 2:3])
colnames(maxmin.melia) <- columnas

maxmin.san <- as.data.frame(SAN[, 2:3])
colnames(maxmin.san) <- columnas

# Creacion de la variable fecha en cada tabla auxiliar

maxmin.amadeus$Fecha <- as.Date(rownames(maxmin.amadeus))
maxmin.cie$Fecha <- as.Date(rownames(maxmin.cie))
maxmin.ibex$Fecha <- as.Date(rownames(maxmin.ibex))
maxmin.inditex$Fecha <- as.Date(rownames(maxmin.inditex))
maxmin.melia$Fecha <- as.Date(rownames(maxmin.melia))
maxmin.san$Fecha <- as.Date(rownames(maxmin.san))

  # Grafico de maximos ----------------------------------------------------------

# Primero se crea cada plot por separado, se guarda en un objeto y finalmente
# se grafica de forma conjunta con grid.arrange() Este procedimiento 
# se repite para todos los graficos conjuntos.

line.ama <- ggplot(data = maxmin.amadeus, mapping = aes(x = Fecha, y = Maximo)) +
  geom_line(color = "red") +
  labs(title = "Amadeus") + xlab(NULL) + ylab(NULL) +
  theme_minimal()

line.cie <- ggplot(data = maxmin.cie, mapping = aes(x = Fecha, y = Maximo)) +
  geom_line(color = "red") +
  labs(title = "Cie") + xlab(NULL) + ylab(NULL) +
  theme_minimal()

line.ibex <- ggplot(data = maxmin.ibex, mapping = aes(x = Fecha, y = Maximo)) +
  geom_line(color = "red") +
  labs(title = "Ibex") + xlab(NULL) + ylab("Precio") +
  theme_minimal()

line.inditex <- ggplot(data = maxmin.inditex, mapping = aes(x = Fecha, y = Maximo)) +
  geom_line(color = "red") +
  labs(title = "Inditex") + xlab(NULL) + ylab(NULL) +
  theme_minimal()

line.melia <- ggplot(data = maxmin.melia, mapping = aes(x = Fecha, y = Maximo)) +
  geom_line(color = "red") +
  labs(title = "Melia") + xlab(NULL) + ylab(NULL) +
  theme_minimal()

line.san <- ggplot(data = maxmin.san, mapping = aes(x = Fecha, y = Maximo)) +
  geom_line(color = "red") +
  labs(title = "Santander") + xlab(NULL) + ylab(NULL) +
  theme_minimal()

# Grafico conjunto de los precios maximos 

grid.arrange(line.ibex, line.ama, line.cie, line.inditex, line.melia, line.san,
             nrow = 2)


  # Cajas y bigotes, antes y despues (parejas) ----------------------------------

# En cada grafico, la parte izquierda (0) es la referente al periodo previo
# al covid, y la derecha (1), la referente al periodo posterior. 

box.ibex <- ggplot(data = Rendimientos, mapping = aes(x = Ibex, y = covid)) +
  geom_boxplot(fill = "chartreuse") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "IBEX 35") +
  theme_minimal()

box.san <- ggplot(data = Rendimientos, mapping = aes(x = Santander, y = covid)) +
  geom_boxplot(fill = "darkseagreen") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Santander") +
  theme_minimal()

box.amadeus <- ggplot(data = Rendimientos, mapping = aes(x = Amadeus, y = covid)) +
  geom_boxplot(fill = "royalblue") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Amadeus") +
  theme_minimal()

box.melia <- ggplot(data = Rendimientos, mapping = aes(x = Melia, y = covid)) +
  geom_boxplot(fill = "cyan2") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Melia") +
  theme_minimal()

box.cie <- ggplot(data = Rendimientos, mapping = aes(x = Cie, y = covid)) +
  geom_boxplot(fill = "lightgoldenrod") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Cie") +
  theme_minimal()

box.inditex <- ggplot(data = Rendimientos, mapping = aes(x = Inditex, y = covid)) +
  geom_boxplot(fill = "paleturquoise") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Inditex") +
  theme_minimal()

# Grafico conjunto de caja y bigotes comparativo antes y despues del covid

grid.arrange(box.ibex, box.amadeus, box.cie, box.inditex, box.melia, box.san, 
             nrow = 2, ncol = 3) 


  # Violin plots ----------------------------------------------------------------

# Se introducen diagramas de violines porque permiten ilustrar mejor los 
# posibles cambios en las distribuciones, puesto que con los de caja 
# y bigotes no es posible visualizar dicha densidad. En el fondo, los 
# diagramas se asemejan a funciones de densidad, pero rotadas 90 grados y 
# vistas en forma de espejo, de ahi su semejanza con el instrumento musical

# Con esta visualizacion podemos ver como efectivamente se produce un 
# aumento en la dispersion de los rendimientos, las distribuciones se 
# vuelven mas amplias, y las observaciones se separan mas de la media, 
# estrechando el diagrama.

# En cada grafico, la parte izquierda (0) es la referente al periodo previo
# al covid, y la derecha (1), la referente al periodo posterior. 

viol.ibex <- ggplot(data = Rendimientos, mapping = aes(x = Ibex, y = covid)) +
  geom_violin(fill = "chartreuse") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "IBEX 35") +
  theme_minimal() 

viol.san <- ggplot(data = Rendimientos, mapping = aes(x = Santander, 
                                                      y = covid)) +
  geom_violin(fill = "darkseagreen") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Santander") +
  theme_minimal()

viol.amadeus <- ggplot(data = Rendimientos, mapping = aes(x = Amadeus, 
                                                          y = covid)) +
  geom_violin(fill = "royalblue") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Amadeus") +
  theme_minimal()

viol.melia <- ggplot(data = Rendimientos, mapping = aes(x = Melia, y = covid)) +
  geom_violin(fill = "cyan2") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Melia") +
  theme_minimal()

viol.cie <- ggplot(data = Rendimientos, mapping = aes(x = Cie, y = covid)) +
  geom_violin(fill = "lightgoldenrod") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Cie") +
  theme_minimal()

viol.inditex <- ggplot(data = Rendimientos, mapping = aes(x = Inditex, 
                                                          y = covid)) +
  geom_violin(fill = "paleturquoise") +
  coord_flip() + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Inditex") +
  theme_minimal()

# El grafico conjunto
grid.arrange(viol.ibex, viol.amadeus, viol.cie, 
             viol.inditex, viol.melia, viol.san, 
             nrow = 2, ncol = 3) 

  
  # Todas las variables comparadas, antes y despues del covid -------------------
colores <- c("chartreuse", "royalblue", "lightgoldenrod",
             "paleturquoise", "cyan", "darkseagreen")

# Antes del covid

# Con melt() se crea un dataset auxiliar que contenga el nombre de los 
# activos como una unica variable. De este modo, todos los graficos 
# de caja y bigote compartiran el eje de ordenadas, y podran ser comparados

melt.aux <- melt(Rendimientos[1:120, 1:6]) # sin covid
colnames(melt.aux) <- c("Activos", "Rendimiento")


ggplot(data = melt.aux, aes(x = Activos, y = Rendimiento, fill = Activos)) + 
  geom_boxplot()+ 
  xlab(NULL) +
  labs(title = "Caja y bigotes, antes del COVID-19") +
  theme_minimal() + 
  scale_fill_manual(values = colores)

# Despues del covid 

# Mismo procedimiento que antes, pero para los datos de despues del covid

melt.covid <- melt(Rendimientos[121:252, 1:6])
colnames(melt.covid) <- c("Activos", "Rendimiento")

ggplot(data = melt.covid, aes(x = Activos, y = Rendimiento, fill = Activos)) + 
  geom_boxplot() + 
  xlab(NULL) + 
  labs(title = "Caja y bigotes, despues del COVID-19") +
  theme_minimal() +
  scale_fill_manual(values = colores)


# Mismos plots, pero con diagrama de violines

# Todos antes del covid

ggplot(data = melt.aux, aes(x = Activos, y = Rendimiento, fill = Activos)) + 
  geom_violin()+ 
  xlab(NULL) +
  labs(title = "Violin, antes del COVID-19") +
  theme_minimal() + 
  scale_fill_manual(values = colores)

# Todos despues del covid

ggplot(data = melt.covid, aes(x = Activos, y = Rendimiento, fill = Activos)) + 
  geom_violin() + 
  xlab(NULL) + 
  labs(title = "Caja y bigotes, despues del COVID-19") +
  theme_minimal() +
  scale_fill_manual(values = colores)



# Podemos observar como despues del covid aparecen muchos mas valores
# extremos. El covid ha generado mayor incertidumbre, riesgo (dispersion),
# en los mercados.


  # Histogramas y kernels -------------------------------------------------------

# En esta seccion se grafican los hisogramas y kernels de los rendimientos.
# El objetivo es ilustrar la "no normalidad" de las distribuciones de los 
# rendimientos, hecho que se contrasta con el test de Shapiro Wilk

hist.ibex <- ggplot(data = Rendimientos, mapping = aes(x = Ibex)) +
  geom_histogram(fill = "chartreuse", color = "black") +
  geom_density(color = "red", bw = 0.002) +
  ylab(NULL) + xlab(NULL) + labs(title = "IBEX 35") +
  theme_minimal()

hist.san <- ggplot(data = Rendimientos, mapping = aes(x = Santander)) +
  geom_histogram(fill = "darkseagreen", color = "black") +
  geom_density(color = "red", bw = 0.002) +
  ylab(NULL) + xlab(NULL) + labs(title = "Santander") +
  theme_minimal()

hist.amadeus <- ggplot(data = Rendimientos, mapping = aes(x = Amadeus)) +
  geom_histogram(fill = "royalblue", color = "black") +
  geom_density(color = "red", bw = 0.002) +
  ylab(NULL) + xlab(NULL) + labs(title = "Amadeus") +
  theme_minimal()

hist.melia <- ggplot(data = Rendimientos, mapping = aes(x = Melia)) +
  geom_histogram(fill = "cyan2", color = "black") +
  geom_density(color = "red", bw = 0.002) +
  ylab(NULL) + xlab(NULL) + labs(title = "Melia") +
  theme_minimal()

hist.cie <- ggplot(data = Rendimientos, mapping = aes(x = Cie)) +
  geom_histogram(fill = "lightgoldenrod1", color = "black") +
  geom_density(color = "red", bw = 0.002) +
  ylab(NULL) + xlab(NULL) + labs(title = "Cie") +
  theme_minimal()

hist.inditex <- ggplot(data = Rendimientos, mapping = aes(x = Inditex)) +
  geom_histogram(fill = "paleturquoise", color = "black") +
  geom_density(color = "red", bw = 0.002) +
  ylab(NULL) + xlab(NULL) + labs(title = "Inditex") +
  theme_minimal()

# El plot conjunto
grid.arrange(hist.ibex, hist.amadeus, hist.cie, hist.inditex, hist.melia, hist.san,
             nrow = 2) 


  # Kernels comparativos con ibex -----------------------------------------------

# En esta seccion se realizan los plots de densidad de cada activo 
# y del ibex, en el mismo plot. Al incluir las densidades del activo y 
# el indice en el mismo grafico, buscamos ilustrar las diferencias 
# entre las dispersiones de los activos respecto al indice. Estas diferencias
# se contrastan con un test de varianza entre el ibex, y cada activo.

k.cie <- ggplot(data = Rendimientos) +
  geom_density(mapping = aes(x = Cie), 
               fill = "lightgoldenrod",
               color = "lightgoldenrod4") +
  geom_density(mapping = aes(x = Ibex), 
               fill = "bisque",
               color = "bisque4",
               alpha = 0.60) + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "Cie e IBEX 35") +
  theme_minimal()

k.inditex <- ggplot(data = Rendimientos) +
  geom_density(mapping = aes(x = Inditex), 
               fill = "paleturquoise",
               color = "paleturquoise4") +
  geom_density(mapping = aes(x = Ibex), 
               fill = "bisque",
               color = "bisque4",
               alpha = 0.60) + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Inditex e IBEX 35") +
  theme_minimal()

k.melia <- ggplot(data = Rendimientos) +
  geom_density(mapping = aes(x = Melia), 
               fill = "cyan",
               color = "cyan4") +
  geom_density(mapping = aes(x = Ibex), 
               fill = "bisque",
               color = "bisque4",
               alpha = 0.60) + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Melia e IBEX 35") +
  theme_minimal()

k.amadeus <- ggplot(data = Rendimientos) +
  geom_density(mapping = aes(x = Amadeus), 
               fill = "royalblue",
               color = "royalblue4") +
  geom_density(mapping = aes(x = Ibex), 
               fill = "bisque",
               color = "bisque4",
               alpha = 0.60) + 
  xlab("Rendimientos") + ylab("Densidad") +
  labs(title = "Amadeus e IBEX 35") +
  theme_minimal()

k.santander <- ggplot(data = Rendimientos) +
  geom_density(mapping = aes(x = Santander), 
               fill = "darkseagreen",
               color = "darkseagreen4") +
  geom_density(mapping = aes(x = Ibex), 
               fill = "bisque",
               color = "bisque4",
               alpha = 0.60) + 
  xlab(NULL) + ylab(NULL) +
  labs(title = "Santander e IBEX 35") +
  theme_minimal()


# El grafico conjunto

grid.arrange(k.amadeus, k.cie, k.inditex, k.melia, k.santander, nrow = 2) 

# Covarianza y correlaciones --------------------------------------------------

rh <- rcorr(as.matrix(Rendimientos[, 1:6]), type = "pearson") # Correlacion de Pearson
rh$r
rh$P

# Visualizacion de las correlaciones

ggcorrplot(rh$r, method = 'square', type = 'lower', lab = TRUE, ) +
  ggtitle("Correlograma de los rendimientos") + 
  theme_bw() +
  theme_minimal() +
  xlab(NULL) + ylab(NULL) 
# Los activos con mayor correlacion con el IBEX-35 son Inditex y Amadeus, 
# con unos valores de 0.79 y 0.71 respectivamente. Análogamente, 
# estos activos son los mismos que presetnaban una función de densidad 
# de mayor semejanza a la del IBEX-35. Meliá también presenta una 
# correlación importante, con un valor de 0.63. El activo que resulta 
# estar menos correlacionado con el índcie es Santander, que presenta 
# una correlación cercana a 0. Adicionalmente, es el activo que menor 
# correlación presenta con los demás. 


# CAPM ------------------------------------------------------------------------

# En esta sección se crea una cartera equiponderada 
# y representarla con el modelo CAPM y la recta SML


# Calculo de la desviación típica de cada activo en el periodo entero descargado

sd_ibex <- sd(Ibex) 
sd_amadeus <- sd(Amadeus)
sd_cie <- sd(Cie)
sd_inditex <- sd(Inditex)
sd_melia <- sd(Melia)
sd_santander <- sd(Santander)

# Cálculo del rendimiento acumulado con el primer día como base

Rend__ibex <- (ibex[257,3]/ibex[1,3]-1)  
Rend__amadeus <- (amadeus[257,3]/amadeus[1,3]-1)
Rend__inditex <- (inditex[257,3]/inditex[1,3]-1)
Rend__melia <- (melia[257,3]/melia[1,3]-1)
Rend__santander <- (santander[253,3]/santander[1,3]-1)
Rend__cie <- (cie[253, 3]/cie[1,3]-1) # acumulados 

# Cálculo de la Beta de las acciones respecto al mercado con la covarianza 
# y la varianza del IBEX

BETA_AMADEUS <- cov(Amadeus, Ibex) / var(Ibex) 
BETA_CIE <- cov(Cie, Ibex) / var(Ibex)
BETA_INDITEX <- cov( Inditex,Ibex) / var(Ibex)
BETA_MELIA <- cov(Melia, Ibex) / var(Ibex)
BETA_SANTANDER <- cov(Santander,Ibex) / var(Ibex)

C.IBEX <- c(sd_ibex, Rend__ibex)

# Se crea una variable para cada compañia donde se encuentre su desviación, 
# su rendimiento acumulado y su beta.

C.amadeus <- c(sd_amadeus, Rend__amadeus, BETA_AMADEUS) 
C.inditex <- c(sd_inditex, Rend__inditex, BETA_INDITEX)
C.melia <- c(sd_melia, Rend__melia, BETA_MELIA)
C.santander <- c(sd_santander, Rend__santander, BETA_SANTANDER)
C.cie <- c(sd_cie, Rend__cie, BETA_CIE)

# Se crea una cartera con la misma ponderación para cada atributo 
# (rendimiento acumulado, Beta y desviación típica)

Rend_cartera <- mean(c(Rend__amadeus ,   
                       Rend__inditex, 
                       Rend__melia, 
                       Rend__santander ,
                       Rend__cie ))

BETA_cartera <- mean( c(BETA_AMADEUS,
                        BETA_CIE,
                        BETA_INDITEX,
                        BETA_MELIA,
                        BETA_SANTANDER))

sd_cartera <- mean(c( 
  sd_amadeus,
  sd_cie,
  sd_melia, 
  sd_inditex, 
  sd_santander))

# Se crea un objeto donde contenga la rentabilidad, desviación y Beta de
# la cartera (ponderación de las  empresas)

C.Cartera <- c(sd_cartera, Rend_cartera, BETA_cartera) 

# Unión de todos los objetos en uno mismo llamado "CAPM"

CAPM <- cbind(C.amadeus, C.inditex, C.melia, C.santander, C.cie, C.Cartera) 

View(CAPM)

# Se renombran las columnas y filas de CAPM

colnames(CAPM) <- c("AMADAEUS", "INDITEX", "MELIA", 
                    "SANTANDER", "CIE", "CARTERA")

rownames(CAPM) <- c("DES", "RENT", "BETA") 

# Obtenemos la traspuesta de la tabla para poder trabajar con las deviaciones, 
# rendimientos y betas como variables

CAPM <- t(CAPM) 
CAPM <- as.data.frame(CAPM)


# Se declara la función de la linea SML teniendo encuenta la Rf = bonos a 10 
# años de España  (0.25%). La Beta ponderada (beta de la cartera) = 0.82

linea_capm <- function(x) 0.0025 + 0.8291976 * (x - 0.0025) 

# Visualizaciones CAPM  -----------------------------------------------------

# El resultado obtenido es que nuestra cartera no es eficiente y todos los 
# valores se encuentran infravalorados respecto al mercado por menor 
# rentabilidad por reisgo asumido a excepción del santander

# El modelo CAPM es incapaz de explicar porqué el santander con tanto 
# rendimiento tiene una beta cercana a cero

ggplot(CAPM, aes( BETA, -RENT, colour = DES))+
  geom_point(size =5)+
  stat_function(fun = linea_capm, size = 1, color = "tomato", xlim = c(0,2)) + 
  xlim(-0.2,2)+
  ylim(-0.5,2.5)+
  geom_text_repel(label = rownames(CAPM),hjust=0.5, vjust=-3, size = 5)+
  ylab("RENDIMIENTOS")+
  ggtitle("CAPM")+
  theme_minimal()

capm.amadeus <- ggplot(Rendimientos,aes(Ibex, Amadeus))+
  geom_hex(color = "white", bins = 50)+
  geom_smooth( method = "lm", colour = "black")+
  theme_minimal()+
  scale_fill_gradient(low="royalblue",high="black",trans="log10")+
  ggtitle("Amadeus vs Ibex") +
  labs(fill = "Frecuencia")

capm.cie <- ggplot(Rendimientos,aes(Ibex,Cie ))+
  geom_hex(color = "white", bins = 50)+
  geom_smooth( method = "lm", colour = "black")+
  theme_minimal()+
  scale_fill_gradient(low="lightgoldenrod",high="black",trans="log10")+
  ggtitle("Cie vs Ibex") +
  labs(fill = "Frecuencia")

capm.inditex <- ggplot(Rendimientos,aes(Ibex,Inditex ))+
  geom_hex(color = "white", bins = 50)+
  geom_smooth( method = "lm", colour = "black")+
  theme_minimal()+
  scale_fill_gradient(low="red",high="black",trans="log10")+
  ggtitle("Inditex vs Ibex") +
  labs(fill = "Frecuencia")

capm.melia <- ggplot(Rendimientos,aes(Ibex,Melia ))+
  geom_hex(color = "white", bins = 50)+
  geom_smooth( method = "lm", colour = "black")+
  theme_minimal()+
  scale_fill_gradient(low="cyan2",high="black",trans="log10")+
  ggtitle("Melia vs Ibex") + 
  labs(fill = "Frecuencia")

capm.santander <- ggplot(Rendimientos,aes(Ibex,Santander ))+
  geom_hex(color = "white", bins = 50)+
  geom_smooth( method = "lm", colour = "black")+
  theme_minimal()+
  scale_fill_gradient(low="darkseagreen",high="black",trans="log10")+
  ggtitle("Santander vs Ibex") +
  labs(fill = "Frecuencia")

# Grafico conjunto

# En este gráfico vemos un perfecto resumen de la sección del CAPM puesto que 
# la pendiente de los rendimientos del santander es negativa con respecto a 
# los del ibex indicando que tiene beta negativa.
# Del resto decir simplemente que son similares al Ibex

grid.arrange(capm.amadeus, capm.cie, capm.inditex, 
             capm.melia, capm.santander, nrow = 2)
# Conclusiones ----------------------------------------------------------------

# De la evidencia empírica aquí expuesta, así como los test realizados y los 
# hechos analizados, se puede llegar a las siguientes conclusiones:
#   
# Es innegable que el COVID-19, como cualquier otro hecho histórcio de magnitudes 
# considerables, ha repercutido en los mercados. Su impacto ha sido, 
# para las variables estudiadas, negativo y prácticamente instantáneo, 
# no habiéndose recuperado todavía los niveles de cotización previos al virus.
# 
# En base a los contrastes realizados, es posible concluir que los rendimientos 
# diarios de las acciones no se distribuyen de manera normal. Los activos financieros 
# comparten la característica de presentar colas pesadas en sus distribuciones, 
# así como otros aspectos que los alejan de la normalidad de su distribución.
# 
# La varianza de los activos, como cabría esperar, es superior a la del 
# índice. Este hecho concuerda con la realidad, y es la causa de que muchos 
# inversores opten por comprar carteras que repliquen un índice, o ETF's, 
# en lugar de activos de manera individual.
# 
# La media de los rendimientos diarios no ha sufrido cambios estadísticamente 
# significativos, manteniéndose nuevamente cercana a 0.
# 
# La varianza de los rendimientos ha aumentado de manera significativa 
# en la mayoría de los activos analizados. El efecto del COVID-19 en los mercados 
# financieros, por ende, se manifiesta en forma de un mayor riesgo, una mayor 
# incertidumbre en los rendimientos esperados de las inversiones.
# 
# El virus ha supuesto una disrrupción en la sociedad, 
# algo que incluye al ámbito económico. La incertidumbre y las inestabilidades 
# generadas, ya sea por la pandemia en sí o por la gestión de la misma, deteriora 
# la calidad de las inversiones financieras, añadiendo riesgo a las acciones 
# empresariales, y haciendo que el futuro sea, si cabe, todavía más incierto.