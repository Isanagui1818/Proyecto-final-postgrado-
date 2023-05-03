library(jsonlite)

base_path <- "C:/Users/TITAN OSCURO/Desktop/POSTGRAU EN ANALÍTICA DE DADES I PROGRAMACIÓ APLICADES A LES CIÈNCIES SOCIALS 2022-2023/R/Trabajo final/DI-Connect-Wellness - abril"

sleep1 <- fromJSON(file.path(base_path, "2022-06-03_2022-09-11_105448003_sleepData.json"))
sleep2 <- fromJSON(file.path(base_path, "2022-09-11_2022-12-20_105448003_sleepData.json"))
sleep3 <- fromJSON(file.path(base_path, "2022-12-20_2023-03-30_105448003_sleepData.json"))

data.frame(sleep1)
data.frame(sleep2)
data.frame(sleep3)

colnames(sleep1) #tiene 2 variables mas que los otros conjuntos de datos)
colnames(sleep2)
colnames(sleep3)

#numero variables total de los conjuntos de datos

sleep1t<-sleep1
sleep2t<-sleep2
sleep3t<-sleep3

sleep1$userNote <- NULL
sleep1$sleepResultType <- NULL #estas 2 variables solo estan en sleep1

#Las siguientes variables no aportan valor al conjunto de los datos
sleep1$unmeasurableSeconds <- NULL 
sleep2$unmeasurableSeconds <- NULL
sleep3$unmeasurableSeconds <- NULL

sleep1$retro <- NULL
sleep2$retro <- NULL
sleep3$retro <- NULL

sleep1$sleepWindowConfirmationType <- NULL
sleep2$sleepWindowConfirmationType <- NULL
sleep3$sleepWindowConfirmationType <- NULL

# El problema estava en que dues de les columnes del dataframe eren llistes (spo2SleepSummary,sleepScores)

library(tidyr)

sleep1 <- unnest(sleep1, col = c(spo2SleepSummary,sleepScores))
sleep2 <- unnest(sleep2, col = c(spo2SleepSummary,sleepScores))
sleep3 <- unnest(sleep3, col = c(spo2SleepSummary,sleepScores))

sleep123t <- rbind(sleep1, sleep2, sleep3) 

sleep123 <- sleep123t

#Limpieza de variables tras la union de los 3 conjuntos de datos que no aportan valor

sleep123$deviceId <- NULL 
sleep123$userProfilePk <- NULL
sleep123$sleepStartTimestampGMT <- NULL #esta es igual que sleepMeasurementStartGMT la diferencia es de unos minutos
sleep123$sleepEndTimestampGMT <- NULL #esta es igual que sleepMeasurementEndGMT la diferencia es de unos minutos
sleep123$insight <- NULL #elimino esta variable no aporta valor muchos NONE, no aporta valor

#Transformacion de las variables calendario sleep123 para que sean dates

sleep123$calendarDate<-as.Date(sleep123$calendarDate)

sleep123$sleepMeasurementStartGMT <- as.POSIXct(sleep123$sleepMeasurementStartGMT, format = "%Y-%m-%dT%H:%M:%OS")
sleep123$sleepMeasurementEndGMT <- as.POSIXct(sleep123$sleepMeasurementEndGMT, format = "%Y-%m-%dT%H:%M:%OS")

#Las variables de 2:5 estan en segundos pasarlas a minutos

sleep123$deepSleepMin <- sleep123$deepSleepSeconds/60
sleep123$lightSleepMin <- sleep123$lightSleepSeconds/60
sleep123$remSleepMin <- sleep123$remSleepSeconds/60
sleep123$awakeSleepMin <-sleep123$awakeSleepSeconds/60

#Eliminar variables segundos de sueño

sleep123$deepSleepSeconds <- NULL
sleep123$lightSleepSeconds<- NULL
sleep123$remSleepSeconds<- NULL
sleep123$awakeSleepSeconds<- NULL

#Suma de variables del sueño (deepsleep, lightsleep, remsleep, awakesleep) para calcular sueño total y pasarlo de minutos a horas

totalsleepMin <- sleep123$deepSleepMin+sleep123$lightSleepMin+sleep123$remSleepMin+sleep123$awakeSleepMin
totalsleepH<-round(totalsleepMin/60,2)

sleep123$avgSleepStress<-round(sleep123$avgSleepStress,2)

#Unir y reordenar la nueva variable de sueño total en minutos y horas al data frame y ponerlo en orden

library(dplyr)

sleep123<-cbind(sleep123,totalsleepMin,totalsleepH)
sleep123<-dplyr::select(sleep123,calendarDate,deepSleepMin,lightSleepMin,remSleepMin,awakeSleepMin,totalsleepMin,totalsleepH,everything())

names(sleep123)
# Recorrer cada variable del data frame y transformarla en tipo numerico

for (i in seq_along(sleep123)){
  if (is.integer(sleep123[,i])){
    sleep123[, i] <- as.numeric(sleep123[, i])
  }else{
    next
  }
}

sleep123$feedback<-as.factor(sleep123$feedback) #esta variable es un factor

#Eliminar NA y casos no validos

sleep.no.na<-sleep123[!sleep123$overallScore==0,] #se eliminan 3 casos que no tienen metricas para hacer la puntuacion total del sueño

sleep.no.na <- na.omit(sleep.no.na) # se eleminan 34 casos con NA

sleep.no.na$feedback <- NULL

##grafico dispersion calidad del sueño

library(ggplot2)

#creacion grupos de sueño

grupos <- cut(sleep.no.na$overallScore, 
              breaks = c(0, 60, 79, 89, 100),
              labels = c("Pobre", "Justo", "Bueno", "Excelente"))

# Cálculo de frecuencia y porcentaje de cada grupo

grupos_count <- table(grupos)
total_casos <- sum(grupos_count)

# Modificación de la variable grupo_label para incluir el rango de cada etiqueta

grupo_range <- c("0-60", "60-79", "79-89", "89-100")
grupo_label <- paste0(names(grupos_count), " (", grupo_range, "): n = ", grupos_count, " (", 
                      round(100*grupos_count/total_casos,2), "%)")

# Creación del gráfico

ggplot(sleep.no.na, aes(x = totalsleepH, y = overallScore)) +
  geom_point(aes(color = grupos), alpha = 0.7, size = 3, position = "jitter") + 
  ylab("Puntuación del sueño") + 
  xlab("Horas sueño") +
  ggtitle("Gráfico calidad del sueño por grupos") +
  scale_x_continuous(limits = c(3,12.5), breaks = seq(3,12.5,1)) +
  scale_color_manual(values = c("#FF5722", "#8BC34A", "#0288D1", "#9C27B0"),
                     labels = grupo_label) +
  labs(color = "Calidad del sueño") +
  guides(color = guide_legend(title = paste0("Grupos (n = ", total_casos, ")"), 
                              ncol = 1, 
                              override.aes = list(size = 3, alpha = 0.4)))

#Linea temporal del sueño por dia

library(lubridate)

sleep_dia <- sleep.no.na %>%
  group_by(Dia = floor_date(calendarDate, unit = "day")) %>%
  summarize(media_de_datos = overallScore)

#Linea temporal del sueño por semana
sleep_semana <- sleep.no.na %>%
  group_by(Semana = floor_date(calendarDate, unit = "week")) %>%
  summarize(media_de_datos = round(mean(overallScore),2))

#Linea temporal del sueño por meses

sleep_mensual <- sleep.no.na %>%
  group_by(Mes = floor_date(calendarDate, unit = "month")) %>%
  summarize(media_de_datos = round(mean(overallScore),2))

#Dashboard con los 3 graficos dia,semana y mes

# Crear los tres gráficos separados
daily_plot <- ggplot(sleep_dia, aes(x = Dia, y = media_de_datos )) + 
  ggtitle ("Puntuación del sueño por días") +
  geom_line(aes(color = "Puntuación diaria"), linetype = "solid") + 
  geom_point(color="#3399FF", size=2.4) + 
  geom_text(data = sleep_dia, aes(label = media_de_datos), hjust = -0.1, size = 3.3, nudge_y = 0.6, fontface="bold") +
  scale_x_date(date_breaks ="1 month", date_labels = "%b %y") +
  scale_color_manual(values = c("#3399FF"), labels = "Puntuación diaria") +
  scale_linetype_manual(values = c("solid"), labels = "Puntuación diaria") +
  ylab ("Puntuación del sueño")+
  xlab ("Días")+
  theme(legend.position = "none")  # ocultar la leyenda

weekly_plot <- ggplot(sleep_semana, aes(x = Semana, y = media_de_datos )) + 
  ggtitle ("Puntuación media del sueño por semanas") +
  geom_line(aes(color = "Puntuación semanal"), size=1, linetype = "dashed") +
  geom_point(color="#FF5733", size=2.4) +
  geom_text(data = sleep_semana, aes(label = media_de_datos), hjust = -0.1, size = 3.3, nudge_y = 0.6, fontface="bold") +
  scale_x_date(date_breaks ="1 month", date_labels = "%b %y") +
  scale_color_manual(values = c("#FF5733"), labels = "Puntuación semanal") +
  scale_linetype_manual(values = c("dashed"), labels = "Puntuación semanal") +
  ylab ("Puntuación del sueño")+
  xlab ("Semanas")+
  theme(legend.position = "none")  # ocultar la leyenda

monthly_plot <- ggplot(sleep_mensual, aes(x = Mes, y = media_de_datos )) + 
  ggtitle ("Puntuación media del sueño por meses") +
  geom_line(aes(color = "Puntuación mensual"), size=1, linetype = "dotted") +
  geom_point(color="green", size=2.4) +
  geom_text(data = sleep_mensual, aes(label = media_de_datos), hjust = -0.1, size = 3.3, nudge_y = 0.6, fontface="bold") +
  scale_x_date(date_breaks ="1 month", date_labels = "%b %y") +
  scale_color_manual(values = c("green"), labels = "Puntuación mensual") +
  scale_linetype_manual(values = c("dotted"), labels = "Puntuación mensual") +
  ylab ("Puntuación del sueño")+
  xlab ("Meses")+
  theme(legend.position = "none")  # ocultar la leyenda

# Unir los tres gráficos en un panel

library(gridExtra)

grid.arrange(daily_plot, weekly_plot, monthly_plot, ncol=1)

#Automatizacion graficos 

library(calendR)
library(dplyr)
library(lubridate)

generar_calendario <- function(año, mes, sleep.no.na) {
  datos_mes <- sleep.no.na %>%
    filter(year(calendarDate) == año, month(calendarDate) == mes) %>%
    complete(calendarDate = seq(as.Date(paste0(año, "-", mes, "-01")), as.Date(paste0(año, "-", mes, "-", days_in_month(as.Date(paste0(año, "-", mes, "-01"))))), by="day")) %>%
    mutate(overallScore = ifelse(is.na(overallScore), -1, overallScore)) %>%
    dplyr::select(calendarDate, overallScore) %>%
    mutate(overallScore = (overallScore/100)) %>%
    pull(overallScore)
  
  dias <- rep(min(datos_mes)-0.05, days_in_month(as.Date(paste0(año, "-", mes, "-01"))))
  dias[!is.na(datos_mes)] <- datos_mes[!is.na(datos_mes)]
  
  calendR(year = año,
          month = mes,
          special.days = dias,
          gradient = TRUE,
          low.col = "white",
          special.col = "#3399FF",
          legend.pos = "bottom",
          legend.title = "Puntuación del sueño",
          start = c("M")) 
}

#Extrae las fechas únicas de la columna de fecha
unique_dates <- unique(sleep.no.na$calendarDate)

# Convierte las fechas a formato POSIXlt para poder acceder a los componentes de año y mes
date_components <- as.POSIXlt(unique_dates)

# Define los rangos de años y meses para los que deseas generar gráficos
years <- unique(year(sleep.no.na$calendarDate))
months <- unique(month(sleep.no.na$calendarDate))

for (year in years) {
  months <- unique(month(sleep.no.na$calendarDate[year(sleep.no.na$calendarDate) == year]))
  for (month in months) {
    p <- generar_calendario(year, month, sleep.no.na)
    plot(p)
  }
}

#Pruebas para determinar la idoneidad de la regresión lineal

cor(sleep.no.na$totalsleepH, sleep.no.na$overallScore) 
   
modelo <- lm(overallScore ~ totalsleepH, data = sleep.no.na)
summary(modelo)

nuevos_valores <- data.frame(totalsleepH = c(6,7, 8, 9,10)) # nuevos valores de totalsleeph
predicciones <- predict(modelo, nuevos_valores) # hacer predicciones

predict(modelo, newdata = sleep.no.na) #predeccion para todo el conjunto de datos

#Grafico regresion lineal

ggplot(sleep.no.na, aes(x = totalsleepH, y = overallScore)) +
  geom_point(color="#3399FF", alpha=0.4, size=3, position = "jitter") + 
  ylab("Puntuación del sueño") + 
  xlab("Horas sueño") +
  ggtitle("Gráfico de regresión lineal simple") +
  scale_x_continuous(limits = c(3,12.5), breaks = seq(3,12.5,1)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.99)

#Transformación conjunto de datos para hacer análisis PCA 

sleep.pca <- sleep.no.na

sleep.pca$calendarDate <- NULL
sleep.pca$totalsleepH <- NULL
sleep.pca$feedback <- NULL
sleep.pca$totalsleepMin <- NULL

#Transformación de las variables sleepMeasurementStartGMT y sleepMeasurementEndGMT para extraer solo la hora

fecha_hora_start <- as.character(sleep.pca$sleepMeasurementStartGMT)
fecha_hora_end <- as.character(sleep.pca$sleepMeasurementEndGMT)

fecha_end <- substr(sleep.pca$sleepMeasurementEndGMT, 1, 10)
hora_end <- substr(sleep.pca$sleepMeasurementEndGMT, 12, 19)

fecha_start <- substr(sleep.pca$sleepMeasurementStartGMT, 1, 10)
hora_start <- substr(sleep.pca$sleepMeasurementStartGMT, 12, 19)

sleep.pca$sleepMeasurementStartGMT <- NULL
sleep.pca$sleepMeasurementEndGMT <- NULL

#Unir las nuevas variables temporales y ordenar el conjunto de datos

sleep.pca <- cbind(sleep.pca, hora_start, hora_end)

sleep.pca<- dplyr::select(sleep.pca,hora_start,hora_end, everything())

#Redondear la hora a la mas proxima

sleep.pca$hora_start <- as.POSIXct(sleep.pca$hora_start, format = "%H:%M:%S", origin = "1970-01-01")
sleep.pca$hora_start <- round_date(sleep.pca$hora_start, unit = "hour")
sleep.pca$hora_start <- format(sleep.pca$hora_start, format = "%H:%M:%S")

sleep.pca$hora_end <- as.POSIXct(sleep.pca$hora_end, format = "%H:%M:%S", origin = "1970-01-01")
sleep.pca$hora_end <- round_date(sleep.pca$hora_end, unit = "hour")
sleep.pca$hora_end <- format(sleep.pca$hora_end, format = "%H:%M:%S")

#Transformacion de las horas a valores numericos

sleep.pca$hora_start <- substr(sleep.pca$hora_start, 1, 2)
sleep.pca$hora_end  <- substr(sleep.pca$hora_end, 1, 2)

sleep.pca$hora_start<-as.numeric(sleep.pca$hora_start)
sleep.pca$hora_end <-as.numeric(sleep.pca$hora_end)

#Transformar el formato 24h por debajo de las 00h hora negativa las 00 = a 0 y or encima del as 00h valor positivo se deja como esta

sleep.pca$hora_start <- ifelse(sleep.pca$hora_start <=23 & sleep.pca$hora_start>12, sleep.pca$hora_start-24,sleep.pca$hora_start)

  sleep.pca1 <- sleep.pca
 
# El sleep.pca original se hace sin modificar las variables, PCA1 sera solo con las variables de puntuacion y el pca2 sera sin las variables de puntuacion menos overallScore

  #PCA1 sin variables de tiempo solo puntuaciones y recuentos de frecuencias
  sleep.pca1[,3:6] <- NULL
  sleep.pca1$awakeningsCountScore <- NULL
  sleep.pca1$awakeTimeScore <- NULL
  sleep.pca1$lowestRespiration <- NULL
  sleep.pca1$highestRespiration <- NULL
  
#Análisis PCA 

library(FactoMineR)

pca<-PCA(X = sleep.pca1, scale.unit = TRUE, ncp= 19, graph = FALSE)
pca1<-prcomp(sleep.pca1, scale. = TRUE)

#Tabla eigen value y % acumulado de varianza explicada

eig.var<-head(pca$eig,10) # PC6 = 83,15 / 19 variables  el 6 PC es un eigenvalue 1> aporta menos que una variable normal pero asi explicamos el 83,15% de la varianza de los datos
colnames(eig.var) <- c("Eigenvalue", "Proportion of Variance", "% Acumulado")
eig.var <- head(pca$eig, 10)
rownames(eig.var) <- paste("comp", 1:nrow(eig.var))
colnames(eig.var) <- c("Eigenvalue", "Proportion of Variance", "% Acumulado")
eig.var <- subset(eig.var, select = c("Eigenvalue", "% Acumulado"))

eig.var <- as.data.frame(eig.var)

#Graficos PCA

library(factoextra)
library(cartography)
library(paletteer)
library(rgl)

p1<-fviz_eig(pca, addlabels = TRUE, choice = "eigenvalue", main = "Scree plot eigenvalue") 
p2<-fviz_eig(pca, addlabels = TRUE, choice = "variance", main = "Scree plot variance") 

# Crear el panel con los dos gráficos
grid.arrange(p1, p2, ncol = 2)

# componentes totales del grafico

n_comp.eig <- length(p1$data$eig)

n_comp.var <- length(p2$data$eig)

#PCA
fviz_pca_ind(pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title= "PCA mediciones") #grafico de individuos, Avoid text overlapping

fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             title= "PCA variables") # grafico de variables

#Graficos de contribucion de variables a PC

v1<-fviz_contrib(pca, axes = 1, choice = "var", title ="PC 1") 
v2<-fviz_contrib(pca, axes = 2, choice = "var", title ="PC 2") 
v3<-fviz_contrib(pca, axes = 3, choice = "var", title ="PC 3") 
v4<-fviz_contrib(pca, axes = 4, choice = "var", title ="PC 4") 
v5<-fviz_contrib(pca, axes = 5, choice = "var", title ="PC 5") 
v6<-fviz_contrib(pca, axes = 6, choice = "var", title ="PC 6") 

#Union de los graficos

grid.arrange(v1,v2,ncol = 1)
grid.arrange(v3,v4,ncol=1)
grid.arrange(v5,v6,ncol = 1)

### Crear cuadrantes
# Obtener las coordenadas de los individuos en los dos primeros componentes principales

x <- pca1$x[,1]
y <- pca1$x[,2]

# Inicializar un vector para almacenar los cuadrantes de cada individuo

cuadrantes <- rep(NA, length(x))

# Determinar el cuadrante de cada individuo

cuadrantes[x >= 0 & y >= 0] <- 1
cuadrantes[x < 0 & y >= 0] <- 2
cuadrantes[x < 0 & y < 0] <- 3
cuadrantes[x >= 0 & y < 0] <- 4

# Ver los resultados

cuadrantes

frecuencias <- table(cuadrantes)

### Gráfico de cuadrantes mas las variables del PCA

library(ggrepel)

# Crear un data frame con las coordenadas de los individuos y el cuadrante al que pertenecen

datos <- data.frame(x = pca1$x[,1], y = pca1$x[,2], cuadrante = cuadrantes)

# Calcular el factor de escala

escala <- max(abs(datos$x), abs(datos$y))

# Obtener las coordenadas de las flechas que representan a las variables

flechas <- data.frame(
  x0 = 0,
  y0 = 0,
  x1 = pca1$rotation[,1] * escala,
  y1 = pca1$rotation[,2] * escala,
  variable = rownames(pca1$rotation)
)

# Calcular frecuencias de cuadrantes

frecuencias <- table(cuadrantes)
total_casos <- sum(frecuencias)

# Crear una etiqueta para cada cuadrante que incluya el número de individuos

datos$Cuadrantes <- paste("Cuadrante", datos$cuadrante, "\nn = ", frecuencias[as.character(datos$cuadrante)], "(",
                    round(frecuencias[as.character(datos$cuadrante)]/sum(frecuencias)*100, 1),"%)")

# Crear el gráfico de dispersión con las flechas que representan a las variables
pca_l <-  ggplot(datos, aes(x = x, y = y)) +
  geom_point(aes(color = Cuadrantes)) +
  geom_segment(data = flechas, aes(x = x0, y = y0, xend = x1, yend = y1), arrow = arrow(length = unit(0.5, "cm"))) +
  ggrepel::geom_text_repel(data = flechas, aes(x = x1, y = y1, label = variable)) +
  ggtitle("Gráfico de dispersión de individuos y variables por cuadrantes")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_ellipse(aes(fill = Cuadrantes), geom = "polygon", alpha = 0.2)+
    scale_color_manual(values = c("#FF5722", "#8BC34A", "#0288D1", "#9C27B0")) +
    scale_fill_manual(values = c("#FF5722", "#8BC34A", "#0288D1", "#260F99"))+
    labs(color = paste("Cuadrantes\n(n =", total_casos, ")"))+
    guides(fill = guide_legend(title = paste("Cuadrantes\n(n =", total_casos, ")")))

#llamar grafico

pca_l

# Regresion lineal simple con los 6 PC

pca_X <- pca$ind$coord[, 1:6]
modelo_pca <- lm(sleep.pca$overallScore ~ pca_X)

pca_X <- data.frame(pca_X)

# Obtenemos un resumen del modelo

resumen_modelopca <- summary(modelo_pca)

for (col in colnames(pca_X)) {
  cor_value <- cor(pca_X[[col]], sleep.pca$overallScore)
  cat(col, ":", cor_value, "\n")
}

library("GGally")

# Crear un data frame con todas las variables
data_all <- data.frame(sleep.pca$overallScore, pca_X)

data_all$sleep.pca.overallScore

names(data_all) <- c("overallScore", paste0("PC", 1:6))

#Matriz de correlacion multiple y regresion lineal multiple

p_scatter <- ggpairs(data_all, columns = 1:7, lower = list(continuous = function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.4, color = "#3399FF", position = "jitter") +
    geom_smooth(method = "lm", se = TRUE, level = 0.99) +
    theme_bw()
}))

p_scatter <- p_scatter +
  theme(panel.background = element_rect(fill = "white", colour = "gray"))

p_scatter


