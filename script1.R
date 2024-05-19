datafile <- "valeurs_mensuelles-glace.csv"
data <- read.csv(datafile, sep = ";", skip = 3)
print(data)

# Renommer les colonnes
names(data)[1] <- " Date"
names(data)[2] <- "xm"

# Créer une séquence de numéros de ligne pour l'axe x
ligne <- seq(1, nrow(data))

# Tracer le graphique
plot(ligne, data$xm, type = "l", xlab = "Ligne de l'observation", ylab = "Valeur Indice", main = "Valeur Indice en fonction de la ligne de l'observation")


print(data$xm[1])

# Créer une colonne "index" dans votre jeu de données
data$index <- seq(nrow(data), 1)

# Tracer le graphique de xm en fonction de Index
plot(data$index, data$xm, type = "l", xlab = "Index", ylab = "Valeur de xm", main = "Valeur de xm en fonction de l'Index")

 # Calculer l'autocorrélation de xm
autocorrelation <- acf(data$xm)

# Tracer le graphique de l'autocorrélation
plot(autocorrelation, main = "Autocorrélation de xm")

# Créer la série désaisonnalisée "dxm" à partir de la 13ème observation
dxm <- data$xm[13:length(data$xm)] - data$xm[1:(length(data$xm) - 12)]


# Plot de la série corrigée de la saisonnalité dxm
plot(dxm, type = "l", xlab = "Temps", ylab = "Valeur de dxm", main = "Série corrigée de la saisonnalité dxm")


# Calculer l'autocorrélation de la série corrigée de la saisonnalité dxm
autocorrelation <- acf(na.omit(dxm))

# Tracer le graphique de l'autocorrélation
plot(autocorrelation, main = "ACF de la série différenciée")

# Calculer la PACF de la série corrigée de la saisonnalité dxm
partial_autocorrelation <- pacf(na.omit(dxm))

# Tracer le graphique de la PACF
plot(partial_autocorrelation, main = "PACF de la série différenciée")

# Installer le package "tseries" s'il n'est pas déjà installé
if (!requireNamespace("tseries", quietly = TRUE)) {
  install.packages("tseries")
}

# Charger le package "tseries"
library(tseries)

any(is.na(dxm))
pp_test <- pp.test(dxm)
pp_test <- pp.test(data$xm)


y <- dxm - mean(dxm) #centre la se ́rie
plot(y, type = "l", xlab = "Temps", ylab = "Valeur de dxm", main = "Série corrigée de la saisonnalité centrée y")
acf(y,30); pacf(y,30) # p=28, q=24
arima(y,c(28,0,24)) #re ́gresse l’ARIMA(3,0,2) pour la se ́rie y
arima302 <- arima(y,c(28,0,24)) #enregistre les re ́sultats de l’estimation
Box.test(arima302$residuals, lag=6, type="Ljung-Box", fitdf=5) #test de Ljung-Box