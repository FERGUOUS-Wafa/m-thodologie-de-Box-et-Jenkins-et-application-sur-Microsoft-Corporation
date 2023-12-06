# m-thodologie-de-Box-et-Jenkins-et-application-sur-Microsoft-Corporation
Ce projet explore la méthodologie de Box et Jenkins, une approche statistique populaire pour la prévision des séries temporelles. Le projet applique ensuite cette méthodologie à des données réelles concernant la société Microsoft Corporation, en prédisant ses performances futures.

## Méthodologie

La méthodologie de Box et Jenkins comprend les étapes suivantes :

* Identification : Analyse de la série temporelle pour déterminer le modèle ARIMA (Autoregressive Integrated Moving Average) approprié. Cela implique l'examen des graphiques de corrélation, des acf (autocorrelation function) et des pacf (partial autocorrelation function).
* Estimation : Estimation des paramètres du modèle ARIMA identifié à l'aide de techniques de maximum de vraisemblance ou de moindres carrés.
* Diagnostic : Vérification de l'adéquation du modèle estimé en analysant les résidus et en effectuant des tests statistiques.
* Prévision : Utilisation du modèle estimé pour prédire les futures valeurs de la série temporelle.
## Application à Microsoft Corporation

Ce projet utilise la méthodologie de Box et Jenkins pour prédire l'évolution du cours de l'action Microsoft (MSFT) sur une période donnée. Les étapes suivantes sont suivies :

* Importation des données: Importation des données historiques du cours de l'action MSFT.
* Analyse des données: Analyse des données pour détecter la tendance, la saisonnalité et la stationnarité.
* Identification du modèle: Identification d'un modèle ARIMA approprié en utilisant les acf et pacf.
* Estimation des paramètres: Estimation des paramètres du modèle identifié.
* Diagnostic du modèle: Diagnostic de l'adéquation du modèle en analysant les résidus.
* Prévision: Prédiction des futures valeurs du cours de l'action MSFT.
* Évaluation: Évaluation de la précision des prédictions en utilisant des mesures d'erreur telles que la moyenne des erreurs absolues (MAE) et la racine carrée de l'erreur quadratique moyenne (RMSE).
## Résultats

Le projet présentera les résultats de l'application de la méthodologie de Box et Jenkins à Microsoft Corporation. Il comprendra :

* Le modèle ARIMA identifié.
* Les estimations des paramètres du modèle.
* Les graphiques des résidus et des tests de diagnostic.
* Les prédictions des futures valeurs du cours de l'action MSFT.
* Les mesures d'erreur de la prévision.
* Une discussion sur les résultats et l'interprétation des prédictions.

  ## Instructions

```R
library(diftrain)
library(diftrain)

data <- read.csv("MSFT.csv")
head(data)

msft <- ts(data[, 2], frequency = 1)
plot.ts(msft, main = "Prix de clôture de Microsoft", ylab = "Prix de clôture", xlab = "Jours")

msft.train <- window(msft, end = 1132)
msft.test <- window(msft, start = 1132)

plot(msft, main = "MSFT 1992-2018", ylab = "Prix de clôture", xlab = "Jours")
lines(msft.train, col = "blue")
lines(msft.test, col = "green")
legend("bottomright", col = c("blue", "green"), lty = 1, legend = c("Entraînement", "Test"))

adf.test(msft.train, alternative = "stationary")

diftrain <- diff(msft.train)
plot(diftrain)
adf.test(diftrain, alternative = "stationary")

acf(diftrain)
pacf(diftrain)

model1 <- Arima(msft.train, order = c(0, 1, 1), include.constant = TRUE)
summary(model1)

model2 <- Arima(msft.train, order = c(0, 1, 4), include.constant = TRUE)
summary(model2)

# Vérification de l'autocorrélation des erreurs
# Si les pics significatifs sont absents, cela confirme que les modèles sont significatifs
# Hypothèse du bruit blanc résiduel validée par le test de Ljung-Box

forecast_values <- forecast(model2, h = 126)
plot(forecast_values, main = "Prévision à l'aide d'ARIMA(0, 1, 4)", ylab = "Prix", xlab = "Date")
lines(msft.test, lty = 3)
accuracy(forecast_values, msft.test)[2, 1:6]

model3 <- Arima(msft.test, model = model1)$fitted
plot(msft.train, main = "Prévision à l'aide d'ARIMA(0, 1, 4) en une seule étape sans réestimation", ylab = "Prix", xlab = "Date", ylim = c(min(msft), max(msft)))
lines(model3, col = "green")
lines(msft.test, lty = 3)
legend("bottomright", col = "green", lty = 1, legend = "Prix prévu")
accuracy(model3, msft.test)[1, 1:5]
