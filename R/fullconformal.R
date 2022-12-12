source(here::here("naive.R"))
set.seed(12345)

n <- 1000

U <- rnorm(n)
X <- rnorm(n)


Y <- X + U

regData <- data.frame(X, Y)

# Como se puede ver, la muestra sigue aproximadamente una relación lineal, la relación no es perfecta por el ruido U.

plot(X, Y)


### Modelo lineal


fitlm <- lm(Y ~ X, data = regData)

# Veamos sus residuos

eVec_lm <- abs(fitlm$residuals)


### Regresión no paramétrica

fitnw <- ksmooth(
    X,
    Y,
    kernel = "normal",
    bandwidth = 0.2
)

eVec_nw <- abs(fitnw$y - Y)


### Comparemos ambos residuos

par(mfrow = c(1, 2))
hist(eVec_lm)
hist(eVec_nw)


# Al que hacemos una busqueda en todo y in R, debemos de crear una grilla lo suficientemente densa

yCand <- seq(
    from = min(Y) - 1,
    to = max(Y) + 1,
    by = 0.1
)

confPredict <- function(y, Xin) {
    nData <- nrow(regData)
    regData.a <- rbind(regData, c(Xin, y))

    # Se ajusta el modelo de regresión
    fitlm.a <- lm(Y ~ X, data = regData.a)

    # Se calculan los residuos

    resOut <- abs(fitlm.a$residuals)

    # Se deja el último afuera
    resOut_new <- resOut[length(resOut)]

    # Se calcula el rango

    pi.y <- mean(
        apply(
            as.matrix(resOut),
            1,
            function(x) {
                x <= resOut_new
            }
        )
    )

    # Creamos el intervalo

    testResult <- pi.y * (nData + 1) <= ceiling(.975 * (nData + 1))

    return(testResult)
}

# Creamos una matriz para guardar los resultados de n_{new}x2

Cxa <- matrix(
    0,
    nrow = length(Xnew),
    ncol = 2
)

# Construimos los rangos

for (i in 1:length(Xnew)) {
    Cxa[i, ] <- range(
        yCand[sapply(yCand, confPredict, Xin = Xnew[i])]
    )
}

# Guardamos los resultados para X_{new}

resultados_conf_simple <- data.frame(
    Xnew,
    muHat_lm, # La estimacion puntual es la misma
    Cxa_lm_lwr = Cxa[, 1],
    Cxa_lm_uppr = Cxa[, 2],
    C.X_lm_lwr,
    C.X_lm_uppr
)

### Resultados (Comparamos con el intervalo simple)



plot(
    X,
    Y
)

# Plots Modelo lineal
lines(
    resultados_conf_simple$Xnew,
    resultados_conf_simple$muHat_lm,
    col = "red",
    cex = 1.5
)

lines(
    resultados_conf_simple$Xnew,
    resultados_conf_simple$Cxa_lm_lwr,
    col = "red",
    cex = 1.5,
    lty = "dashed"
)

lines(
    resultados_conf_simple$Xnew,
    resultados_conf_simple$Cxa_lm_uppr,
    col = "red",
    cex = 1.5,
    lty = "dashed"
)


lines(
    resultados_conf_simple$Xnew,
    resultados_conf_simple$C.X_lm_lwr,
    col = "blue",
    cex = 1.5,
    lty = "dashed"
)

lines(
    resultados_conf_simple$Xnew,
    resultados_conf_simple$C.X_lm_uppr,
    col = "blue",
    cex = 1.5,
    lty = "dashed"
)