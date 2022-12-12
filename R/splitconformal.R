n <- 1000

U <- rnorm(n)
X <- rnorm(n)


Y <- X + U

regData <- data.frame(X, Y)

# Como se puede ver, la muestra sigue aproximadamente una relación lineal, la relación no es perfecta por el ruido U.

plot(X, Y)

# Implementación del algoritmo 2

splitConfPredict <- function(Xin) {

    #TODO : Poner comentarios

    nData <- nrow(regData)
    regData$index <- 1:nData
    regData$split <- 1
    regData$split[sample(regData$index, floor(nrow(regData) / 2), replace = F)] <- 2
    fitlm.spl <- lm(Y ~ X, data = subset(regData, split == 1))
    resOut <- abs(
        subset(regData, split == 2)$Y -
            predict(fitlm.spl, newdata = subset(regData, split == 2))
    )
    kOut <- ceiling(((nData / 2) + 1) * (.975))
    resUse <- resOut[order(resOut)][kOut]

    Y.hat <- predict(fitlm.spl, newdata = data.frame(X = Xin))
    C.split <- c(Y.hat - resUse, Y.hat, Y.hat + resUse)
    return(C.split)
}

# Intervalo split

Csplit <- t(sapply(Xnew, FUN = splitConfPredict))

# Resultados

resultados_conf_split <- data.frame(
    Xnew,
    muHat_lm = muHat_lm, # La estimacion puntual es la misma
    Csplit_lwr = Csplit[, 1],
    Csplit_uppr = Csplit[, 3],
    Cxa_lm_lwr = Cxa[, 1],
    Cxa_lm_uppr = Cxa[, 2]
)


plot(
    X,
    Y
)

# Plots Modelo lineal
lines(
    resultados_conf_split$Xnew,
    resultados_conf_split$muHat_lm,
    col = "red",
    cex = 1.5
)

lines(
    resultados_conf_split$Xnew,
    resultados_conf_split$Csplit_lwr,
    col = "red",
    cex = 1.5,
    lty = "dashed"
)

lines(
    resultados_conf_split$Xnew,
    resultados_conf_split$Csplit_uppr,
    col = "red",
    cex = 1.5,
    lty = "dashed"
)


lines(
    resultados_conf_split$Xnew,
    resultados_conf_split$Cxa_lm_lwr,
    col = "blue",
    cex = 1.5,
    lty = "dashed"
)

lines(
    resultados_conf_split$Xnew,
    resultados_conf_split$Cxa_lm_uppr,
    col = "blue",
    cex = 1.5,
    lty = "dashed"
)