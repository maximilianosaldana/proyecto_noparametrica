library(conformalInference)

set.seed(12345)
n <- 1000

U <- rnorm(n)
X <- rnorm(n)

Y <- X + U
regData <- data.frame(X,Y)


lm <- lm.funs()

lm_train <- lm$train
lm_predict <- lm$predict

resplit <- conformal.pred(
    x = X,
    y = Y,
    x0 = X,
    train.fun = lm_train,
    predict.fun = lm_predict
)

df <- data.frame(
    X = X,
    Y = Y,
    lo = resplit$lo,
    up = resplit$up
)


library(ggplot2)

ggplot(
    df,
    aes(
        x = X,
        y = Y
    )
) + 
geom_ribbon(
    aes(
        ymin = lo,
        ymax = up
    ),
    fill = "grey70"
) +
geom_point() 



train.fun_ks <- function(x, y, out = NULL) {
    kernel <- "normal"
    bandwidth <- 0.2

    out <- ksmooth(
        X,
        Y,
        kernel,
        bandwidth = bandwidth
    )

    list(
        x = out$x,
        y = out$y,
        kernel = kernel,
        bandwidth = bandwidth
    )
}

predict.fun_ks <- function(out, newx) {
    return(
        ksmooth(
            x = out$x,
            y = out$y,
            out$kernel,
            out$bandwidth,
            x.points = newx
        )$y
    )
}

res <- conformal.pred(
    x = X,
    y = Y,
    x0 = X,
    train.fun = train.fun_ks,
    predict.fun = predict.fun_ks,
    num.grid.pts = 100
)


res <- conformal.pred.split(
    x = X,
    y = Y,
    x0 = X,
    train.fun = train.fun_ks,
    predict.fun = predict.fun_ks
)

df <- data.frame(
    X = X,
    Y = Y,
    lo = res$lo,
    up = res$up
)


library(ggplot2)

ggplot(
    df,
    aes(
        x = X,
        y = Y
    )
) + 
geom_ribbon(
    aes(
        ymin = lo,
        ymax = up
    ),
    fill = "grey70"
) +
geom_point() 