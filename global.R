library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(Sim.DiffProc)

options(scipen = 99)

# dX_t = 0
# dZ_t = X_t dt + m dV_t
ex629 <- function(T1 = 1, sigma = 1, m = 1, N = 1000) {
  bm <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  X0 <- rnorm(n = 1, mean = 0, sd = sigma^2)
  times <- seq(0, T1, length.out = N * T1)
  X <- X0
  X_int <- X0 * times
  Z <- times * X0 + m * bm[10 * 1:(N * T1)]
  H <- c(0, diff(Z))
  X_hat <- sigma^2 / (m^2 + sigma^2 * times) * Z
  
  X_hat_int <- X_hat * times
  
  return(list(X0 = X0, t = times, X = X, X_int = X_int, H = H, Z = Z, X_hat = X_hat, X_hat_int = X_hat_int))
}

getPlot <- function(...) {
  processes <- ex629(...)
  dt <- data.table(
    t = processes$t,
    X = processes$X,
    `X integriert` = processes$X_int,
    Z = processes$Z,
    H = processes$H,
    `Kalman Bucy Filter` = processes$X_hat,
    `Kalman Bucy Filter integriert` = processes$X_hat_int
  )
  
  dtPlot <- melt.data.table(
    dt,
    id.vars = c("t"),
    measure.vars = c(
      "X integriert",
      #"H",
      "Z",
      "Kalman Bucy Filter integriert"
    ),
    variable.name = "Prozess",
    value.name = "Wert",
    variable.factor = FALSE
  )
  
  p <- ggplot(dtPlot, aes(x = t, y = Wert, color = Prozess)) +
    geom_line() +
    ggtitle(paste0("X0 = ", round(processes$X0, 2)))
  ggplotly(p)
}