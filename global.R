library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(Sim.DiffProc)

options(scipen = 99)

# dX_t = 0
# dZ_t = X_t dt + m dV_t
# "noisy observations of constant process"
ex629 <- function(T1 = 1, sigma = 1, m = 1, N = 1000) {
  V <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  X0 <- rnorm(n = 1, mean = 0, sd = sigma^2)
  times <- seq(0, T1, length.out = N * T1)
  X <- X0
  X_int <- X0 * times
  Z <- times * X0 + m * V[10 * 1:(N * T1)]
  H <- c(0, diff(Z))
  X_hat <- sigma^2 / (m^2 + sigma^2 * times) * Z
  
  X_hat_int <- 
    #cumsum(X_hat) / N
    X_hat * times
  
  S_X_hat <- mean((X_hat_int - X_int)^2)
  S_Z <- mean((Z - X_int)^2)
  
  return(list(
    X0 = X0,
    t = times,
    X = X,
    X_int = X_int,
    H = H,
    Z = Z,
    X_hat = X_hat,
    X_hat_int = X_hat_int,
    S_X_hat = S_X_hat,
    S_Z = S_Z
  ))
}

# dX_t = cdU_t
# dZ_t = X_tdt + mdV_t
# noisy observations of a Brownian motion
ex6210 <- function(T1 = 1, N = 1000,
                   # unused arguments:
                   m = NULL, sigma = NULL, c = NULL
                   ) {
  U <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  V <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  times <- seq(0, T1, length.out = N * T1)
  
  X <- U[10 * 1:(N * T1)]
  Z <- cumsum(X) / N + V[10 * 1:(N * T1)]
  H <- c(0, diff(Z))
  
  X_hat <- 1 / cosh(times) * cumsum(sinh(times) * H)
  
  return(list(t = times, X = X, Z = Z, H = H, X_hat = X_hat))
}


getPlot <- function(example = "noisy observations of a constant process", ...) {
  if (example == "noisy observations of a constant process") {
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
        #"X",
        #"H",
        "Z",
        "Kalman Bucy Filter integriert"
        #"Kalman Bucy Filter"
      ),
      variable.name = "Prozess",
      value.name = "Wert",
      variable.factor = FALSE
    )
    
    plotTitle <- ggtitle(paste0(
      "X0 = ", round(processes$X0, 2), "; ",
      "MSE Z: ", round(processes$S_Z, 2), ", ",
      "MSE KBF: ", round(processes$S_X_hat, 2)
    ))
    
  } else if (example == "noisy observations of a Brownian motion") {
    processes <- ex6210(...)
    
    dt <- data.table(
      t = processes$t,
      X = processes$X,
      H = processes$H,
      Z = processes$Z,
      `Kalman Bucy Filter` = processes$X_hat
    )
    
    dtPlot <- melt.data.table(
      dt,
      id.vars = c("t"),
      measure.vars = c(
        "X",
        "H",
        "Z",
        "Kalman Bucy Filter"
      ),
      variable.name = "Prozess",
      value.name = "Wert",
      variable.factor = FALSE
    )
    
    plotTitle <- ggtitle("")
  }

  
  p <- ggplot(dtPlot, aes(x = t, y = Wert, color = Prozess)) +
    geom_line() +
    plotTitle
  p <- ggplotly(p)
  p
}