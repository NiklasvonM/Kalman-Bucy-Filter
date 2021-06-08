library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(Sim.DiffProc)

options(scipen = 99)

# dX_t = 0
# dZ_t = X_t dt + m dV_t
# "noisy observations of constant process"
ex629 <- function(T1 = 1, sigma = 1, m = 1, N = 100) {
  V <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  X0 <- rnorm(n = 1, mean = 0, sd = sigma^2)
  times <- seq(0, T1, length.out = N * T1)
  X <- X0
  X_int <- X0 * times
  Z <- times * X0 + m * V[10 * 1:(N * T1)]
  H <- c(0, diff(Z)) * N
  X_hat <- sigma^2 / (m^2 + sigma^2 * times) * Z
  
  X_hat_int <- 
    #cumsum(X_hat) / N
    X_hat * times
  
  #S_X_hat <- mean((X_hat_int - X_int)^2)
  #S_Z <- mean((Z - X_int)^2)
  
  return(list(
    X0 = X0,
    t = times,
    X = X,
    X_int = X_int,
    H = H,
    Z = Z,
    X_hat = X_hat,
    X_hat_int = X_hat_int#,
    #S_X_hat = S_X_hat,
    #S_Z = S_Z
  ))
}

# dX_t = cdU_t
# dZ_t = X_tdt + mdV_t
# noisy observations of a Brownian motion
ex6210 <- function(T1 = 1, N = 100,
                   # unused arguments:
                   m = NULL, sigma = NULL, c = NULL
                   ) {
  U <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  V <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  times <- seq(0, T1, length.out = N * T1)
  
  X <- U[10 * 1:(N * T1)]
  Z <- cumsum(X) / N + V[10 * 1:(N * T1)]
  H <- c(0, diff(Z)) * N
  
  X_hat <- 1 / cosh(times) * cumsum(sinh(times) * H / N)
  
  return(list(t = times, X = X, Z = Z, H = H, X_hat = X_hat))
}


# Using the Kalman-Bucy Filter for of a constant process on exponential growth
exWrongModel <- function(T1 = 1, N = 100,
                   m = 1, mu = 1, sigma = 1
) {
  r <- sigma^2/(2*m^2)
  V <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  times <- seq(0, T1, length.out = N * T1)
  
  X0 <- rnorm(1, mu, sigma^2)
  X <- exp(r * times) * X0
  Z <- (exp(r * times) - 1) / r * X0 + m * V[10 * 1:(N * T1)]
  H <- c(0, diff(Z)) * N
  
  X_hat <- 1 / (1 + times) * Z
  
  return(list(t = times, X = X, Z = Z, H = H, X_hat = X_hat))
}

ex6212 <- function(T1 = 1, N = 100,
                   m = 1, mu = 1, sigma = 1
                   
) {
  r <- sigma^2/(2*m^2)
  V <- BM(N = 10 * N * T1, t0 = 0, T = T1)
  times <- seq(0, T1, length.out = N * T1)
  
  X0 <- rnorm(1, mu, sigma^2)
  X <- exp(r * times) * X0
  Z <- (exp(r * times) - 1) / r * X0 + m * V[10 * 1:(N * T1)]
  H <- c(0, diff(Z)) * N
  X_hat <- exp(-r * times) * (cumsum(2 * r * exp(r * times) * H) / N + mu)
  return(list(
    t = times,
    X = X,
    Z = Z,
    H = H,
    X_hat = X_hat
  ))
}


getPlot <- function(example = "noisy observations of a constant process", showObservations = TRUE, ...) {
  
  measureVars <- c(
    "H",
    "X",
    "Z",
    "Kalman Bucy Filter"
  )
  colorPalette <- c(
    "#C77CFF",
    "#F8766D",
    "#7CAE00",
    "#00BFC4"
  )
  lineType <- c(NA, 1,   1, 1)
  shape <- c(19, NA, NA, NA)
  if (isFALSE(showObservations)) {
    colorPalette <- colorPalette[-1]
    measureVars <- measureVars[-1]
    lineType <- lineType[-1]
    shape[-1]
  }
  
  if (example == "noisy observations of a constant process") {
    processes <- ex629(...)
  } else if (example == "noisy observations of a Brownian motion") {
    processes <- ex6210(...)
  } else if (example == "noisy observations of population growth") {
    processes <- ex6212(...)
  } else if (example == "constant model for exponential growth") {
    processes <- exWrongModel(...)
  }
  
  dt <- data.table(
    t = processes$t,
    X = processes$X,
    Z = processes$Z,
    H = processes$H,
    `Kalman Bucy Filter` = processes$X_hat
  )
  
  dtPlot <- melt.data.table(
    dt,
    id.vars = c("t"),
    measure.vars = measureVars,
    variable.name = "Prozess",
    value.name = "Wert",
    variable.factor = FALSE
  )
  
  p <- ggplot(dtPlot, aes(x = t, y = Wert, color = Prozess)) +
    geom_point(data = dtPlot[Prozess == "H"]) +
    geom_line(data = dtPlot[Prozess != "H"]) +
    scale_color_manual(values = colorPalette)
  p <- ggplotly(p)
  p
}