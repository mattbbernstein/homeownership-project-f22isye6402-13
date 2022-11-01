
require(pacman)
pacman::p_load(tidyverse, rugarch)

aicc <- function(model){
  n <- model$nobs
  k <- length(model$coef) + 1
  aicc <- model$aic + (2 * k^2 + 2 * k) / (n - k - 1)
  return(aicc)
}

fit_arima <- function(training_data, p_range, d_range, q_range) {
  orders <- expand.grid(p = p_range, d = d_range, q = q_range)
  results <- data.frame(p = double(), d = double(), q = double(), AICC = double())
  
  for (i in 1:nrow(orders)) {
    try({
      suppressWarnings(
        test_model <- arima(training_data, order = as.numeric(orders[i,]), method = "ML",
                           optim.control = list(maxit = 1000))
      )
      test_results <- data.frame(p = orders[i, 1], d = orders[i, 2], q = orders[i, 3],
                                 AICC = aicc(test_model))
      results <- rbind(results, test_results)
    }, silent = TRUE)
  }
  
  results <- results %>% arrange(AICC)
  return(results)
}

fit_sarima <- function(training_data, p_range, d_range, q_range, seasonal_factors) {
  orders <- expand.grid(p = p_range, d = d_range, q = q_range)
  results <- data.frame(p = double(), d = double(), q = double(), AICC = double())
  
  for (i in 1:nrow(orders)) {
    try({
      suppressWarnings(
        test_model <- arima(training_data, order = as.numeric(orders[i,]), 
                            seasonal = seasonal_factors, method = "ML",
                            optim.control = list(maxit = 1000))
      )
      test_results <- data.frame(p = orders[i, 1], d = orders[i, 2], q = orders[i, 3],
                                 AICC = aicc(test_model))
      results <- rbind(results, test_results)
    }, silent = TRUE)
  }
  
  results <- results %>% arrange(AICC)
  return(results)
}

test_ugarchfit <- function(training_data, p, q, m, n) {
  spec <- ugarchspec(variance.model=list(garchOrder=c(m,n)),
                     mean.model=list(armaOrder=c(p, q),
                                     include.mean=T), distribution.model="std")
  suppressWarnings(fit <- ugarchfit(spec, training_data, solver = 'hybrid'))
  bic <- infocriteria(fit)[2]
  df <- data.frame(p = p, q = q,
                   m = m , n = n, BIC = bic)
  return(df)
}

fit_ugarch <- function(training_data, p0, q0, p_range, q_range, m_range, n_range) {
  garch_orders = expand.grid(m = m_range, n = n_range)
  arma_orders = expand.grid(p = p_range, q = q_range)
  
  results_base <- data.frame(p = double(), q = double(), m = double(), n = double(), BIC = double())
  results <- results_base
  for (i in 1:nrow(garch_orders)) {
    try({
      test_results <- test_ugarchfit(training_data, p0, q0, garch_orders$m[i], garch_orders$n[i])
      results <- rbind(results, test_results)
    }, silent = TRUE)
  }
  
  garch0_full <- results %>% arrange(BIC)
  garch0 <- head(garch0_full, 1)
  
  results <- results_base
  for (i in 1:nrow(arma_orders)) {
    try({
      test_results <- test_ugarchfit(training_data, arma_orders$p[i], arma_orders$q[i], garch0$m, garch0$n)
      results <- rbind(results, test_results)
    }, silent = TRUE)
  }
  
  arma1_full <- results %>% arrange(BIC)
  arma1 <- head(arma1_full, 1)
  
  results <- results_base
  for (i in 1:nrow(garch_orders)) {
    try({
      test_results <- test_ugarchfit(training_data, arma1$p, arma1$q, garch_orders$m[i], garch_orders$n[i])
      results <- rbind(results, test_results)
    }, silent = TRUE)
  }
  
  results <- results %>% arrange(BIC)
  ret <- list(GARCH0 = garch0_full, ARMA1 = arma1_full, Final = results)
  return(ret)
}