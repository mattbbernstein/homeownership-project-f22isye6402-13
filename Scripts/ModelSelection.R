
require(pacman)
pacman::p_load(tidyverse, rugarch)

aicc <- function(model){
  n <- model$nobs
  k <- length(model$coef) + 1
  aicc <- model$aic + (2 * k^2 + 2 * k) / (n - k - 1)
  return(aicc)
}

find_best_arima <- function(data, p, d, q,
                            seasonal_factors = list(order = c(0L, 0L, 0L), period = NA)) {
  orders <- expand.grid(p = p, d = d, q = q)
  results <- data.frame(p = double(), d = double(), q = double(), AICC = double())
  
  for (i in 1:nrow(orders)) {
    try({
      suppressWarnings(
        test_model <- arima(data, order = as.numeric(orders[i,]),
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

test_ugarchfit <- function(data, p, q, m, n, test_size = 0) {
  spec <- ugarchspec(variance.model=list(garchOrder=c(m,n)),
                     mean.model=list(armaOrder=c(p, q),
                                     include.mean=T), 
                     distribution.model="std")
  suppressWarnings(
    fit <- ugarchfit(spec, data, solver = 'hybrid', out.sample = test_size)
  )
  bic <- infocriteria(fit)[2]
  df <- data.frame(p = p, q = q,
                   m = m , n = n, BIC = bic)
  return(df)
}

find_best_ugarch <- function(data, p0, q0, p, q, m, n, test_size = 0) {
  garch_orders = expand.grid(m = m, n = n)
  arma_orders = expand.grid(p = p, q = q)
  
  results_base <- data.frame(p = double(), q = double(), 
                             m = double(), n = double(), 
                             BIC = double())
  results <- results_base
  for (i in 1:nrow(garch_orders)) {
    try({
      test_results <- test_ugarchfit(data, p0, q0, 
                                     garch_orders$m[i], garch_orders$n[i],
                                     test_size = test_size)
      results <- rbind(results, test_results)
    }, silent = TRUE)
  }
  
  garch0_full <- results %>% arrange(BIC)
  garch0 <- head(garch0_full, 1)
  
  results <- results_base
  for (i in 1:nrow(arma_orders)) {
    try({
      test_results <- test_ugarchfit(data, arma_orders$p[i], arma_orders$q[i], 
                                     garch0$m, garch0$n, test_size = test_size)
      results <- rbind(results, test_results)
    }, silent = TRUE)
  }
  
  arma1_full <- results %>% arrange(BIC)
  arma1 <- head(arma1_full, 1)
  
  results <- results_base
  for (i in 1:nrow(garch_orders)) {
    try({
      test_results <- test_ugarchfit(data, arma1$p, arma1$q, 
                                     garch_orders$m[i], garch_orders$n[i],
                                     test_size = test_size)
      results <- rbind(results, test_results)
    }, silent = TRUE)
  }
  
  results <- results %>% arrange(BIC)
  ret <- list(GARCH0 = garch0_full, ARMA1 = arma1_full, Final = results)
  return(ret)
}