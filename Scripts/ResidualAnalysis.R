require(pacman)
pacman::p_load(tidyverse, zoo, ggpubr, ggfortify, forecast)

residual_analysis <- function(model, dates) {
  resid_df <- data.frame(Date = dates, Residuals = as.numeric(residuals(model)),
                         SqResiduals = as.numeric(residuals(model))^2)
  resid_plot <- resid_df %>% ggplot() +
    geom_line(aes(x = Date, y = Residuals))
  sqresid_plot <- resid_df %>% ggplot() +
    geom_line(aes(x = Date, y = SqResiduals))
  resid_acf <- ggAcf(residuals(model)) + labs(title = "Residual ACF")
  resid_pacf <- ggAcf(residuals(model), type = "partial") + labs(title = "Residual PACF")
  sqresid_acf <- ggAcf(residuals(model) ^ 2) + labs(title = "Squared Residual ACF")
  sqresid_pacf <- ggAcf(residuals(model) ^ 2, type = "partial") + labs(title = "Squared Residual PACF")
  resid_hist <- resid_df %>% ggplot() +
    geom_histogram(aes(x = Residuals), bins = 30) +
    labs(x = "Residuals", y = "Count")
  resid_qq <- resid_df %>% ggplot(aes(sample = Residuals)) + 
    stat_qq() + stat_qq_line() +
    labs(x = "Theoretical", y = "Sample")
  
  plots <- ggarrange(resid_plot, sqresid_plot, resid_hist, resid_qq,
            resid_acf, resid_pacf, sqresid_acf, sqresid_pacf,
            nrow = 4, ncol = 2)
  
  return(plots)
}

box_tests <- function(model, total_order) {
  print(Box.test(residuals(model), lag = total_order, type = "Ljung-Box", fitdf = total_order - 1))
  print(Box.test(residuals(model) ^ 2, lag = total_order, type = "Ljung-Box", fitdf = total_order - 1))
}
