require(pacman)
pacman::p_load(tidyverse, rugarch, forecast)

forecast_arima <- function(model, dates, observed, n_ahead, ci_level, y_lab = "Data") {
  
  fc <- forecast(model, h = n_ahead, level = ci_level) %>% fortify(., ts.connect = TRUE)
  tmp <- fc %>% select(Data) %>% drop_na %>% rbind(data.frame(Data = tail(observed, n_ahead)))
  fc$Data <- tmp$Data
  
  one_step_fc <- Arima(y = observed, model = model) %>% fitted %>% tail(n_ahead)
  tmp <- fc %>% select(Fitted) %>% drop_na %>% rbind(data.frame(Fitted = one_step_fc))
  fc$Fitted <- tmp$Fitted
  
  fc <- fc %>% rename("Forecast" = "Point Forecast",
                      "Observed" = "Data")
  fc <- cbind(fc, Date = dates) %>% select(-Index)

  lo_ci <- paste0("Lo ", ci_level)
  hi_ci <- paste0("Hi ", ci_level)
  ci_pct <- paste0(ci_level, "%")
  
  ribbon_vals <- vector(mode = "list", length = 1)
  names(ribbon_vals) <- c(ci_pct)
  ribbon_vals[[ci_pct]] <- "lightBlue"

  fc_plot <- tail(fc, n_ahead*2) %>% ggplot() +
    geom_line(aes(x = Date, y = Observed, color = "Observed")) +
    geom_line(data = subset(fc, !is.na(Forecast)), 
              aes(x = Date, y = Forecast, color = "Forecast"), linetype = "dashed") +
    geom_ribbon(aes(x = Date, ymin = get(lo_ci), ymax = get(hi_ci), fill = ci_pct), alpha = 0.4) +
    scale_fill_manual("CI", values = ribbon_vals) +
    scale_color_manual("Series", values = c("Observed" = "black", "Forecast" = "red")) +
    labs(x = "Date", y = y_lab, title = "Observed vs Forecast")

  return(list(data = fc, plot = fc_plot))
}
  
forecast_ugarchroll <- function(model, dates, test_data, n_ahead, y_lab = c("Mean", "Variance")) {
  fc <- ugarchforecast(model, n.ahead = 1, n.roll = n_ahead)
  fc <- data.frame(Mean = as.numeric(fitted(fc)[-1]), CV = as.numeric(sigma(fc)[-1]))

  data <- model@model$modeldata$data
  training <- head(data, model@model$modeldata$T)
  fc_tail <- data.frame(Date = head(dates, length(training)),
                        Observed = training,
                        Fitted = as.numeric(fitted(model)),
                        Forecast = rep(NA, length(training)),
                        CondVar = rep(NA, length(training)))
  fc <- data.frame(Date = tail(dates, length(fc$Mean)),
                   Observed = test_data, 
                   Fitted = rep(NA, length(fc$Mean)),
                   Forecast = fc$Mean,
                   CondVar = fc$CV)
  fc <- rbind(fc_tail, fc)
  connect_idx <- nrow(fc)-n_ahead
  fc$Forecast[connect_idx] <- fc$Observed[connect_idx]

  plot1 <- tail(fc, n_ahead*2) %>% ggplot() +
    geom_line(aes(x = Date, y = Observed, color = "Observed")) +
    geom_line(data = subset(fc, !is.na(Forecast)), aes(x = Date, y = Forecast, color = "Forecast"), linetype = "dashed") +
    scale_color_manual("", values = c("Observed" = "black", "Forecast" = "red")) +
    labs(y = y_lab[1], title = "Mean: Observed vs Forecast")
  plot2 <- tail(fc, n_ahead*2) %>% ggplot() +
    geom_line(aes(x = Date, y = Observed^2, color = "Observed^2")) +
    geom_line(data = subset(fc, !is.na(CondVar)), aes(x = Date, y = CondVar, color = "Forecast Cond Variance"), linetype = "dashed") +
    scale_color_manual("", values = c("Observed^2" = "black", "Forecast Cond Variance" = "red")) +
    labs(y = y_lab[2], title = "Variance: Observed vs Forecast")
  
  plot <- ggarrange(plot1, plot2,
            nrow = 2, ncol = 1)
  
  return(list(data = fc, plot = plot))
}

forecast_ugarchroll_future <- function(model, dates, test_data, n_ahead, y_lab = c("Mean", "Variance")) {
  fc <- data.frame(Mean = double(), CV <- double()
  data <- model@model$modeldata$data
  model_spec <- getspec(model)
  roll_data <- data
  for (i in 1:n_ahead) {
    if(i > 1) {
      roll_data <- c(roll_data, test_data[i-1])
    }
    this_model <- ugarchfit(model_spec, roll_data, solver = 'hybrid', out.sample = 0)
    this_fc <- ugarchforecast(this_model, n.ahead = 1)
    this_fc <- data.frame(Mean  = as.numeric(fitted(fc)), CV = as.numeric(sigma(fc)))
    fc <- rbind(fc, this_fc)
  }

  training <- head(data, model@model$modeldata$T)
  fc_tail <- data.frame(Date = head(dates, length(training)),
                        Observed = training,
                        Fitted = as.numeric(fitted(model)),
                        Forecast = rep(NA, length(training)),
                        CondVar = rep(NA, length(training)))
  fc <- data.frame(Date = tail(dates, length(fc$Mean)),
                   Observed = test_data, 
                   Fitted = rep(NA, length(fc$Mean)),
                   Forecast = fc$Mean,
                   CondVar = fc$CV)
  fc <- rbind(fc_tail, fc)
  connect_idx <- nrow(fc)-n_ahead
  fc$Forecast[connect_idx] <- fc$Observed[connect_idx]

  plot1 <- tail(fc, n_ahead*2) %>% ggplot() +
    geom_line(aes(x = Date, y = Observed, color = "Observed")) +
    geom_line(data = subset(fc, !is.na(Forecast)), aes(x = Date, y = Forecast, color = "Forecast"), linetype = "dashed") +
    scale_color_manual("", values = c("Observed" = "black", "Forecast" = "red")) +
    labs(y = y_lab[1], title = "Mean: Observed vs Forecast")
  plot2 <- tail(fc, n_ahead*2) %>% ggplot() +
    geom_line(aes(x = Date, y = Observed^2, color = "Observed^2")) +
    geom_line(data = subset(fc, !is.na(CondVar)), aes(x = Date, y = CondVar, color = "Forecast Cond Variance"), linetype = "dashed") +
    scale_color_manual("", values = c("Observed^2" = "black", "Forecast Cond Variance" = "red")) +
    labs(y = y_lab[2], title = "Variance: Observed vs Forecast")
  
  plot <- ggarrange(plot1, plot2,
            nrow = 2, ncol = 1)
  
  return(list(data = fc, plot = plot))
}

mape <- function(observed, forecast) {
  ret <- mean(abs(forecast - observed)/abs(observed))
  return(ret)
}

mae <- function(observed, forecast) {
  ret <- mean(abs(forecast - observed))
  return(ret)
}

prec_measure <- function(observed, forecast) {
  ret <- sum((observed - forecast)^2)/sum((observed - mean(observed))^2)
  return(ret)
}