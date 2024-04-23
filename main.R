library(readr)
library(MASS)

calculate_mape <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Lengths of actual and predicted vectors must be equal")
  }

  sum_percentage_error <- sum(abs((actual - predicted) / actual), na.rm = TRUE)

  mape <- (sum_percentage_error / length(actual)) * 100
  return(mape)
}

all_data <- read.csv("He_Kelly_Manela_Factors_And_Test_Assets_monthly.csv", header = TRUE)

slope_shock_list <- c()
slope_market_list <- c()

for (i in 10:34) {
  coli <- all_data[, i]
  col3 <- all_data[, 4]
  col5 <- all_data[, 6]

  lm_model <- lm(coli ~ col3 + col5)

  slope_shock <- coef(lm_model)[2]
  slope_market <- coef(lm_model)[3]

  slope_shock_list <- c(slope_shock_list, slope_shock)
  slope_market_list <- c(slope_market_list, slope_market)

  cat(paste0("FF25-", i - 9, "\n"))
  cat("R2:", summary(lm_model)$r.squared, "\n")
  cat("slopeShock:", slope_shock, "\n")
  cat("slopeMarket:", slope_market, "\n")
  cat("=======================\n")
}

cat("Finished beta counting...\n")
cat("=======================\n")

all_r2 <- c()

for (i in 1:516) {
  row <- all_data[i, 10:34]

  lm_model <- lm(row ~ slope_shock_list + slope_market_list)

  cat(paste0("adding row ", i, "'s R2:", summary(lm_model)$r.squared, "\n"))
  all_r2 <- c(all_r2, summary(lm_model)$r.squared)
}

average_r2 <- mean(all_r2)
cat("=======================\n")
cat("Average R2 is:\n")
cat(average_r2, "\n")
