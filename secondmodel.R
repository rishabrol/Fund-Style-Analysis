########## packages ########## 
library(readxl)
library(dplyr)
library(quadprog)
library(ggplot2)
library(tidyr)
########## data ########## 
start_date <- as.Date("2009-12-31")
end_date <- as.Date("2024-12-31")

process_fund_data <- function(filepath, ticker) {
  data <- read_excel(filepath)
  
  data <- data %>%
    mutate(Date = as.Date(Date, format = "%d-%b-%y")) %>% 
    arrange(Date)
  
  data <- data %>%
    filter(Date >= start_date & Date <= end_date)
  
  # log return
  data <- data %>%
    mutate(logret = c(NA, 100 * diff(log(Close)))) %>%
    mutate(DateInt = as.integer(format(Date, "%Y%m%d"))) %>%
    select(DateInt, logret)
  
  data <- data %>% filter(!is.na(logret))
  
  assign(paste0("df_", ticker), data, envir = .GlobalEnv)
}

# 5 funds
process_fund_data("/Users/luyilin/Desktop/six_styles_data/iShares Core S&P Total Market ETF (ITOT).xlsx", "ITOT")
process_fund_data("/Users/luyilin/Desktop/six_styles_data/iShares Russell 1000 Growth ETF (IWF).xlsx", "IWF")
process_fund_data("/Users/luyilin/Desktop/six_styles_data/T. Rowe Price Small- Cap Value Fund (PRSVX).xlsx", "PRSVX")
process_fund_data("/Users/luyilin/Desktop/six_styles_data/Vanguard Wellesley Income Fund (VWINX).xlsx", "VWINX")
process_fund_data("/Users/luyilin/Desktop/six_styles_data/Vanguard 500 Index Fund (VFIAX).xlsx", "VFIAX")
# risk free
rf_data <- read.csv("/Users/luyilin/Desktop/six_styles_data/Rf_daily_2010_2024.csv") %>%
  select(Date, RF) %>%
  mutate(DateInt = as.integer(Date)) %>%
  select(DateInt, RF) %>%
  mutate(RF = RF * 100)

# excess return
generate_excess_return <- function(df_fund, ticker) {
  df_excess <- df_fund %>%
    left_join(rf_data, by = "DateInt") %>%
    mutate(excess_logret = logret - RF) %>%
    select(DateInt, excess_logret)
  
  assign(paste0(ticker, "_excessreturn"), df_excess, envir = .GlobalEnv)
}

generate_excess_return(df_ITOT, "ITOT")
generate_excess_return(df_IWF, "IWF")
generate_excess_return(df_PRSVX, "PRSVX")
generate_excess_return(df_VWINX, "VWINX")
generate_excess_return(df_VFIAX, "VFIAX")

# 2*3 style
style_value_weight <- read.csv("/Users/luyilin/Desktop/six_styles_data/Average_Value_Weighted_Returns.csv")
style_value_weight <- style_value_weight %>%
  rename(
    SmallValue = 'SMALL.LoBM',
    SmallNeutral = 'ME1.BM2',
    SmallGrowth = 'SMALL.HiBM',
    BigValue = 'BIG.LoBM',
    BigNeutral = 'ME2.BM2',
    BigGrowth = 'BIG.HiBM'
  )

style_value_weight_logret <- style_value_weight %>%
  mutate(
    DateInt = as.integer(Date),
    # logret
    SmallValue_logret = 100 * log(1 + SmallValue/100),
    SmallNeutral_logret = 100 * log(1 + SmallNeutral/100),
    SmallGrowth_logret = 100 * log(1 + SmallGrowth/100),
    BigValue_logret = 100 * log(1 + BigValue/100),
    BigNeutral_logret = 100 * log(1 + BigNeutral/100),
    BigGrowth_logret = 100 * log(1 + BigGrowth/100)
  ) %>%
  select(DateInt, SmallValue_logret, SmallNeutral_logret, SmallGrowth_logret,
         BigValue_logret, BigNeutral_logret, BigGrowth_logret)

# - risk free
style_value_weight_excess <- style_value_weight_logret %>%
  left_join(rf_data, by = "DateInt") %>%
  mutate(
    SmallValue_excess = SmallValue_logret - RF,
    SmallNeutral_excess = SmallNeutral_logret - RF,
    SmallGrowth_excess = SmallGrowth_logret - RF,
    BigValue_excess = BigValue_logret - RF,
    BigNeutral_excess = BigNeutral_logret - RF,
    BigGrowth_excess = BigGrowth_logret - RF
  ) %>%
  select(DateInt, SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
         BigValue_excess, BigNeutral_excess, BigGrowth_excess)

# long term gov bond log return
long_term_bond <- read.csv("/Users/luyilin/Desktop/six_styles_data/Long_term_govbond_log_return_daily.csv", stringsAsFactors = FALSE)

long_term_bond <- long_term_bond %>%
  mutate(
    Date = as.Date(Date),  # 先强制转成真正的Date对象
    DateInt = as.integer(format(Date, "%Y%m%d")),  # 再转整数
    log_return = log_return * 100  # log return放大到百分比单位
  )

long_term_bond_excess <- long_term_bond %>%
  mutate(DateInt = as.integer(Date)) %>%
  left_join(rf_data, by = "DateInt") %>%
  mutate(Bond_excess = log_return - RF) %>%
  select(DateInt, Bond_excess)

# - RF
long_term_bond_excess <- long_term_bond %>%
  left_join(rf_data, by = "DateInt") %>%
  mutate(
    Bond_excess = log_return - RF
  ) %>%
  select(DateInt, Bond_excess) %>%
  rename(Date = DateInt)

########## Model 2: sum weights to 1; allow shorting ##########
##### 2yr 1fund #####
start_rolling <- 20100101
end_rolling <- 20111231

X_factors <- style_value_weight_excess %>%
  inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))

X_factors_window <- X_factors %>%
  filter(DateInt >= start_rolling & DateInt <= end_rolling) %>%
  select(DateInt, SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
         BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)

ITOT_excessreturn <- ITOT_excessreturn %>%
  mutate(DateInt = as.integer(DateInt))

Y_fund_window <- ITOT_excessreturn %>%
  filter(DateInt >= start_rolling & DateInt <= end_rolling) %>%
  select(DateInt, excess_logret)

dates_common <- intersect(X_factors_window$DateInt, Y_fund_window$DateInt)

X_mat <- X_factors_window %>%
  filter(DateInt %in% dates_common) %>%
  arrange(DateInt) %>%
  select(-DateInt) %>%
  as.matrix()

Y_vec <- Y_fund_window %>%
  filter(DateInt %in% dates_common) %>%
  arrange(DateInt) %>%
  pull(excess_logret) %>%
  matrix(ncol = 1)

cat("dim(X_mat):", dim(X_mat), "\n")
cat("dim(Y_vec):", dim(Y_vec), "\n")

# 1 constraint
Dmat <- t(X_mat) %*% X_mat
dvec <- t(X_mat) %*% Y_vec
Amat <- matrix(1, nrow = 7, ncol = 1)
bvec <- 1
meq <- 1

qp_result <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)

# beta
beta_hat <- qp_result$solution
names(beta_hat) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                     "BigValue", "BigNeutral", "BigGrowth", "Bond")

# alpha
fitted <- X_mat %*% beta_hat
alpha_hat <- mean(Y_vec - fitted)

residuals <- Y_vec - (fitted + alpha_hat)

residual_sd <- sd(residuals)

R2 <- 1 - var(residuals) / var(Y_vec)

cat("------Results------\n")
cat("Beta:\n"); print(round(beta_hat,4))
cat("Alpha:\n"); print(round(alpha_hat,4))
cat("Residual SD:\n"); print(round(residual_sd,4))
cat("R²:\n"); print(round(R2,4))

##### rolling 1fund #####
start_year <- 2010
end_year <- 2023 # the starting of the last rolling window

rolling_results <- list()

for (year in start_year:end_year) {
  start_rolling <- year * 10000 + 101   # 20100101 ~ 
  end_rolling   <- (year + 1) * 10000 + 1231  # 20111231 ~ 
  
  X_factors <- style_value_weight_excess %>%
    inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))
  
  X_factors_window <- X_factors %>%
    filter(DateInt >= start_rolling & DateInt <= end_rolling) %>%
    select(DateInt, SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
           BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)
  
  ITOT_excessreturn <- ITOT_excessreturn %>%
    mutate(DateInt = as.integer(DateInt))
  
  Y_fund_window <- ITOT_excessreturn %>%
    filter(DateInt >= start_rolling & DateInt <= end_rolling) %>%
    select(DateInt, excess_logret)
  
  dates_common <- intersect(X_factors_window$DateInt, Y_fund_window$DateInt)
  
  X_mat <- X_factors_window %>%
    filter(DateInt %in% dates_common) %>%
    arrange(DateInt) %>%
    select(-DateInt) %>%
    as.matrix()
  
  Y_vec <- Y_fund_window %>%
    filter(DateInt %in% dates_common) %>%
    arrange(DateInt) %>%
    pull(excess_logret) %>%
    matrix(ncol = 1)
  
  if (nrow(X_mat) > 0) {
    Dmat <- t(X_mat) %*% X_mat
    dvec <- t(X_mat) %*% Y_vec
    Amat <- matrix(1, nrow = 7, ncol = 1)
    bvec <- 1
    meq <- 1
    
    qp_result <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
    
    beta_hat <- qp_result$solution
    names(beta_hat) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                         "BigValue", "BigNeutral", "BigGrowth", "Bond")
    
    fitted <- X_mat %*% beta_hat
    alpha_hat <- mean(Y_vec - fitted)
    
    residuals <- Y_vec - (fitted + alpha_hat)
    residual_sd <- sd(residuals)
    
    R2 <- 1 - var(residuals) / var(Y_vec)
    
    rolling_results[[paste0(year, "_", year+1)]] <- list(
      beta = round(beta_hat,4),
      alpha = round(alpha_hat,4),
      residual_sd = round(residual_sd,4),
      R2 = round(R2,4)
    )
  }
}

print(rolling_results)

##### plot 1fund#####
years <- names(rolling_results)

# dataframe
rolling_factors_df <- data.frame(
  Period = character(),
  SmallValue = numeric(),
  SmallNeutral = numeric(),
  SmallGrowth = numeric(),
  BigValue = numeric(),
  BigNeutral = numeric(),
  BigGrowth = numeric(),
  Bond = numeric(),
  Alpha = numeric(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)

for (period in names(rolling_results)) {
  result <- rolling_results[[period]]
  
  rolling_factors_df <- rolling_factors_df %>%
    add_row(
      Period = period,
      SmallValue = as.numeric(result$beta["SmallValue"]),
      SmallNeutral = as.numeric(result$beta["SmallNeutral"]),
      SmallGrowth = as.numeric(result$beta["SmallGrowth"]),
      BigValue = as.numeric(result$beta["BigValue"]),
      BigNeutral = as.numeric(result$beta["BigNeutral"]),
      BigGrowth = as.numeric(result$beta["BigGrowth"]),
      Bond = as.numeric(result$beta["Bond"]),
      Alpha = as.numeric(result$alpha),
      R2 = as.numeric(result$R2)
    )
}

rolling_factors_df <- rolling_factors_df %>%
  mutate(Year = as.numeric(substr(Period, 1, 4)))

rolling_factors_long <- rolling_factors_df %>%
  pivot_longer(cols = c(SmallValue, SmallNeutral, SmallGrowth,
                        BigValue, BigNeutral, BigGrowth),
               names_to = "Factor",
               values_to = "Beta")

# 6 betas plot
ggplot(rolling_factors_long, aes(x = Year, y = Beta, color = Factor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ggtitle("ITOT: Exposure of 6 Style Factors Over Time") +
  xlab("Year") +
  ylab("Beta") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.margin = margin(10, 50, 10, 10)) +
  scale_color_manual(values = c(
    "SmallValue" = "darkblue",
    "SmallNeutral" = "darkgreen",
    "SmallGrowth" = "purple",
    "BigValue" = "blue",
    "BigNeutral" = "green",
    "BigGrowth" = "orange"
  ))

# Alpha
ggplot(rolling_factors_df, aes(x = Year, y = Alpha)) +
  geom_line() +
  geom_point() +
  ggtitle("ITOT: Alpha Over Time") +
  xlab("Year") +
  ylab("Alpha") +
  theme_minimal()

# R²
ggplot(rolling_factors_df, aes(x = Year, y = R2)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  ggtitle("ITOT: R² Over Time") +
  xlab("Year") +
  ylab(expression(R^2)) +
  theme_minimal()

##### rolling 5 funds #####
tickers <- c("ITOT", "IWF", "PRSVX", "VWINX", "VFIAX")

rolling_results_all <- list()

for (ticker in tickers) {
  fund_excess <- get(paste0(ticker, "_excessreturn"))
  
  rolling_results <- list()
  
  for (year in 2010:2023) {
    
    start_rolling <- year * 10000 + 101
    end_rolling   <- (year + 1) * 10000 + 1231
    
    X_factors <- style_value_weight_excess %>%
      inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))
    
    X_factors_window <- X_factors %>%
      filter(DateInt >= start_rolling & DateInt <= end_rolling) %>%
      select(DateInt, SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
             BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)
    
    fund_excess <- fund_excess %>%
      mutate(DateInt = as.integer(DateInt))
    
    Y_fund_window <- fund_excess %>%
      filter(DateInt >= start_rolling & DateInt <= end_rolling) %>%
      select(DateInt, excess_logret)
    
    dates_common <- intersect(X_factors_window$DateInt, Y_fund_window$DateInt)
    
    X_mat <- X_factors_window %>%
      filter(DateInt %in% dates_common) %>%
      arrange(DateInt) %>%
      select(-DateInt) %>%
      as.matrix()
    
    Y_vec <- Y_fund_window %>%
      filter(DateInt %in% dates_common) %>%
      arrange(DateInt) %>%
      pull(excess_logret) %>%
      matrix(ncol = 1)
    
    if (nrow(X_mat) > 0) {
      Dmat <- t(X_mat) %*% X_mat
      dvec <- t(X_mat) %*% Y_vec
      Amat <- matrix(1, nrow = 7, ncol = 1)
      bvec <- 1
      meq <- 1
      
      qp_result <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
      
      beta_hat <- qp_result$solution
      names(beta_hat) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                           "BigValue", "BigNeutral", "BigGrowth", "Bond")
      
      fitted <- X_mat %*% beta_hat
      alpha_hat <- mean(Y_vec - fitted)
      
      residuals <- Y_vec - (fitted + alpha_hat)
      residual_sd <- sd(residuals)
      
      R2 <- 1 - var(residuals) / var(Y_vec)
      
      rolling_results[[paste0(year, "_", year+1)]] <- list(
        beta = round(beta_hat, 4),
        alpha = round(alpha_hat, 4),
        residual_sd = round(residual_sd, 4),
        R2 = round(R2, 4)
      )
    }
  }
  
  rolling_results_all[[ticker]] <- rolling_results
}

# 5funds result
print(rolling_results_all[["ITOT"]])
print(rolling_results_all[["IWF"]])
print(rolling_results_all[["PRSVX"]])
print(rolling_results_all[["VWINX"]])
print(rolling_results_all[["VFIAX"]])

##### plot 5 funds#####
for (ticker in tickers) {
  rolling_results <- rolling_results_all[[ticker]]
  
  rolling_factors_df <- data.frame(
    Period = character(),
    SmallValue = numeric(),
    SmallNeutral = numeric(),
    SmallGrowth = numeric(),
    BigValue = numeric(),
    BigNeutral = numeric(),
    BigGrowth = numeric(),
    Bond = numeric(),
    Alpha = numeric(),
    R2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (period in names(rolling_results)) {
    result <- rolling_results[[period]]
    
    rolling_factors_df <- rolling_factors_df %>%
      add_row(
        Period = period,
        SmallValue = as.numeric(result$beta["SmallValue"]),
        SmallNeutral = as.numeric(result$beta["SmallNeutral"]),
        SmallGrowth = as.numeric(result$beta["SmallGrowth"]),
        BigValue = as.numeric(result$beta["BigValue"]),
        BigNeutral = as.numeric(result$beta["BigNeutral"]),
        BigGrowth = as.numeric(result$beta["BigGrowth"]),
        Bond = as.numeric(result$beta["Bond"]),
        Alpha = as.numeric(result$alpha),
        R2 = as.numeric(result$R2)
      )
  }
  
  rolling_factors_df <- rolling_factors_df %>%
    mutate(Year = as.numeric(substr(Period, 1, 4)))
  
  rolling_factors_long <- rolling_factors_df %>%
    pivot_longer(cols = c(SmallValue, SmallNeutral, SmallGrowth,
                          BigValue, BigNeutral, BigGrowth, Bond),
                 names_to = "Factor",
                 values_to = "Beta")
  
  p <- ggplot(rolling_factors_long, aes(x = Year, y = Beta, color = Factor)) +
    geom_line(linewidth = 1) +  
    geom_point(size = 2) +
    ggtitle(paste0(ticker, ": Rolling Style Exposures (Beta)")) +
    xlab("Year") +
    ylab("Beta") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 50, 10, 10)
    ) +
    scale_color_manual(values = c(
      "SmallValue" = "darkblue",
      "SmallNeutral" = "darkgreen",
      "SmallGrowth" = "purple",
      "BigValue" = "blue",
      "BigNeutral" = "green",
      "BigGrowth" = "orange",
      "Bond" = "red"
    ))
  
  ggsave(
    filename = paste0(ticker, "_7Factors.png"),
    plot = p,
    width = 8, height = 6, dpi = 300
  )
}

##### alpha #####
rolling_alpha_df <- data.frame(
  Period = character(),
  Fund = character(),
  Alpha = numeric(),
  stringsAsFactors = FALSE
)

for (fund in names(rolling_results_all)) {
  fund_results <- rolling_results_all[[fund]]
  
  for (period in names(fund_results)) {
    result <- fund_results[[period]]
    
    rolling_alpha_df <- rolling_alpha_df %>%
      add_row(
        Period = period,
        Fund = fund,
        Alpha = as.numeric(result$alpha)
      )
  }
}

rolling_alpha_df <- rolling_alpha_df %>%
  mutate(Year = as.numeric(substr(Period, 1, 4)))

ggplot(rolling_alpha_df, aes(x = Year, y = Alpha, color = Fund)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  ggtitle("Rolling Alpha of 5 Funds Over Time") +
  xlab("Year") +
  ylab("Alpha") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 50, 10, 10)
  ) +
  scale_color_manual(values = c(
    "ITOT" = "blue",
    "IWF" = "orange",
    "PRSVX" = "green",
    "VWINX" = "purple",
    "VFIAX" = "darkred"
  ))

########## analysis ##########
##### 1 VFIAX BigValue, BigNeutral #####
rolling_vfiax <- rolling_results_all[["VFIAX"]]

X_factors <- style_value_weight_excess %>%
  inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))

X_mat_full <- X_factors %>%
  select(SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
         BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)

factor_sd <- apply(X_mat_full, 2, sd)
names(factor_sd) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                      "BigValue", "BigNeutral", "BigGrowth", "Bond")

contribution_over_time <- data.frame(
  Period = character(),
  BigValue_pct = numeric(),
  BigNeutral_pct = numeric(),
  Total_pct = numeric(),
  stringsAsFactors = FALSE
)

for (period in names(rolling_vfiax)) {
  beta_vec <- rolling_vfiax[[period]]$beta
  # β² × Var(X)
  factor_contrib_raw <- (beta_vec^2) * (factor_sd^2)
  total_contrib <- sum(factor_contrib_raw)

  bigvalue_pct <- factor_contrib_raw["BigValue"] / total_contrib * 100
  bigneutral_pct <- factor_contrib_raw["BigNeutral"] / total_contrib * 100
  
  contribution_over_time <- contribution_over_time %>% 
    add_row(
      Period = period,
      BigValue_pct = round(bigvalue_pct, 2),
      BigNeutral_pct = round(bigneutral_pct, 2),
      Total_pct = round(bigvalue_pct + bigneutral_pct, 2)
    )
}

contribution_over_time <- contribution_over_time %>%
  mutate(Year = as.numeric(substr(Period, 1, 4)))

print(contribution_over_time)


##### 2 WINX 2010~2016: BigNeutral，BigValue #####
#       20170101~20221231: BigNeutral
#       2021~2024: BigNeutral, SmallNeutral
rolling_vwinx <- rolling_results_all[["VWINX"]]

X_factors <- style_value_weight_excess %>%
  inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))
X_mat_full <- X_factors %>%
  select(SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
         BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)
factor_sd <- apply(X_mat_full, 2, sd)
names(factor_sd) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                      "BigValue", "BigNeutral", "BigGrowth", "Bond")

contribution_vwinx_allfactors <- data.frame(
  Period = character(),
  SmallValue_pct = numeric(),
  SmallNeutral_pct = numeric(),
  SmallGrowth_pct = numeric(),
  BigValue_pct = numeric(),
  BigNeutral_pct = numeric(),
  BigGrowth_pct = numeric(),
  Bond_pct = numeric(),
  Total_pct = numeric(),
  Year = numeric(),
  stringsAsFactors = FALSE
)

for (period in names(rolling_vwinx)) {
  year <- as.numeric(substr(period, 1, 4))
  
  beta_vec <- rolling_vwinx[[period]]$beta
  
  factor_contrib_raw <- (beta_vec^2) * (factor_sd^2)
  total_contrib <- sum(factor_contrib_raw)
  
  contrib_row <- data.frame(
    Period = period,
    SmallValue_pct = round(factor_contrib_raw["SmallValue"] / total_contrib * 100, 2),
    SmallNeutral_pct = round(factor_contrib_raw["SmallNeutral"] / total_contrib * 100, 2),
    SmallGrowth_pct = round(factor_contrib_raw["SmallGrowth"] / total_contrib * 100, 2),
    BigValue_pct = round(factor_contrib_raw["BigValue"] / total_contrib * 100, 2),
    BigNeutral_pct = round(factor_contrib_raw["BigNeutral"] / total_contrib * 100, 2),
    BigGrowth_pct = round(factor_contrib_raw["BigGrowth"] / total_contrib * 100, 2),
    Bond_pct = round(factor_contrib_raw["Bond"] / total_contrib * 100, 2),
    Total_pct = round(sum(factor_contrib_raw) / total_contrib * 100, 2),  # 应该是100
    Year = year
  )
  
  contribution_vwinx_allfactors <- contribution_vwinx_allfactors %>%
    add_row(contrib_row)
}

print(contribution_vwinx_allfactors)


##### 3 ITOT BigValue, BigNeutral #####
rolling_itot <- rolling_results_all[["ITOT"]]
X_factors <- style_value_weight_excess %>%
  inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))
X_mat_full <- X_factors %>%
  select(SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
         BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)
factor_sd <- apply(X_mat_full, 2, sd)
names(factor_sd) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                      "BigValue", "BigNeutral", "BigGrowth", "Bond")

contribution_itot <- data.frame(
  Period = character(),
  BigValue_pct = numeric(),
  BigNeutral_pct = numeric(),
  Total_pct = numeric(),
  stringsAsFactors = FALSE
)

for (period in names(rolling_itot)) {
  beta_vec <- rolling_itot[[period]]$beta
  factor_contrib_raw <- (beta_vec^2) * (factor_sd^2)
  total_contrib <- sum(factor_contrib_raw)
  bigvalue_pct <- factor_contrib_raw["BigValue"] / total_contrib * 100
  bigneutral_pct <- factor_contrib_raw["BigNeutral"] / total_contrib * 100
  contribution_itot <- contribution_itot %>%
    add_row(
      Period = period,
      BigValue_pct = round(bigvalue_pct, 2),
      BigNeutral_pct = round(bigneutral_pct, 2),
      Total_pct = round(bigvalue_pct + bigneutral_pct, 2)
    )
}
contribution_itot <- contribution_itot %>%
  mutate(Year = as.numeric(substr(Period, 1, 4)))

print(contribution_itot)


##### 4 IWF BigValue #####
rolling_iwf <- rolling_results_all[["IWF"]]
X_factors <- style_value_weight_excess %>%
  inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))
X_mat_full <- X_factors %>%
  select(SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
         BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)
factor_sd <- apply(X_mat_full, 2, sd)
names(factor_sd) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                      "BigValue", "BigNeutral", "BigGrowth", "Bond")

contribution_iwf <- data.frame(
  Period = character(),
  BigValue_pct = numeric(),
  BigNeutral_pct = numeric(),
  Total_pct = numeric(),
  stringsAsFactors = FALSE
)

for (period in names(rolling_iwf)) {
  beta_vec <- rolling_iwf[[period]]$beta
  factor_contrib_raw <- (beta_vec^2) * (factor_sd^2)
  total_contrib <- sum(factor_contrib_raw)
  bigvalue_pct <- factor_contrib_raw["BigValue"] / total_contrib * 100
  bigneutral_pct <- factor_contrib_raw["BigNeutral"] / total_contrib * 100
  contribution_iwf <- contribution_iwf %>%
    add_row(
      Period = period,
      BigValue_pct = round(bigvalue_pct, 2),
      BigNeutral_pct = round(bigneutral_pct, 2),
      Total_pct = round(bigvalue_pct + bigneutral_pct, 2)
    )
}

contribution_iwf <- contribution_iwf %>%
  mutate(Year = as.numeric(substr(Period, 1, 4)))

print(contribution_iwf)


##### 5 PRSVX 2010~2012: SmallNeutral #####
#       2013~2017: SmallNeutral, SmallGrowth
#       2018~2020: SmallNeutral
#       2021~2024: SmallNeutral, BigNeutral, SmallGrowth
rolling_prsvx <- rolling_results_all[["PRSVX"]]

X_factors <- style_value_weight_excess %>%
  inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))

X_mat_full <- X_factors %>%
  select(SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
         BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)

factor_sd <- apply(X_mat_full, 2, sd)

names(factor_sd) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                      "BigValue", "BigNeutral", "BigGrowth", "Bond")

contribution_prsvx_allfactors <- data.frame(
  Period = character(),
  SmallValue_pct = numeric(),
  SmallNeutral_pct = numeric(),
  SmallGrowth_pct = numeric(),
  BigValue_pct = numeric(),
  BigNeutral_pct = numeric(),
  BigGrowth_pct = numeric(),
  Bond_pct = numeric(),
  Total_pct = numeric(),
  Year = numeric(),
  stringsAsFactors = FALSE
)

for (period in names(rolling_prsvx)) {
  year <- as.numeric(substr(period, 1, 4))
  
  beta_vec <- rolling_prsvx[[period]]$beta
  
  factor_contrib_raw <- (beta_vec^2) * (factor_sd^2)
  total_contrib <- sum(factor_contrib_raw)
  
  contrib_row <- data.frame(
    Period = period,
    SmallValue_pct = round(factor_contrib_raw["SmallValue"] / total_contrib * 100, 2),
    SmallNeutral_pct = round(factor_contrib_raw["SmallNeutral"] / total_contrib * 100, 2),
    SmallGrowth_pct = round(factor_contrib_raw["SmallGrowth"] / total_contrib * 100, 2),
    BigValue_pct = round(factor_contrib_raw["BigValue"] / total_contrib * 100, 2),
    BigNeutral_pct = round(factor_contrib_raw["BigNeutral"] / total_contrib * 100, 2),
    BigGrowth_pct = round(factor_contrib_raw["BigGrowth"] / total_contrib * 100, 2),
    Bond_pct = round(factor_contrib_raw["Bond"] / total_contrib * 100, 2),
    Total_pct = round(sum(factor_contrib_raw) / total_contrib * 100, 2),  # 理论上是100
    Year = year
  )
  
  contribution_prsvx_allfactors <- contribution_prsvx_allfactors %>%
    add_row(contrib_row)
}

print(contribution_prsvx_allfactors)


##### plot factors contribution #####
# 5
contribution_prsvx_long <- contribution_prsvx_allfactors %>%
  select(Year, SmallValue_pct, SmallNeutral_pct, SmallGrowth_pct,
         BigValue_pct, BigNeutral_pct, BigGrowth_pct, Bond_pct) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Factor",
    values_to = "Contribution"
  )

ggplot(contribution_prsvx_long, aes(x = Year, y = Contribution, color = Factor)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  ggtitle("PRSVX: Contribution of 7 Factors Over Time") +
  xlab("Year") +
  ylab("Contribution (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 50, 10, 10)
  )

# 2
contribution_vwinx_long <- contribution_vwinx_allfactors %>%
  select(Year, SmallValue_pct, SmallNeutral_pct, SmallGrowth_pct,
         BigValue_pct, BigNeutral_pct, BigGrowth_pct, Bond_pct) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Factor",
    values_to = "Contribution"
  )

ggplot(contribution_vwinx_long, aes(x = Year, y = Contribution, color = Factor)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  ggtitle("VWINX: Rolling Style Exposures (Beta)") +
  xlab("Year") +
  ylab("Contribution (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 50, 10, 10)
  )

###### PPT Plots ######
factor_colors <- c(
  "SmallValue" = "darkblue",
  "SmallNeutral" = "darkgreen",
  "SmallGrowth" = "purple",
  "BigValue" = "blue",
  "BigNeutral" = "green",
  "BigGrowth" = "orange",
  "Bond" = "red"
)

contribution_list <- list(
  "VFIAX" = contribution_over_time,           
  "VWINX" = contribution_vwinx_allfactors,
  "ITOT" = contribution_itot,
  "IWF" = contribution_iwf,
  "PRSVX" = contribution_prsvx_allfactors
)

for (fund_name in names(contribution_list)) {
  
  df <- contribution_list[[fund_name]]

  if ("SmallValue_pct" %in% names(df)) {

    df_long <- df %>%
      select(Year, SmallValue_pct, SmallNeutral_pct, SmallGrowth_pct,
             BigValue_pct, BigNeutral_pct, BigGrowth_pct, Bond_pct) %>%
      pivot_longer(
        cols = -Year,
        names_to = "Factor",
        values_to = "Contribution"
      ) %>%
      mutate(Factor = gsub("_pct", "", Factor))
  } else {
    df_long <- df %>%
      select(Year, BigValue_pct, BigNeutral_pct) %>%
      pivot_longer(
        cols = -Year,
        names_to = "Factor",
        values_to = "Contribution"
      ) %>%
      mutate(Factor = gsub("_pct", "", Factor)) 
  }
  
  p <- ggplot(df_long, aes(x = Year, y = Contribution, color = Factor)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ggtitle(paste0(fund_name, ": Contribution of Factors Over Time")) +
    xlab("Year") +
    ylab("Contribution (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      plot.margin = margin(10, 50, 10, 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_color_manual(values = factor_colors)
  
  ggsave(
    filename = paste0(fund_name, ": Rolling Style Exposures (Betas).png"),
    plot = p,
    width = 8, height = 6, dpi = 300
  )
}


X_factors <- style_value_weight_excess %>%
  inner_join(long_term_bond_excess, by = c("DateInt" = "Date"))

X_mat_full <- X_factors %>%
  select(SmallValue_excess, SmallNeutral_excess, SmallGrowth_excess,
         BigValue_excess, BigNeutral_excess, BigGrowth_excess, Bond_excess)

factor_sd <- apply(X_mat_full, 2, sd)
names(factor_sd) <- c("SmallValue", "SmallNeutral", "SmallGrowth",
                      "BigValue", "BigNeutral", "BigGrowth", "Bond")

for (ticker in tickers) {
  
  rolling_result <- rolling_results_all[[ticker]]
  
  contribution_allfactors <- data.frame(
    Period = character(),
    SmallValue_pct = numeric(),
    SmallNeutral_pct = numeric(),
    SmallGrowth_pct = numeric(),
    BigValue_pct = numeric(),
    BigNeutral_pct = numeric(),
    BigGrowth_pct = numeric(),
    Bond_pct = numeric(),
    Total_pct = numeric(),
    Year = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (period in names(rolling_result)) {
    year <- as.numeric(substr(period, 1, 4))
    beta_vec <- rolling_result[[period]]$beta
    
    factor_contrib_raw <- (beta_vec^2) * (factor_sd^2)
    total_contrib <- sum(factor_contrib_raw)
    
    contrib_row <- data.frame(
      Period = period,
      SmallValue_pct = round(factor_contrib_raw["SmallValue"] / total_contrib * 100, 2),
      SmallNeutral_pct = round(factor_contrib_raw["SmallNeutral"] / total_contrib * 100, 2),
      SmallGrowth_pct = round(factor_contrib_raw["SmallGrowth"] / total_contrib * 100, 2),
      BigValue_pct = round(factor_contrib_raw["BigValue"] / total_contrib * 100, 2),
      BigNeutral_pct = round(factor_contrib_raw["BigNeutral"] / total_contrib * 100, 2),
      BigGrowth_pct = round(factor_contrib_raw["BigGrowth"] / total_contrib * 100, 2),
      Bond_pct = round(factor_contrib_raw["Bond"] / total_contrib * 100, 2),
      Total_pct = 100,
      Year = year
    )
    
    contribution_allfactors <- contribution_allfactors %>%
      add_row(contrib_row)
  }
  
  assign(paste0("contribution_", ticker, "_allfactors"), contribution_allfactors)

  contribution_long <- contribution_allfactors %>%
    select(Year, SmallValue_pct, SmallNeutral_pct, SmallGrowth_pct,
           BigValue_pct, BigNeutral_pct, BigGrowth_pct, Bond_pct) %>%
    pivot_longer(
      cols = -Year,
      names_to = "Factor",
      values_to = "Contribution"
    )
  
  p <- ggplot(contribution_long, aes(x = Year, y = Contribution, color = Factor)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    ggtitle(paste0(ticker, ": Factor Contribution % Over Time")) +
    xlab("Year") +
    ylab("Contribution (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      plot.margin = margin(10, 50, 10, 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_color_manual(values = c(
      "SmallValue_pct" = "darkblue",
      "SmallNeutral_pct" = "darkgreen",
      "SmallGrowth_pct" = "purple",
      "BigValue_pct" = "blue",
      "BigNeutral_pct" = "green",
      "BigGrowth_pct" = "orange",
      "Bond_pct" = "red"
    ))
  
  ggsave(
    filename = paste0(ticker, "_FactorContribution.png"),
    plot = p,
    width = 8, height = 6, dpi = 300
  )
}
































