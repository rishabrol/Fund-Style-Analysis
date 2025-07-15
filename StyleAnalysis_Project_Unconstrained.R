########## PACKAGES ##########
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

########## ROLLING STYLE EXPOSURE (Beta) PLOTS ##########

for (fund in names(rolling_results_all)) {
  
  rolling_data <- rolling_results_all[[fund]]
  
  beta_df <- data.frame(
    Period = names(rolling_data),
    t(sapply(rolling_data, function(x) x$beta))
  )
  
  beta_df <- beta_df %>%
    mutate(Year = as.numeric(substr(Period, 1, 4))) %>%
    pivot_longer(cols = c(SmallValue, SmallNeutral, SmallGrowth,
                          BigValue, BigNeutral, BigGrowth, Bond),
                 names_to = "Factor",
                 values_to = "Beta")
  
  p <- ggplot(beta_df, aes(x = Year, y = Beta, color = Factor)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    ggtitle(paste0(fund, ": Rolling Style Exposures (Betas)")) +
    xlab("Year") +
    ylab("Beta Weight") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 10)
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
  
  print(p)
}

########## FACTOR CONTRIBUTION % PLOTS ##########

for (fund in names(contribution_results)) {
  
  contrib_df <- contribution_results[[fund]] %>%
    mutate(Year = as.numeric(substr(Period, 1, 4))) %>%
    pivot_longer(cols = c(SmallValue, SmallNeutral, SmallGrowth,
                          BigValue, BigNeutral, BigGrowth, Bond),
                 names_to = "Factor",
                 values_to = "Contribution")
  
  p <- ggplot(contrib_df, aes(x = Year, y = Contribution, color = Factor)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    ggtitle(paste0(fund, ": Factor Contribution % Over Time")) +
    xlab("Year") +
    ylab("Contribution (%)") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 10)
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
  
  print(p)
}

########## INDIVIDUAL ROLLING ALPHA & R² PLOTS (RED LINE + BLACK DOTS) ##########

for (fund in names(rolling_results_all)) {
  
  rolling_data <- rolling_results_all[[fund]]
  
  alpha_df <- data.frame(
    Year = as.numeric(substr(names(rolling_data), 1, 4)),
    Alpha = sapply(rolling_data, function(x) x$alpha)
  )
  
  r2_df <- data.frame(
    Year = as.numeric(substr(names(rolling_data), 1, 4)),
    R2 = sapply(rolling_data, function(x) x$R2)
  )
  
  # Rolling Alpha Plot
  p_alpha <- ggplot(alpha_df, aes(x = Year, y = Alpha)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line(color = "red", size = 1.2) +
    geom_point(color = "black", size = 2) +
    ggtitle(paste0(fund, ": Rolling Alpha Over Time")) +
    xlab("Year") +
    ylab("Alpha (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  print(p_alpha)
  
  # Rolling R² Plot
  p_r2 <- ggplot(r2_df, aes(x = Year, y = R2)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line(color = "red", size = 1.2) +
    geom_point(color = "black", size = 2) +
    ggtitle(paste0(fund, ": Rolling R² Over Time")) +
    xlab("Year") +
    ylab(expression(R^2)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  print(p_r2)
}

########## COMBINED ROLLING ALPHA PLOT (All Funds Together) ##########

alpha_all_funds <- data.frame()

for (fund in names(rolling_results_all)) {
  
  rolling_data <- rolling_results_all[[fund]]
  
  temp_df <- data.frame(
    Year = as.numeric(substr(names(rolling_data), 1, 4)),
    Alpha = sapply(rolling_data, function(x) x$alpha),
    Fund = fund
  )
  
  alpha_all_funds <- bind_rows(alpha_all_funds, temp_df)
}

ggplot(alpha_all_funds, aes(x = Year, y = Alpha, color = Fund)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  ggtitle("Rolling Alpha Comparison Across Funds") +
  xlab("Year") +
  ylab("Alpha (%)") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = c(
    "ITOT" = "blue",
    "IWF" = "orange",
    "PRSVX" = "green",
    "VWINX" = "purple",
    "VFIAX" = "darkred"
  ))

########## DONE ##########

########## TABLES: FACTOR CONTRIBUTION TABLES ##########

contribution_tables <- list()

for (fund in names(contribution_results)) {
  
  contrib_df <- contribution_results[[fund]] %>%
    mutate(Year = as.numeric(substr(Period, 1, 4))) %>%
    select(Year, SmallValue, SmallNeutral, SmallGrowth,
           BigValue, BigNeutral, BigGrowth, Bond)
  
  contribution_tables[[fund]] <- contrib_df
  
  cat("\n\n====================\n")
  cat(paste0("Factor Contribution Table for Fund: ", fund, "\n"))
  cat("====================\n")
  
  print(contrib_df)
}

