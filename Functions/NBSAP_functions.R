### RLI functions ###

### Diagnostics Function ###

diagnostics <- function(model){
  # Residual vs. fitted plot helps check for heteroskedasticity and non-linearity
  residuals <- resid(model)
  fitted_values <- fitted(model)
  plot(fitted_values, residuals, 
       xlab = "Fitted Values", 
       ylab = "Residuals", 
       main = "Residuals vs Fitted Values")
  abline(h = 0, col = "red")
  
  #BP Test for heteroskedasticity
  library(lmtest)
  library(car)
  simple_model <- lm(residuals^2 ~ fitted_values)
  cat("BP Test for heteroskedasticity.\nIf p < 0.05, reject null of homoskedasticity.\n\n")
  print(bptest(simple_model))
  
  #Normality of residuals
  qqnorm(residuals)
  qqline(residuals, col = "red")
  
  #Checking for multicollinearity
  cat('Checking Multicollinearity\nWant adjusted VIF < 5.')
  vif_values <- vif(model)
  print(vif_values)
  
  #Checking normality 
  cat("\nShapiro-Wilk test for level-1 residuals \nIf p < 0.05, reject null of normality.\nIf W close to 1, suggests data close to normal.\n")
  print(shapiro.test(residuals))
  
  cat("\nShapiro-Wilk test for level-2 residuals \n")
  print(shapiro.test(ranef(model)$cname[,1]))
  
  
  cat("\nChecking Cook's Distance for influential points\nMore than 1 could be a problem.\n")
  residuals <- resid(model)
  hatvalues <- hatvalues(model)
  cooks_distance <- (residuals^2 / (length(residuals) * sigma(model)^2)) * (hatvalues / (1 - hatvalues)^2)
  plot(cooks_distance, type = "h", main = "Manual Cook's Distance for Two-Level Model", xlab = "Index", ylab = "Cook's Distance")
  print("Max Cook's Distance is:")
  print(max(cooks_distance))
}

### BOOT SIMULATION TIME FUNCTION ###

time <- function(start_time,n){
  cat("It's estimated to take", as.numeric(start_time[3])*n/60, "minutes to run", n, "simultations" )
}


### BOOTSTRAP TABLE FUNCTION ###  bootstrap_table(boot_results, model)
bootstrap_table <- function(boot_results, model){
  library(dplyr)
  bootstrap_estimates <- boot_results$t
  estimates <- apply(bootstrap_estimates, 2, mean)
  ci_lower <- apply(bootstrap_estimates, 2, quantile, 0.025)
  ci_upper <- apply(bootstrap_estimates, 2, quantile, 0.975)
  df <- data.frame(ci_lower,ci_upper)
  df <- df %>%
    mutate(Significant = case_when(
      ((ci_lower>0&ci_upper>0)|(ci_lower<0&ci_upper<0)|(ci_lower==0&ci_upper!=0)|(ci_lower!=0&ci_upper==0))~ "*",
      TRUE ~ ""
    ))
  SE <- sqrt(diag(vcov(model)))
  r_estimate <- fixef(model)
  rci_lower <- r_estimate - 1.959964*SE
  rci_upper <- r_estimate + 1.959964*SE
  df2 <- data.frame(rci_lower,rci_upper)
  df2 <- df2 %>%
    mutate(Significant_rlmer = case_when(
      ((rci_lower>0&rci_upper>0)|(rci_lower<0&rci_upper<0)|(rci_lower==0&rci_upper!=0)|(rci_lower!=0&rci_upper==0))~ "*",
      TRUE ~ ""
    ))
    
  options(scipen = 999)
  # Create a function to format numbers to three decimal places
  formated <- function(x) {
    formatted <- format(round(x, 4))
    return(formatted)
  }
  # Create a summary table
  boot_summary <- data.frame(
    Boot_Estimate = round(estimates,4),
    'Boot_95_CI' = paste0('[',formated(ci_lower),',',formated(ci_upper),']'),
    Boot_Sig = df$Significant,
    rlmer_Estimate = round(fixef(model_1b.10r),4),
    'rlmer_95_CI' = paste0('[',formated(rci_lower),',',formated(rci_upper),']'),
    rlmer_Sig = df2$Significant_rlmer
  )
  return(boot_summary)
  print(boot_summary)
}




### Kable Maker Function ### table_maker(df,n)

table_maker <- function(df, n = 6, cap = "Hierarchical Model", countries, obs = 20*countries, random_slopes) {
  library("kableExtra")
  library("broom.mixed")
  library('dplyr')
  
  # Calculate the number of observations
  #observations <- countries * 20
  
  # Create the table with the specified footnotes
  nice_table <- kable(df, format = "html", caption = cap) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
    add_header_above(c(" " = 1, "Fixed Effects" = n)) %>%
    footnote(
      general = paste('* Indicates 95% CI does not contain 0 prior to rounding; Countries =', countries, '; Observations =', obs),
      footnote_as_chunk = TRUE
    ) %>%
    footnote(
      general = paste("Random slope terms included:", random_slopes),
      footnote_as_chunk = TRUE
    )
  
  print(nice_table)
}


#### Table Function for different sized models ### bs_table(boot_results, original_model, model)


bs_table <- function(boot_results, original_model, model){
  library(dplyr)
  options(scipen = 999)
  
  bootstrap_estimates <- boot_results$t
  estimates <- apply(bootstrap_estimates, 2, mean)
  ci_lower <- apply(bootstrap_estimates, 2, quantile, 0.025)
  ci_upper <- apply(bootstrap_estimates, 2, quantile, 0.975)
  ci_int <- paste0('[',format(round(ci_lower,4),nsmall=4),',',format(round(ci_upper,4),nsmall=4),']')
  params <- names(fixef(original_model))
  df_boot <- data.frame(params,estimates,ci_lower,ci_upper,ci_int)
  df_boot <- df_boot %>%
    mutate(Significant = case_when(
      ((ci_lower>0&ci_upper>0)|(ci_lower<0&ci_upper<0)|(ci_lower==0&ci_upper!=0)|(ci_lower!=0&ci_upper==0))~ "*",
      TRUE ~ ""
    ))
  
  r_estimate <- fixef(model)
  SE <- sqrt(diag(vcov(model)))
  r_ci_lower <- r_estimate - 1.959964*SE
  r_ci_upper <- r_estimate + 1.959964*SE
  options(scipen = 999)
  r_ci_int <- paste0('[',format(round(r_ci_lower,4),nsmall=4),',',format(round(r_ci_upper,4),nsmall=4),']')
  r_params <- names(fixef(model))
  df_mod <- data.frame(r_params, r_estimate, r_ci_lower,r_ci_upper,r_ci_int)
  df_mod <- df_mod %>%
    mutate(Significant_rlmer = case_when(
      ((r_ci_lower>0&r_ci_upper>0)|(r_ci_lower<0&r_ci_upper<0)|(r_ci_lower==0&r_ci_upper!=0)|(r_ci_lower!=0&r_ci_upper==0))~ "*",
      TRUE ~ ""
    ))
  
  df_final <- data.frame(parameter = unique(union(df_mod$r_params, df_boot$params)))
  df_final$boot_estimate <- rep(NA,length(df_final$parameter))
  df_final$boot_CI <- rep(NA,length(df_final$parameter))
  df_final$boot_sig <- rep(NA,length(df_final$parameter))
  df_final$rlmer_estimate <- rep(NA,length(df_final$parameter))
  df_final$rlmer_CI <- rep(NA,length(df_final$parameter))
  df_final$rlmer_sig <- rep(NA,length(df_final$parameter))
  
  x <- NA
  
  for(i in 1:length(df_final$parameter)){
    if(df_final$parameter[i] %in% df_boot$params){
      df_final$boot_estimate[i] <- format(round(df_boot[df_final$parameter[i] == df_boot$params,]$estimates,4),nsmall=4)
      df_final$boot_CI[i] <- df_boot[df_final$parameter[i] == df_boot$params,]$ci_int
      df_final$boot_sig[i] <- df_boot[df_final$parameter[i] == df_boot$params,]$Significant
    }else{
      x <- NA
    }
  }
  
  for(i in 1:length(df_final$parameter)){
    if(df_final$parameter[i] %in% df_mod$r_params){
      df_final$rlmer_estimate[i] <- format(round(df_mod[df_final$parameter[i] == df_mod$r_params,]$r_estimate,4), nsmall=4)
      df_final$rlmer_CI[i] <- df_mod[df_final$parameter[i] == df_mod$r_params,]$r_ci_int
      df_final$rlmer_sig[i] <- df_mod[df_final$parameter[i] == df_mod$r_params,]$Significant    
    }else{
      x <- NA
    }
  }
  return(df_final)
}

