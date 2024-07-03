spearman_brown_double <- function(rel){
  corrected_rel = c()
  
  for (i in seq_along(rel)){
    if (is.na(rel[i])){
      corrected_rel[i] = NA
    } else if (rel[i] > 0){
      corrected_rel[i] = (2*rel[i]) / (1 + rel[i])
    } else {
      corrected_rel[i] = -1*(2*abs(rel[i])) / (1 + abs(rel[i]))
    }
  }
  
  
  return(corrected_rel)
}

FisherZ <- function(rho)  {0.5*log((1+rho)/(1-rho)) }   #converts r to z

FisherZInv <- function(z) {(exp(2*z)-1)/(1+exp(2*z)) }   #converts back again

fisher_cor_mean <- function(corr_values){
  corr_values[corr_values == 1] = 0.99
  corr_values[corr_values == -1] = -0.99
  corr_values[corr_values > 1 | corr_values < -1] = NA
  z_values = FisherZ(corr_values)
  mean_value = mean(z_values, na.rm = TRUE)
  mean_corr = FisherZInv(mean_value)
  return(mean_corr)
}


print_sem <- function(sem_object){
  sem_sum = summary(sem_object, fit.measures = TRUE)
  df = sem_sum[["fit"]][["df"]]
  chisq = sem_sum[["fit"]][["chisq"]]
  cfi = sem_sum[["fit"]][["cfi"]]
  rmsea = sem_sum[["fit"]][["rmsea"]]
  p = sem_sum[["fit"]][["pvalue"]]
  
  message = paste0(
    "$\\chi^2(",
    df,
    ") = ",
    round(chisq, 2),
    ", p = ",
    papaja::apa_p(p),
    ", CFI = ",
    round(cfi, 2),
    ", RMSEA = ",
    round(rmsea, 2),
    "$"
  )
  
  return(message)
}
