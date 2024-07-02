library(tidyverse)
library(glue)
library(lavaan)
library(tidySEM)

get_names <- function(names, pattern){
  matches = names[stringr::str_detect(names, pattern)]
  return(matches)
}

model_results <- read.csv("data/levy_data/results/levy_mat_parameters.csv") %>% 
  mutate(
    Subject = parse_number(sub)
  )

params <- c("a", "v", "t", "st", "alpha")
intelligence <- c("APModd", "APMeven", "PS", "PC", "M", "C")


wide_model <- model_results %>% 
  pivot_wider(
    names_from = c(task, condition),
    values_from = params
  ) %>% 
  select(-sub)

intelligence_results <- read.csv2("data/iq_data/ERPData.csv")

data <- wide_model %>%
  left_join(., intelligence_results) %>%
  mutate(across(everything(), as.numeric)) %>% 
  mutate(APM = APMeven + APModd) %>% 
  mutate(across(everything(), ~scale(.)[, 1]))


a_vars <- get_names(names(wide_model), "^a_")
v_vars <- get_names(names(wide_model), "^v_")
t_vars <- get_names(names(wide_model), "^t_")
st_vars <- get_names(names(wide_model), "^st_")
alpha_vars <- get_names(names(wide_model), "^alpha_")
sternberg_vars <- get_names(names(wide_model), "sternberg")
posner_vars <- get_names(names(wide_model), "posner")
hick_vars <- get_names(names(wide_model), "hick")

psych::fa.parallel(
  data[, intelligence],
  fa = "fa",
  fm = "ml"
)

g_factor <- "
  # Intelligence measurement model
  bis =~ PC + PS + M + C
  apm =~ 1*APMeven + 1*APModd
  
  g =~ 1*bis + 1*apm
"

true_g_factor <- "
g=~ PC + PS + C + M + APM"

cor(data[, intelligence], use = "pairwise.complete.obs")

intelligence_model <- sem(model = true_g_factor, data = data, estimator = "ML")
summary(intelligence_model, fit.measures = TRUE, standardized = TRUE)

alpha_factor <- glue::glue("
  # non-decision time Model
  alpha =~ {paste(alpha_vars, collapse = ' + ')}
  alpha_sternberg_s1 ~~ s*alpha_sternberg_s1
  alpha_sternberg_s3 ~~ s*alpha_sternberg_s3
  alpha_sternberg_s5 ~~ s*alpha_sternberg_s5
  alpha_hick_0bit ~~ h*alpha_hick_0bit
  alpha_hick_1bit ~~ h*alpha_hick_1bit
  alpha_hick_2bit ~~ h*alpha_hick_2bit
  alpha_posner_ni ~~ p*alpha_posner_ni
  alpha_posner_pi ~~ p*alpha_posner_pi

  sternberg =~ alpha_sternberg_s1 + alpha_sternberg_s3 + alpha_sternberg_s5
  hick =~ alpha_hick_0bit + alpha_hick_1bit + alpha_hick_2bit

  posner =~ 1*alpha_posner_ni + 1*alpha_posner_pi
  
  hick ~~ 0*alpha
  posner ~~ 0*alpha 
  sternberg ~~ 0*alpha
")

combined_model <- glue::glue(
  "
  {alpha_factor}
  
  {true_g_factor}
  
  g ~~ alpha
  
  g ~~ 0*hick + 0*sternberg + 0*posner
  "
)


alpha_model <- sem(alpha_factor, data = data, std.ov = TRUE, estimator = "GLS")
summary(alpha_model, fit.measures = TRUE, standardized = TRUE)

test <- sem(model = combined_model, data=data, std.ov =TRUE, estimator = "GLS")
summary(test, fit.measures = TRUE, standardized = TRUE)

graph_sem(alpha_model)
