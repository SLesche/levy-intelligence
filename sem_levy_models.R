library(tidyverse)
library(glue)
library(lavaan)
library(semPlot)

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
g=~ PC + PS + C + M + APM

PC ~ 0*1
PS ~ 0*1
C ~ 0*1
M ~ 0*1
APM ~ 0*1
"

cor(data[, intelligence], use = "pairwise.complete.obs")

intelligence_model <- sem(model = true_g_factor, data = data, estimator = "ML")
summary(intelligence_model, fit.measures = TRUE, standardized = TRUE)

alpha_factor <- glue::glue("
  # non-decision time Model
  alpha =~ {paste(alpha_vars, collapse = ' + ')}
  alpha_sternberg_s1 ~~ s.alpha*alpha_sternberg_s1
  alpha_sternberg_s3 ~~ s.alpha*alpha_sternberg_s3
  alpha_sternberg_s5 ~~ s.alpha*alpha_sternberg_s5
  alpha_hick_0bit ~~ h.alpha*alpha_hick_0bit
  alpha_hick_1bit ~~ h.alpha*alpha_hick_1bit
  alpha_hick_2bit ~~ h.alpha*alpha_hick_2bit
  alpha_posner_ni ~~ p.alpha*alpha_posner_ni
  alpha_posner_pi ~~ p.alpha*alpha_posner_pi

  sternberg =~ sload.alpha*alpha_sternberg_s1 + sload.alpha*alpha_sternberg_s3 + sload.alpha*alpha_sternberg_s5
  # hick =~ alpha_hick_0bit + alpha_hick_1bit + alpha_hick_2bit

  posner =~ pload.alpha*alpha_posner_ni + pload.alpha*alpha_posner_pi

  # hick ~~ 0*alpha
  posner ~~ 0*alpha
  sternberg ~~ 0*alpha
    
    
  {paste(c(alpha_vars, ''), collapse = ' ~ 0*1\n')}

  
")


alpha_model <- sem(alpha_factor, data = data, std.ov = TRUE, estimator = "ML", missing = "fiml")
summary(alpha_model, fit.measures = TRUE, standardized = TRUE)


v_factor <- glue::glue("
  # non-decision time Model
  v =~ {paste(v_vars, collapse = ' + ')}
  v_sternberg_s1 ~~ s.v*v_sternberg_s1
  v_sternberg_s3 ~~ s.v*v_sternberg_s3
  v_sternberg_s5 ~~ s.v*v_sternberg_s5
  v_hick_0bit ~~ h.v*v_hick_0bit
  v_hick_1bit ~~ h.v*v_hick_1bit
  v_hick_2bit ~~ h.v*v_hick_2bit
  v_posner_ni ~~ p.v*v_posner_ni
  v_posner_pi ~~ p.v*v_posner_pi

  sternberg =~ sload.v*v_sternberg_s1 + sload.v*v_sternberg_s3 + sload.v*v_sternberg_s5
  # hick =~ v_hick_0bit + v_hick_1bit + v_hick_2bit

  posner =~ pload.v*v_posner_ni + pload.v*v_posner_pi

  # hick ~~ 0*v
  posner ~~ 0*v
  sternberg ~~ 0*v
  
  {paste(c(v_vars, ''), collapse = ' ~ 0*1\n')}

")

v_model <- sem(v_factor, data = data, std.ov = TRUE, estimator = "ML", missing = "fiml")
summary(v_model, fit.measures = TRUE, standardized = TRUE)


full_combined_model <- glue::glue(
  "
  {alpha_factor}
  
  {v_factor}
  
  {true_g_factor}
  
  g ~~ v + alpha
  sternberg ~~ posner
  "
)

combined_model <- glue::glue(
  "
  {alpha_factor}
  
  {true_g_factor}
  
  g ~~ alpha
  sternberg ~~ posner
  "
)
v_combined_model <- glue::glue(
  "
  {v_factor}
  
  {true_g_factor}
  
  g ~~ v
  sternberg ~~ posner
  "
)

alpha_g <- sem(model = combined_model, data=data, std.ov =TRUE, estimator = "ML", orthogonal = TRUE)
summary(alpha_g, fit.measures = TRUE, standardized = TRUE)

v_g <- sem(model = v_combined_model, data=data, std.ov =TRUE, estimator = "ML", orthogonal = TRUE)
summary(v_g, fit.measures = TRUE, standardized = TRUE)

