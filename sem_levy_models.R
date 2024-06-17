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
  mutate(across(everything(), as.numeric))

a_vars <- get_names(names(wide_model), "^a_")
v_vars <- get_names(names(wide_model), "^v_")
t_vars <- get_names(names(wide_model), "^t_")
st_vars <- get_names(names(wide_model), "^st_")
alpha_vars <- get_names(names(wide_model), "^alpha_")
sternberg_vars <- get_names(names(wide_model), "sternberg")
posner_vars <- get_names(names(wide_model), "posner")
hick_vars <- get_names(names(wide_model), "hick")

psych::fa.parallel(
  data[, t_vars],
  fa = "fa",
  fm = "ml"
)

# Construct measurement models using glue
a_factor <- glue::glue("
  # Boundary Separation Model
  a =~ {paste(a_vars, collapse = ' + ')}
")

v_factor <- glue::glue("
  # Drift Rate Model
  v =~ {paste(v_vars, collapse = ' + ')}
")

t_factor <- glue::glue("
  # non-decision time Model
  t =~ {paste(t_vars, collapse = ' + ')}
")

st_factor <- glue::glue("
  # sd of non-decision time Model
  st =~ {paste(st_vars, collapse = ' + ')}
")

alpha_factor <- glue::glue("
  # non-decision time Model
  alpha =~ {paste(alpha_vars, collapse = ' + ')}
")

sternberg_factor <- glue(
  "
  # Measurement factor for sternberg
  sternberg =~ {paste(sternberg_vars, collapse = ' + ')}
  "
)

hick_factor <- glue(
  "
  # Measurement factor for hick
  hick =~ {paste(hick_vars, collapse = ' + ')}
  "
)

posner_factor <- glue(
  "
  # Measurement factor for posner
  posner =~ {paste(posner_vars, collapse = ' + ')}
  "
)

g_factor <- "
  # Intelligence measurement model
  bis =~ PS + PC + M + C
  apm =~ APMeven + APModd
  
  g =~ bis + apm
"


# Construction model
base_model <- glue(
  "
  {g_factor}
  
  {v_factor}
  
  {a_factor}
  
  {t_factor}
  
  {st_factor}
  
  {alpha_factor}
  
  {sternberg_factor}
  
  {posner_factor}
  
  {hick_factor}
  
  g ~~ 0*sternberg + 0*hick + 0*posner
  a ~~ 0*sternberg + 0*hick + 0*posner
  t ~~ 0*sternberg + 0*hick + 0*posner
  v ~~ 0*sternberg + 0*hick + 0*posner
  st ~~ 0*sternberg + 0*hick + 0*posner
  alpha ~~ 0*sternberg + 0*hick + 0*posner
  
  g ~~ alpha + v + t
  "
)

base_model <- glue(
  "
  {g_factor}
  
  {v_factor}
  
  g ~~ v
  "
)

test <- sem(model = base_model, data=data, std.ov =TRUE, estimator = "ML",missing="fiml")
summary(test, fit.measures = TRUE, standardized = TRUE)

graph_sem(test)
