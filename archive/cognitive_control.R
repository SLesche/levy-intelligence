# Library
library(acdcquery)
library(tidyverse)

db_path <- "data/acdc.db"
conn <- connect_to_db(db_path)

arguments <- list() %>% 
  add_argument(
    conn,
    "percentage_neutral",
    "equal",
    0
  )

results <- query_db(conn, arguments, c("default", "task_name", "n_trials"))

good_ids <- results %>% 
  filter(!is.na(rt), !is.na(accuracy)) %>% 
  filter(rt > 0.15, rt < 2, accuracy == 0 | accuracy == 1) %>% 
  count(congruency, subject) %>% 
  filter(n < 300, n > 90) %>% 
  pull(subject)

data <- results %>% 
  filter(subject %in% good_ids) %>% 
  filter(!is.na(rt), !is.na(accuracy)) %>% 
  filter(rt > 0.15, rt < 2, accuracy == 0 | accuracy == 1) %>% 
  mutate(
    resp = accuracy
  ) %>% 
  select(task_name, subject, congruency, resp, rt)
  
filepath <- "./data/cognitive_control/raw_data/" 

data %>%
  group_by(task_name, subject, congruency) %>%
  tidyr::nest() %>%
  mutate(filename = paste0(filepath, task_name, "_", congruency, "_subject_", subject, ".csv")) %>%
  purrr::pwalk(~write.csv(..4, file = ..5, row.names = FALSE))

model_results <- read.csv("data/cognitive_control/results/levy_mat_parameters.csv")

wide_model <- model_results %>% 
  pivot_wider(
    id_cols = c("sub", "task"),
    names_from = "condition",
    values_from = c("a", "v", "t", "st", "alpha")
  )

model_results %>% 
  ggplot(
    aes(
      x =alpha,
      group = condition,
      fill = condition
    )
  )+
  geom_density()+
  facet_wrap(~task)
