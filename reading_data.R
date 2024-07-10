library(dplyr)

posner_data <- haven::read_sav("./data/Posner_RawData_0503.sav")
hick_data <- haven::read_sav("./data/Hick_RawData_1901.sav")
sternberg_data <- haven::read_sav("./data/Sternberg_RawData_1901.sav")

is_outlier <- function(vector){
  mean = mean(vector, na.rm = TRUE)
  sd = sd(vector, na.rm = TRUE)
  
  is_outlier = (vector < mean - 3*sd) | (vector > mean + 3*sd)
  return(is_outlier)
}

posner_data_clean <- posner_data %>% 
  group_by(Subject, condNEW) %>% 
  mutate(is_outlier = is_outlier(RT)) %>% 
  filter(is_outlier == 0) %>% 
  select(
    subject = Subject,
    condition = condNEW,
    resp = Accuracy,
    rt = RT
  ) %>% 
  mutate(condition = ifelse(condition == 1, "pi", "ni"),
         task = "posner") %>% 
  ungroup()

hick_data_clean <- hick_data %>% 
  group_by(Subject, condNEW) %>% 
  mutate(is_outlier = is_outlier(RT)) %>% 
  filter(is_outlier == 0) %>% 
  select(
    subject = Subject,
    condition = condNEW,
    resp = Accuracy,
    rt = RT
  ) %>% 
  mutate(
    condition = case_when(
      condition == 1 ~ "0bit",
      condition == 2 ~ "1bit",
      condition == 3 ~ "2bit"
    ),
    task = "hick"
  ) %>% 
  ungroup()

sternberg_data_clean <- sternberg_data %>% 
  group_by(Subject, condNEW) %>% 
  mutate(is_outlier = is_outlier(RT)) %>% 
  filter(is_outlier == 0) %>% 
  select(
    subject = Subject,
    condition = condNEW,
    resp = Accuracy,
    rt = RT
  ) %>% 
  mutate(
    condition = case_when(
      condition == 1 ~ "s1",
      condition == 2 ~ "s3",
      condition == 3 ~ "s5"
    ),
    task = "sternberg"
  ) %>% 
  ungroup()

hick_data_removed <- 1 - nrow(hick_data_clean) / nrow(hick_data)
posner_data_removed <- 1 - nrow(posner_data_clean) / nrow(posner_data)
sternberg_data_removed <- 1 - nrow(sternberg_data_clean) / nrow(sternberg_data)

# Need to save per participant with sub, cond, response, rt,
filepath <- "./data/levy_data/" 

full_data_clean <- rbind(posner_data_clean, sternberg_data_clean, hick_data_clean)

full_data_clean %>%
  group_by(task, subject, condition) %>%
  tidyr::nest() %>%
  mutate(filename = paste0(filepath, task, "/condition_", condition, "/", task, "_data_subject", subject, ".csv")) %>%
  purrr::pwalk(~write.csv(..4, file = ..5, row.names = FALSE))

