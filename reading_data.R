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
  mutate(condition = condition -1) %>% 
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
  ungroup()



# Need to save per participant with sub, cond, response, rt,

filepath_posner <- "./data/levy_data/posner/posner_data_subject_"
posner_data_clean %>%
  group_by(subject) %>%
  tidyr::nest() %>%
  mutate(filename = paste0(filepath_posner, subject, ".csv")) %>%
  purrr::pwalk(~write.csv(..2, file = ..3, row.names = FALSE))

filepath_sternberg <- "./data/levy_data/sternberg/"
sternberg_data_clean %>%
  group_by(subject, condition) %>%
  tidyr::nest() %>%
  mutate(filename = paste0(filepath_sternberg, paste0("condition_"), condition, "/sternberg_data_subject_", subject, ".csv")) %>%
  purrr::pwalk(~write.csv(..3, file = ..4, row.names = FALSE))

