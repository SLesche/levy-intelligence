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
levy_data <- posner_ni %>% 
  select(
    subject = Subject,
    condition = condNEW,
    resp = Accuracy,
    rt = RTms
  )

filepath <- "./data/levy_data/posner/posner_ni_data_subject"

levy_data %>%
  group_by(subject) %>%
  tidyr::nest() %>%
  mutate(filename = paste0(filepath, subject, ".csv")) %>%
  purrr::pwalk(~write.csv(..2, file = ..3, row.names = FALSE))

true_rt = posner_ni$RT

hist(true_rt, breaks = 100, xlim = c(0, 5))
