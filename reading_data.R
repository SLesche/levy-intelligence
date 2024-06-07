library(dplyr)

posner_data <- haven::read_sav("./data/Posner_RawData_0503.sav")
hick_data <- haven::read_sav("./data/Hick_RawData_1901.sav")
sternberg_data <- haven::read_sav("./data/Sternberg_RawData_1901.sav")

posner_data_clean <- posner_data %>% 
  filter(RT < 5)

posner_data_clean %>% pull(RTms) %>% hist(breaks = 100)

posner_ni <- posner_data_clean %>% 
  filter(condNEW == 2)

few_trials <- posner_ni %>% 
  count(Subject) %>% 
  filter(n < 275) %>% 
  pull(Subject)

posner_ni <- posner_ni %>% 
  filter(Subject != few_trials)

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
