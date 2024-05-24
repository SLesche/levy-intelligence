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

true_rt = posner_ni$RT

hist(true_rt, breaks = 100, xlim = c(0, 5))
