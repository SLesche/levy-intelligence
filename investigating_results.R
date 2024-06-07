library(dplyr)

model_results <- read.csv("data/levy_data/posner/levy_mat_parameter.csv") %>% 
  mutate(
    Subject = readr::parse_number(sub)
  )

intelligence_results <- read.csv2("data/iq_data/ERPData.csv")

data <- intelligence_results %>% 
  left_join(., model_results) %>% 
  mutate_all(as.numeric)


cor(data$PS, data$v, use = "pairwise.complete.obs")
