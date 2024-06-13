library(dplyr)

model_results <- read.csv("data/levy_data/posner/levy_mat_posner.csv") %>% 
  mutate(
    Subject = sub
  )

intelligence_results <- read.csv2("data/iq_data/ERPData.csv")

data <- intelligence_results %>% 
  left_join(., model_results) %>% 
  mutate_all(as.numeric)


cor(data$PS, data$t, use = "pairwise.complete.obs")

hist(data$t, breaks = 40)

cor(data$v_ni, data$alpha)
