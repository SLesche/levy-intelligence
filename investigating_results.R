library(dplyr)

model_results <- read.csv("data/levy_data/posner/levy_mat_posner.csv") %>% 
  mutate(
    Subject = sub
  )

intelligence_results <- read.csv2("data/iq_data/ERPData.csv")

data <- intelligence_results %>% 
  left_join(., model_results) %>% 
  mutate_all(as.numeric)

params <- c("a_ni", "a_pi", "v_ni", "v_pi", "t", "st", "alpha")

intelligence <- c("APModd", "APMeven", "PS", "PC", "M", "C")
cor(data[, intelligence], data[, params], use = "pairwise.complete.obs")

hist(data$st, breaks = 40)

hist(data$PC)
cor(data$v_ni, data$alpha)

plot(data$v_pi, data$APMeven)
