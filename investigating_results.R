library(tidyverse)

model_results <- read.csv("data/levy_data/results/levy_mat_parameters.csv") %>% 
  mutate(
    Subject = parse_number(sub)
  )

intelligence_results <- read.csv2("data/iq_data/ERPData.csv")

data <- model_results %>%
  left_join(., intelligence_results) %>%
  mutate(across(-c(task, sub, condition), as.numeric))

params <- c("a", "v", "t", "st", "alpha")
intelligence <- c("APModd", "APMeven", "PS", "PC", "M", "C")

get_correlation <- function(data){
  cors = cor(data[, intelligence], data[, params], use = "pairwise.complete.obs")
  return(cors)
}

nested_data <- data %>% 
  group_by(task, condition) %>% 
  nest() %>% 
  mutate(
    cors = map(data, get_correlation)
  )

results <- data.frame()

for (irow in 1:nrow(nested_data)){
  df = as.data.frame(nested_data$cors[[irow]])
  
  df$task = nested_data$task[irow]
  df$condition = nested_data$condition[irow]
  
  results = rbind(results, df)
}


results <- results %>% 
  mutate(
    measure = rownames(.)
  ) %>% 
  pivot_longer(
    cols = c("a", "v", "t", "st", "alpha"),
    names_to = "param",
    values_to = "cor"
  )

results %>% 
  ggplot(
    aes(
      x = cor,
      fill = param,
    )
  )+
  geom_boxplot()+
  theme_classic()


hist(data$alpha)
