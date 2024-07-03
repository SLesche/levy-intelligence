library(tidyverse)

model_results <- read.csv("data/levy_data/results/levy_mat_parameters.csv") %>% 
  mutate(
    Subject = parse_number(sub)
  )

intelligence_results <- read.csv2("data/iq_data/ERPData.csv")

data <- model_results %>%
  left_join(., intelligence_results) %>%
  mutate(across(-c(task, sub, condition), as.numeric))

data$APM = data$APMeven + data$APModd
data$BIS = data$PC + data$PS + data$C + data$M

params <- c("a", "v", "t", "st", "alpha")
# intelligence <- c("APM")
intelligence <- c("APM", "BIS", "PS", "PC", "M", "C")

get_correlation <- function(data){
  cors = cor(data[, intelligence], data[, params], use = "pairwise.complete.obs")
  return(cors)
}

run_lm <- function(data){
  data = data %>% 
    mutate(
      across(c(a, t, v, st, alpha), ~scale(.)[, 1])
    )
  lm = lm(APM ~ a + t + v + st + alpha, data = data)
  return(lm)
}

nested_data <- data %>% 
  group_by(task, condition) %>% 
  nest() %>% 
  mutate(
    cors = map(data, get_correlation),
    lm = map(data, run_lm)
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
  mutate(
    measure = str_remove(measure, "\\d+$")
  ) %>% 
  pivot_longer(
    cols = c("a", "v", "t", "st", "alpha"),
    names_to = "param",
    values_to = "cor"
  )

plot_by_measure <- results %>% 
  ggplot(
    aes(
      x = param,
      y = cor,
      fill = param,
    )
  )+
  facet_wrap(~measure)+
  geom_boxplot()+
  theme_classic()+
  geom_hline(yintercept = 0, color = "red")

