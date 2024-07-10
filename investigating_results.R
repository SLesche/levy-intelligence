library(tidyverse)

model_results <- read.csv("data/levy_data/results/levy_mat_parameters.csv") %>% 
  mutate(
    Subject = parse_number(sub)
  )

intelligence_results <- read.csv2("data/iq_data/ERPData.csv")

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

hick_data_mean_values <- hick_data_clean %>% 
  group_by(task, condition, subject) %>% 
  summarize(
    mean_rt = mean(rt, na.rm = TRUE),
    mean_acc = mean(resp, na.rm = TRUE)
  )

posner_data_mean_values <- posner_data_clean %>% 
  group_by(task, condition, subject) %>% 
  summarize(
    mean_rt = mean(rt, na.rm = TRUE),
    mean_acc = mean(resp, na.rm = TRUE)
  )

sternberg_data_mean_values <- sternberg_data_clean %>% 
  group_by(task, condition, subject) %>% 
  summarize(
    mean_rt = mean(rt, na.rm = TRUE),
    mean_acc = mean(resp, na.rm = TRUE)
  )

task_data_mean_values <- rbind(hick_data_mean_values, posner_data_mean_values, sternberg_data_mean_values)

data <- model_results %>%
  left_join(., intelligence_results) %>%
  mutate(across(-c(task, sub, condition), as.numeric)) %>% 
  left_join(., task_data_mean_values, by = c("Subject" = "subject", "task", "condition"))


data$APM = data$APMeven + data$APModd
data$BIS = data$PC + data$PS + data$C + data$M

params <- c("a", "v", "t", "st", "alpha")
# intelligence <- c("APM")
intelligence <- c("APM", "BIS", "PS", "PC", "M", "C")

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

