#library(magrittr)
#library(dplyr)
library(tidyverse)
#library(ggplot2)

# The relationship between the number of syntactic errors and the values of the syntactic complexity metrics, colored by task type
df <- read_csv("data.csv")
df %>% 
  mutate(task_type = str_extract(name, "[12].txt"),
         task_type = str_remove(task_type, ".txt")) %>% 
  pivot_longer(names_to = "syntactic_complexity", values_to = "values", av_depth:lemma_sim_all) %>% 
  group_by(syntactic_complexity, task_type) %>% 
  mutate(average = mean(values)) %>% 
  ggplot(aes(values, errors, color = task_type))+
  geom_point(size = 0.1)+
  labs(col = "Task type") +
  geom_smooth(method = "glm", 
              method.args = list(family = poisson), 
              aes(group = task_type), se = FALSE)+
  geom_rug(aes(x = average), sides = "b")+
  facet_wrap(~syntactic_complexity, scales = "free_x")+
  scale_colour_grey() +
  theme_bw()

ggsave("figure_7.png", device = "png", width = 9, height = 7)

# FOR TYPE 1 ESSAYS
df1 <- read_csv("data_1.csv")
colnames(df1)
M1 <- glm(errors ~ ., 
          family =  "poisson", 
          data =  df1[,-c(1, 22, 24)]) # remove id, type and name columns
summary(M1)
# Find aliases (linearly dependent terms)
alias(glm(errors ~ ., family =  "poisson", data =  df1[,-c(1, 22, 24)])) # We need to remove the num_compl_tu variable
M1 <- glm(errors ~ ., family =  "poisson", data =  df1[,-c(1, 22, 24, 13)])
summary(M1)
# Calculate the variation inflation factors of all predictors
car::vif(M1) # Remove pos_sim_nei, lemma_sim_nei, pos_sim_all, lemma_sim_all
M1 <- glm(errors ~ ., family =  "poisson", data =  df1[,-c(1, 22, 24, 13, 18:21)])
summary(M1)
# Choose the variables satisfying CI
data_result <- df1 %>% select("max_depth", "num_advcl" , "num_sent" , "num_adj_noun" , "num_noun_inf")
data_result
# Scale the data
scaled_data <- data_result %>%
  mutate(across(everything(), scale))
# scaled_data <- data_result %>% mutate_at(c("max_depth",
#                                            "num_advcl",
#                                            "num_sent",
#                                            "num_adj_noun",
#                                            "num_noun_inf"), scale)
scaled_data
# Take the average for each row for the selected metrics
rowMeans(scaled_data)
df1$mean_value <- rowMeans(scaled_data)
# The relationship between the number of errors and the average of the transformed metrics
df1 %>%
  ggplot(aes(mean_value, errors))+
  geom_point(size = 0.1)+
  xlab("generalized complexity metric") +
  geom_smooth(method = "glm", 
              method.args = list(family = poisson),
              se = TRUE)+
  theme_bw()

ggsave("figure_2_type1.png", device = "png", width = 9, height = 7)

# The relationship between the number of syntactic errors and the values of syntactic complexity metrics separately
df1 %>% 
  select("max_depth", "num_advcl" , "num_sent" , "num_adj_noun" , "num_noun_inf", "errors") %>% 
  glm(errors ~ ., 
      family =  "poisson", 
      data = .) ->
  final_fit
summary(final_fit)
library(ggeffects)
p <- ggpredict(final_fit) %>% 
  plot()
library(ggpubr)
ggarrange(p[[1]], p[[2]], p[[3]],p[[4]], p[[5]])

ggsave("figure_4_type1.png", device = "png", width = 9, height = 7)

#FOR TYPE 2 ESSAYS
df2 <- read_csv("data_2.csv")
colnames(df2)
M2 <- glm(errors ~ ., 
          family =  "poisson", 
          data =  df2[,-c(1, 22, 24)]) 
summary(M2)
# Find aliases (linearly dependent terms)
alias(glm(errors ~ ., family =  "poisson", data =  df2[,-c(1, 22, 24)])) # We need to remove the num_compl_tu variable
M2 <- glm(errors ~ ., family =  "poisson", data =  df2[,-c(1, 22, 24, 13)])
summary(M2)
# Calculate the variation inflation factors of all predictors
car::vif(M2)
M2 <- glm(errors ~ ., family =  "poisson", data =  df2[,-c(1, 22, 24, 13, 18:21)])
summary(M2)
# Choose the variables satisfying CI
data_result <- df2 %>% select("num_advcl" , "num_sent" , "num_tok", "av_len_sent", "num_cl", "num_tu" , "num_coord" , "num_part_noun")
data_result
# Scale the data
scaled_data <- data_result %>% mutate_at(c("num_advcl" , "num_sent" , "num_tok", "av_len_sent", "num_cl", "num_tu" , "num_coord" , "num_part_noun"), scale)
scaled_data
# Take the average for each row for the selected metrics
rowMeans(scaled_data)
df2$mean_value <- rowMeans(scaled_data)
# The relationship between the number of errors and the average of the transformed metrics
df2 %>%
  ggplot(aes(mean_value, errors))+
  geom_point(size = 0.1)+
  xlab("generalized complexity metric") +
  geom_smooth(method = "glm", 
              method.args = list(family = poisson),
              se = TRUE)+
  theme_bw()

ggsave("figure_3_type2.png", device = "png", width = 9, height = 7)

# The relationship between the number of syntactic errors and the values of syntactic complexity metrics separately
df2 %>% 
  select("num_advcl" , "num_sent" , "num_tok", "av_len_sent", "num_cl", "num_tu" , "num_coord" , "num_part_noun", "errors") %>% 
  glm(errors ~ ., 
      family =  "poisson", 
      data = .) ->
  final_fit
summary(final_fit)
library(ggeffects)
ggpredict(final_fit) %>% 
  plot() ->
  p
library(ggpubr)
ggarrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], ncol=4, nrow=2)

ggsave("figure_5_type2.png", device = "png", width = 9, height = 7)

# The density distribution of syntactic complexity metrics for two types of essays 
df_all <- df %>% 
  mutate(task_type = str_extract(name, "[12].txt"), 
         task_type = str_remove(task_type, ".txt")) %>% 
  pivot_longer(names_to = "syntactic_complexity", values_to = "values", av_depth:lemma_sim_all) %>% 
  group_by(syntactic_complexity, task_type) %>% 
  mutate(average = mean(values)) 
ggp2 <- ggplot(df_all, aes(x = values, fill=task_type)) + # Draw each column as density 
  geom_density(alpha=0.6) + 
  facet_wrap(~ syntactic_complexity, scales = "free")+ 
  scale_fill_grey() + theme_classic() 
ggp2 

ggsave("figure_6.png", device = "png", width = 9, height = 7) 

# Kendall rank correlation coefficients across Task 1 essays
library(corrplot) 
df1 = read_csv("data_1.csv") 
cc = cor(df1[, 2:21], method = "kendal") 
ggp2 <- corrplot(cc, method = "color")
ggp2 

ggsave("figure_8_type1.png", device = "png", width = 9, height = 7) 

# Kendall rank correlation coefficients across Task 2 essays
df2 = read_csv("data_2.csv") 
cc = cor(df2[, 2:21], method = "kendal") 
ggp2 <- corrplot(cc, method = "color")
ggp2 

ggsave("figure_9_type2.png", device = "png", width = 9, height = 7) 

# The mean value of syntactic complexity metrics by quantiles for two types of essays
pd <- position_dodge(.15)
ggp2 <- ggplot(error_bars, aes(x=quantile, y=mean, group=type, color=type)) + 
  geom_line(position = pd) +
  geom_point(position = pd)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=pd) +
  facet_wrap(~ feature, scales = "free") +
  scale_colour_grey() +
  theme_linedraw()

ggp2
ggsave("figure_1.png", device = "png", width = 9, height = 7) 