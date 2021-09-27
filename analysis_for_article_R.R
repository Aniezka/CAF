install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(tidyverse)
library(ggplot2)
df <- read_csv("data.csv")

#For Type 1 essays
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
scaled_data <- data_result %>% mutate_at(c("max_depth",
                                           "num_advcl",
                                           "num_sent",
                                           "num_adj_noun",
                                           "num_noun_inf"), scale)
scaled_data
# Take the average for each row for the selected metrics
rowMeans(scaled_data)
df1$mean_value <- rowMeans(scaled_data)
# Plot the relationship between the number of errors and the average of the transformed metrics
df1 %>%
  ggplot(aes(mean_value, errors))+
  geom_point(size = 0.1)+
  xlab("generalized complexity metric") +
  geom_smooth(method = "glm", 
              method.args = list(family = poisson),
              se = TRUE)+
  theme_bw()
ggsave("figure_2.png", device = "png", width = 9, height = 7)

# The relationship between the number of syntactic errors and the values of syntactic complexity metrics separately
df1 %>% 
  select("max_depth", "num_advcl" , "num_sent" , "num_adj_noun" , "num_noun_inf", "errors") %>% 
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
ggarrange(p[[1]], p[[2]], p[[3]],p[[4]], p[[5]])

ggsave("figure_4.png", device = "png", width = 9, height = 7)

#For Type 2 essays
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
# Plot the relationship between the number of errors and the average of the transformed metrics
df2 %>%
  ggplot(aes(mean_value, errors))+
  geom_point(size = 0.1)+
  xlab("generalized complexity metric") +
  geom_smooth(method = "glm", 
              method.args = list(family = poisson),
              se = TRUE)+
  theme_bw()
ggsave("figure_3.png", device = "png", width = 9, height = 7)

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

ggsave("figure_5.png", device = "png", width = 9, height = 7)


###
# Code for density_plot
###

"task type" 
df_all <- df %>% 
  mutate(task_type = str_extract(name, "[12].txt"), 
         task_type = str_remove(task_type, ".txt")) %>% 
  pivot_longer(names_to = "syntactic_complexity", values_to = "values", av_depth:lemma_sim_all) %>% 
  group_by(syntactic_complexity, task_type) %>% 
  mutate(average = mean(values)) 

ggp2 <- ggplot(df_all, aes(x = values, fill=task_type)) + # Draw each column as density 
  geom_density(alpha=0.2) + 
  facet_wrap(~ syntactic_complexity, scales = "free")+ 
  scale_fill_grey() + theme_classic() 
ggp2 
ggsave("density_plot.png", device = "png", width = 9, height = 7) 


###
# Code for correlations
###

library(corrplot) 
df1 = read_csv("data_1.csv") 
cc = cor(df1[, 2:21], method = "kendal") 
ggp2 <- corrplot(cc)
ggp2 
ggsave("kendal_corr_1.png", device = "png", width = 9, height = 7) 

df2 = read_csv("data_2.csv") 
cc = cor(df2[, 2:21], method = "kendal") 
ggp2 <- corrplot(cc)
ggp2 
ggsave("kendal_corr_2.png", device = "png", width = 9, height = 7) 

###
# Code for lines with error bars
###


# This is for an essay type
df %>% 
  mutate(type=substr(name, nchar(name) - 4, nchar(name) - 4)) -> df


{error_bars <- data.frame()
  
  {for (col in colnames(df[, 2:(length(df)-3)])){
    
    mutate(df, quantile = ntile(df[, col], 4)) -> df_quant # 4 quantiles
    
    df_quant %>%
      select(col, quantile, type) %>% 
      group_by(quantile, type) %>%
      summarise_each(funs(mean, sd)) -> x  # get mean and sd
    
    x %>% 
      mutate(feature=col) -> x  # keep in mind feature name to facet over it later
    
    error_bars <- rbind(error_bars, x)
  }
}
}

ggp2 <- ggplot(error_bars, aes(x=quantile, y=mean, group=type, color=type)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap(~ feature, scales = "free")

ggp2
ggsave("errors_bars.png", device = "png", width = 9, height = 7) 


