library(pacman)
p_load(data.table,tidyverse)

list <- ls()
list

cc_list_df %>% 
  arrange(desc(weight)) %>% 
  head(10)



# df: 国家，参与国际合作的电影数量 ------------------------------------------------------

df_list_1 <- 
  cc_list_df[,c(1,3)] %>% 
  magrittr::set_colnames(c("country","n"))

df_list_2 <- 
  cc_list_df[,c(2,3)] %>% 
  magrittr::set_colnames(c("country","n"))


df_country_sum <- 
  rbind(df_list_1,df_list_2) %>% 
  group_by(country) %>% 
  summarise(sum = sum(n)) %>% 
  arrange(desc(sum))


