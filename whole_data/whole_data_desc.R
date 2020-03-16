# #
# # 后台执行！！！！！
# library(tidyverse)
# 
# # 宽数据：imdb
# imdb <- read_csv("/data1/zhiliang/RWorkSpace/one_belt_one_road/data/imdb_0103.csv")
# 

non_countries_vars <- colnames(imdb) %>% .[1:32]

ncol(imdb)

countries_vars



num <- 33:275


a <- 1:50
b <- c(1:32,51:100)
c <- c(1:32,101:150)
d <- c(1:32,151:200)
e <- c(1:32,201:235)
f <- c(1:32,236:275)


imdb_a <- imdb %>% .[a]
imdb_b <- imdb %>% .[b]
imdb_c <- imdb %>% .[c]
imdb_d <- imdb %>% .[d]
imdb_e <- imdb %>% .[e]
imdb_f <- imdb %>% .[f]


for (i in num) {
    assign(paste0("df_imdb_",i), imdb[,c(1,i)])
}


ls()

for(i in 1:6) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
  nam <- paste("r", i, sep = ".")
  assign(nam, 1:i)
}
ls(pattern = "^r..$")






wide_to_long <- function(df){
  df <- imdb %>%
      gather(-non_countries_vars,key = "country",value = "produce") %>%  
       filter(produce != 0)
}



wide_to_long(imdb_a)



# # 长数据：imdb_long
#
# imdb_long <- imdb %>%
#   gather(-non_countries_vars,key = "country",value = "produce") %>%  #将多列的国家转成一列
#   filter(produce != 0)
#
# write_csv(imdb_long,"/data1/zhiliang/RWorkSpace/one_belt_one_road/data/imdb_long.csv")