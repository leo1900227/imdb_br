
library(pacman)
p_load(tidyverse,data.table,magrittr,splitstackshape,qdapTools)
imdb <- read_csv("imdb_1224.csv")



# 国家 ----------------------------------------------------------------------
df_place_of_production <- mtabulate(strsplit(as.character(imdb$place_of_production), "||", fixed = TRUE))


write_csv(df_place_of_production,"df_place_of_production.csv")


# 电影类型 --------------------------------------------------------------------
df_genres <- mtabulate(strsplit(as.character(imdb$genres), "||", fixed = TRUE))


write_csv(df_genres,"df_genres.csv")
