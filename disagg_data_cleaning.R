library(dplyr)
library(tidyr)
setwd("/Users/joekidson/Library/Mobile Documents/com~apple~CloudDocs/Economics Masters/Time Series/Project/Inflation_GitHub")

folds <- list.files("disagg_data")
#folds <- c("itemindices2005")

count <- 0
for (f in folds) {
  print(f)
  sub_fs <- list.files(sprintf("disagg_data/%s", f))
  for (fs in sub_fs) {
    print(fs)
    path <- sprintf("disagg_data/%s/%s", f, fs)
    print(path)
    if (count == 0) {
      df <- read.csv(path)
      count <- 1
      } else {
      temp <- read.csv(path)
      names(temp) <- tolower(names(temp))
      df <- df %>% rbind(temp)
      }
    }
  }

View(df)
write.csv(df, "Data/disagg_data_merged2.csv")
head(df)

df_check <- df %>% group_by(item_id) %>% summarise(n = n()) %>% arrange(n, order = desc())
View(df_check)
View(df %>% filter(item_id == 210102))

df2 <- read.csv("ukpanel.csv")
nrow(df2[,2:93])
head(df2)
nrow(df2)
ncol(df2)

df_wide <- df %>% select(index_date, item_id, item_index) %>% pivot_wider(id_cols = index_date, names_from = item_id, values_from = item_index)
View(df_wide)

df %>% mutate(year = substr(index_date, 1,4))

head(df_wide)

include <- c("01", "04", "07", "10")
df_wide2 <- df_wide %>% filter(substr(index_date, 5, 6) %in% include)
head(df_wide2)
write.csv(df_wide)

head(df_wide)
nas <- colSums(is.na(df_wide2))
df_nas <- as.data.frame(nas[2:length(nas)])
colnames(df_nas) <- c('nas')
df_keep <- df_nas %>% filter(nas == 0)
head(df_keep)
as.matrix(df_keep)[,1]

nrow(df_nas)
nrow(df_nas %>% filter(!(nas == 0)))
head(df_nas)


df_nas <- df_wide2 %>%
  select(!index_date) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "item_index", values_to = "nas")
df_keep <- df_nas %>% filter(nas == 0)
df_wide2 <- df_wide2 %>% select(index_date, df_keep$item_index)

head(df_wide2)


head(df)
nrow(df %>% distinct(index_date))
