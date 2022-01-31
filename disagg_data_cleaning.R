library(dplyr)
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
      } else {
      temp <- read.csv(path)
      names(temp) <- tolower(names(temp))
      df <- df %>% rbind(temp)
      }
    }
  }

View(df)
