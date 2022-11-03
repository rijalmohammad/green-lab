library(tidyverse)
library(dplyr)
library(ggplot2)
library(bayestestR)

columns = c(
  "browser",
  "language",
  "benchmark",
  "avg_memory",
  "avg_cpu"
)

#memcpu_data will store the necessary data for analysis
memcpu_data = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(memcpu_data) = columns
memcpu_data
memcpu_csv_paths <-  list.files('../data/mem-cpu/', 
                         recursive=TRUE,
                         pattern="^192.168.0.118.*\\.csv",
                         full.names=TRUE
)
memcpu_csv_paths

#format data from trepn and save in a file (without execution time)
for (index in (1: length(memcpu_csv_paths))) {
  #p_f_b means programming language, benchmark function, browser
  memcpu_csv_path = memcpu_csv_paths[index]
  memcpu_csv_path
  
  #init_memcpu_data_p_f_b is the merged data we get from the trepn 
  init_memcpu_data_p_f_b = memcpu_csv_path %>%
    lapply(read.csv) %>%
    bind_rows
  
  #remove row with n/a data
  init_memcpu_data_p_f_b %>% drop_na()

  init_cpu_data_p_f_b = init_memcpu_data_p_f_b %>%
    filter(cpu > 0) %>%
    filter(cpu <= 100)

  avg_cpu_load = mean(init_cpu_data_p_f_b$cpu)

  init_mem_data_p_f_b = init_memcpu_data_p_f_b %>%
    filter(mem > 0)
  
  avg_memory_usage = mean(init_memcpu_data_p_f_b$mem)

  #extract browser, language, and benchmark
  csv_path_split = memcpu_csv_path %>%
    strsplit('/', fixed='TRUE')

  browser = csv_path_split %>%
    rapply(nth, n=7)

  folder_path = csv_path_split %>%
    rapply(nth, n=6) %>%
    strsplit('-', fixed='TRUE')

  language = folder_path %>%
    rapply(nth, n=7)

  benchmark = folder_path %>%
    rapply(nth, n=8)
  
  data_item = list(
    browser,
    language,
    benchmark,
    avg_memory_usage,
    avg_cpu_load
  )
  
  data_item
  
  memcpu_data[nrow(memcpu_data) + 1,] <- data_item
}

memcpu_data

write.csv(memcpu_data,"memcpu_data.csv", row.names = FALSE)

