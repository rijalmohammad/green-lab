library(tidyverse)
library(dplyr)
library(bayestestR)

columns = c(
  "browser",
  "language",
  "benchmark",
  "avg_energy"
)

#energy_data will store the necessary data for analysis
energy_data = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(energy_data) = columns

energy_csv_paths <-  list.files('../data/trepn', 
                         recursive=TRUE,
                         pattern="^192-168-0-118-5556.*\\.csv",
                         full.names=TRUE
)
energy_csv_paths

#format data from trepn and save in a file (without execution time)
for (index in (1:length(energy_csv_paths))) {
  #p_f_b means programming language, benchmark function, browser
  energy_csv_path = energy_csv_paths[index]
  energy_csv_path
  
  #init_energy_data_p_f_b is the merged data we get from the trepn 
  init_energy_data_p_f_b = energy_csv_path %>%
    lapply(read.csv) %>%
    bind_rows
  
  init_energy_data_p_f_b
  
  colnames(init_energy_data_p_f_b)[3] = "time"
  colnames(init_energy_data_p_f_b)[4]="battery_power"
  colnames(init_energy_data_p_f_b)[7]="cpu"

  init_energy_data_p_f_b = init_energy_data_p_f_b %>%
    drop_na() %>%
    filter(cpu > 0) %>%
    filter(cpu <= 100)
  
  init_energy_data_p_f_b
  
  length(init_energy_data_p_f_b$time)
  length(init_energy_data_p_f_b$battery_power)
  
  avg_energy_consumption = area_under_curve(init_energy_data_p_f_b$time, init_energy_data_p_f_b$battery_power)  # gives area under curve - for the single run (ms * uW)
  
  avg_energy_consumption
  
  joule_avg_energy_consumption = avg_energy_consumption / (10^9) 

  joule_avg_energy_consumption

  #extract browser, language, and benchmark
  csv_path_split = energy_csv_path %>%
    strsplit('/', fixed='TRUE')
  
  browser = csv_path_split %>%
    rapply(nth, n=6)
  browser
  
  folder_path = csv_path_split %>%
    rapply(nth, n=5) %>%
    strsplit('-', fixed='TRUE')
  
  language = folder_path %>%
    rapply(nth, n=7)
  language

  benchmark = folder_path %>%
    rapply(nth, n=8)
  benchmark
  
  data_item = list(
    browser,
    language,
    benchmark,
    joule_avg_energy_consumption
  )
  
  data_item
  
  energy_data[nrow(energy_data) + 1,] <- data_item
  energy_data
}

energy_data

write.csv(energy_data,"energy_consumption_data.csv", row.names = FALSE)

