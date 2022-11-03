library(tidyverse)
library(dplyr)

#time_data
time_column = c(
  "browser",
  "language",
  "benchmark",
  "start_time_stamp",
  "end_time_stamp",
  'execution_time'
)

time_data = data.frame(matrix(nrow = 0, ncol=length(time_column)))
colnames(time_data) = time_column

timestamp_paths  <-  list.files("../data/trepn",
                                recursive=TRUE,
                                pattern="*Output.txt",
                                full.names=TRUE
)
timestamp_paths

for (tindex in (1:length(timestamp_paths))) {
  timestamp_filepath = timestamp_paths[tindex]
  timestamp_filepath
  
  length_time_stamp = 10
  #init_data_p_f_b is the merged data we get from the trepn 
  timestamp_raw_data =  read_tsv(timestamp_filepath,
                                 col_names = c('start_timestamp', 'end_timestamp'),
                                 show_col_types = FALSE)
  timestamp_raw_data
  
  start_time_stamp = as.double(str_split_fixed(timestamp_raw_data$start_timestamp, 'start timestamp: ', 2)[,2])[1:10]
  end_time_stamp = as.double(str_split_fixed(timestamp_raw_data$end_timestamp, 'end timestamp: ', 2)[,2])[1:10]
  
  execution_time = (end_time_stamp - start_time_stamp) * 1000 #converting to millisecond

  timestamp_path_split = timestamp_filepath %>%
    strsplit('/', fixed='TRUE')
  timestamp_path_split
  
  browser = timestamp_path_split %>%
    rapply(nth, n=6)
  browser = str_replace(browser, 'Output.txt', "")
  
  browser_list = rep(c(browser),times=c(length_time_stamp))
  browser_list
  
  timestamp_folder_path = timestamp_path_split %>%
    rapply(nth, n=5) %>%
    strsplit('-', fixed='TRUE')
  
  language = timestamp_folder_path %>%
    rapply(nth, n=7)
  
  language
  language_list = rep(c(language),times=c(length_time_stamp))
  language_list
  
  benchmark = timestamp_folder_path %>%
    rapply(nth, n=8)
  benchmark_list = rep(c(benchmark),times=c(length_time_stamp))
  benchmark_list
  
  
  tmp_dataframe = bind_cols(
    browser_list,
    language_list,
    benchmark_list,
    start_time_stamp,
    end_time_stamp,
    execution_time
  )
  
  nrow(tmp_dataframe)
  
  for (data_index in (1:nrow(tmp_dataframe))) {
    time_data[nrow(time_data) + 1,] <- tmp_dataframe[data_index,]
  }
}

time_data

write.csv(time_data,"execution_time_data.csv", row.names = FALSE)

