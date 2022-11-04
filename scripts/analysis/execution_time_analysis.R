library(tidyverse)
library(ggplot2)
library(svglite)
library(bestNormalize)
library(effectsize)

execution_time_data_csv_path = './execution_time_data.csv'
execution_time_data = execution_time_data_csv_path %>%
  lapply(read.csv) %>%
  bind_rows

execution_time_data = execution_time_data[,c(
  "browser",
  "language",
  "benchmark",
  "execution_time"
)]

execution_time_data

#set browser, language, and benchmark as factor
factor_cols = c(
  "browser",
  "language",
  "benchmark"
)

execution_time_data[factor_cols] = lapply(execution_time_data[factor_cols], factor)

#BY_LANGUAGE - RQ1
execution_time_by_language = execution_time_data %>%
  group_by(language)

execution_time_by_language_summary = execution_time_by_language %>%
  summarize(
    count = n(),
    Minimum =round( min(execution_time),3),
    Q1 =round( quantile(execution_time, 0.25),3),
    Median =round( median(execution_time),3),
    Mean =round( mean(execution_time),3),
    SD =round( sd(execution_time),3),
    Q3 =round( quantile(execution_time, 0.75),3),
    Maximum =round(max(execution_time),3)
  )

execution_time_by_language_summary

write.csv(
  execution_time_by_language_summary,
  "./execution_time_results/language/execution_time_by_language_summary.csv",
  row.names = FALSE
)


# quartiles <- quantile(execution_time_data$execution_time, probs=c(.25, .75), na.rm = FALSE)
# IQR <- IQR(execution_time_data$execution_time)
# 
# Lower <- quartiles[1] - 1.5*IQR
# Upper <- quartiles[2] + 1.5*IQR
# 
# Lower
# Upper
# 
# data_no_outlier <- subset(execution_time_data, execution_time_data$execution_time >= Lower & execution_time_data$execution_time <= Upper)
# data_no_outlier
# 
# # box_plot_language_vs_execution_time_data_per_benchmark
# boxplot_language_execution_time = ggplot(data_no_outlier, aes(x=benchmark, y=execution_time, fill=language)) +
#   theme(axis.title = element_text(size = 10),
#         plot.title = element_text(size = 14),
#         legend.position = "bottom",
#   ) +
#   geom_boxplot(outlier.size=.4, outlier.shape = NA) +
#   labs(title="Execution Time (ms) for benchmark function per language", x="Benchmark Functions", y="Execution Time (ms)") +
#   stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
#                geom = "point", shape = 18, size = 2,
#                show.legend = FALSE, aes(fill=factor(language)))
# 
# boxplot_language_execution_time

#box_plot_language_vs_execution_time_data_per_benchmark
boxplot_language_execution_time = ggplot(execution_time_data, aes(x=benchmark, y=execution_time, fill=language)) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
  ) +
  geom_boxplot(outlier.size=.4,) +
  labs(title="Execution Time (ms) for benchmark function per language", x="Benchmark Functions", y="Execution Time (ms)") +
  stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE, aes(fill=factor(language)))

boxplot_language_execution_time

ggsave(
  file="./execution_time_results/language/boxplot_language_execution_time.png",
  plot=boxplot_language_execution_time,
)

check_normality_data <- function(func_data, saveHistogram = FALSE, saveQQPlot = FALSE, normalized=FALSE) {
  par(mfrow=c(1,3))
  
  normalized_text = if(normalized) '_normalized' else ''
  
  if (saveHistogram) {
    png(paste("./execution_time_results/histogram_execution_time",normalized_text,".png", sep = ""))
    func_data %>%
      hist(breaks=20,main="Execution Time Distribution")
    dev.off()
    func_data %>%
      hist(breaks=20,main="Execution Time Distribution")
  } else {
    func_data %>%
      hist(breaks=20,main="Execution Time Distribution")
  }
  
  if (saveQQPlot) {
    png(paste("./execution_time_results/qq_plot_execution_time",normalized_text,".png", sep = ""))
    car::qqPlot(func_data, main="QQ Plot of Execution Time", xlab="Normality Quantiles", ylab="Samples")
    dev.off()
    car::qqPlot(func_data, main="QQ Plot of Execution Time", xlab="Normality Quantiles", ylab="Samples")
  } else {
    car::qqPlot(func_data, main="QQ Plot of Execution Time", xlab="Normality Quantiles", ylab="Samples")
  }
  
  shapiro_execution_time_test = shapiro.test(func_data) 
  print(shapiro_execution_time_test)
  
  capture.output(shapiro_execution_time_test, file = paste("./execution_time_results/shapiro_execution_time",normalized_text,".txt", sep=""))
  par(mfrow=c(1,1))
}

#check_normality_of_whole_data
check_normality_data(execution_time_data$execution_time, saveHistogram = TRUE)

#best normalize whole data
bestNormExecutionTime = bestNormalize(execution_time_data$execution_time)
bestNormExecutionTime
execution_time_data$norm_execution_time = bestNormExecutionTime$x.t

#to confirm the data transformed in normally distributed
check_normality_data(execution_time_data$norm_execution_time, saveHistogram = TRUE, saveQQPlot = TRUE, normalized=TRUE)

#anova-analysis
execution_time_by_language <- norm_execution_time ~ language
res.aov_language_execution_time <- aov(execution_time_by_language, data = execution_time_data)
# Summary of the analysis
execution_time_sum_aov_language = summary(res.aov_language_execution_time)
execution_time_sum_aov_language
capture.output(execution_time_sum_aov_language, file = "./execution_time_results/language/anova_one_way_language_execution_time.txt")

#tukey test
tukeyHSD_execution_time_language = TukeyHSD(res.aov_language_execution_time)
tukeyHSD_execution_time_language
capture.output(tukeyHSD_execution_time_language, file = "./execution_time_results/language/tukeyHSD_execution_time_language.txt")

#effect size
eta_squared_execution_time_language = eta_squared(res.aov_language_execution_time)
eta_squared_execution_time_language
capture.output(eta_squared_execution_time_language, file = "./execution_time_results/eta_squared_execution_time_language.txt")

#BY_BROWSER - RQ2
execution_time_browser = execution_time_data %>%
  group_by(browser)

execution_time_browser_summary = execution_time_browser %>%
  summarize(
    count = n(),
    Minimum =round( min(execution_time),3),
    Q1 =round( quantile(execution_time, 0.25),3),
    Median =round( median(execution_time),3),
    Mean =round( mean(execution_time),3),
    SD =round( sd(execution_time),3),
    Q3 =round( quantile(execution_time, 0.75),3),
    Maximum =round(max(execution_time),3)
  )

execution_time_browser_summary

write.csv(
  execution_time_browser_summary,
  "./execution_time_results/browser/execution_time_browser_summary.csv",
  row.names = FALSE
)

#box_plot_browser_vs_execution_time_data_per_benchmark

boxplot_browser_execution_time = ggplot(execution_time_data, aes(x=benchmark, y=execution_time, fill=browser)) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
  ) +
  geom_boxplot(outlier.size=.4) +
  labs(title="Execution Time (ms) for benchmark function per browser", x="Benchmark Functions", y="Execution Time (ms)") +
  stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE, aes(fill=factor(browser)))

boxplot_browser_execution_time

ggsave(
  file="./execution_time_results/browser/boxplot_browser_execution_time.png",
  plot=boxplot_browser_execution_time,
)

#anova-analysis
execution_time_browser <- norm_execution_time ~ browser
res.aov_browser_execution_time <- aov(execution_time_browser, data = execution_time_data)
# Summary of the analysis
execution_time_sum_aov_browser = summary(res.aov_browser_execution_time)
execution_time_sum_aov_browser
capture.output(execution_time_sum_aov_browser, file = "./execution_time_results/browser/anova_one_way_browser_execution_time.txt")


#t_test_for_two_variables
res.t_test_browser_execution_time = t.test(execution_time_browser, data = execution_time_data, alternative = "two.sided", var.equal = FALSE)
res.t_test_browser_execution_time
capture.output(res.t_test_browser_execution_time, file = "./execution_time_results/browser/t_test_browser_execution_time.txt")

#two-way-anova
res.aov_two_way_execution_time <- aov(norm_execution_time ~ language * browser, data = execution_time_data)
execution_time_two_way_test = summary(res.aov_two_way_execution_time)
execution_time_two_way_test
capture.output(execution_time_two_way_test, file = "./execution_time_results/anova_two_way_test.txt")

#language_browser statistics
execution_time_data$language_browser = paste(execution_time_data$language, " and ", execution_time_data$browser)

execution_time_by_language_browser = execution_time_data %>%
  group_by(language_browser)

execution_time_by_language_browser_summary = execution_time_by_language_browser %>%
  summarize(
    count = n(),
    Minimum =round( min(execution_time),3),
    Q1 =round( quantile(execution_time, 0.25),3),
    Median =round( median(execution_time),3),
    Mean =round( mean(execution_time),3),
    SD =round( sd(execution_time),3),
    Q3 =round( quantile(execution_time, 0.75),3),
    Maximum =round(max(execution_time),3)
  )

execution_time_by_language_browser_summary

write.csv(
  execution_time_by_language_browser_summary,
  "./execution_time_results/execution_time_by_language_browser_summary.csv",
  row.names = FALSE
)

density_curve_execution_time_language_browser = ggplot(execution_time_data, aes(x=execution_time, color=language_browser, fill=language_browser)) +
  geom_density(alpha=0.3) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14)
  ) +
  labs(title="Execution Time (ms) for language and browser", x="Execution Time (ms)", y="Density")

density_curve_execution_time_language_browser

ggsave(
  file="./execution_time_results/density_curve_execution_time_language_browser.png",
  plot=density_curve_execution_time_language_browser,
)

