library(tidyverse)
library(ggplot2)
library(svglite)
library(bestNormalize)

memcpu_data_csv_path = './memcpu_data.csv'
memcpu_data = memcpu_data_csv_path %>%
  lapply(read.csv) %>%
  bind_rows
memcpu_data

memory_data = memcpu_data[,c(
  "browser",
  "language",
  "benchmark",
  "avg_memory"
)]

#set browser, language, and benchmark as factor
factor_cols = c(
  "browser",
  "language",
  "benchmark"
)

memory_data[factor_cols] = lapply(memory_data[factor_cols], factor)

#BY_LANGUAGE - RQ1
memory_by_language = memory_data %>%
  group_by(language)

memory_by_language_summary = memory_by_language %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_memory),3),
    Q1 =round( quantile(avg_memory, 0.25),3),
    Median =round( median(avg_memory),3),
    Mean =round( mean(avg_memory),3),
    SD =round( sd(avg_memory),3),
    Q3 =round( quantile(avg_memory, 0.75),3),
    Maximum =round(max(avg_memory),3)
  )

memory_by_language_summary

write.csv(
  memory_by_language_summary,
  "./memory_results/language/memory_by_language_summary.csv",
  row.names = FALSE
)


# quartiles <- quantile(memory_data$avg_memory, probs=c(.25, .75), na.rm = FALSE)
# IQR <- IQR(memory_data$avg_memory)
# 
# Lower <- quartiles[1] - 1.5*IQR
# Upper <- quartiles[2] + 1.5*IQR 
# 
# Lower
# Upper
# 
# data_no_outlier <- subset(memory_data, memory_data$avg_memory >= Lower & memory_data$avg_memory <= Upper)
# data_no_outlier

#box_plot_language_vs_memory_data_per_benchmark
# boxplot_language_avg_memory = ggplot(data_no_outlier, aes(x=benchmark, y=avg_memory, fill=language)) +
#   theme(axis.title = element_text(size = 10),
#         plot.title = element_text(size = 14),
#         legend.position = "bottom",
#   ) +
#   geom_boxplot(outlier.size=.4, outlier.shape = NA) +
#   labs(title="Memory Usage (Bytes) for benchmark function per language", x="Benchmark Functions", y="Memory Usage (Bytes)") +
#   stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
#                geom = "point", shape = 18, size = 2,
#                show.legend = FALSE, aes(fill=factor(language)))
# 
# boxplot_language_avg_memory

#box_plot_language_vs_memory_data_per_benchmark
boxplot_language_avg_memory = ggplot(memory_data, aes(x=benchmark, y=avg_memory, fill=language)) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
  ) +
  geom_boxplot(outlier.size=.4,) +
  labs(title="Memory Usage (Bytes) for benchmark function per language", x="Benchmark Functions", y="Memory Usage (Bytes)") +
  stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE, aes(col=factor(language)))

boxplot_language_avg_memory

ggsave(
  file="./memory_results/language/boxplot_language_avg_memory.png",
  plot=boxplot_language_avg_memory,
)

check_normality_data <- function(func_data, saveHistogram = FALSE, saveQQPlot = FALSE, normalized=FALSE) {
  par(mfrow=c(1,3))
  
  normalized_text = if(normalized) '_normalized' else ''
  
  if (saveHistogram) {
    png(paste("./memory_results/histogram_memory_consumption",normalized_text,".png", sep = ""))
    func_data %>%
      hist(breaks=20,main="Memory Usage Distribution")
    dev.off()
    func_data %>%
      hist(breaks=20,main="Memory Usage Distribution")
  } else {
    func_data %>%
      hist(breaks=20,main="Memory Usage Distribution")
  }
  
  if (saveQQPlot) {
    png(paste("./memory_results/qq_plot_memory_consumption",normalized_text,".png", sep = ""))
    car::qqPlot(func_data, main="QQ Plot of Memory Usage", xlab="Normality Quantiles", ylab="Samples")
    dev.off()
    car::qqPlot(func_data, main="QQ Plot of Memory Usage", xlab="Normality Quantiles", ylab="Samples")
  } else {
    car::qqPlot(func_data, main="QQ Plot of Memory Usage", xlab="Normality Quantiles", ylab="Samples")
  }
  
  shapiro_memory_test = shapiro.test(func_data) 
  print(shapiro_memory_test)
  
  capture.output(shapiro_memory_test, file = paste("./memory_results/shapiro_memory_consumption",normalized_text,".txt", sep=""))
  par(mfrow=c(1,1))
}

#check_normality_of_whole_data
check_normality_data(memory_data$avg_memory, saveHistogram = TRUE)

#best normalize whole data
bestNormAvgMemory = bestNormalize(memory_data$avg_memory)
bestNormAvgMemory
memory_data$norm_memory = bestNormAvgMemory$x.t

#to confirm the data transformed in normally distributed
check_normality_data(memory_data$norm_memory, saveHistogram = TRUE, saveQQPlot = TRUE, normalized=TRUE)

#anova-analysis
memory_by_language <- norm_memory ~ language
res.aov_language_memory <- aov(memory_by_language, data = memory_data)
# Summary of the analysis
memory_sum_aov_language = summary(res.aov_language_memory)
memory_sum_aov_language
capture.output(memory_sum_aov_language, file = "./memory_results/language/anova_one_way_language_memory.txt")

# The output includes the columns F value and Pr(>F) corresponding to the p-value of the test.
# Interpret the result of one-way ANOVA tests - As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences between the groups highlighted with “*" in the model summary.

# tukeyHSD_language = TukeyHSD(res.aov_language)
# tukeyHSD_language

#BY_BROWSER - RQ2
memory_by_browser = memory_data %>%
  group_by(browser)

memory_by_browser_summary = memory_by_browser %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_memory),3),
    Q1 =round( quantile(avg_memory, 0.25),3),
    Median =round( median(avg_memory),3),
    Mean =round( mean(avg_memory),3),
    SD =round( sd(avg_memory),3),
    Q3 =round( quantile(avg_memory, 0.75),3),
    Maximum =round(max(avg_memory),3)
  )

memory_by_browser_summary

write.csv(
  memory_by_browser_summary,
  "./memory_results/browser/memory_by_browser_summary.csv",
  row.names = FALSE
)

#box_plot_browser_vs_memory_data_per_benchmark

boxplot_browser_avg_memory = ggplot(memory_data, aes(x=benchmark, y=avg_memory, fill=browser)) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
  ) +
  geom_boxplot(outlier.size=.4) +
  labs(title="Memory Usage (Bytes) for benchmark function per browser", x="Benchmark Functions", y="Memory Usage (Bytes)") +
  stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE, aes(fill=factor(browser)))

boxplot_browser_avg_memory

ggsave(
  file="./memory_results/browser/boxplot_browser_avg_memory.png",
  plot=boxplot_browser_avg_memory,
)

#anova-analysis
memory_by_browser <- norm_memory ~ browser
res.aov_browser_memory <- aov(memory_by_browser, data = memory_data)
# Summary of the analysis
memory_sum_aov_browser = summary(res.aov_browser_memory)
memory_sum_aov_browser
capture.output(memory_sum_aov_browser, file = "./memory_results/browser/anova_one_way_browser_memory.txt")

#t_test_for_two_variables
res.t_test_browser_avg_memory = t.test(memory_by_browser, data = memory_data, alternative = "two.sided", var.equal = FALSE)
res.t_test_browser_avg_memory
capture.output(res.t_test_browser_avg_memory, file = "./memory_results/browser/t_test_browser_memory.txt")


#two-way-anova
res.aov_two_way_memory <- aov(norm_memory ~ language * browser, data = memory_data)
memory_two_way_test = summary(res.aov_two_way_memory)
memory_two_way_test
capture.output(memory_two_way_test, file = "./memory_results/anova_two_way_test.txt")

#language_browser statistics
memory_data$language_browser = paste(memory_data$language, " and ", memory_data$browser)

memory_by_language_browser = memory_data %>%
  group_by(language_browser)

memory_by_language_browser_summary = memory_by_language_browser %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_memory),3),
    Q1 =round( quantile(avg_memory, 0.25),3),
    Median =round( median(avg_memory),3),
    Mean =round( mean(avg_memory),3),
    SD =round( sd(avg_memory),3),
    Q3 =round( quantile(avg_memory, 0.75),3),
    Maximum =round(max(avg_memory),3)
  )

memory_by_language_browser_summary

write.csv(
  memory_by_language_browser_summary,
  "./memory_results/memory_by_language_browser_summary.csv",
  row.names = FALSE
)

density_curve_memory_language_browser = ggplot(memory_data, aes(x=avg_memory, color=language_browser, fill=language_browser)) +
  geom_density(alpha=0.3) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14)
  ) +
  labs(title="Memory Usage for language and browser", x="Memory Usage (Bytes)", y="Density")

density_curve_memory_language_browser

ggsave(
  file="./memory_results/density_curve_memory_language_browser.png",
  plot=density_curve_memory_language_browser,
)
