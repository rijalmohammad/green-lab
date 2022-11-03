library(tidyverse)
library(ggplot2)
library(svglite)
library(bestNormalize)

memcpu_data_csv_path = './memcpu_data.csv'
memcpu_data = memcpu_data_csv_path %>%
  lapply(read.csv) %>%
  bind_rows
memcpu_data

cpu_data = memcpu_data[,c(
  "browser",
  "language",
  "benchmark",
  "avg_cpu"
)]

cpu_data
cpu_data = na.omit(cpu_data)
cpu_data

#set browser, language, and benchmark as factor
factor_cols = c(
  "browser",
  "language",
  "benchmark"
)

cpu_data[factor_cols] = lapply(cpu_data[factor_cols], factor)

#BY_LANGUAGE - RQ1
cpu_by_language = cpu_data %>%
  group_by(language)

cpu_by_language_summary = cpu_by_language %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_cpu),3),
    Q1 =round( quantile(avg_cpu, 0.25),3),
    Median =round( median(avg_cpu),3),
    Mean =round( mean(avg_cpu),3),
    SD =round( sd(avg_cpu),3),
    Q3 =round( quantile(avg_cpu, 0.75),3),
    Maximum =round(max(avg_cpu),3)
  )

cpu_by_language_summary

write.csv(
  cpu_by_language_summary,
  "./cpu_results/language/cpu_by_language_summary.csv",
  row.names = FALSE
)


# quartiles <- quantile(cpu_data$avg_cpu, probs=c(.25, .75), na.rm = FALSE)
# IQR <- IQR(cpu_data$avg_cpu)
# 
# Lower <- quartiles[1] - 1.5*IQR
# Upper <- quartiles[2] + 1.5*IQR 
# 
# Lower
# Upper
# 
# data_no_outlier <- subset(cpu_data, cpu_data$avg_cpu >= Lower & cpu_data$avg_cpu <= Upper)
# data_no_outlier

#box_plot_language_vs_cpu_data_per_benchmark
# boxplot_language_avg_cpu = ggplot(data_no_outlier, aes(x=benchmark, y=avg_cpu, fill=language)) +
#   theme(axis.title = element_text(size = 10),
#         plot.title = element_text(size = 14),
#         legend.position = "bottom",
#   ) +
#   geom_boxplot(outlier.size=.4, outlier.shape = NA) +
#   labs(title="CPU Usage (%) for benchmark function per language", x="Benchmark Functions", y="CPU Usage (%)") +
#   stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
#                geom = "point", shape = 18, size = 2,
#                show.legend = FALSE, aes(fill=factor(language)))
# 
# boxplot_language_avg_cpu

#box_plot_language_vs_cpu_data_per_benchmark
boxplot_language_avg_cpu = ggplot(cpu_data, aes(x=benchmark, y=avg_cpu, fill=language)) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
  ) +
  geom_boxplot(outlier.size=.4,) +
  labs(title="CPU Usage (%) for benchmark function per language", x="Benchmark Functions", y="CPU Usage (%)") +
  stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE, aes(fill=factor(language)))

boxplot_language_avg_cpu

ggsave(
  file="./cpu_results/language/boxplot_language_avg_cpu.png",
  plot=boxplot_language_avg_cpu,
)

check_normality_data <- function(func_data, saveHistogram = FALSE, saveQQPlot = FALSE, normalized=FALSE) {
  par(mfrow=c(1,3))
  
  normalized_text = if(normalized) '_normalized' else ''
  
  if (saveHistogram) {
    png(paste("./cpu_results/histogram_cpu_consumption",normalized_text,".png", sep = ""))
    func_data %>%
      hist(breaks=20,main="CPU Usage Distribution")
    dev.off()
    func_data %>%
      hist(breaks=20,main="CPU Usage Distribution")
  } else {
    func_data %>%
      hist(breaks=20,main="CPU Usage Distribution")
  }
  
  if (saveQQPlot) {
    png(paste("./cpu_results/qq_plot_cpu_consumption",normalized_text,".png", sep = ""))
    car::qqPlot(func_data, main="QQ Plot of CPU Usage", xlab="Normality Quantiles", ylab="Samples")
    dev.off()
    car::qqPlot(func_data, main="QQ Plot of CPU Usage", xlab="Normality Quantiles", ylab="Samples")
  } else {
    car::qqPlot(func_data, main="QQ Plot of CPU Usage", xlab="Normality Quantiles", ylab="Samples")
  }
  
  shapiro_cpu_test = shapiro.test(func_data) 
  print(shapiro_cpu_test)
  
  capture.output(shapiro_cpu_test, file = paste("./cpu_results/shapiro_cpu_consumption",normalized_text,".txt", sep=""))
  par(mfrow=c(1,1))
}

#check_normality_of_whole_data
check_normality_data(cpu_data$avg_cpu, saveHistogram = TRUE)

#best normalize whole data
bestNormAvgcpu = bestNormalize(cpu_data$avg_cpu)
bestNormAvgcpu
cpu_data$norm_cpu = bestNormAvgcpu$x.t

#to confirm the data transformed in normally distributed
check_normality_data(cpu_data$norm_cpu, saveHistogram = TRUE, saveQQPlot = TRUE, normalized=TRUE)

#anova-analysis
cpu_by_language <- norm_cpu ~ language
res.aov_language_cpu <- aov(cpu_by_language, data = cpu_data)
# Summary of the analysis
cpu_sum_aov_language = summary(res.aov_language_cpu)
cpu_sum_aov_language
capture.output(cpu_sum_aov_language, file = "./cpu_results/language/anova_one_way_language_cpu.txt")

# The output includes the columns F value and Pr(>F) corresponding to the p-value of the test.
# Interpret the result of one-way ANOVA tests - As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences between the groups highlighted with “*" in the model summary.

# tukeyHSD_language = TukeyHSD(res.aov_language)
# tukeyHSD_language

#BY_BROWSER - RQ2
cpu_by_browser = cpu_data %>%
  group_by(browser)

cpu_by_browser_summary = cpu_by_browser %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_cpu),3),
    Q1 =round( quantile(avg_cpu, 0.25),3),
    Median =round( median(avg_cpu),3),
    Mean =round( mean(avg_cpu),3),
    SD =round( sd(avg_cpu),3),
    Q3 =round( quantile(avg_cpu, 0.75),3),
    Maximum =round(max(avg_cpu),3)
  )

cpu_by_browser_summary

write.csv(
  cpu_by_browser_summary,
  "./cpu_results/browser/cpu_by_browser_summary.csv",
  row.names = FALSE
)

#box_plot_browser_vs_cpu_data_per_benchmark

boxplot_browser_avg_cpu = ggplot(cpu_data, aes(x=benchmark, y=avg_cpu, fill=browser)) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
  ) +
  geom_boxplot(outlier.size=.4) +
  labs(title="CPU Usage (%) for benchmark function per browser", x="Benchmark Functions", y="CPU Usage (%)") +
  stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE, aes(fill=factor(browser)))

boxplot_browser_avg_cpu

ggsave(
  file="./cpu_results/browser/boxplot_browser_avg_cpu.png",
  plot=boxplot_browser_avg_cpu,
)

#anova-analysis
cpu_by_browser <- norm_cpu ~ browser
res.aov_browser_cpu <- aov(cpu_by_browser, data = cpu_data)
# Summary of the analysis
cpu_sum_aov_browser = summary(res.aov_browser_cpu)
cpu_sum_aov_browser
capture.output(cpu_sum_aov_browser, file = "./cpu_results/browser/anova_one_way_browser_cpu.txt")

#t_test_for_two_variables
res.t_test_browser_avg_cpu = t.test(cpu_by_browser, data = cpu_data, alternative = "two.sided", var.equal = FALSE)
res.t_test_browser_avg_cpu
capture.output(res.t_test_browser_avg_cpu, file = "./cpu_results/browser/t_test_browser_cpu.txt")


#two-way-anova
res.aov_two_way_cpu <- aov(norm_cpu ~ language * browser, data = cpu_data)
cpu_two_way_test = summary(res.aov_two_way_cpu)
cpu_two_way_test
capture.output(cpu_two_way_test, file = "./cpu_results/anova_two_way_test.txt")

#language_browser statistics
cpu_data$language_browser = paste(cpu_data$language, " and ", cpu_data$browser)

cpu_by_language_browser = cpu_data %>%
  group_by(language_browser)

cpu_by_language_browser_summary = cpu_by_language_browser %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_cpu),3),
    Q1 =round( quantile(avg_cpu, 0.25),3),
    Median =round( median(avg_cpu),3),
    Mean =round( mean(avg_cpu),3),
    SD =round( sd(avg_cpu),3),
    Q3 =round( quantile(avg_cpu, 0.75),3),
    Maximum =round(max(avg_cpu),3)
  )

cpu_by_language_browser_summary

write.csv(
  cpu_by_language_browser_summary,
  "./cpu_results/cpu_by_language_browser_summary.csv",
  row.names = FALSE
)

density_curve_cpu_language_browser = ggplot(cpu_data, aes(x=avg_cpu, color=language_browser, fill=language_browser)) +
  geom_density(alpha=0.3) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14)
  ) +
  labs(title="CPU Usage for language and browser", x="CPU Usage (%)", y="Density")

density_curve_cpu_language_browser

ggsave(
  file="./cpu_results/density_curve_cpu_language_browser.png",
  plot=density_curve_cpu_language_browser,
)
