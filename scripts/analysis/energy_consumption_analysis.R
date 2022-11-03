library(tidyverse)
library(ggplot2)
library(svglite)
library(bestNormalize)

energy_data_csv_path = './energy_consumption_data.csv'
energy_data = energy_data_csv_path %>%
  lapply(read.csv) %>%
  bind_rows

energy_data = energy_data[,c(
  "browser",
  "language",
  "benchmark",
  "avg_energy"
)]

energy_data

#set browser, language, and benchmark as factor
factor_cols = c(
  "browser",
  "language",
  "benchmark"
)

energy_data[factor_cols] = lapply(energy_data[factor_cols], factor)

#BY_LANGUAGE - RQ1
energy_by_language = energy_data %>%
  group_by(language)

energy_by_language_summary = energy_by_language %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_energy),3),
    Q1 =round( quantile(avg_energy, 0.25),3),
    Median =round( median(avg_energy),3),
    Mean =round( mean(avg_energy),3),
    SD =round( sd(avg_energy),3),
    Q3 =round( quantile(avg_energy, 0.75),3),
    Maximum =round(max(avg_energy),3)
  )

energy_by_language_summary

write.csv(
  energy_by_language_summary,
  "./energy_results/language/energy_by_language_summary.csv",
  row.names = FALSE
)


# quartiles <- quantile(energy_data$avg_energy, probs=c(.25, .75), na.rm = FALSE)
# IQR <- IQR(energy_data$avg_energy)
# 
# Lower <- quartiles[1] - 1.5*IQR
# Upper <- quartiles[2] + 1.5*IQR 
# 
# Lower
# Upper
# 
# data_no_outlier <- subset(energy_data, energy_data$avg_energy >= Lower & energy_data$avg_energy <= Upper)
# data_no_outlier

#box_plot_language_vs_energy_data_per_benchmark
# boxplot_language_avg_energy = ggplot(data_no_outlier, aes(x=benchmark, y=avg_energy, fill=language)) +
#   theme(axis.title = element_text(size = 10),
#         plot.title = element_text(size = 14),
#         legend.position = "bottom",
#   ) +
#   geom_boxplot(outlier.size=.4, outlier.shape = NA) +
#   labs(title="Energy Consumption (Joule) for benchmark function per language", x="Benchmark Functions", y="Energy Consumption(Joule)") +
#   stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
#                geom = "point", shape = 18, size = 2,
#                show.legend = FALSE, aes(fill=factor(language)))
# 
# boxplot_language_avg_energy

#box_plot_language_vs_energy_data_per_benchmark
boxplot_language_avg_energy = ggplot(energy_data, aes(x=benchmark, y=avg_energy, fill=language)) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
  ) +
  geom_boxplot(outlier.size=.4,) +
  labs(title="Energy Consumption (Joule) for benchmark function per language", x="Benchmark Functions", y="Energy Consumption(Joule)") +
  stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE, aes(fill=factor(language)))

boxplot_language_avg_energy

ggsave(
  file="./energy_results/language/boxplot_language_avg_energy.png",
  plot=boxplot_language_avg_energy,
)

check_normality_data <- function(func_data, saveHistogram = FALSE, saveQQPlot = FALSE, normalized=FALSE) {
  par(mfrow=c(1,3))
  
  normalized_text = if(normalized) '_normalized' else ''
  
  if (saveHistogram) {
    png(paste("./energy_results/histogram_energy_consumption",normalized_text,".png", sep = ""))
    func_data %>%
      hist(breaks=20,main="Energy Consumption Distribution")
    dev.off()
    func_data %>%
      hist(breaks=20,main="Energy Consumption Distribution")
  } else {
    func_data %>%
      hist(breaks=20,main="Energy Consumption Distribution")
  }
  
  if (saveQQPlot) {
    png(paste("./energy_results/qq_plot_energy_consumption",normalized_text,".png", sep = ""))
    car::qqPlot(func_data, main="QQ Plot of Energy Consumption", xlab="Normality Quantiles", ylab="Samples")
    dev.off()
    car::qqPlot(func_data, main="QQ Plot of Energy Consumption", xlab="Normality Quantiles", ylab="Samples")
  } else {
    car::qqPlot(func_data, main="QQ Plot of Energy Consumption", xlab="Normality Quantiles", ylab="Samples")
  }
  
  shapiro_energy_test = shapiro.test(func_data) 
  print(shapiro_energy_test)
  
  capture.output(shapiro_energy_test, file = paste("./energy_results/shapiro_energy_consumption",normalized_text,".txt", sep=""))
  par(mfrow=c(1,1))
}

#check_normality_of_whole_data
check_normality_data(energy_data$avg_energy, saveHistogram = TRUE)

#best normalize whole data
bestNormAvgEnergy = bestNormalize(energy_data$avg_energy)
bestNormAvgEnergy
energy_data$norm_energy = bestNormAvgEnergy$x.t

#to confirm the data transformed in normally distributed
check_normality_data(energy_data$norm_energy, saveHistogram = TRUE, saveQQPlot = TRUE, normalized=TRUE)

#anova-analysis
energy_by_language <- norm_energy ~ language
res.aov_language_energy <- aov(energy_by_language, data = energy_data)
# Summary of the analysis
energy_sum_aov_language = summary(res.aov_language_energy)
energy_sum_aov_language
capture.output(energy_sum_aov_language, file = "./energy_results/language/anova_one_way_language_energy.txt")

# The output includes the columns F value and Pr(>F) corresponding to the p-value of the test.
# Interpret the result of one-way ANOVA tests - As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences between the groups highlighted with “*" in the model summary.

# tukeyHSD_language = TukeyHSD(res.aov_language_energy)
# tukeyHSD_language

#BY_BROWSER - RQ2
energy_by_browser = energy_data %>%
  group_by(browser)

energy_by_browser_summary = energy_by_browser %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_energy),3),
    Q1 =round( quantile(avg_energy, 0.25),3),
    Median =round( median(avg_energy),3),
    Mean =round( mean(avg_energy),3),
    SD =round( sd(avg_energy),3),
    Q3 =round( quantile(avg_energy, 0.75),3),
    Maximum =round(max(avg_energy),3)
  )

energy_by_browser_summary

write.csv(
  energy_by_browser_summary,
  "./energy_results/browser/energy_by_browser_summary.csv",
  row.names = FALSE
)

#box_plot_browser_vs_energy_data_per_benchmark

boxplot_browser_avg_energy = ggplot(energy_data, aes(x=benchmark, y=avg_energy, fill=browser)) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
  ) +
  geom_boxplot(outlier.size=.4) +
  labs(title="Energy Consumption (Joule) for benchmark function per browser", x="Benchmark Functions", y="Energy Consumption(Joule)") +
  stat_summary(fun = mean, color = "grey", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE, aes(fill=factor(browser)))

boxplot_browser_avg_energy

ggsave(
  file="./energy_results/browser/boxplot_browser_avg_energy.png",
  plot=boxplot_browser_avg_energy,
)

#anova-analysis
energy_by_browser <- norm_energy ~ browser

res.aov_browser_energy <- aov(energy_by_browser, data = energy_data)
# Summary of the analysis
energy_sum_aov_browser = summary(res.aov_browser_energy)
energy_sum_aov_browser
capture.output(energy_sum_aov_browser, file = "./energy_results/browser/anova_one_way_browser_energy.txt")

#t_test_for_two_variables
res.t_test_browser_energy = t.test(energy_by_browser, data = energy_data, alternative = "two.sided", var.equal = FALSE)
res.t_test_browser_energy
capture.output(res.t_test_browser_energy, file = "./energy_results/browser/t_test_browser_energy.txt")

#two-way-anova
res.aov_two_way_energy <- aov(norm_energy ~ language * browser, data = energy_data)
energy_two_way_test = summary(res.aov_two_way_energy)
energy_two_way_test
capture.output(energy_two_way_test, file = "./energy_results/anova_two_way_test.txt")

# tukeyHSD_two_way = TukeyHSD(res.aov_two_way_energy)
# tukeyHSD_two_way

energy_data$language_browser = paste(energy_data$language, " and ", energy_data$browser)

energy_by_language_browser = energy_data %>%
  group_by(language_browser)

energy_by_language_browser_summary = energy_by_language_browser %>%
  summarize(
    count = n(),
    Minimum =round( min(avg_energy),3),
    Q1 =round( quantile(avg_energy, 0.25),3),
    Median =round( median(avg_energy),3),
    Mean =round( mean(avg_energy),3),
    SD =round( sd(avg_energy),3),
    Q3 =round( quantile(avg_energy, 0.75),3),
    Maximum =round(max(avg_energy),3)
  )

energy_by_language_browser_summary

write.csv(
  energy_by_language_browser_summary,
  "./energy_results/energy_by_language_browser_summary.csv",
  row.names = FALSE
)

density_curve_energy_language_browser = ggplot(energy_data, aes(x=avg_energy, color=language_browser, fill=language_browser)) +
  geom_density(alpha=0.3) +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 14)
  ) +
  labs(title="Energy Consumption (Joule) for language and browser", x="Energy Consumption (Joule)", y="Density")

ggsave(
  file="./energy_results/density_curve_energy_language_browser.png",
  plot=density_curve_energy_language_browser,
)
