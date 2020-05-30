library(boot)
library(ggplot2)

# bootstrap compatible mean function
sample_mean <- function(data, index) {
  return(mean(data[index]))
}


# normal example
set.seed(314)
normal_data = rnorm(50, 4, 1.5)
normal_data_df = data.frame(samples = normal_data)

set.seed(5359)
normal_boot = boot(normal_data, sample_mean, R = 1000)
normal_boot

normal_boot_results = data.frame(mean_estimates = normal_boot$t)

ggplot(normal_data_df) + geom_histogram(aes(x = samples), binwidth = 0.4, color = "red") + 
  ylab("Count") + xlab("Data Observation") + theme_bw() + 
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

ggplot(normal_boot_results) + geom_histogram(aes(x = mean_estimates), binwidth = 0.025, color = "blue") + 
  ylab("Count") + xlab("Mean Estimate") + theme_bw() + 
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))
