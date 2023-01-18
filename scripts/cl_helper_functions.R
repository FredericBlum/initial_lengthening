library(tidyverse)

load_data <- function(csv_file, n = 0, outlier_mad = 0) {
  data <- read_delim(csv_file, delim = ",")
  
  total_number <- data %>% nrow()
  
  # filter minimum values
  duration_filtered <- data %>% filter(duration > n)
  number_dur <- data %>% filter(duration <= n) %>% nrow()
  data <- duration_filtered
  
  nonfinal_data <- data %>% filter(final != 1)
  
  # filter outlier at certain Absolute Deviation around the mean
  mad_data <- nonfinal_data %>%
    mutate(median = median(logDuration), mad = mad(logDuration),
           threshold = median+outlier_mad*mad) %>% filter(logDuration < threshold)
  
  msd_data <- nonfinal_data %>% mutate(mean = mean(logDuration), sd = sd(logDuration),
                              sd_threshold = mean+outlier_mad*sd) %>% 
    filter(logDuration < sd_threshold)
  
  threshold_mad <- mad_data %>% pull(threshold) %>% min() %>% exp()
  threshold_msd <- msd_data %>% pull(sd_threshold) %>% min() %>% exp()
  
  number_mad <- nonfinal_data %>% filter(duration > threshold_mad) %>% nrow()
  number_msd <- nonfinal_data %>% filter(duration > threshold_msd) %>% nrow()
  
  cat("In total", total_number, "consonants are part of this dataset.\n",
      number_dur, "consonants have been excluded at", n, "ms and below.\n",
      "Thresholds have been determined using", outlier_mad, "times the median absolute deviation.\n",
      "All calculations have been made on the log-scale, but the numbers are reported on ms-scale for better understanding.\n",
      "The threshold for short consonants is", round(threshold_mad, 1), "ms\n",
      number_mad, "consonants have been excluded.\n",
      "If the standard deviation from the mean of", outlier_mad, "would have been chosen,", 
      number_msd, "consonants would have been excluded at a threshold of", round(threshold_msd, 1), "ms.")
  
  mad_data <- mad_data %>%
    mutate(
      duration = (duration * 1000)/1000,
      utt_initial = as.factor(utt_initial),
      word_initial = as.factor(word_initial)
    ) %>% 
    select(
      duration, ph, initial, Language, speaker, sound_class,
      z_logSpeechRate, z_logPhonWord, z_logWordFormFreq, fbc
    )
  # return filtered data
  return(mad_data)
}
