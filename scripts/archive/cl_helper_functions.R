library(tidyverse)

load_data <- function(csv_file, n = 0, outlier_msd = 0) {
  data <- read_delim(csv_file, delim = ",") %>% 
    select(
      duration, logDuration, ph, utt_initial, word_initial, Language, speaker,
      sound_class, z_logSpeechRate, z_logPhonWord, z_logWordFormFreq, final
    )
  
  total_number <- data %>% nrow()
  
  # filter minimum values
  duration_filtered <- data %>% filter(duration > n)
  number_dur <- data %>% filter(duration <= n) %>% nrow()
  cat(number_dur, "consonants have been excluded at", n, "ms and below.\n")

  nonfinal_data <- duration_filtered %>% filter(final != 1)

  z_data <- data.frame()
  for(spk in unique(filtered$speaker)) {
    spk_data <- filtered %>% filter(speaker == spk)
    
    # filter outlier at certain 3x Standard Deviation around the mean
    # Chose median() instead of mean() for MAD
    msd_data <- spk_data %>% 
      mutate(mean = mean(logDuration), sd = sd(logDuration),
             sd_threshold = mean+outlier_msd*sd) %>% 
      filter(logDuration < sd_threshold)
    
    threshold_msd <- msd_data %>% pull(sd_threshold) %>% min() %>% exp()
  
    msd_data <- msd_data %>% 
      mutate(
        duration = (duration * 1000)/1000,
        utt_initial = as.factor(utt_initial),
        word_initial = as.factor(word_initial)
      )
    z_data = rbind(z_data, spk_data)
  }  
  
  # return filtered data
  return(z_data)
}
