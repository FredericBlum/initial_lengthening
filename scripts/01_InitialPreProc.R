library(tidyverse)

data <- read_csv('../data/new_predictors_fb.csv')
langs <- read_csv('utils/languages.csv') %>% 
  mutate(Language = Name,
         Language_ID = Glottocode) %>% 
  select(Language_ID, Language)

phones <- unique(data$ph) %>% as_tibble() %>%  
  filter(
    !grepl("(a|e|i|o|u|y|V|Q|E|I|O|A|U)", value),
    !grepl("(@|&|1|2|3|6|7|8|9|\\{|\\})", value),
    !(value %in% c("M","M:")),
    value != "****"
)

count_wds <- data %>% group_by(Language_ID) %>% 
  summarise(TypesInLang = n_distinct(wd_ID))

data <- data[sample(1:nrow(data)), ] %>% 
  filter(ph %in% phones$value) %>% 
  mutate(Glottocode = Language_ID) %>% 
  mutate(WordFormFreq = WordCount/PhonemesWord)

filtered <- data %>% left_join(langs) %>% 
  left_join(count_wds) %>% 
  mutate(
    logWordFormFreq = log(WordFormFreq/TypesInLang),
    logSpeechRate = log(SpeechRate)
    )

z_data <- data.frame()
for(lang in unique(filtered$Language)) {
  lang_data <- filtered %>% filter(Language == lang) %>% 
    mutate(
      z_logSpeechRate = round((logSpeechRate - mean(logSpeechRate)) / sd(logSpeechRate), 3),
           z_logWordFormFreq = round((logWordFormFreq - mean(logWordFormFreq)) / sd(logWordFormFreq), 3),
           z_logPhonWord = round((logPhonWord - mean(logPhonWord)) / sd(logPhonWord), 3),
           )

  z_data = rbind(z_data, lang_data)
}  

write_csv(z_data, '../data/consonant_data.csv')
