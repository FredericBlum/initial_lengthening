library(tidyverse)

data <- read_csv('../data/new_predictors_fb.csv')
langs <- read_csv('utils/languages.csv') %>% 
  mutate(Language = Name,
         Language_ID = Glottocode) %>% 
  select(Language_ID, Language)

vowels <- c("i", "ei:", "o~", "1", "e:", "A~", "E:", "A", "u:", "{~", "{", "{:", "O:", "i:", "I:", "E", "2:", "u", "O", "7", "9", "6", "M", "i~", "o:", "Ai", "ei", "a", "I", "o", "o}", "y", "o}:", "@:", "}", "a_?\\", "U", "U:", "6:", "ai", "EI", "ia", "e:~", "OU", "o:~", "E:~", "O:~", "}:", "a:", "a:~", "@", "e", "Ai:", "ua", "@`", "ui", "M:", "1:", "u_?\\", "@e", "@:e", "oi", "aU", "Q", "a~", "e~", "u~", "}:~", "e_?\\", "o_?\\", "a~_?\\", "o~_?\\", "eI", "@U", "I@", "aI", "V", "A:", "3:", "3", "OI", "e@", "U@", "uo", "2", "y2", "@~", "ie", "1a", "y:", "e_r", "o_r", "I~", "O~", "E~", "9~", "i:~", "@:~", "u:~", "r\\=`", "m=", "z=", "v=", "v=~", "N=", "n=", "l=", "r=")
voiced_Cs <- c("M\\", "B_o", "l", "r", "n_j", "w~", "m_w", "l`", "n", "m:", "r_j", "4", "J", "5", "l_j", "j", "N:", "m_j", "w", "l:", "m", "j:", "N", "n_jn", "J_+", "w_j", "n:", "r\\`", "mm", "nn", "ll", "rr", "ww", "jj", "r_w", "l_w", "r\\", "n`", "w:", "Nm", "J:", "H", "r:", "P", "n_t", "l_t", "L", "N_w", "n_w", "j~", "r`", "m_p", "n_p", "z_j", "G", "z\\", "z`", "B", "z", "R", "R_j", "B_j", "v_j", "j\\", "Z", "v", "r\\_r", "r\\:_r", "?\\", "D", "Z_w", "R_w",  "z_w", "d", "dz\\", "b_w", "dZ_j", "g", "J\\", "dz`", "dz", "b_j", "b", "d_<", "b_<", "bb", "dd", "d`d`", "gg", "g_wg_w", "J\\J\\", "d_j", "d_j_<", "g_j", "g_w", "d`", "d:", "dZ", "b:", "d:_j", "J\\:", "g:", "g:_w", "gb", "b_j_<", "dZ_w", "d_d", "d_w_<", "d:`", "d_w","", "")
voiceless_Cs <- c("f_j", "f", "l_0", "r_0", "n_0", "J_0", "m_0", "N_0", "h", "K", "X", "S","X:", "x:", "C", "S_j", "X_>", "x", "s", "s:", "s\\", "S:", "f_w", "T", "s_j", "s`", "X:_w", "X\\", "p\\", "X_w", "s\\:", "SS", "ss", "ff", "s_h", "S_h", "f_h", "s\\_h", "h\\", "f:", "S_j_h", "s_>", "h~", "s_w", "S_>", "S_w", "s_h_w", "p\\:", "T:",
                  "t_>", "p", "tS", "tS_w", "q", "ts_h", "t`", "q_h", "tS_j", "c", "?", "t_h", "k_h", "t`_h", "k", "tS_>", "k_j", "ts`_h", "p_h", "p_>", "t", "t:", "ts", ">\\", "ts\\", "ts_>", "ts`", "p_j", "k_w", "?_j", "tS_h", "k:", "tt", "t`t`", "kk", "k:_w", "k_h_w", "t_w", "t_w_h", "c_h", "t_j_h", "p:", "cx", "ts\\:", "kx_>", "k_w_>", "k_j_h", "tts", "ks", "tt_j", "p_j_h",  "t_j", "ttS", "q:", "q_>", "ts\\_h", "k_>", "q_w", "q_w_>", "t:S", "kp", "t:s", "tK_>", "tS_w_>", "t:S_w", "t_d", "p_w", "t:`",  "t:_j", "t_d_w", "c:",  "O\\", "|\\", "!\\", "|\\|\\", "=\\", "O\\_v", "|\\_v", "!\\_v", "|\\|\\_v", "=\\_v", "O\\~", "|\\~", "!\\~", "|\\|\\~", "=\\~", "O\\_h", "|\\_h", "!\\_h", "|\\|\\_h", "=\\_h", "O\\~_h", "|\\~_h", "!\\~_h", "|\\|\\~_h", "=\\~_h", "O\\_>", "|\\_>", "!\\_>", "|\\|\\_>", "=\\_>", "O\\X", "|\\X", "!\\X", "|\\|\\X", "=\\X", "O\\q", "|\\q", "!\\q", "|\\|\\q", "=\\q", "O\\q_h", "|\\q_h", "!\\q_h", "|\\|\\q_h", "=\\q_h", "O\\qX_>", "|\\qX_>", "!\\qX_>", "|\\|\\qX_>", "=\\qX_>")
geminates <- c("?:", "??", "44", "44_cl", "44_rl", "4_j4_j", "4_j4_j_cl", "4_j4_j_rl", "b:", "bb", "b_jb_j", "b_jb_j_cl", "b_jb_j_rl", "c:", "C:", "cc", "CC", "d:", "D:", "dd", "d`d`", "DD", "dd_j", "dd_j_cl", "dd_j_rl", "ddz", "ddZ", "d:_j", "d:_j_cl", "d_jd_j", "d_jd_j_cl", "d_jd_j_rl", "d:_j_rl", "d:z", "d:Z", "f:", "F:", "ff", "f:f", "FF", "g:", "gg", "gg_j", "gg_j_cl", "gg_j_rl", "gg_w", "g:_j", "g:_j_cl", "g_jg_j", "g_jg_j_cl", "g_jg_j_rl", "g:_j_rl", "g:_w", "g_wg_w", "h:", "hh", "h:h", "j:", "J:", "J\\:", "jj", "j:j", "JJ", "J\\J\\", "k:", "k:_h", "k:_j", "k:_j_cl", "k_jk_j", "k_jk_j_cl", "k_jk_j_rl", "k:_j_rl", "kk", "k:k", "kk_j", "kk_j_cl", "kk_j_rl", "k:_w", "l:", "L:", "ll", "l:l", "LL", "m:", "m:_j", "m:_j_cl", "m_jm_j", "m_jm_j_cl", "m_jm_j_rl", "m:_j_rl", "mm", "m:m", "mm_j", "mm_j_cl", "mm_j_rl", "n:", "N:", "N\\:", "nn", "n:n", "NN", "N\\N\\", "p:", "p\\:", "p\\:_cl", "p:_j", "p\\:_j", "p:_j_cl", "p\\:_j_cl", "p_jp_j", "p\\_jp\\_j", "p_jp_j_cl", "p\\_jp\\_j_cl", "p_jp_j_rl", "p\\_jp\\_j_rl", "p:_j_rl", "p\\:_j_rl", "pp", "p:p", "p\\p\\", "p\\p\\_cl", "p\\p\\_j", "pp_j", "p\\p\\_j_cl", "pp_j_cl", "p\\p\\_j_rl", "pp_j_rl", "p\\p\\_rl", "p\\:_rl", "q:", "qq", "r:", "R:", "rr", "r:r", "r\\:_r", "RR", "s:", "s\\:", "S:", "ss", "s:s", "s\\s\\", "SS", "S:S", "t:", "t:`", "T:", "t_dt_d", "t:_j", "t:_j_cl", "t:_j_rl", "t_jt_j", "t_jt_j_cl", "t_jt_j_rl", "t:s", "ts\\:", "t:S", "tsts", "ts\\ts\\", "tStS", "tt", "t`t`", "t:t", "tt`", "TT", "tt_j", "tt_j_cl", "tt_j_rl", "tts", "ttS", "v:", "vv", "v:v", "w:", "ww", "x:", "X:", "x:_j", "x_jx_j", "X:_w", "X_wX_w", "xx", "XX", "xx_j", "XX_w", "z:", "Z:", "zz", "ZZ", "d:`", "t:S_w")
sonorants <- c("M\\", "l", "l_0", "r", "r_0", "n_j", "w~", "m_w", "l`", "n", "m:", "r_j", "4", "J", "5", "l_j", "j", "N:", "m_j", "w", "l:", "m", "j:", "N", "n_jn", "J_+", "w_j", "n:", "r\\`", "mm", "nn", "ll", "rr", "ww", "jj", "r_w", "l_w", "r\\", "n_0", "J_0", "m_0", "n`", "w:", "Nm", "J:", "N_0", "H", "r:", "P", "n_t", "l_t", "L", "N_w",  "n_w", "j~", "r`", "m_p", "n_p", "")
stops <- c("t_>", "p", "d", "dz\\", "tS", "tS_w", "q", "ts_h", "t`", "q_h", "tS_j", "c", "?", "t_h", "k_h", "t`_h", "b_w", "k", "dZ_j", "tS_>", "g", "k_j", "J\\", "dz`", "ts`_h", "p_h", "p_>", "t", "t:", "ts", "dz", ">\\", "ts\\", "b_j", "ts_>", "ts`", "p_j", "b", "d_<", "b_<", "k_w", "?_j", "tS_h", "k:", "bb", "dd", "tt", "d`d`", "t`t`", "gg", "g_wg_w", "kk", "k:_w", "k_h_w", "t_w", "t_w_h", "c_h", "J\\J\\", "d_j", "d_j_<", "t_j_h", "p:", "cx", "ts\\:", "kx_>", "k_w_>", "k_j_h", "tts", "ks", "tt_j", "p_j_h", "g_j", "t_j", "ttS", "g_w", "q:", "d`", "q_>", "d:", "dZ", "ts\\_h", "k_>", "q_w", "q_w_>", "t:S", "b:", "d:_j", "kp", "J\\:", "g:", "g:_w", "t:s", "gb", "tK_>", "b_j_<", "dZ_w", "tS_w_>", "t:S_w", "d_d", "t_d", "d_w_<", "p_w", "t:`", "d:`", "t:_j", "t_d_w", "d_w", "c:", "O\\", "|\\", "!\\", "|\\|\\", "=\\", "O\\_v", "|\\_v", "!\\_v", "|\\|\\_v", "=\\_v", "O\\~", "|\\~", "!\\~", "|\\|\\~", "=\\~", "O\\_h", "|\\_h", "!\\_h", "|\\|\\_h", "=\\_h", "O\\~_h", "|\\~_h", "!\\~_h", "|\\|\\~_h", "=\\~_h", "O\\_>", "|\\_>", "!\\_>", "|\\|\\_>", "=\\_>", "O\\X", "|\\X", "!\\X", "|\\|\\X", "=\\X", "O\\q", "|\\q", "!\\q", "|\\|\\q", "=\\q", "O\\q_h", "|\\q_h", "!\\q_h", "|\\|\\q_h", "=\\q_h", "O\\qX_>", "|\\qX_>", "!\\qX_>", "|\\|\\qX_>", "=\\qX_>", "")
affricates <- c("dz\\", "tS", "ts_h", "tS_j", "dZ_j", "tS_>", "dz`", "ts`_h", "ts", "dz", "ts\\", "ts_>", "ts`", "tS_h", "cx", "ts\\:", "kx_>", "tts", "ks", "ttS", "dZ", "ts\\_h", "t:S", "O\\qX_>", "|\\qX_>", "!\\qX_>", "|\\|\\qX_>", "=\\qX_>", "tS_w", "tK_>", "tS_w_>", "t:S_w", "dZ_w", "")
fricatives <- c("B_o", "_j", "z_j", "G", "z\\", "h", "z`", "K", "X", "B", "z", "S", "R", "X:", "x:", "C", "S_j", "R_j", "X_>", "x", "s", "s:", "s\\", "S:", "f_w", "T", "s_j", "s`", "B_j", "v_j", "j\\", "Z", "v", "r\\_r", "r\\:_r", "X:_w", "X\\", "?\\", "p\\", "X_w", "s\\:", "SS", "ss", "ff", "s_h", "S_h", "f_h", "s\\_h", "h\\", "D", "f:", "S_j_h", "s_>", "h~", "Z_w", "s_w", "S_>", "S_w", "T:", "R_w", "s_h_w", "p\\:", "z_w", "f_j", "f", "")
sibilants  <-  c("z_j", "z\\", "z`", "z", "S", "S_j", "s", "s:", "s\\", "S:", "s_j", "s`", "Z", "s\\:", "SS", "ss", "s_h", "S_h", "s\\_h", "S_j_h", "s_>", "z_w", "")

data <- data %>% as_tibble() %>%  
  mutate(
  voicing = ifelse(
    ph %in% voiced_Cs, "voiced", ifelse(
      ph %in% voiceless_Cs, "unvoiced", "unclear"
    )
  ),
  sound_class = ifelse(
    ph %in% sonorants, "sonorant", ifelse(
      ph %in% stops, "stops", ifelse(
        ph %in% fricatives, "fricatives", ifelse(
          ph %in% geminates, "geminate", ifelse(
            ph %in% vowels, "vowel", "unclear"
          )
        )
      )
    )
  ),
  fbc = ifelse(
    following_sound %in% vowels, 0, ifelse(
      following_sound == "final", 0, 1
    )
  )
) 

data <- data %>% 
  filter(
    !(sound_class %in% c("vowel", "geminate")),
    ph != "****",
    !(grepl(":", ph))
    ) %>% 
  mutate(
    initial = ifelse(utt_initial==1, "utt_initial", ifelse(
      word_initial==1, "word_initial", "non_initial"
    )),
    sound_class = paste(sound_class, voicing)
  )

count_wds <- data %>% group_by(Language_ID) %>% 
  summarise(TypesInLang = n_distinct(wd_ID))

data <- data[sample(1:nrow(data)), ] %>% 
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
