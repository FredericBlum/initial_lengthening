###############################################################################
# This code is only slightly adapted from the following publication:
# Guzmán Naranjo, M., & Becker, L. (2021). Statistical bias control in typology.
# Linguistic Typology, 26(3), 605–670. https://doi.org/10.1515/lingty-2021-0002

# We thank the authors for sharing their code, making it possible to re-use
# this method easily for new studies, and thus actively helping other scholars.
###############################################################################
library(ape)
library(tidyverse)

## functions

## this is the main function for getting the whole family tree for a language
get_chain <- function(.id) {
  cid <- .id
  pid <- get_parent(cid)
  chain <- c(.id, pid)
  while(!is.na(pid)) {
    pid <- get_parent(cid)
    if(!is.na(pid))
      chain <- c(chain, pid)
    cid <- pid
  }
  chain <- chain[!is.na(chain)]
  return(chain)
}

get_names <- function(.idf, db=families)
  sapply(.idf, function(.id)
    db[db$id == .id, ]$name)
get_num_languages <- function(.idf, db=families)
  sapply(.idf, function(.id)
    db[db$id == .id, ]$child_language_count)
get_num_families <- function(.idf, db=families)
  sapply(.idf, function(.id)
    db[db$id == .id, ]$child_family_count)
get_parent <- function(.id, db=families){
  db[db$id == .id, ]$parent_id
}

build_phylos <- function(.lfd, .var, .micro_family=FALSE
                         , distance=FALSE) {
  .var <- enquo(.var)
  ## extract family chain
  chains <- sapply(.lfd$id,
                   function(x) {
                     print(x)
                     c(get_names(get_chain(x)), 'TOP__')
                   })
  ## get the family chains
  chain.1 <- chains %>% sapply(function(x)(paste(x, collapse=';')))
  if(.micro_family)
    chain.1 <- sapply(chain.1, function(x) str_remove(x, '^.*?;'))
  all.vals <- unique(unlist(strsplit(chain.1, ';')))
  all.vals <- all.vals[!is.na(all.vals)]
  ## build dataframes
  df.philo <- select(.lfd, !!.var) 
  for(col in all.vals){
    print(col)
    df.philo[,col] <- as.integer(str_detect(chain.1, col))
  }
  df.philo <- distinct(df.philo)
  df.philo_d <- dist.b(df.philo)
  if (distance) {
    df.philo_d
  } else {
    as.dist(1/df.philo_d) %>%
      hclust() %>%
      as.phylo()
  }
}

## distance function

dist.b <- function(X) {
  m <- as.matrix(as.data.frame(X)[,-1])
  rownames(m) <- as.data.frame(X)[,1]
  m <- cbind(1:nrow(m), m)
  apply(m, 1, function(r) {
    cat(r[1], '/', nrow(m), '- ', sep='')
    r[r==0] <- -1
    rowSums(t(t(m[,-1])==r[-1]))
  })
}


## build data from glottolog

## extract glottolog info
## https://cdstar.shh.mpg.de/bitstreams/EAEA0-D501-DBB8-65C4-0/glottolog_languoid.csv.zip
families <- read_csv('languoid.csv')

## build family dataset
## extract language data

lang_gloto_data <- families %>%
  select(id, family_id, parent_id, name)

fam_ids <- families %>% pull(family_id)
fam_gloto_data <- families %>%
  select(id, name)

## we do a double left join to have the data in two columns
lang_fam_gloto_data <-
  left_join(lang_gloto_data,
            fam_gloto_data,
            by=c('family_id'='id'),
            suffix=c('_language', '_macro_family')) %>%
  left_join(fam_gloto_data,
            by=c('parent_id'='id'),
            suffix=c('_macro_family', '_micro_family')) %>%
  rename(name_micro_family=name) %>%
  mutate(name_micro_family =
           case_when(is.na(name_micro_family) ~ name_language,
                     TRUE ~ name_micro_family),
         name_macro_family =
           case_when(is.na(name_macro_family) ~ name_language,
                     TRUE ~ name_macro_family)) %>% 
  rename(Family=name_macro_family)

# Run functions
langs <- read_csv('../doreco/cldf/languages.csv')

df_aff <- read_tsv('data.tsv')

lfd <- filter(lang_fam_gloto_data, id %in% langs$ID)
langs <- langs %>% left_join(lfd, by=join_by(ID==id)) %>%
  mutate(phylo=name_micro_family,
         Language=Name) 

aff_phylo <- build_phylos(lfd, name_micro_family, .micro_family=TRUE)

write_csv(langs, 'languages.csv')
write_rds(aff_phylo, 'df-phylo.rds')
