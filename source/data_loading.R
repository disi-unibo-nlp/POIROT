##########################################
## Data loading
##########################################

getFilePath <- function(lang, fileName) {
  paste0("data/", lang, "/", fileName)
}

loadData <- function(lang) {
  
  if (!(lang %in% c("it", "en"))) {
    stop("Not supported language")
  }
  
  
  # Slang words
  # ---------------
  
  slang_words <- readRDS(getFilePath(lang, "slang_words/slang_words.rds"))
  
  
  # Stopwords
  # ---------------
  
  stopwords <- readRDS(getFilePath(lang, "stopwords.rds"))
  
  
  # Concepts
  # ---------------
  
  poem_concept = readRDS(getFilePath(lang, "concepts/poem_concept.rds"))
  hellerdor_concept = readRDS(getFilePath(lang, "concepts/hellerdor_concept.rds"))
  all_concept = readRDS(getFilePath(lang, "concepts/all_concept.rds"))
  
  
  # Documents
  # ---------------
  
  # Documents
  # Columns needed: facebook_id, text, creation_year
  documents <- # LOAD DATAFRAME HERE
  all_docs_min_date <- min(documents$creation_date)
  all_docs_max_date <- max(documents$creation_date)
  
  # Statistics about characters
  summary(stringi::stri_length(documents$original_text))
  
  # Statistics about words
  summary(stringi::stri_count(documents$original_text, regex="\\w+"))
  
  
  # Entities
  # ---------------
  
  # Reconciled entities detected by NER
  reconciled_entities <- readRDS(getFilePath(lang, "reconciled_entities.rds"))
  
  
  # Opinion Lexicon
  # ---------------
  
  # Hu & Liu
  hu_liu_pos <- readRDS(getFilePath(lang, "opinion_mining/hu_liu_pos.rds"))
  hu_liu_neg <- readRDS(getFilePath(lang, "opinion_mining/hu_liu_neg.rds"))
  
  # Negative entities by class
  neg_entities <- as.vector(reconciled_entities$entity_id[plyr::laply(
    reconciled_entities$labels,
    function(entity_labels) {any(c("Disease", "Symptom") %in% entity_labels)})])
  
  # Integrative opinion words for Achalasia
  achalasia_pos <- readRDS(getFilePath(lang, "opinion_mining/achalasia_pos.rds"))
  achalasia_neg <- readRDS(getFilePath(lang, "opinion_mining/achalasia_neg.rds"))
  
  # Opinion related to normalized emotes
  emote_pos <- readRDS(getFilePath(lang, "opinion_mining/emote_pos.rds"))
  emote_neg <- readRDS(getFilePath(lang, "opinion_mining/emote_neg.rds"))
  
  # Global lists
  pos_words <- unique(c(hu_liu_pos, achalasia_pos, emote_pos))
  neg_words <- unique(c(hu_liu_neg, neg_entities, achalasia_neg, emote_neg))
  
  
  # Term-document matrix
  # ---------------
  
  # Term-document matrix with all standard and entity terms (i.e., tags after NER reconciliation)
  tdmc_ner_unlemmatized <- readRDS(getFilePath(lang, "tdmc_ner_unlemmatized.rds"))
  tdmc_ner_lemmatized <- readRDS(getFilePath(lang, "tdmc_ner_lemmatized.rds"))
  
  
  # Gold standards
  # ---------------
  
  kc_no_ner_pos <- readRDS(getFilePath("it", "gold_standards/kc_pos.rds"))
  kc_no_ner_neg <- readRDS(getFilePath("it", "gold_standards/kc_neg.rds"))
  
  
  # Global Adjacencies
  # ---------------
  
  global_adjacencies <- readRDS(getFilePath("it", "global_adjacencies.rds"))
  
  
  list(
    slang_words = slang_words,
    stopwords = stopwords,
    concepts = list(poem = poem_concept, hellerdor = hellerdor_concept, all = all_concept),
    opinion_lexicon = list(pos_words = pos_words, neg_words = neg_words),
    documents = documents,
    reconciled_entities = reconciled_entities,
    tdmc = tdmc_ner_lemmatized,
    gold_standards = list(kc_no_ner_pos = kc_no_ner_pos, kc_no_ner_neg = kc_no_ner_neg),
    global_adjacencies = global_adjacencies
  )
  
}