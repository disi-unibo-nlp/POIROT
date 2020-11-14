##########################################
## Term-document matrix
##########################################


#' Build a term document matrix from textual documents
#'
#' @param documents The document dataframe with "doc_id" and "text" columns
#' @param stopwords The string array of stopwords to delete
#' @param lang The corpus language
#' @param lemmatizeYN True if you want to lemmatize the document for tdm construction, False otherwise
#' @return The term document matrix after sparse features removal, and the vocabulary
buildCompleteTdm <- function(documents, stopwords, lang, lemmatizeYN = FALSE) {
  
  # Build the corpus from the documents
  documents_corpus <- tm::Corpus(tm::DataframeSource(documents))
  
  # Apply transformations to all the documents in the corpus
  # STEP 1 - Lowercasing
  documents_corpus <- tm::tm_map(documents_corpus, tm::content_transformer(tolower))
  # STEP 2 - Punctuation replacement
  # To work around the default behavior of removePunctuation() that joines single words separated
  # by punctuation in case of blank space lacking between them, creates a custom function that
  # replaces than removes punctuation characters
  # Source: https://bit.ly/38cncT2
  replacePunctuationExceptEntityTags <- function(x) {
    # Replace all punctuation with a blank space, except for entity tags
    # The \\1 sintax refers to the first group (...) and means 'whatever was matched
    # replace it with that'. Having only entity tags pattern in a group, each time
    # an entity is detected \\1 will replace it by the same value (so it keeps them here)
    # The space inside the replacement string avoid terms union
    gsub(paste0("(", entity_tag_regex_pattern, ")|[[:punct:]]+"), "\\1 ", x)
  }
  documents_corpus <- tm::tm_map(documents_corpus, replacePunctuationExceptEntityTags)
  # STEP 3 - Numbers removal
  replaceNumberExceptEntityTags <- function(x) {
    gsub(paste0("(", entity_tag_regex_pattern, ")|[[:digit:]]+"), "\\1 ", x)
  }
  documents_corpus <- tm::tm_map(documents_corpus, replaceNumberExceptEntityTags)
  # STEP 4 - Extra whitespace removal
  documents_corpus <- tm::tm_map(documents_corpus, tm::stripWhitespace)
  # STEP 5 - Stopwords removal
  # Remove stopwords considering also spaces as word separator
  removeWordsBySpaceSep <- function(messages, words) {
    lapply(messages, function(x) {
      x <- unlist(strsplit(x, " "))
      x <- x[!x %in% words]
      paste(x, collapse = " ")
    })
  }
  documents_corpus <- tm::tm_map(documents_corpus, removeWordsBySpaceSep, stopwords)
  if (lemmatizeYN) {
    # Lemmatization
    lemmatizeCorpus <- function(messages, lang, entity_tag_regex_pattern = NULL) {
      lapply(messages, function(x) {
        x <- lemmatize(x, lang, entity_tag_regex_pattern)
      })
    }
    documents_corpus <- tm::tm_map(documents_corpus, lemmatizeCorpus, lang, entity_tag_regex_pattern)
    # Alternative lemmatization only for english text
    # corpus <- tm_map(corpus, textstem::lemmatize_strings)
  }
  
  # Extract a term-document matrix from the documents corpus
  tdmc <- tm::TermDocumentMatrix(documents_corpus)
  
  tdmc
  
}


#' Remove terms (features) below a certain document frequency threshold,
#' distinguishing between standard and entity (if present)
#'
#' @param tdmc The complete term-document matrix
#' @param entitiesYN True if entity terms are present, false otherwise
#' @param min_standard_term_freq The minimum value of a standard feature's document frequency expressed as percentage
#' (term frequencies divided by the total sum; i.e. 0.01 -> at least 1%)
#' @param min_entity_term_freq The minimum value of an entity feature's document frequency expressed as percentage
#' (term frequencies divided by the total sum; i.e. 0.001 -> at least 0.1%)
#' @return The term-document matrix after sparse features removal, and the vocabulary
removeSparseFeatures <- function(tdmc, entitiesYN = TRUE,
                                 min_standard_term_freq, min_entity_term_freq) {
  
  # STEP 1 - FROM TDM TO DFM QUANTEDA CONVERSION
  # ----------------
  
  # Create a document-feature matrix (quanteda) for the term-document matrix (tm)
  # A dfm has documents on the rows and features on the columns (instead of tdm)
  # This is helpful for quanteda analysis
  documents_dfm <- as.dfm(tdmc)
  
  # STEP 2 - STANDARD AND ENTITY TERMS SEPARATION
  # ----------------
  
  # Create a dfm with only standard terms as features
  standard_terms_dfm <- documents_dfm %>%
    dfm_select(
      pattern = entity_tag_regex_pattern,
      selection = "remove",
      valuetype = "regex")
  
  if (entitiesYN) {
    # Create a dfm with only entity terms as features
    entity_terms_dfm <- documents_dfm %>%
      dfm_select(
        pattern = entity_tag_regex_pattern,
        selection = "keep",
        valuetype = "regex")
  }
  
  # STEP 3 - SPARSE FEATURES REMOVAL
  # ----------------
  
  # Standard terms and entity terms have different thresholds
  # Some important types of entities (releavant for the analysis) extracted by NER have a low
  # frequency within the documents: normally the entity removal threshold is therefore less rigid
  # (entities are not eliminated even if they are infrequent)
  
  # Remove the standard terms that appear in less than a certain percentage of the documents
  # Typically to use LSA every word should appear in at least 2 documents
  standard_terms_dfm <- standard_terms_dfm %>%
    dfm_trim(min_docfreq = min_standard_term_freq, docfreq_type = "prop")
  
  if (entitiesYN) {
    # Remove the entity terms that appear in less than a certain percentage of the documents
    entity_terms_dfm <- entity_terms_dfm %>%
      dfm_trim(min_docfreq = min_entity_term_freq, docfreq_type = "prop")
  }
  
  # STEP 4 - FINAL DFM CONSTRUCTION
  # ----------------
  
  if (entitiesYN) {
    # Combine the two document-feature matrixes (same documents but concatenated features)
    # NOTE: duplicated features are not possible (a term related to an entity is always recognized as entity
    # thanks to the reconciliation phase)
    options(warn = -1)
    documents_dfm <- cbind(standard_terms_dfm, entity_terms_dfm)
    options(warn = 0)
  } else {
    documents_dfm <- standard_terms_dfm
  }
  
  # STEP 5 - FROM DFM TO TDM CONVERSION
  # ----------------
  
  # A DocumentFeatureMatrix cannot be easily translated into a TermDocumentMatrix (tm)
  # So it is converted in a DocumentTermMatrix (tm)
  dtms <- quanteda::convert(documents_dfm, to = "tm")
  
  # Convert tdms to standard R format
  tdm <- t(as.matrix(dtms))
  
  # VOCABULARY EXTRACTION
  # -------------------------------------------------
  
  # Save the array with the selected terms (extracts the vocabulary)
  terms <- rownames(tdm)
  
  if (entitiesYN) {
    
    # Extract the terms without their NER type tag
    terms_without_tag <- getEntityIdsFromTags(terms)
    entity_terms_without_tag <- getEntityIdsFromTags(colnames(entity_terms_dfm))
    
    # Extract the terms types related to the NER for each selected term
    terms_types <- getEntityTypesFromTags(terms, NA)
    
    # Extract the terms class related to the NER for each selected term
    terms_classes <- getEntityClassesFromTags(terms, NA)
    
  }
  
  list(tdm = tdm,
       dfm = documents_dfm,
       st_dfm = standard_terms_dfm,
       et_dfm = entity_terms_dfm,
       voc = list(
         terms = terms,
         terms_without_tag = terms_without_tag,
         entity_terms_without_tag = entity_terms_without_tag,
         terms_types = terms_types,
         terms_classes = terms_classes))
  
}


#' Apply term weighting to a term document matrix
#'
#' @param tdm The term-document matrix
#' @param mode The type of term weighting to apply
#' @return The term-document matrix after term weighting
applyTermWeightingToTdm <- function(tdm, mode = "unsupervised") {
  
  if (!(mode %in% c("unsupervised", "supervised"))) {
    stop("Not valid term weighting mode")
  }
  
  if (mode == "unsupervised") {
    
    # Weight terms in term-doc matrix according to the entropy-based tf-idf variant
    # Apply two factors in order to improve the information content
    # 1) a local factor on the terms of the same doc
    # 2) a global factor: the terms that appears in many documents have a low information content
    lw_logtf(tdm) * (1 - entropy(tdm))
    
  } else if (mode == "supervised") {
    
    # Not yet implemented
    
  }
  
  
}


#' Apply term weighting to a document vector based on a term-document matrix
#'
#' @param doc_vec The document vector
#' @param tdm The term document matrix
#' @return The document vector after term weighting
applyTermWeightingToDocument <- function(doc_vec, tdm) {
  
  lw_logtf(doc_vec) * (1 - entropy(tdm))
  
}