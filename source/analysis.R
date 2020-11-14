###########################################
## Load Packages
###########################################

source("source/packages_loading.R")


###########################################
## Load Utils
###########################################
  
# ≈ 1min
source("source/data_loading.r")
source("source/utils/preprocessing/quality_preprocessing.R")
source_python("source/utils/preprocessing/lemmatization.py")
source("source/utils/preprocessing/documents_filtering.R")
source("source/utils/opinion_mining/opinion_mining_lexicon.R")
source("source/utils/ner/ner_utils.R")
source("source/utils/tdm/tdm_utils.R")
source("source/utils/lsa/lsa_utils.R")
source("source/utils/lsa/lsa_utils_v2.R")
source("source/utils/algebra/algebra_utils.R")
source("source/utils/sim_dist/sim_dist_utils.R")
source("source/utils/hierarchical_clustering/hc_utils.R")
source("source/utils/evaluation/gold_standards_evaluation.R")
source("source/analysis_visual.R")


###########################################
## Load Pre-Calculated Data
###########################################

lang = "it"
data <- loadData(lang)

slang_words <- data$slang_words
stopwords <- data$stopwords
concepts <- data$concepts
opinion_lexicon <- data$opinion_lexicon
documents <- data$documents
reconciled_entities <- data$reconciled_entities
tdmc <- data$tdmc
gold_standards <- data$gold_standards
global_adjacencies <- data$global_adjacencies
rm(data)


###########################################
## Modules for DTM4ED LSA-based implementation
###########################################

lemmatizeYN = TRUE

#' MODULE 1
#' Preprocessing pipeline with the aim of improving the documents' quality.
#' @param texts the string array with the original text documents
#' @return the string array after the quality preprocessing pipeline
qualityPreprocessingFun <- function(texts) {
  cleanDocuments(texts, slang_words)
}

#' MODULE 2
#' Classify text documents.
#' @param texts the string array with the text content of each document
#' @return a string array with the class of each document
docsClassificationFun <- function(texts) {
  opinionLexiconClassification(texts,
                               pos_words = opinion_lexicon$pos_words,
                               neg_words = opinion_lexicon$neg_words,
                               pos_opinion_min = 2,
                               neg_opinion_max = -2)
}

#' MODULE 3
#' Perform feature selection from the complete term-document matrix.
#' @param tdmc the complete term-document matrix
#' @return a data frame with
#' the vocabulary (terms, terms_without_tag, entity_terms_without_tag, terms_types, terms_classes),
#' the final term-document matrix,
#' the document-term matrix considering all terms,
#' and the one considering only standard or entity terms
featureSelectionFun <- function(tdmc) {
  # Italian
  # All docs --> 0.002, 0.0001
  # Poem --> 0.005, 0
  removeSparseFeatures(tdmc,
                       min_standard_term_freq = 0.002,
                       min_entity_term_freq = 0.0001)
}

#' MODULE 4
#' Apply term weighting to a term-document matrix.
#' @param tdm the term-document matrix
#' @return the weighted term-document matrix
termWeightingFun <- function(tdm) {
  applyTermWeightingToTdm(tdm, mode = "unsupervised")
}

# Apply preprocessing operations to a new document
userQueryPreprocessingNer <- function(doc) {
  # Quality preprocessing
  doc <- qualityPreprocessingFun(doc)
  # Tags entities in the document
  doc <- tagEntities(reconciled_entities, doc, stopwords)$tagged_text
  if (lemmatizeYN) {
    # Lemmatize
    doc <- lemmatize(unlist(doc), lang, entity_tag_regex_pattern)
  }
  # Lowercasing
  doc <- tolower(doc)
  doc
}


###########################################
## Analysis
###########################################

################################
## Build Complete Term-Document Matrix
################################

# Documents filtering
concept = concepts$all
message("N docs before filtering: ", nrow(documents))
documents <- filterDocumentsByContent(documents, content_regex = concept$concept_regex)
message("N docs after filtering: ", nrow(documents))
print(documents$original_text[1:5])

# Quality preprocessing
documents$quality_text <- qualityPreprocessingFun(documents$original_text)

# Entity tagging
ner_res <- tagEntities(reconciled_entities, documents$quality_text, stopwords)
documents$text <- ner_res$tagged_text
documents$id_tagged_text <- ner_res$id_tagged_text

# Opinion mining class for each document
documents$class <- docsClassificationFun(documents$id_tagged_text)

# Build complete term-document matrix
tdmc <- buildCompleteTdm(documents, stopwords, lang, lemmatizeYN)

################################
## Feature Selection
################################

# Feature selection from the complete term-document matrix
tdm_voc <- featureSelectionFun(tdmc)
voc = tdm_voc$voc
tdm = tdm_voc$tdm
dfm = tdm_voc$dfm
st_dfm = tdm_voc$st_dfm
et_dfm = tdm_voc$et_dfm
rm(tdm_voc)

################################
## Term Weighting
################################

tdmw <- termWeightingFun(tdm)
dfmw <- quanteda::as.dfm(t(tdmw))

################################
## Language Modeling
## Semantic Space Construction
################################

lsa_data <- performLSA(tdmw, alg = "rsvd", k = 100) # ≈ 1min (all docs)
lsa_data2 <- performLSA(tdmw, alg = "lsa", k = 100) # ≈ 8min (all docs)
lsa_data2$computation_time - lsa_data$computation_time

# Power law curve with the significance of each latent semantic space dimension (LSA Sigma matrix)
showLSASkPowerLawCurve(lsa_data$lsam$sk)

# Calculate the significance of each latent dimension to select the ones to work with
# The informative contribution given by the dimensions associated with the eigenvalues that follow a knee point
# is lower, making an approximation possible
skcurv_mins <- findLSASkMins(lsa_data$lsam$sk)

# From a computational point of view, more than two dimensions are required
skcurv_mins <- skcurv_mins[skcurv_mins > 2]

# Wordclouds and statistics about the corpus
showWordCloud(dfm, term_labels = voc$terms_without_tag)
showWordCloud(dfm, term_labels = voc$terms_without_tag, max_words = 100)
showWordCloud(dfmw, term_labels = voc$terms_without_tag)
showWordCloud(dfmw, term_labels = voc$terms_without_tag, max_words = 100)
showWordCloud(et_dfm, term_labels = voc$entity_terms_without_tag)
showComparisonWordCloud(
  documents,
  text_field = "quality_text",
  classes = c("Positive", "Negative"),
  colors = c("red", "green"),
  stopwords = stopwords,
  min_termfreq = 3,
  max_words = 350)

# 2D terms and documents distribution in the latent semantic space
# - dims_offset => Explicit: 2:3; Implicit: -1.
# Italian
# - all_docs / poem <- 2:3
# English
# - all_docs <- 3:4
dims_2d <- 2:3
tr <- getTermsRepresentationWithNER(voc$terms_classes)
doc_class_cols <- getColsForOpinionClasses(documents$class)
show2DLSADataDistribution(df = lsa_data$tls, dims_offset = dims_2d, texts = voc$terms_without_tag)
show2DLSADataDistribution(df = lsa_data$dls, dims_offset = dims_2d)
show2DLSADataDistribution(df = lsa_data$dls, dims_offset = dims_2d, point_cols = doc_class_cols)
show2DLSADataDistribution(df = lsa_data$tlsn, dims_offset = dims_2d, texts = voc$terms_without_tag)
show2DLSADataDistribution(df = lsa_data$tlsn, dims_offset = dims_2d, texts = voc$terms_without_tag, text_cols = tr$terms_class_cols)
show2DLSADataDistribution(df = lsa_data$dlsn, dims_offset = dims_2d)
show2DLSADataDistribution(df = lsa_data$dlsn, dims_offset = dims_2d, point_cols = doc_class_cols)

# Complete 2D visualization of the latent semantic space
pdf(file = paste0("plots/", lang, "_", concept$concept_name, "_lsa_2d.pdf"), width = 25.6, height = 14.4)
show2DLSA(
  tlsn = lsa_data$tlsn,
  dlsn = lsa_data$dlsn,
  dims_offset = dims_2d,
  doc_class_cols = doc_class_cols,
  terms_labels = voc$terms_without_tag,
  terms_class_cols = tr$terms_class_cols,
  terms_class_legend_text = tr$terms_class_legend_text,
  terms_class_legend_fill = tr$terms_class_legend_fill,
  qdlsn = NULL)
dev.off()

################################
## t-SNE
################################

# - t-SNE (t-Distributed Stochastic Neighbor Embedding, 2008) is a non-linear dimensionality
#   reduction algorithm used for exploring high-dimensional data
# - It maps multi-dimensional data to two or more dimensions suitable for human observation
# - It can deal with more complex patterns of Gaussian clusters in multidimensional space
#   compared to PCA (1933)

# Applies t-SNE on terms and documents (not normalized), considering all LSA dimensions
# Italian
# all docs --> perplexity = 50
# poem --> perplexity = 20
tsne_lsa_tls <- Rtsne::Rtsne(
  lsa_data$tls, dims = 2, check_duplicates = FALSE, perplexity = 20, verbose = TRUE, max_iter = 500)
tsne_lsa_dls <- Rtsne::Rtsne(
  lsa_data$dls, dims = 2, check_duplicates = FALSE, perplexity = 20, verbose = TRUE, max_iter = 500)
pdf(file = paste0("plots/", lang, "_", concept$concept_name, "_tsne_2d.pdf"), width = 25.6, height = 14.4)
showTsneTerms(
  tsne_docs = tsne_lsa_dls,
  tsne_terms = tsne_lsa_tls,
  doc_class_cols = doc_class_cols,
  terms_labels = voc$terms_without_tag,
  terms_class_cols = tr$terms_class_cols,
  terms_class_legend_text = tr$terms_class_legend_text,
  terms_class_legend_fill = tr$terms_class_legend_fill)
dev.off()

# Applies t-SNE on terms and documents (normalized), considering all LSA dimensions
# Italian
# all docs --> perplexity = 100
# poem --> perplexity = 10
tsne_lsa_tlsn <- Rtsne::Rtsne(
  lsa_data$tlsn, dims = 2, check_duplicates = FALSE, perplexity = 10, verbose = TRUE, max_iter = 500)
tsne_lsa_dlsn <- Rtsne::Rtsne(
  na.exclude(lsa_data$dlsn), dims = 2, check_duplicates = FALSE, perplexity = 10, verbose = TRUE, max_iter = 500)
pdf(file = paste0("plots/", lang, "_", concept$concept_name, "_tsne_n_2d.pdf"), width = 25.6, height = 14.4)
showTsneTerms(
  tsne_docs = tsne_lsa_dlsn,
  tsne_terms = tsne_lsa_tlsn,
  doc_class_cols = doc_class_cols,
  terms_labels = voc$terms_without_tag,
  terms_class_cols = tr$terms_class_cols,
  terms_class_legend_text = tr$terms_class_legend_text,
  terms_class_legend_fill = tr$terms_class_legend_fill)
dev.off()

################################
## Semantic Space Dimensions
################################

min_index = 2
lsa_dims <- 1:skcurv_mins[min_index]

################################
## Significant Terms
################################

# Italian all docs
# - min_index = 2
term_norms <- getOrderedTermNorms(voc$terms, lsa_data$tlsn, lsa_dims = dims_2d)
term_norms <- getOrderedTermNorms(voc$terms, lsa_data$tlsn, lsa_dims = lsa_dims)
term_norms <- getOrderedTermNorms(voc$terms, lsa_data$tls, lsa_dims = lsa_dims)
term_norms <- getOrderedTermNorms(voc$terms, lsa_data$tls)
term_norms <- getOrderedTermNorms(voc$terms, lsa_data$tlsn)

################################
## Semantically Nearest Documents
################################

new_doc <- makeUserQuery(
  "Gemelli roma",
  userQueryPreprocessingNer,
  applyTermWeightingToDocument,
  tdmw,
  lsa_data$lsam)

# Top-N documents semantically related to the user query
n_nearest <- 10
nearest_docs <- findTopNearestDocuments(
  texts = documents$text,
  dls = lsa_data$dls,
  lsa_dims = lsa_dims,
  query_ls = new_doc$ls_q,
  n = n_nearest)
print(nearest_docs)

################################
## Gold Standard Evaluation
################################

# Gold standard evaluation
eval_res <- evaluateLatentSemanticSpace(
  gold_standards$kc_no_ner_pos, gold_standards$kc_no_ner_neg,
  voc$terms_without_tag, voc$terms,
  tdmw, applyTermWeightingToDocument, lsa_data$lsam, lsa_dims,
  list(list(.7, .7), list(.8, .8)))

################################
## Calibration
################################

lsa_k_to_test <- c(50, 100, 200, 300, 500, 800)
pvalue_thresholds_to_test <- list(
  # Common positive and negative thresholds
  list(.7, .7), list(.75, .75), list(.8, .8), list(.85, .85), list(.9, .9),
  # Thresholds with gap
  list(.7, .6), list(.75, .6), list(.8, .6), list(.85, .6), list(.9, .6),
  list(.7, .65), list(.75, .65), list(.8, .65), list(.85, .65), list(.9, .65),
  list(.75, .7), list(.8, .7), list(.85, .7), list(.9, .7),
  list(.8, .75), list(.85, .75), list(.9, .75),
  list(.85, .8), list(.9, .8))

all_eval_res <- list()

# Global space evaluation variables
best_k <- NA
best_min <- NA
best_eval_res <- NA

# Search the best latent space by gold standards representation
for (k in lsa_k_to_test) {
  
  message("testing k=", k)
  
  # Perform LSA
  lsa_data <- performLSA(tdmw, alg = "rsvd", k = k)
  
  # Detect minimums (possible knee points)
  skcurv_mins <- findLSASkMins(lsa_data$lsam$sk)
  skcurv_mins <- skcurv_mins[skcurv_mins > 2]
  message("n minima to test: ", length(skcurv_mins))
  
  # Local space evaluation variables (i.e., local wrt the current dimensionality k)
  best_local_min <- NA
  best_local_eval_res <- NA
  
  # Search the best minimum
  for (min in skcurv_mins) {
    
    message("testing min=", min)
    
    # Select the dimension range
    lsa_dims <- 1:min
    
    # Gold standard evaluation
    eval_res <- evaluateLatentSemanticSpace(
      gold_standards$kc_no_ner_pos, gold_standards$kc_no_ner_neg,
      voc$terms_without_tag, voc$terms,
      tdmw, applyTermWeightingToDocument, lsa_data$lsam, lsa_dims,
      pvalue_thresholds_to_test)
    
    # Select the best pvalue threshold settings
    eval_res_best_settings <- selectLatentSpaceEvaluationWithMaximumScore(eval_res)
    
    message("min_pos_pvalue", eval_res_best_settings$min_pos_pvalue)
    message("max_neg_pvalue", eval_res_best_settings$max_neg_pvalue)
    message("accuracy=", eval_res_best_settings$accuracy)
    message("precision=", eval_res_best_settings$precision)
    message("tpr/fpr=", eval_res_best_settings$tpr_fpr_ratio)
    message("f1=", eval_res_best_settings$f1)
    
    # If the current tpr-fpr ratio improves the previous better result,
    # select the current minimum as knee point
    if (is.na(best_local_eval_res) ||
        best_local_eval_res$tpr_fpr_ratio < eval_res_best_settings$tpr_fpr_ratio) {
      best_local_min <- min
      best_local_eval_res <- eval_res_best_settings
    }
    
    # Save the result for current k + min
    all_eval_res <- c(eval_res,
                      list(
                        k = k,
                        min = min,
                        eval_res = eval_res
                      ))
    
  }
  
  # If the current best tpr-fpr ratio (obtained on all the minima)
  # exceeds that previously found with other k-values,
  # select the current k and the related min as the best combination
  if (is.na(best_eval_res) ||
      best_eval_res$tpr_fpr_ratio < best_local_eval_res$tpr_fpr_ratio) {
    best_k <- k
    best_min <- min
    best_eval_res <- best_local_eval_res
    best_lsa_data <- lsa_data
  }
  
}

################################
## Phenomena explanation
################################

# Italian
# poem (k=100, min_index = 2)
# - POS
#   - userQueryPreprocessingNer("familiari"), min_similarity_closeness = 0.93, n_semantically_closest_terms = 5, max_pvalue_threshold = 0.001
#   - userQueryPreprocessingNer("costamagna"), min_similarity_closeness = 0.93, n_semantically_closest_terms = 5, max_pvalue_threshold = 0.001
#   - userQueryPreprocessingNer("gemelli"), min_similarity_closeness = 0.93, n_semantically_closest_terms = 5, max_pvalue_threshold = 0.001
#   - userQueryPreprocessingNer("reflusso"), min_similarity_closeness = 0.9, n_semantically_closest_terms = 50, max_pvalue_threshold = 0.001
# - NEG
#   - userQueryPreprocessingNer("ppi"), min_similarity_closeness = 0.9, n_semantically_closest_terms = 50, max_pvalue_threshold = 0.001
#   - userQueryPreprocessingNer("reflusso"), min_similarity_closeness = 0.9, n_semantically_closest_terms = 50, max_pvalue_threshold = 0.001
#   - userQueryPreprocessingNer("infiammazione"), min_similarity_closeness = 0.98, n_semantically_closest_terms = 5, max_pvalue_threshold = 0.001
#   - userQueryPreprocessingNer("difficolta"), min_similarity_closeness = 0.9, n_semantically_closest_terms = 5, max_pvalue_threshold = 0.001
#   - userQueryPreprocessingNer("dolore"), min_similarity_closeness = 0.98, n_semantically_closest_terms = 5, max_pvalue_threshold = 0.0000000000001
#   - userQueryPreprocessingNer("problemi liquido solido"), min_similarity_closeness = 0.96, n_semantically_closest_terms = 5, max_pvalue_threshold = 0.0000000001

# all --> term_norm_threshold = 6, min_similarity_closeness = 0.8, n_semantically_closest_terms = 5, max_pvalue_threshold = 0.0001

# Italian all docs
# - "poem", 6, 0.8, 5, 0.0001
# - userQueryPreprocessingNer("padova costantini")
# English all docs
# - term_norm_threshold = 11.5

pos_description <- descriptiveAnalysisLSA(
  tdm = tdmw,
  termWeightingDocFun = applyTermWeightingToDocument,
  lsa_data = lsa_data,
  lsa_dims = lsa_dims,
  starting_query = userQueryPreprocessingNer("costamagna"),
  term_norm_threshold = 0.5,
  class = "Positive",
  doc_classes = documents$class,
  min_similarity_closeness = 0.93,
  n_semantically_closest_terms = 5,
  max_pvalue_threshold = 0.001)
print(pos_description$text)
print(pos_description$pvalue)

neg_description <- descriptiveAnalysisLSA(
  tdm = tdmw,
  termWeightingDocFun = applyTermWeightingToDocument,
  lsa_data = lsa_data,
  lsa_dims = lsa_dims,
  starting_query = userQueryPreprocessingNer("ppi"),
  term_norm_threshold = NULL,
  class = "Negative",
  doc_classes = documents$class,
  min_similarity_closeness = 0.96,
  n_semantically_closest_terms = 5,
  max_pvalue_threshold = 0.0000000001)
print(neg_description$text)
print(neg_description$pvalue)

pdf(file = "plots/lsa_2d_pos_description.pdf", width = 25.6, height = 14.4)
show2DLSA(
  tlsn = lsa_data$tlsn,
  dlsn = lsa_data$dlsn,
  dims_offset = dims_2d,
  doc_class_cols = doc_class_cols,
  terms_labels = voc$terms_without_tag,
  terms_class_cols = tr$terms_class_cols,
  terms_class_legend_text = tr$terms_class_legend_text,
  terms_class_legend_fill = tr$terms_class_legend_fill,
  qdlsn = pos_description$lsn)
dev.off()

showClassDescriptionIREffectiveness(
  lsa_data = lsa_data,
  lsa_dims = lsa_dims,
  class_description_query = pos_description,
  doc_classes = documents$class,
  class = "Positive")

showClassDescriptionIREffectiveness(
  lsa_data = lsa_data,
  lsa_dims = lsa_dims,
  class_description_query = neg_description,
  doc_classes = documents$class,
  class = "Negative")

################################
## Locality Sensitive Hashing (LSH)
################################

# Show the tradeoff between precision and recall for approximate near negihbor search
# to enable a good selection of the parameters
hashfun_number = 120
s_curve <- get_s_curve(hashfun_number, n_bands_min = 5, n_rows_per_band_min = 5, plot = TRUE)

seed = 1
bands_number = 10
rows_per_band = 32

# Calculate pairs of similar documents
similar_docs <- LSHR::get_similar_pairs_cosine(
  lsa_data$dlsn[,lsa_dims], bands_number = bands_number, rows_per_band = rows_per_band, seed = seed)
similar_docs <- similar_docs[order(-N)]

# Calculate pairs of similar terms
similar_terms <- LSHR::get_similar_pairs_cosine(
  lsa_data$tlsn[,lsa_dims], bands_number = bands_number, rows_per_band = rows_per_band, seed = seed)
similar_terms <- similar_terms[order(-N)]

################################
## Hierarchical Clustering
################################

# Standard HCA on terms

std_clusters_lsa <- standardHca(x = lsa_data$tls[, lsa_dims],
                                dist_method = "cosine",
                                labels = voc$terms)

showDendrogramStandardHca(agglomerate = std_clusters_lsa$agglomerate,
                          d.labels = voc$terms,
                          d.labels_cols = tr$terms_class_cols,
                          d.labels_cex = 0.6,
                          d.title = "Term dendrogram",
                          d.x_label = "terms")

# Pvalue HCA on terms

pv_clusters_lsa <- pvalueHca(x = lsa_data$tls[, lsa_dims],
                             dist_method = "cosine",
                             nboot = 50,
                             r = seq(1.0, 2.0, by = .1),
                             min_pvalue = .95,
                             labels = voc$terms)

showDendrogramPvalueHca(agglomerate = pv_clusters_lsa$agglomerate,
                        min_pvalue = .95)


################################
## Relevant Adjacencies
################################

rel_adjacencies <- calculateRelevantAdjacencies(tls = lsa_data$tls,
                                                lsa_dims = lsa_dims,
                                                terms = voc$terms,
                                                term_norms = term_norms$norm,
                                                top_n = 20,
                                                min_similarity = .95)
