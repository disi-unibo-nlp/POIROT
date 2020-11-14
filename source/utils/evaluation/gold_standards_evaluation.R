##########################################
## Evaluation based on Gold Standards
##########################################


#' Filter only terms with known correlations that are in the vocublary and get their indexes.
#' For this check purpose considers only terms' labels (their ids, without the eventual entity tag form).
#' Id refers to the reference name for the term.
#'
#' @param known_correlations Known terms correlations (expressed by terms' ids):
#' list of items with id, doc_1, doc_2, and correlation category
#' @param term_labels The complete labels for the terms inside the vocabulary
#' @return The indexes for the detectable correlations according to the vocabulary
knownCorrelationsToIdxs <- function(known_correlations, term_labels) {
  
  known_correlations_idxs <- apply(
    known_correlations, 1,
    function(kc) {
      doc_1_idxs <- kc$doc_1 %>% .[. %in% term_labels] %>% map(function(t) which(term_labels == t))
      doc_2_idxs <- kc$doc_2 %>% .[. %in% term_labels] %>% map(function(t) which(term_labels == t))
      if (length(doc_1_idxs) > 0 & length(doc_2_idxs) > 0) {
        list(id = kc$id, doc_1 = doc_1_idxs, doc_2 = doc_2_idxs, category = kc$category)
      } else {
        NA
      }
    })
  
  known_correlations_idxs
  
}


#' Calculate the indexes for the detectable positive and negative gold standards.
#'
#' @param kc_pos All positive gold standards
#' @param kc_neg All negative gold standards
#' @param term_ids The vocabulary term ids for which search matches on gold standards
#' @return The positive and negative detectable gold standard indexes
getIndexesForEvaluation <- function(kc_pos, kc_neg, term_ids) {
  
  # Filter only terms with known correlations that are in LSA space and get their indexes
  kc_pos_idxs <- knownCorrelationsToIdxs(kc_pos, term_ids)
  kc_neg_idxs <- knownCorrelationsToIdxs(kc_neg, term_ids)
  
  # Keep only known correlations with at least one detected term both in doc_1 and doc_2
  kc_pos_idxs <- kc_pos_idxs[!is.na(kc_pos_idxs)]
  kc_neg_idxs <- kc_neg_idxs[!is.na(kc_neg_idxs)]
  
  list(kc_pos_idxs = kc_pos_idxs, kc_neg_idxs = kc_neg_idxs)
  
}


#' Calculate the cosine similarity between the two documents involved by each known correlation.
#'
#' @param kc_idxs Known terms correlations with terms expressed by vocabulary indexes
#' @param term_labels The term labels to use for semantic similarity research
#' @param tdm The term-document matrix
#' @param termWeightingDocFun The term weighting function to apply on the binary query
#' @param lsa The matrixes resulting from LSA application
#' @param lsa_dims The LSA dimensions to work with (range)
#' @return The list containing the cosine similarities for each known correlation
cosSimBetweenKnownCorrelations <- function(kc_idxs, term_labels, tdm, termWeightingDocFun, lsa, lsa_dims) {
  
  pb <- progress_bar$new(total = length(kc_idxs))
  
  kc_cos_sim <- lapply(kc_idxs, function(corr_group) {
    
    # Get textual terms for correlated documents (this time using the complete form adopted during LSA)
    doc_1 <- corr_group$doc_1 %>% map(function(idx) term_labels[idx])
    doc_2 <- corr_group$doc_2 %>% map(function(idx) term_labels[idx])
    
    # Convert lists to string queries
    doc_1_query <- paste(unlist(doc_1), collapse = ' ')
    doc_2_query <- paste(unlist(doc_2), collapse = ' ')
    
    # Perform LSA fold-in
    doc_1_lsa <- makeQuery(doc_1_query, tdm, termWeightingDocFun, lsa)
    doc_2_lsa <- makeQuery(doc_2_query, tdm, termWeightingDocFun, lsa)
    
    # Calculate the cosine similarity between documents' position in latent space
    cosine_similarity <- cosine(
      doc_1_lsa$ls_q[lsa_dims],
      doc_2_lsa$ls_q[lsa_dims])
    
    pb$tick()
    
    list(
      id = corr_group$id,
      doc_1 = doc_1,
      doc_2 = doc_2,
      cos_sim = cosine_similarity,
      category = corr_group$category)
    
  })
  
  kc_cos_sim
  
}


#' Build a confusion matrix based on the ability to highlight known correlations
#' (i.e., positive and negative gold standards)
#' 
#' @param kc_pos The dataframe with the list of positive known correlated queries (id, doc_1, doc_2, category)
#' @param kc_neg The dataframe with the list of negative known correlated queries (id, doc_1, doc_2, category)
#' @param kc_pos_found The dataframe with the detected positive queries and their cosine similarities
#' in the latent semantic space (id, doc_1, doc_2, cos_sim, category)
#' @param kc_neg_found The dataframe with the detected negative queries and their cosine similarities
#' in the latent semantic space (id, doc_1, doc_2, cos_sim, category)
#' @param min_pos_max_neg_pvalues An list of pairs with the minimum cosine similarity to say that two known queries are semantically related,
#' and the maximum one to say that are not. Example, c(c(0.7, 0.7), c(0.8, 0.8))
#' @return A list of results for each pvalue threshold pair:
#' accuracy, precision, misclassification rate, tpr, tnr, fpr, fnr, tpr/tnr rate, pos/neg counts by category
buildGoldStandardConfusionMatrix <- function(kc_pos, kc_neg,
                                             kc_pos_found, kc_neg_found,
                                             min_pos_max_neg_pvalues) {
  
  n_total_kc_pos <- nrow(kc_pos)
  n_total_kc_neg <- nrow(kc_neg)
  
  n_detected_kc_pos <- nrow(kc_pos_found)
  n_detected_kc_neg <- nrow(kc_neg_found)
  
  # Calculate the number of undetected known queries' correlations
  # (i.e., no terms found in at least one of the two queries)
  n_missed_kc_pos <- n_total_kc_pos - n_detected_kc_pos
  n_missed_kc_neg <- n_total_kc_neg - n_detected_kc_neg
  
  # Calculate a confusion matrix for each pair of min and max pvalue threshold
  confusion_matrices <- lapply(min_pos_max_neg_pvalues, function(p) {
    
    # Keep the detected known queries with a sufficient cosine similarity between them
    valid_kc_pos_found <- filter(kc_pos_found, cos_sim >= p[[1]])
    valid_kc_neg_found <- filter(kc_neg_found, cos_sim <= p[[2]])
    
    n_valid_kc_pos <- nrow(valid_kc_pos_found)
    n_valid_kc_neg <- nrow(valid_kc_neg_found)
    
    # Calculate confusion matrix
    tp <- n_valid_kc_pos
    tn <- n_valid_kc_neg
    fp <- n_total_kc_neg - n_valid_kc_neg - n_missed_kc_neg
    fn <- n_total_kc_pos - n_valid_kc_pos - n_missed_kc_pos 
    
    # Calculate metrix
    accuracy <- (tp + tn) / (tp + tn + fp + fn) * 100
    precision <- tp / (tp + fp) * 100
    misclassification_rate <- (fp + fn) / (tp + tn + fp + fn) * 100
    tpr <- tp / (tp + fn) * 100 # recall
    tnr <- tn / (tn + fp) * 100
    fpr <- fp / (tn + fp) * 100
    fnr <- fn / (tp + fn) * 100
    tpr_fpr_ratio <- tpr / fpr # useful for evaluation (e.g., ROC curve)
    f1 <- (2 * tp) / ((2 * tp) + fp + fn)
    
    # Count the number of known correlations for each category
    total_kc_pos_counts_by_category <- data.frame(table(kc_pos$category))
    colnames(total_kc_pos_counts_by_category) <- c("category", "counts")
    total_kc_neg_counts_by_category <- data.frame(table(kc_neg$category))
    colnames(total_kc_neg_counts_by_category) <- c("category", "counts")
    
    # Count the number of detectable known correlations for each category
    detectable_kc_pos_counts_by_category <- data.frame(table(unlist(kc_pos_found$category)))
    colnames(detectable_kc_pos_counts_by_category) <- c("category", "counts")
    detectable_kc_neg_counts_by_category <- data.frame(table(unlist(kc_neg_found$category)))
    colnames(detectable_kc_neg_counts_by_category) <- c("category", "counts")
    
    # Count the number of detected known correlations for each category
    detected_kc_pos_counts_by_category <- data.frame(table(unlist(valid_kc_pos_found$category)))
    colnames(detected_kc_pos_counts_by_category) <- c("category", "counts")
    detected_kc_neg_counts_by_category <- data.frame(table(unlist(valid_kc_neg_found$category)))
    colnames(detected_kc_neg_counts_by_category) <- c("category", "counts")
    
    # Build a table to compare total conts with detected and valid ones
    pos_counts_by_category <- merge(
      x = total_kc_pos_counts_by_category,
      y = detectable_kc_pos_counts_by_category,
      by = "category",
      all.x = TRUE) %>% merge(
        detected_kc_pos_counts_by_category,
        by = "category",
        all.x = TRUE) 
    colnames(pos_counts_by_category) <- c("category", "all", "detectable", "detected")
    neg_counts_by_category <- merge(
      x = total_kc_neg_counts_by_category,
      y = detectable_kc_neg_counts_by_category,
      by = "category",
      all.x = TRUE) %>% merge(
        detected_kc_neg_counts_by_category,
        by = "category",
        all.x = TRUE) 
    colnames(neg_counts_by_category) <- c("category", "all", "detectable", "detected")
    
    # Replace NA with 0
    pos_counts_by_category[is.na(pos_counts_by_category)] = 0
    neg_counts_by_category[is.na(neg_counts_by_category)] = 0
    
    list(
      min_pos_pvalue = p[[1]],
      max_neg_pvalue = p[[2]],
      accuracy = accuracy,
      precision = precision,
      misclassification_rate = misclassification_rate,
      tpr = tpr,
      tnr = tnr,
      fpr = fpr,
      fnr = fnr,
      tpr_fpr_ratio = tpr_fpr_ratio,
      f1 = f1,
      pos_counts_by_category = pos_counts_by_category,
      neg_counts_by_category = neg_counts_by_category
    )
    
  })
  
  confusion_matrices
  
}


#' Evaluate the latent semantic space quality based on the ability
#' to highlight known correlations (i.e., positive and negative gold standards)
#' 
#' @param kc_pos The dataframe with the list of positive known correlated queries (id, doc_1, doc_2, category)
#' @param kc_neg The dataframe with the list of negative known correlated queries (id, doc_1, doc_2, category)
#' @param term_ids The identifier of the terms
#' @param term_labels The term labels for visualization purposes
#' @param tdm The term-document matrix
#' @param termWeightingDocFun The term weighting function to apply at each artificial document in gold standards
#' @param lsa The data resulting from the application of LSA
#' @param lsa_dims The dimensions to consider
#' @param min_pos_max_neg_pvalues An array of pairs with the minimum cosine similarity to say that two known queries are semantically related,
#' and the maximum one to say that are not. Example, c(c(0.7, 0.7), c(0.8, 0.8))
#' @return A list of results for each pvalue threshold pair:
#' accuracy, precision, misclassification rate, tpr, tnr, fpr, fnr, tpr/tnr rate, pos/neg counts by category
evaluateLatentSemanticSpace <- function(
  kc_pos, kc_neg,
  term_ids, term_labels,
  tdm, termWeightingDocFun, lsa, lsa_dims,
  min_pos_max_neg_pvalues) {
  
  # Gets the indexes for detectable gold standards, searching term ids
  kc_idxs <- getIndexesForEvaluation(kc_pos, kc_neg, term_ids)
  
  # Calculates the cosine similarity between the positive correlated documents' positions in LSA space
  # for each pair where there is at least one recognized term in each group
  kc_pos_cos_sim <- cosSimBetweenKnownCorrelations(kc_idxs$kc_pos_idxs, term_labels, tdm, termWeightingDocFun, lsa, lsa_dims)
  
  # Calculates the cosine similarity between the negative correlated documents' positions in LSA space
  # for each pair where there is at least one recognized term in each group
  kc_neg_cos_sim <- cosSimBetweenKnownCorrelations(kc_idxs$kc_neg_idxs, term_labels, tdm, termWeightingDocFun, lsa, lsa_dims)
  
  # Converts list of lists to dataframe
  kc_pos_cos_sim <- as.data.frame(do.call(rbind, kc_pos_cos_sim))
  kc_neg_cos_sim <- as.data.frame(do.call(rbind, kc_neg_cos_sim))
  
  # Removes NA value from cosine similarities
  # This is possible if all vectors' components are 0-value for the considered min-dependent dimensions
  kc_pos_cos_sim <- kc_pos_cos_sim[!is.na(unlist(kc_pos_cos_sim$cos_sim)),]
  kc_neg_cos_sim <- kc_neg_cos_sim[!is.na(unlist(kc_neg_cos_sim$cos_sim)),]
  
  # Performs evaluation
  eval_res <- buildGoldStandardConfusionMatrix(
    kc_pos, kc_neg,
    kc_pos_cos_sim, kc_neg_cos_sim,
    min_pos_max_neg_pvalues)
  
  eval_res
  
}


#' Select the evaluation results with a maximum score.
#' 
#' @param eval_res The list of lists containing the evaluation results obtained through different pvalue thresholds
#' @param feature The feature for which search the maximum value (e.g., tpr/fpr, f1, accuracy)
#' @return the eval_res item with the maximum feature value
selectLatentSpaceEvaluationWithMaximumScore <- function(eval_res, feature = "tpr_fpr_ratio") {
  
  # Convert list of list to data frame to easy select all feature values
  df = as.data.frame(do.call(rbind, lapply(eval_res, unlist)))
  
  # Detect the index of the result with the maximum tpr-fpr ratio and return the related list
  eval_res[[which(df[feature]==max(df[feature]))]]
  
}
