##########################################
## Import R scripts
##########################################

source("source/utils/lsa/query.R")



##########################################
## Utility functions for LSA
##########################################


#' Perform a normalization (to unit vector conversion) of each row of a dataframe
#'
#' @param x The dataframe with unnormalized vectors as rows
#' @return The dataframe with normalized vectors as rows
normRows <- function(x) x / apply(x, 1, norm, "2")


#' Replace each item of a numeric vector with the difference with the previous one
#'
#' @param x The numeric vector
#' @return The numeric vector after the computation of the difference between
#' each item and the previous one
fordiff <- function(x) x[2:length(x)] - x[1:(length(x) - 1)]


#' Detect the local minimums in a numeric vector
#'
#' @param x The numeric vector
#' @return The local minimums of the x
minimums <- function(x) which(x - data.table::shift(x, 1) < 0
                              & x - data.table::shift(x, 1, type = 'lead') < 0)


#' Calculate the cosine similarities between a vector and each row of a matrix
#'
#' @param x The matrix
#' @param q The vector
#' @return The vector with the cosine similarities
cosines <- function(x, q) apply(x, 1, cosine, as.vector(q))


#' Get the highest n values of a vector
#'
#' @param x The vector
#' @param n The number of top item to consider
#' @return The top n items inside x
top <- function(x, n) order(x, decreasing = TRUE)[1:n]


#' Perform LSA on a term-document matrix and with a specific target dimensionality 
#'
#' @param tdm The term-document matrix
#' @param alg The algorithm to use
#' - lsa, standard LSA based on SVD
#' - rsvd, Fast Randomized Singular Value Decomposition
#' (https://github.com/erichson/rSVD#:~:text=Fast%20Randomized%20Singular%20Value%20Decomposition,data%20sets%20with%20high%20accuracy.)
#' @param k Optional, the number of dimensions for the lantent space
#' @return The LSA matrices and intermediate results for similarity calculation
performLSA <- function(tdm, alg, k = 100) {
  
  if (!(alg %in% c("lsa", "rsvd"))) {
    stop("Not valid algorithm")
  }
  
  start_time <- Sys.time()
  
  if (alg == "lsa") {
    
    # Decompose the term document matrix using the lsa function
    # Executes the SVD and makes the matrix with a reduced number of dimensions calculated automatically
    # - tk matrix with terms' vectors (U)
    # - sk diagonal matrix (Sigma)
    # - dk matrix with documents' vectors (V)
    # If k is not specified, it automatically choses the rank with an heuristic
    options(warn = -1)
    lsam <- if (is.null(k)) lsa(tdm) else lsa(tdm, dims = k)
    options(warn = 0)
    
  } else if (alg == "rsvd") {
    
    # A fast probabilistic algorithm that can be used to compute the near optimal low-rank
    # singular value decomposition of massive data sets with high accuracy.
    # The key idea is to compute a compressed representation of the data to capture the essential information.
    # This compressed representation can then be used to obtain the low-rank svd.
    # If k is not specified, use the number of columns (documents) in the tdm 
    
    lsam <- if (is.null(k)) rsvd(tdm) else rsvd(tdm, k = k)
    names(lsam) <- c("sk", "tk", "dk")
    
  }
  
  # Calculate U * Sigma^1/2
  tksrs <- lsam$tk %*% diag(sqrt(lsam$sk))
  rownames(tksrs) <- rownames(tdm)
  
  # Calculate the matrices with the positions of the terms and the documents in the latent space
  tls <- lsam$tk %*% diag(lsam$sk)
  dls <- lsam$dk %*% diag(lsam$sk)
  
  # Normalize the vectors with the positions of terms and documents (length 1)
  tlsn <- normRows(tls)
  dlsn <- normRows(dls)
  
  end_time <- Sys.time()
  
  list(lsam = lsam, tksrs = tksrs, tls = tls, dls = dls, tlsn = tlsn, dlsn = dlsn,
       computation_time = end_time - start_time)
  
}


#' Find the minimums inside the curvature function for the eigenvector distribution.
#'
#' @param sk The eigenvalues
#' @param search_range The range to consider for the research
#' @return the minimums in the curvature function
findLSASkMins <- function(sk, search_range = 1:20) {
  
  # Detect the knee point in the sequence of eigenvalues between the local mins of the
  # curvature function
  skd <- fordiff(sk)
  skdd <- fordiff(skd)
  skcurv <- skdd[search_range] / (1 + (skd[search_range]) ^ 2) ^ 1.5
  plot(search_range, skcurv, type="b")
  
  # Detect minimums in the curvature function (sequence of eigenvalues)
  minimums(skcurv)
  
}


#' Find the N documents semantically closest to a query.
#'
#' @param texts The textual content of the documents
#' @param dls The documents' coordinates in the latent semantic space
#' @param lsa_dims The dimensions of the latent semantic space
#' @param query_ls The query object (new dummy document) embedding
#' @param n The number of nearest documents to search
#' @return the textual content of the nearest documents
findTopNearestDocuments <- function(texts, dls, lsa_dims, query_ls, n = 10) {
  texts[top(
    cosines(dls[, lsa_dims], query_ls[lsa_dims]),
    n)]
}


#' Calculate and sort term norms.
#'
#' @param terms The string array for term labels
#' @param term_vectors The matrix with the array representation of each term
#' @param lsa_dims The dimensions to consider
#' @return term norms in descending order
getOrderedTermNorms <- function(terms, term_vectors, lsa_dims = NULL) {
  
  if (is.null(lsa_dims)) {
    lsa_dims = 1:ncol(term_vectors)
  }
  
  # Calculates the norm of the terms in the latent space
  term_norms <- apply(term_vectors[,lsa_dims], 1, norm, "2")
  term_norms <- data.frame(term = terms, norm = term_norms, row.names = NULL)
  
  # Sorts decreasingly terms in the selected dimensions of the latent space by their norm
  term_norms <- orderBy(~-norm, term_norms)
  
  term_norms
  
}


#' Perform all the preliminary operations for the calculation of the similarity
#' between a query and the terms in the latent space
#'
#' @param q The query (a set of key words to research in the documents)
#' @param tdm The term document matrix
#' @param termWeightingDocFun The term weighting function to apply on the binary query
#' @param lsam The LSA matrix decomposition after SVD
#' @return The original string query (q),
#' the binary query document in the latent space (bin_q),
#' the query document after tf-idf weighting (w_q),
#' the query document in the latent space (ls_q = dls = V * Sigma) and its normalization (lsn_q),
#' the query document equivalent to V matrix rows (dk = V),
#' the V * Sigma^1/2 element for semantic similarity calculation between query and terms (dksrs)
makeQuery <- function(q, tdm, termWeightingDocFun, lsam) {
  
  # THEORY
  # uk * sigmak * vk_t
  # -> uk (U) = matrix terms x latent variables
  # -> vk (V) = matrix documents x latent variables
  
  # LSA PACKAGE
  # lsam$tk * lsam$sk * lsam$dk
  # -> lsam$tk = uk (U)
  # -> lsam$dk = vk (V)
  # -> dls = lsam$dk %*% diag(lsam$sk) = vk * sigmak = V * Sigma
  
  # Create the query vector (binary vector)
  # Transform the query in a vector representing the presence/absence
  # of each term of the bag of words representation
  bin_q <- query(q, rownames(tdm))
  
  # The query is like a new document to add to the latent space
  # So it applies all the transformations made to those already inside
  # (term weighting, normalization) in order to fold it into the LSA space
  # The query vector is now equivalent to a column of tdm matrix
  w_q <- termWeightingDocFun(bin_q, tdm)
  
  # Calculate the position of the query in the latent space (V * Sigma)
  ls_q <- t(w_q) %*% lsam$tk
  
  # Calculate the normalized query vector in latent space for visualization purposes
  lsn_q <- normRows(ls_q)
  
  # Transform the query vector in a new document (row of V matrix)
  # q_k = q^T * U_k * Sigma_k^-1
  dk <- ls_q %*% diag(lsam$sk ^ -1)
  
  # The similarity between a query and some terms is calculated as a cosine similarity
  # considering the V representation for the query and the terms vectors multiplicated
  # by Sigma^1/2.
  # cosine(V * Sigma^1/2, U * Sigma^1/2) = cosine(dk %*% diag(sqrt(lsam$sk), lsam$tk %*% diag(sqrt(lsam$sk)))
  # dksrs is so one of the two necessary elements for similarity calculation
  dksrs <- dk %*% diag(sqrt(lsam$sk))
  
  # Return a named list with the results
  list(q = q, bin_q = bin_q, w_q = w_q, ls_q = ls_q, lsn_q = lsn_q, dk = dk, dksrs = dksrs)
  
}


#' Fold-in a custom textual document in a latent semantic space.
#' Apply the same transformation strategy applied to original documents.
#' unlike a query, the document is not necessarily composed only of the dictionary terms,
#' but is a text freely expressed by a user (similar to a new post).
#'
#' @param doc The custom textual document
#' @param preprocessingDocFun The preprocessing function to apply to the document before the fold-in
#' @param termWeightingDocFun The term weighting function to apply on the binary query
#' @param tdm The term document matrix to enrich
#' @param lsam The latent semantic space on which perform document fold-in
#' @return The document, its binary vector representation, its weighted vector, its position in the latent space,
#' its normalized position for visualization purposes, the component for similarity calculation
makeUserQuery <- function(doc, preprocessingDocFun, termWeightingDocFun, tdm, lsam) {
  
  # Performs document-level preprocessing
  doc <- preprocessingDocFun(doc)
  
  # Custom document fold-in
  makeQuery(doc, tdm, termWeightingDocFun, lsam)
  
}


#' Calculate the correlation between a query and a class inside a latent space,
#' using chi-square test
#'
#' @param q The query to consider for correlation
#' @param tdm The term document matrix
#' @param lsa The LSA decomposition after SVD
#' @param dls The documents' positions in the latent space
#' @param lsa_dims The LSA dimensions to consider
#' @param class The class to consider for correlation
#' @param doc_classes The classes related to each document of the entire dataset
#' @param debug Boolean. True to display detailed output for each chisquared test, False otherwise.
#' @return The chi-square result for the correlation between q and class, and the query object
#' Prints on standard output the contingency table, the expected number of related documents
#' and the observed one
calculateQueryClassCorrelationLSA <- function(q, tdm, termWeightingDocFun, lsam, dls, lsa_dims, class, doc_classes, debug = F) {
  
  # Converts the query as a document in the LSA space (query document)
  qd <- makeQuery(q, tdm, termWeightingDocFun, lsam)
  
  # Calculates the number of occurrences of the target class between all classes
  # It fixes the maximum number of results returned from the ranking-based semantic search model
  class_count <- sum(doc_classes == class)
  
  # Verifies objectively if there is a semantic correlation between the query and the class
  # In the latent space, the documents considered could also not contain the terms inside the query
  # but terms semantically related to them
  
  # Builds the contingency table
  q.vs.class <- base::table(1:nrow(dls) %in% top(cosines(dls[, lsa_dims], qd$ls_q[lsa_dims]), class_count),
                            doc_classes == class)
  dimnames(q.vs.class) <- list(q = c("No", "Yes"), class = c("No", "Yes"))
  
  # Performs a chi-squared test between the query and the class
  chisqtest <- chisq.test(q.vs.class, correct = FALSE)
  
  if (debug) {
    print(as.character(q))
    print(q.vs.class)
    cat('Expected: ', chisqtest$expected, "\n")
    cat('Observed: ', chisqtest$observed, "\n")
  }
  
  list(chisqtest = chisqtest, query = qd)
  
}


#' Perform a greedy descriptive analysis for a certain class in the latent space.
#' If specified, start from a certain query document; otherwise start from the most representative term
#' with high norm for the opinion class to analyze.
#'
#' @param tdm The term-document matrix
#' @param termWeightingDocFun The term weighting function to apply on the binary query
#' @param lsa_data The data resulting from the application of LSA
#' @param lsa_dims The LSA dimensions to consider during the analysis
#' @param starting_query Optional. The query document from which start the analysis
#' @param term_norm_threshold The minimum norm for a relevant term
#' @param class The class for which build a description
#' @param doc_classes The class of each document
#' @param min_similarity_closeness The minimum value for cosine similarity that must be satisfied during step-by-step analysis
#' @param n_semantically_closest_terms The number of semantically closest terms to consider during step-by-step analysis
#' @param max_pvalue_threshold The minimum pvalue that must be satisfied by a class description
#' @return The list with the results of the descriptive analysis:
#' - The description for the opinion class in original language (cd) and in english (cd_en)
#' - The p-value score related to the opinion class description (pvalue)
#' - The position of the opinion class description document in the latent space (ls)
#' - The normalized position of the opinion class description document in the latent space (lsn)
descriptiveAnalysisLSA <- function(tdm,
                                   termWeightingDocFun,
                                   lsa_data, lsa_dims,
                                   starting_query = NULL,
                                   term_norm_threshold = NULL,
                                   class, doc_classes,
                                   min_similarity_closeness,
                                   n_semantically_closest_terms,
                                   max_pvalue_threshold) {
  
  term_norms <- getOrderedTermNorms(rownames(tdm), lsa_data$tls, lsa_dims)
  
  if (is.null(starting_query)) {
    
    # The starting query document is not specified...
    
    # For each term with an high norm, perform a chi-squared test
    # in order to verify the correlation (low p-value) between the term and the class
    high_norm_terms <- term_norms %>% filter(norm > term_norm_threshold)
    high_norm_terms_chisqtest_class <- lapply(
      high_norm_terms$term,
      function(q) calculateQueryClassCorrelationLSA(
        q, tdm, termWeightingDocFun, lsa_data$lsam, lsa_data$dls, lsa_dims, class, doc_classes))
    high_norm_terms_chisqtest_class <- data.frame(
      term = high_norm_terms$term,
      norm = high_norm_terms$norm,
      pvalue = unlist(
        lapply(lapply(high_norm_terms_chisqtest_class, `[[`, "chisqtest"), `[[`, "p.value"),
        use.names = FALSE),
      # The I marker allows the creation of a dataframe without a new column for each coordinate
      # but with only one vector column
      ls = I(lapply(lapply(high_norm_terms_chisqtest_class, `[[`, "query"), `[[`, "ls_q")),
      lsn = I(lapply(lapply(high_norm_terms_chisqtest_class, `[[`, "query"), `[[`, "lsn_q")),
      dksrs = I(lapply(lapply(high_norm_terms_chisqtest_class, `[[`, "query"), `[[`, "dksrs"))
    )
    
    # Order high norm terms from those most correlated with the opinion_class to those less correlated
    high_norm_terms_chisqtest_class <- orderBy(~pvalue, high_norm_terms_chisqtest_class)
    
    # Select the high norm term with the lowest pvalue as the starting one
    top_pvalue_term <- head(high_norm_terms_chisqtest_class, 1)
    top_pvalue_term <- list(
      text = as.character(top_pvalue_term$term[[1]]),
      pvalue = top_pvalue_term$pvalue[[1]],
      ls = top_pvalue_term$ls[[1]],
      lsn = top_pvalue_term$lsn[[1]],
      dksrs = top_pvalue_term$dksrs[[1]]
    )
    
    # Initialize class description query data with top pvalue term data
    class_description_query <- top_pvalue_term
    
  } else {
    
    # The starting query document is already specified...
    
    # Calculate the correlation between the starting query document and the class
    sq_chisqtest_class <- calculateQueryClassCorrelationLSA(
      starting_query, tdm, termWeightingDocFun, lsa_data$lsam, lsa_data$dls, lsa_dims, class, doc_classes)
    sq <- list(
      text = starting_query,
      pvalue = (sq_chisqtest_class[["chisqtest"]])[["p.value"]],
      ls = (sq_chisqtest_class[["query"]])[["ls_q"]],
      lsn = (sq_chisqtest_class[["query"]])[["lsn_q"]],
      dksrs = (sq_chisqtest_class[["query"]])[["dksrs"]]
    )
    
    # Initialize the class description query
    class_description_query <- sq
    
  }
  
  analysis_finished <- FALSE
  while (!analysis_finished) {
    
    # VISUAL (R-PRECISION)
    
    # Remove from U * Sigma^1/2 the previously selected terms for opinion_class description 
    # in order to not select them again
    tksrs_ncd <- lsa_data$tksrs[!(rownames(lsa_data$tksrs) %in% unlist(base::strsplit(class_description_query$text, " "))), ]
    
    # Calculate the semantic similarity between the query and the terms, and order them
    # cosine(V * Sigma^1/2, U * Sigma^1/2)
    # cosine between -1 and 1 because the vector space also includes the negative quadrants
    closeness_terms_ranking <- sort(
      cosines(tksrs_ncd[, lsa_dims], class_description_query$dksrs[lsa_dims]),
      decreasing = TRUE)
    
    # Select the N semantically nearest terms that satisfy a minimum closeness threshold
    nearest_terms <- names(
      head(
        closeness_terms_ranking[closeness_terms_ranking > min_similarity_closeness],
        n_semantically_closest_terms))
    
    # Sort the nearest terms by norm length descending order
    nearest_terms_norms <- dplyr::filter(term_norms, term %in% nearest_terms)
    nearest_terms_norm_ranking <- orderBy(~-norm, nearest_terms_norms)
    
    # Navigate the nearest terms from the one with the highest norm to the one with the lowest norm
    # The first met term with a sufficient low pvalue is used for the query extension
    # representing the class description
    new_term_chisqtest <- NULL
    i <- 1
    while (is.null(new_term_chisqtest) && i <= length(nearest_terms)) {
      
      # Create the new query temp (class description)
      new_class_description <- paste(class_description_query$text, as.character(nearest_terms[[i]]))
      
      # Calculate the correlation between the new class description and the class
      near_term_chisqtest <- calculateQueryClassCorrelationLSA(
        new_class_description, tdm, termWeightingDocFun, lsa_data$lsam, lsa_data$dls, lsa_dims, class, doc_classes)
      
      # Get the pvalue of the chisquared test
      near_term_pvalue <- (near_term_chisqtest[["chisqtest"]])[["p.value"]]
      
      # Check if the pvalue is low enough
      if (near_term_pvalue < max_pvalue_threshold) {
        new_term_chisqtest <- near_term_chisqtest
      }
      
      # Go to the next nearest term with a lower norm
      i <- i + 1
      
    }
    
    if (is.null(new_term_chisqtest)) {
      # If no one of the nearest terms (independently by their norm) has a pvalue low enough
      # stops the descriptive analysis (class description completed)
      analysis_finished <- TRUE
    } else {
      # Update class data with the selected term
      class_description_query <- list(
        text = (new_term_chisqtest[["query"]])[["q"]],
        pvalue = (new_term_chisqtest[["chisqtest"]])[["p.value"]],
        ls = (new_term_chisqtest[["query"]])[["ls_q"]],
        lsn = (new_term_chisqtest[["query"]])[["lsn_q"]],
        dksrs = (new_term_chisqtest[["query"]])[["dksrs"]]
      )
      
    }
    
  }
  
  class_description_query
  
}

#' Calculate the top-N semantic adjacencies for each term in the latent space,
#' above a certain similarity threshold.
#' With the same similarity, priority is given to adjacent terms with higher norms.
#'
#' @param tls The coordinates of the terms in the latent space
#' @param lsa_dims The LSA dimensions to consider
#' @param terms The term labels
#' @param term_norms The vector with the 2-norm of each term
#' @param top_n The number of neighbors to research for each term
#' @param min_similarity The minimum similarity for a top-N closest term to be considered adjacent
#' @return
#' - the adjacency matrix
#' - a list of list containing the top-N terms above threshold semantically related to each term
calculateRelevantAdjacencies <- function(tls, lsa_dims, terms, term_norms,
                                         top_n = 20, min_similarity = .95) {
  
  # 1) COSINE SIMILARITY IN [0,1]
  # ----------------------------------
  
  # Calculates an adjacency matrix with the cosine similarity between each pair of term
  adj_matrix <- cosineSim(t(tls[, lsa_dims]))
  
  # Remaps cosine similarity values to [0, 1] range
  adj_matrix <- base::apply(adj_matrix, 1:2, function(x) changeRange(x, -1, 1, 0, 1))
  
  # Use term labels for adjacency matrix rownames
  rownames(adj_matrix) <- terms
  
  # Converts the distance matrix to a named list of dataframe with term, similarity and norm columns
  adj_list <- lapply(seq_len(ncol(adj_matrix)), function(i) {
    df <- as.data.frame(as.table(adj_matrix[, i]))
    colnames(df) <- c("term", "similarity")
    df$norm <- term_norms
    df
  })
  names(adj_list) <- rownames(adj_matrix)
  
  # Keeps only the relevant adjacencies for each term
  rel_adj_list <- lapply(seq_along(adj_list), function(i) {
    df <- adj_list[[i]]
    current_term <- names(adj_list)[[i]]
    # Removes the term itself from dataframe rows
    df <- df[!(df$term == current_term), ]
    # Orders each dataframe inside the list by cosine similarity and norm
    # (in case of equal similarity)
    sorted_df <- df[with(df, order(-similarity, -norm)), ]
    # Keeps the first N terms
    top_n_df <- head(sorted_df, top_n)
    # Deletes neighbor terms without a sufficient similarity
    top_n_high_correlation_df <- filter(top_n_df, similarity > min_similarity)
    top_n_high_correlation_df
  })
  names(rel_adj_list) <- rownames(adj_matrix)
  
  # Keeps only terms which have at least one relevant adjacency
  rel_adj_list <- rel_adj_list[lapply(rel_adj_list, nrow) > 0]
  
}
