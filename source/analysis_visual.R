##########################################
## Utilities
##########################################

# Associate a default color to each class
opinionClassColor <- function(x) switch(as.character(x),
                                        Negative = "red",
                                        Neutral = "darkgray",
                                        Positive = "green")


# Calculate an array with each opinion class associated to its color
getColsForOpinionClasses <- function(opinion_classes) {
  opinion_classes_cols <- sapply(opinion_classes, opinionClassColor)
  opinion_classes_cols
}


# Define the visual representation of entity terms
getTermsRepresentationWithNER <- function(terms_classes) {
  
  # Save the possible terms classes
  terms_class_levels <- na.omit(unlist(unique(terms_classes)))
  
  # Create a raibow palette containing a color for each entity class 
  #entity_classes_palette <- palette(rainbow(length(terms_class_levels)))
  crp.rg <- colorRampPalette(c("red","yellow","cyan","blue","red"))
  entity_classes_palette = crp.rg(length(terms_class_levels))
  
  # Define a function in order to associate a default color to each entity type
  entityTypeColor <- function(x) {
    if (is.na(x)) {
      return("black")
    } else {
      return(entity_classes_palette[which(terms_class_levels == x)])
    }
  }
  
  # Define the textual and the colored info for legend rendering
  terms_class_legend_text <- c("NA", terms_class_levels)
  terms_class_legend_fill <- unlist(lapply(c(NA, terms_class_levels), function(x) entityTypeColor(x)))
  
  # Calculate an array with each entity type class associated to its color
  terms_class_cols <- sapply(terms_classes, entityTypeColor)
  
  list(terms_class_legend_text = terms_class_legend_text,
       terms_class_legend_fill = terms_class_legend_fill,
       terms_class_cols = terms_class_cols)
  
}


##########################################
## Visual Functions
##########################################

#' Draw the histogram for opinion scores distribution
#'
#' @param scores The score array
showOpinionScoreDistribution <- function(scores) {
  min_score <- min(scores)
  max_score <- max(scores)
  p <- ggplot() + aes(scores) +
    geom_histogram(binwidth = 1) +
    scale_x_continuous(breaks = min_score:max_score)
  print(p)
}

#' Show the percentages of positive and negative opinion classes
#'
#' @param very_pos The binary array for positive documents
#' @param very_neg The binary array for negative documents
showOpinionClassPercentages <- function(very_pos, very_neg) {
  # Count the number of positive and negative documents
  pos_count <- sum(very_pos)
  neg_count <- sum(very_neg)
  # Calculate the percentage of not neutral opinions
  pos_percentage <- round(100 * pos_count / (pos_count + neg_count))
  neg_percentage <- round(100 * neg_count / (pos_count + neg_count))
  message("Percentage of pos documents: ", pos_percentage)
  message("Percentage of neg documents: ", neg_percentage)
}


#' Show a plot with the calculated eigenvectors.
#' The singular values are in descending order and form a power law curve with a few values higher
#' at the beginning and more minor values in the rest of the graph.
#'
#' @param sk The array of eigenvectors after SVD
showLSASkPowerLawCurve <- function(sk) {
  plot(1:length(sk), sk, type = "b")
}

#' Show a 2D representation for LSA arrays.
#'
#' @param df The dataframe to consider with the coordinate of the latent space as columns
#' @param dims_offset The dimensionality offset for the selection od the 2D coordinates
#' @param texts Optional. The text to display for each entry.
#' @param text_cols Optional. The color to use for each displayed text.
#' @param point_cols Optional. The color to use for each entry. 
show2DLSADataDistribution <- function(df, dims_offset = 1:2, texts = NULL, text_cols = NULL, point_cols = NULL) {
  if (is.null(point_cols)) {
    point_cols = rep("black", nrow(df))
  }
  plot(df[,dims_offset], pch = 18, cex = 0.8, col = point_cols)
  if (!is.null(texts)) {
    if (!is.null(text_cols)) {
      text(df[,dims_offset], labels = texts, cex = 0.8, pos = 1, col = text_cols)
    } else {
      text(df[,dims_offset], labels = texts, cex = 0.8, pos = 1)
    }
  }
  # Display the origin
  points(0, 0, pch = 20, cex = 3, col = "blue")
}

#' Show on standard output a 2D graph with the positions of documents and terms,
#' considering dimensions 2 and 3 of LSA space
#'
#' @param tlsn The terms' normalized positions in the latent space
#' @param dlsn The documents' normalized positions in the latent space
#' @param doc_class_cols A vector with the color to use for each document
#' @param terms_labels A vector with the textual string related to each term
#' @param terms_class_cols Optional. A vector with the color to use for each term
#' @param terms_class_legend_text Optional. A vector with the unique texts for legend representation
#' @param terms_class_legend_fill Optional. A vector with the color associated to each legend textual item
#' @param qdlsn Optional. The query document normalized position in the latent space
show2DLSA <- function(tlsn, dlsn,
                      dims_offset = 1:2,
                      doc_class_cols,
                      terms_labels,
                      terms_class_cols = NULL,
                      terms_class_legend_text = NULL, terms_class_legend_fill = NULL,
                      qdlsn = NULL) {
  
  # Create a bidimensional space considering dimensions 2 and 3 of the latent space
  # Display documents
  plot(dlsn[,dims_offset], pch = 20, cex = 0.8, col = doc_class_cols)
  # Display terms
  text(tlsn[,dims_offset], labels = terms_labels, cex = 0.8, pos = 1, col = terms_class_cols)
  # Display the origin
  points(0, 0, pch = 20, cex = 3, col = "blue")
  
  if (!is.null(qdlsn)) {
    # Display the query document
    points(qdlsn[dims_offset][1], qdlsn[dims_offset][2], pch = 20, cex = 3, col = "orange")
  }
  
  if (!is.null(terms_class_legend_text) && !is.null(terms_class_legend_fill)) {
    # Display the term legend
    legend("topleft",
           inset = .02,
           title = "Term Legend",
           legend = terms_class_legend_text,
           fill = terms_class_legend_fill,
           cex = 0.8,
           text.font = 4,
           bg = "lightgray")
  }
  
  # Display the document opinion legend
  legend("bottomleft",
         inset = .02,
         title = "Document Opinion Legend",
         legend = c("Positive", "Neutral", "Negative"),
         fill = c("green", "gray", "red"),
         cex = 0.8,
         text.font = 4,
         bg = "lightgray")
  
}

#' Show 2D t-SNE results for term vectors.
#'
#' @param tsne_docs The 2D coordinates for documents
#' @param tsne_terms The 2D coordinates for terms
#' @param doc_class_cols The color class of each document
#' @param terms_labels The textual label of each term
#' @param terms_class_cols The color class of each term
#' @param terms_class_legend_text The text description for each term class
#' @param terms_class_legend_fill The color representation for each term class
showTsneTerms <- function(tsne_docs,
                          tsne_terms,
                          doc_class_cols,
                          terms_labels,
                          terms_class_cols,
                          terms_class_legend_text, terms_class_legend_fill) {
  
  # Empty plot
  plot(tsne_docs$Y, pch = 20, cex = 0.8, col = doc_class_cols)
  
  # Display terms
  text(tsne_terms$Y, labels = terms_labels, cex = 0.8, col = terms_class_cols)
  
  # Display the origin
  points(0, 0, pch = 20, cex = 3, col = "blue")
  
  # Display the term legend
  legend("topleft",
         inset = .02,
         title = "Term Legend",
         legend = terms_class_legend_text,
         fill = terms_class_legend_fill,
         cex = 0.8,
         text.font = 4,
         bg = "lightgray")
  
  # Display the document opinion legend
  legend("bottomleft",
         inset = .02,
         title = "Document Opinion Legend",
         legend = c("Positive", "Neutral", "Negative"),
         fill = c("green", "gray", "red"),
         cex = 0.8,
         text.font = 4,
         bg = "lightgray")
  
}

#' Show a word cloud.
#'
#' @param dfm The quanteda document-term matrix
#' @param term_labels The term labels to show (optionally different from the ones in dfm)
#' @param max_words The maximum number of words to display
showWordCloud <- function(dfm, term_labels = NULL, max_words = NULL) {
  
  if (!is.null(term_labels)) {
    colnames(dfm) <- term_labels
  }
  
  if (is.null(max_words)) {
    max_words = quanteda::nfeat(dfm)
  }
  
  options(warn = -1)
  
  quanteda::textplot_wordcloud(dfm,
                     rotation = 0.25,
                     max_words = max_words,
                     color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
  
  options(warn = 0)
  
}

#' Show a comparative word cloud.
#'
#' @param documents The documents to use as corpus
#' @param docid_field The column name containing the document identifier
#' @param text_field The column name containing the document text
#' @param class_field The column name containing the document class
#' @param classes The classes to consider
#' @param colors The colors to use for the classes
#' @param stopwords The stopwords to remove
#' @param min_termfreq The minimum frequency for the terms to consider
#' @param max_words The maximum number of words to display
showComparisonWordCloud <- function(documents, docid_field = "doc_id", text_field = "text", class_field = "class",
                                    classes, colors, stopwords, min_termfreq, max_words = NULL) {
  
  dfm_comparison <- dfm(
    corpus_subset(
      corpus(
        documents,
        docid_field = docid_field,
        text_field = text_field),
      class %in% classes),
    remove = stopwords, remove_punct = TRUE, groups = class_field) %>%
    dfm_trim(min_termfreq = min_termfreq)
  
  if (is.null(max_words)) {
    max_words = quanteda::nfeat(dfm_comparison)
  }
  
  textplot_wordcloud(dfm_comparison, comparison = TRUE, max_words = max_words, color = colors)
  
}

#' Show the IR effectiveness for a class description.
#'
#' @param lsa_data The data resulting from the application of LSA
#' @param lsa_dims The dimensions to consider
#' @param class_description_query The query object for the class description
#' @param doc_classes The class of each document
#' @param class The class for which measure IR effectiveness
showClassDescriptionIREffectiveness <- function(lsa_data, lsa_dims, class_description_query, doc_classes, class) {
  
  # Check how classes are distributed in the semantic search result compared to
  # the current class description
  class_count <- sum(doc_classes == class)
  nearest <- sort(
    cosines(lsa_data$dls[, lsa_dims], class_description_query$ls[lsa_dims]),
    decreasing = T,
    index.return = T)
  damcols2 <- rep("black", nrow(lsa_data$dls))
  damcols2[doc_classes == class] <- "red"
  plot(1:class_count, nearest$x[1:class_count],
       pch = 20, cex = 0.7, col = damcols2[match(nearest$ix, 1:nrow(lsa_data$dls))])
  
}

#' Show the dendrogram resulting from a Standard Hierarchical Clustering Analysis (HCA)
#'
#' @param agglomerate The hierarchical agglomerate from hclust
#' @param d.labels The labels to display in the dendrogram
#' @param d.labels_cols The colors to use for the labels in the dendrogram
#' @param d.labels_cex The font size for the labels in the dendrogram
#' @param d.title The title of the dendrogram
#' @param d.type The type of the dendrogram
#' @param d.x_label The description to display for x axis in the dendrogram
#' @param d.y_label The description to display for y axis in the dendrogram
#' @param d.horiz Boolean. True for an horizontal visualization of the dendrogram
showDendrogramStandardHca <- function(agglomerate,
                                      d.labels, d.labels_cols, d.labels_cex = 1,
                                      d.title = "dendrogram", d.type = "rectangle",
                                      d.x_label = "items", d.y_label = "height",
                                      d.horiz = FALSE,
                                      cut_height = NULL) {
  
  # Draw the dendrogram
  h_dendrogram <- as.dendrogram(agglomerate) %>%
    dendextend::set("labels", d.labels) %>%
    dendextend::set("labels_cex", d.labels_cex)
  h_dendrogram <- h_dendrogram %>%
    dendextend::set("labels_colors",
                    if (is.null(d.labels_cols)) rep("black", length(d.labels)) else d.labels_cols)
  graphics::plot(h_dendrogram,
                 main = d.title,
                 type = d.type,
                 xlab = d.x_label, ylab = d.y_label,
                 horiz = d.horiz)
  
  if (!is.null(cut_height)) {
    # Draw red borders around the clusters at a certain height
    rect.hclust(agglomerate, h = cut_height, border = "red")
  }
  
}

#' Show the dendrogram resulting from a Pvalue Hierarchical Clustering Analysis (HCA)
#'
#' @param agglomerate The hierarchical agglomerate from p-value hclust
#' @param min_pvalue The minimum AU p-value for clusters retrieving
showDendrogramPvalueHca <- function(agglomerate,
                                    min_pvalue) {
  
  # Plot a dendrogram with p-values
  graphics::plot(agglomerate)
  
  # Add rectangles around clusters highly supported by the data in order to highlight them
  # (AU p-value larger than a min pvalue)
  pvclust::pvrect(agglomerate, alpha = min_pvalue, pv = "au", type = "geq")
  
}