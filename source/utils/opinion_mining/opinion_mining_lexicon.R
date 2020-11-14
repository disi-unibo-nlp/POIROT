##########################################
## Opinion Mining
##########################################


#' Calculate the opinion-lexicon score for text documents.
#' 
#' @param texts the string array with the text content of each document
#' @param pos_words the string array related to positive lexicon
#' @param neg_words the string array related to negative lexicon
#' @param .progress "text" if a progress indication is desired, "none" otherwise
#' @return a int array with the opinion score of each document
scoreSentiment = function(texts, pos_words, neg_words, .progress='none')
{
  
  scores = plyr::laply(texts, function(text, pos_words, neg_words) {
    
    # Extract only alphabetic characters and eliminate non-ascii ones
    text = gsub('[^A-z ]', ' ', text)
    
    # Word-based tokenization
    word_list = str_split(text, '\\s+')
    words = unlist(word_list)
    
    # Compare the words with the positive and negative opinion lexicon
    # match() returns a vector with first match positions
    pos_matches = match(words, pos_words)
    neg_matches = match(words, neg_words)
    
    # Convert match positions to booleans
    pos_matches = !is.na(pos_matches)
    neg_matches = !is.na(neg_matches)
    
    # Calculate the message score considering 1 and 0 for T and F
    score = sum(pos_matches) - sum(neg_matches)
    
    return(score)
    
  }, pos_words, neg_words, .progress=.progress)
  
  return(scores)
  
}

#' Classify text documents as positive, negative, or neutral.
#' 
#' @param texts the string array with the text content of each document
#' @param pos_words the string array related to positive lexicon
#' @param neg_words the string array related to negative lexicon
#' @param pos_opinion_min the minimum opinion score to consider a document as belonging to a positive class
#' @param neg_opinion_max the maximum opinion score to consider a document as belonging to a negative class
#' @return a string array with the opinion class of each document
opinionLexiconClassification <- function(texts, pos_words, neg_words, pos_opinion_min, neg_opinion_max) {
  
  # Calculate an opinion mining score lexicon based for each document
  scores <- scoreSentiment(
    texts,
    pos_words, neg_words,
    .progress = 'text')
  
  # Show the opinion score distribution between all documents
  showOpinionScoreDistribution(scores)
  
  # Detect documents without a neutral opinion
  c_docs = list(
    very_pos = as.numeric(scores >= pos_opinion_min),
    very_neg = as.numeric(scores <= neg_opinion_max)
  )
  
  # Show the percentage of positive and negative documents
  showOpinionClassPercentages(c_docs$very_pos, c_docs$very_neg)
  
  # Define the class of each document
  classes <- with(c_docs,
                  ifelse(very_pos == 1, "Positive",
                         ifelse(very_neg == 1, "Negative",
                                "Neutral")))
  
  classes
  
}