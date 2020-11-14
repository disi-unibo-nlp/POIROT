#' Filter textual documents by their content.
#' The selection is done with the application of a lowercase regex.
#'
#' @param documents The document dataframe with a text column
#' @param content_regex The regex for matching research
#' @return A dataset with the documents having a textual content which matches with the regex
filterDocumentsByContent <- function(documents, text_col = "text", content_regex) {
  
  documents[grepl(content_regex, tolower(str_replace_all(documents[[text_col]],"[^[:graph:]]", " "))), ]
  
}

#' Filter textual documents by their class
#'
#' @param documents The document dataframe with class column
#' @param class The class value of interest
#' @return A dataframe with the subset of documents with the specified class
filterDocumentsByClass <- function(documents, class) {
  
  idxs <- documents$class == class
  list(
    idxs = idxs,
    documents = documents[idxs, ]
  )
  
}

#' Filter textual documents by their creation date
#'
#' @param documents The document dataframe with creation_date column
#' @param min_creation_date The minimum creation date as string yyyy-mm-dd (e.g., "2020-07-11")
#' @param max_creation_date The maximum creation date as string yyyy-mm-dd (e.g., "2020-07-11")
#' @return A dataframe with the subset of documents with a creation date belonging to the specified range
filterDocumentsByCreationDate <- function(documents, min_creation_date, max_creation_date) {
  
  documents[documents$creation_date >= min_creation_date
            & documents$creation_date <= max_creation_date, ]
  
}
