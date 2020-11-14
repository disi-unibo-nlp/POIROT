##########################################
## Named Entity Recognition and Linking
##########################################


# Define elements for entity tagging
entity_opening_tag <- "<e>"
entity_closing_tag <- "</e>"
types_opening_tag <- "<t>"
types_closing_tag <- "</t>"
class_opening_tag <- "<c>"
class_closing_tag <- "</c>"
wikidata_opening_tag <- "<wd>"
wikidata_closing_tag <- "</wd>"
wikilink_opening_tag <- "<wl>"
wikilink_closing_tag <- "</wl>"
types_separator <- ";"


# Define entity tagging regex pattern
entity_tag_regex_pattern <- paste0(entity_opening_tag, "(.*?)", entity_closing_tag)


#' Build a tag string for entity data.
#'
#' @param entity_id The identifier of the entity
#' @param types The type labels for the entity
#' @param class The macro class of the entity
#' @param wikidata_id The wikidata identier of the entity
#' @param wiki_link The link to the wikipedia page related to the entity
#' @return The entity tag string.
getEntityTag <- function(entity_id, types, class, wikidata_id, wiki_link) {
  paste0(
    entity_opening_tag,
    entity_id,
    types_opening_tag, paste(types, collapse = types_separator), types_closing_tag,
    class_opening_tag, class, class_closing_tag,
    wikidata_opening_tag, wikidata_id, wikidata_closing_tag,
    wikilink_opening_tag, wiki_link, wikilink_closing_tag,
    entity_closing_tag)
}


#' Convert an entity tag to the related data
#'
#' @param entity_tag The entity tag
#' @return The data contained in the entity tag.
entityTagToData <- function(entity_tag) {
  # Split the current entity tag
  entity_data <- strsplit(entity_tag, paste(
    entity_opening_tag,
    types_opening_tag,
    paste0(types_closing_tag, class_opening_tag),
    paste0(class_closing_tag, wikidata_opening_tag),
    paste0(wikidata_closing_tag, wikilink_opening_tag),
    paste0(wikilink_closing_tag, entity_closing_tag),
    sep = "|"))[[1]]
  # Retrieve data
  entity_id <- entity_data[2]
  types <- strsplit(entity_data[3], types_separator)[[1]]
  class <- entity_data[4]
  wikidata_id <- entity_data[5]
  wiki_link <- entity_data[6]
  # Return data
  list(entity_id = entity_id, types = types, class = class, wikidata_id = wikidata_id, wiki_link = wiki_link)
}


#' Convert a list of textual entity tags into a structured dataframe.
#'
#' @param entity_tags The list of entity tags
#' @return The dataframe representation of the entity tags with columns: entity_id, types and class.
entityTagsToDf <- function(entity_tags) {
  # Use rbind to merge the list of dataframes into a single dataframe to return
  do.call("rbind", lapply(entity_tags, function(x) {
    # Split the current entity tag
    entity_data <- entityTagToData(x)
    # Create the dataframe
    data.frame(
      entity_id = entity_data$entity_id,
      types = I(list(entity_data$types)),
      class = entity_data$class,
      wikidata_id = entity_data$wikidata_id,
      wiki_link = entity_data$wiki_link,
      stringsAsFactors = F
    )
  }))
}


#' Get the entity ids from a list of entity tags or the string itself if it does
#' not match with an entity tag. Alternatively, return a default value in case of mismatch.
#'
#' @param entity_tags The list of entity tags
#' @param default_value Optional. The default value to use in case of entity mismatch
#' @return The list of entity ids.
getEntityIdsFromTags <- function(entity_tags, default_value = NULL) {
  unlist(lapply(entity_tags, function(x) {
    if (grepl(entity_tag_regex_pattern, x)) {
      return(entityTagToData(x)$entity_id)
    } else {
      return(if (is.null(default_value)) x else default_value)
    }
  }))
}


#' Get the entity types from a list of entity tags or the string itself if it does
#' not match with an entity tag. Alternatively, return a default value in case of mismatch.
#'
#' @param entity_tags The list of entity tags
#' @param default_value Optional. The default value to use in case of entity mismatch
#' @return The list of entity types.
getEntityTypesFromTags <- function(entity_tags, default_value = NULL) {
  unlist(lapply(entity_tags, function(x) {
    if (grepl(entity_tag_regex_pattern, x)) {
      return(entityTagToData(x)$types)
    } else {
      return(if (is.null(default_value)) x else default_value)
    }
  }))
}


#' Get the entity classes from a list of entity tags or the string itself if it does
#' not match with an entity tag. Alternatively, return a default value in case of mismatch.
#'
#' @param entity_tags The list of entity tags
#' @param default_value Optional. The default value to use in case of entity mismatch
#' @return The list of entity classes.
getEntityClassesFromTags <- function(entity_tags, default_value = NULL) {
  unlist(lapply(entity_tags, function(x) {
    if (grepl(entity_tag_regex_pattern, x)) {
      return(entityTagToData(x)$class)
    } else {
      return(if (is.null(default_value)) x else default_value)
    }
  }))
}


#' Get the entity wikidata ids from a list of entity tags or the string itself if it does
#' not match with an entity tag. Alternatively, return a default value in case of mismatch.
#'
#' @param entity_tags The list of entity tags
#' @param default_value Optional. The default value to use in case of entity mismatch
#' @return The list of entity wikidata ids.
getEntityWikidataIdsFromTags <- function(entity_tags, default_value = NULL) {
  unlist(lapply(entity_tags, function(x) {
    if (grepl(entity_tag_regex_pattern, x)) {
      return(entityTagToData(x)$wikidata_id)
    } else {
      return(if (is.null(default_value)) x else default_value)
    }
  }))
}


#' Get the entity wikipedia links from a list of entity tags or the string itself if it does
#' not match with an entity tag. Alternatively, return a default value in case of mismatch.
#'
#' @param entity_tags The list of entity tags
#' @param default_value Optional. The default value to use in case of entity mismatch
#' @return The list of entity wikipedia links.
getEntityWikiLinksFromTags <- function(entity_tags, default_value = NULL) {
  unlist(lapply(entity_tags, function(x) {
    if (grepl(entity_tag_regex_pattern, x)) {
      return(entityTagToData(x)$wiki_link)
    } else {
      return(if (is.null(default_value)) x else default_value)
    }
  }))
}


#' Tag in an unsupervised way the entities that match with the patterns recognized by NER (precalculated results).
#' Each matched text used by the NER for tagging entity, if detected, is replaced with the reconciled entitity tag.
#'
#' @param reconciled_entities The entity dictionary to use
#' @param text The dataframe with the text column to tag
#' @param stopwords The stopwords to remove from entity tagging
#' @return The tagged text
tagEntities <- function(reconciled_entities, text, stopwords) {

  tagged_text <- text # Complete entity tagging
  id_tagged_text <- text # Matched text replaced with entity id
  
  # Replace each matchable word inside documents with the related entity tag
  entity_tagging_pb <- progress_bar$new(total = nrow(reconciled_entities))
  
  # For each detected entity...
  apply(reconciled_entities, 1, function(x) {
    
    # Check the words that were used to recognize it
    # Note: use unique in the case that the entity id is equal to a matched text
    words_to_replace <- unique(c(gsub("_", " ", x$entity_id), x$matched_texts))
    
    # Remove stopwords from the matchable words in order to reduce false positives
    words_to_replace <- words_to_replace[!(words_to_replace %in% stopwords)]
    
    # Define the regex pattern to research for entity tag replacement
    # '\b' matches between a word character and a non-word character (word boundary)
    # The '\b' prefix and suffix allows a search for exact words
    # '(?<!...)' uses lookbehind to avoid word boundary matches that starts with entity tag
    # (ensures no matches inside entity id texts already replaced)
    # Replace dots with a form having escaping characters in order to avoid "any character" special mean
    words_to_replace_pattern <- paste(
      lapply(words_to_replace, function(word) paste0("\\b(?<!", entity_opening_tag, ")", gsub(".", "\\.", word, fixed = T), "\\b")),
      collapse = "|")
    
    # Define the entity tag as the replacement string
    # Keep a space before and after an entity tag in order to simplify parsing
    replacement <- paste0(" ", getEntityTag(x$entity_id, x$labels, x$class, x$wikidata_id, x$wiki_link), " ")
    
    # Substitute occurrences in the original documents containing the concept
    # '<<-' operator allows an update on the variable defined in the parent environment
    # (instead considering a function-local copy)
    # Use stringi because is more efficient then gsub with large data
    if (words_to_replace_pattern != "") {
      tagged_text <<- stringi::stri_replace_all_regex(
        tagged_text,
        words_to_replace_pattern,
        replacement
      )
      id_tagged_text <<- stringi::stri_replace_all_regex(
        id_tagged_text,
        words_to_replace_pattern,
        x$entity_id
      )
    }
    
    # Upgrade progress bar
    entity_tagging_pb$tick()
    
  })
  
  list(
    tagged_text = tagged_text,
    id_tagged_text = id_tagged_text)
  
}
