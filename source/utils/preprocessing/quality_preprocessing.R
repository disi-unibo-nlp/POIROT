#  ----------------------------------------------------------------------------------------------------------------------------
# | STEP 1. Uniforms text format encoding                                                                                      |
# |                                                                                                                            |
# | ASCII//TRANSLIT                                                                                                            |
# | https://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding |
#  ----------------------------------------------------------------------------------------------------------------------------

uniformTextFormatEncoding <- function(messages) {
  # Converts messages' encoding from Latin1 to ASCII
  # TRANSLIT is the version for accented characters handling
  messages <- iconv(messages, "latin1", "ASCII//TRANSLIT", sub="")
  
  return(messages)
}



#  -------------------------------------------------------------------------------------------------------
# | Spaces and normalization                                                                              |
# | Reference Link: https://github.com/livioivil/TextWiller/tree/master/R                                 |
#  -------------------------------------------------------------------------------------------------------

#  ---------------------------
# | STEP 2. Normalize Symbols |
#  ---------------------------

normalizeSymbols <- function(message) {
  # &#10; is the ASCII character LF (Line Feed)
  message <- gsub("&#10;", " ", message)
  # Removes tab
  message <- gsub("\t", " ", message)
  
  return(message)
}

normalizeAllSymbols <- function(messages) {
  return(purrr::map_chr(messages, normalizeSymbols))
}

#  --------------------------------------------------------------------
# | STEP 3. Normalizes emotes with standard sentiment tags replacement |
#  --------------------------------------------------------------------

normalizeEmotes <- function(message, perl = TRUE) {
  
  # General emotes
  
  # EMOTEGOOD :) :-) :] :-] =) =] => :> ^^ ^_^ ^-^ ^o^ : ) (: :'D
  message <- gsub("\\:\\)+|\\:\\-\\)+|\\:\\]+|\\:\\-\\]+|\\=\\)+|\\=\\]+|\\=\\>|\\:\\>|\\^\\^|\\^\\_+\\^|\\^\\-\\^|\\^o\\^|\\:[[:blank:]]\\)+|[[:blank:]]\\([[:blank:]]?\\:|\\:\\'D+", ' EMOTEGOOD ', message, perl)
  # EMOTEGOOD :d :D :-d :-D =d =D 8d 8D :')
  message <- gsub("\\:d+|\\:D+|\\:\\-d+|\\:\\-D+|\\=d+|\\=D+|8d+|8D+|\\:\\'+\\)+|v\\.v", ' EMOTEGOOD ', message, perl)
  # EMOTELOVE <3 :*
  message <- gsub("\\<3+|\u2764|\u2665|\\:\\*+", ' EMOTELOVE ', message, perl)
  # EMOTEBAD :( :-( :[ :-[ =[ =( : ( ):
  message <- gsub("\\:\\(+|\\:\\-\\(+|\\:\\[+|\\:\\-\\[+|\\=\\[+|\\=\\(+|\\:[[:blank:]]\\(|[[:blank:]]\\([[:blank:]]?\\:", ' EMOTEBAD ', message, perl)
  # EMOTEBAD :'( :-[ D:
  message <- gsub("\\:\\'+\\(+|\\:\\'\\[|D\\:|\\:\\-\\[", ' EMOTEBAD ', message, perl)
  # EMOTEBAD :| :/ =/ :x :-|
  message <- gsub("\\:\\|\\:/+|\\=/+|\\:x", ' EMOTEBAD ', message, perl)
  # EMOTEBAD #_# X_X x_x X.X x.x >.< >_< >.> >_>
  message <- gsub("\\#\\_+\\#|X\\_+X|x\\_+x|X\\.X|x\\.x|>\\.<|>\\_+<|>\\_+>|>\\.>", ' EMOTEBAD ', message, perl)
  # EMOTEWINK ;) ;-) ;] ;-] ;> ;d ;D ;o
  message <- gsub("\\;\\)+|\\;\\-\\)+|\\;\\]|\\;\\-\\]|\\;\\>|;d+|;D+|;o", ' EMOTEWINK ', message, perl)
  # EMOTESHOCK O.o o.o O.O o.O O_o o_o O_O o_O etc
  message <- gsub("O\\.o|o\\.o|O\\.O|o\\.O|O\\_+o|o\\_+o|O\\_+O|o\\_+O|\\:OO+|\\=O+|\\-\\.\\-|u\\.u|u\\.\u00F9|\u00F9\\.u|u\\_+u|\u00E7\u00E7|\u00E7_+\u00E7|t_+t|\u00F9\\_+\u00F9|\u00F9\\.\u00F9|\\:oo+|0\\_+0|\\=\\_+\\=|\\.\\_+\\.|\u00F2\u00F2|\u00F2\\_+\u00F2|\\*u+\\*|\\-\\_+\\-|\u00F9\u00F9|\\-\\,\\-|\\-\\-\\'|\\.\\-\\.|\\'\\-\\'", ' EMOTESHOCK ', message, perl)
  # EMOTEAMAZE *_* *-* *o* *.*
  message <- gsub("\\*\\_+\\*|\\*\\-\\*|\\*\\.\\*", ' EMOTEAMAZE ', message, perl)
  # EMOTEJOKE :P :p =P =p XD xD xd d:
  message <- gsub("\\:P+[^e]|\\:p+[^e]|\\=P+|\\=p+|XD+|xD+|xd+|[[:blank:]]d\\:", ' EMOTEJOKE ', message, perl)
  
  # Facebook emotes
  # TODO
  # https://www.r-bloggers.com/emojis-analysis-in-r/
  # http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html
  # http://opiateforthemass.es/articles/emoticons-in-R/
  
  return(message)
}

normalizeAllEmotes <- function(messages) {
  return(purrr::map_chr(messages, normalizeEmotes))
}

#  ----------------------------------------------------------------------------------
# | STEP 4. Splits attached words                                                    |
# |                                                                                  |
# | After removal of punctuation or white spaces, words can be attached.             |
# | This happens especially when deleting the periods at the end of the sentences.   |
# | The corpus might look like: "the brown dog is lostEverybody is looking for him". |
# | So there is a need to split "lostEverybody" into two separate words.             |
# | https://hub.packtpub.com/clean-social-media-data-analysis-python/                |
#  ----------------------------------------------------------------------------------

splitAllAttachedWords <- function(messages) {
  messages <- gsub("([a-z])([A-Z])", "\\1 \\2", messages)
  return(messages)
}

#  ------------------------------------
# | STEP 5. Convert text to lower case |
#  ------------------------------------

toLowerCase <- function(messages) {
  # Converts messages to lowercase
  messages <- tolower(messages)
  return(messages)
}

#  -----------------------------------------------------------------------------------------------------------------
# | STEP 6. Internet Slang abbreviation translator                                                                  |
# |                                                                                                                 |
# | General approach                                                                                                |
# | https://medium.com/nerd-stuff/python-script-to-turn-text-message-abbreviations-into-actual-phrases-d5db6f489222 |
#  -----------------------------------------------------------------------------------------------------------------

#' Translates italian abbreviated internet slang into the corresponding phrase
#'
#' @param sentences The sentences array in which operate the word substitution
#' @return The corrected sentences
#' @examples
#' sentences <- c("Ho subito un intervento di poem e cmq sto ancora male", "Nn ne posso pi?")
#' corrected_sentences <- translateItalianSlang(sentences)
#' c("ho subito un intervento di poem e comunque sto ancora male", "non ne posso pi?")
translateSlang <- function(sentences, slang_words) {
  
  # Replace dot in order to escape regex meaning as "any character"
  slang_words$slang <- gsub('.', '\\.', slang_words$slang, fixed=T)
  
  # Replace pattern slang words in order to make gsub replace entire words
  slang_words$slang <- paste0('\\<', slang_words$slang, '\\>')
  
  # Define a function for slang-phrase substitution inside all input sentences
  # fixed = false enables regex
  return(textclean::mgsub(x = tolower(sentences), pattern = as.vector(slang_words$slang), replacement = as.vector(slang_words$phrase), fixed = F))
  
}

#  --------------------------------------------------------------------------------------------------
# | STEP 7. Removes URLs                                                                             |
# | Source: https://stackoverflow.com/questions/42460803/regex-in-r-remove-multiple-urls-from-string |
#  --------------------------------------------------------------------------------------------------

removeURLs <- function(message) {
  message <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", " ", message)
  return(message)
}

removeAllURLs <- function(messages) {
  return(purrr::map_chr(messages, removeURLs))
}

#  ----------------------------------------------------------
# | STEP 8. Removes extra spaces between words inside a text |
#  ----------------------------------------------------------

removeUnnecessarySpaces <- function(message) {
  # Removes the spaces at the beginning and at the end of the text
  message <- gsub("^[[:blank:]]*", "", message)
  message <- gsub("[[:blank:]]*$", "", message)
  
  # Removes repeated internal spaces
  message <- gsub("\\s+", " ", message)
  
  return(message)
}

removeAllUnnecessarySpaces <- function(messages) {
  return(purrr::map_chr(messages, removeUnnecessarySpaces))
}

replaceSpacesWithUnderscore <- function(messages) {
  messages <- gsub(" ", "_", messages)
  return(messages)
}



#  -------------------------------------------------------------------------------------------------------
# | Words Correction                                                                                      |
# | https://rustyonrampage.github.io/text-mining/2017/11/28/spelling-correction-with-python-and-nltk.html |
#  -------------------------------------------------------------------------------------------------------

#  ---------------------------------
# | STEP 9. Fixing Word Lengthening |
#  ---------------------------------

# Word Lengthening is a type of spelling mistake in which characters within a word are repeated wrongly
# (for example, "faaantasticooooo" instead of "fantastico").
# Italian words (as well as english words) have a max of two repeated characters.
# Additional characters need to ripped off, otherwise we might add misleading information.
# The next function rip offs repeated characters more than 2.
# It would fail in the case of words that have actually no repeated characters at all
# (for example, "faaantasticooooo" will return "faantasticoo").
# So we need spell correction after word lengthening to get the correct word.
# Mentioned also: https://hub.packtpub.com/clean-social-media-data-analysis-python/

source_python("source/utils/preprocessing/word_lengthening.py")



#  -----------------------------------------------------------------
# | Pipeline                                                        |
#  -----------------------------------------------------------------

cleanDocuments <- function(messages, slang_words) {
  # Step 1
  message('uniformizing text format encoding...')
  messages <- uniformTextFormatEncoding(messages)
  # Step 2
  message('normalizing symbols...')
  messages <- normalizeAllSymbols(messages)
  # Step 3
  message('normalizing emotes...')
  messages <- normalizeAllEmotes(messages)
  # Step 4
  message('splitting attached words...')
  messages <- splitAllAttachedWords(messages)
  # Step 5
  message('lowercasing text...')
  messages <- toLowerCase(messages)
  # Step 6
  message('translating italian slang...')
  messages <- translateSlang(messages, slang_words)
  # Step 7
  message('removing URLs...')
  messages <- removeAllURLs(messages)
  # Step 8
  message('removing unnecessary spaces...')
  messages <- removeAllUnnecessarySpaces(messages)
  # Step 9
  message('reducing lengthening...')
  messages <- reduceAllLengthening(messages)
  
  return(messages)
}