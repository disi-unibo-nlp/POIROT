##########################################################################
## Custom lsa_0.73.1 query implementation
## ---------------
## Avoids symbol replacements in order to use also tagged terms
##########################################################################

query <- function( qtext, termlist, stemming=FALSE, language="german" ) {
    
    # qtext: string with the query words, whitespace separated
    # termlist: list of allowed terms
    # dtm: original doc-term-matrix (no weighting applied!)
    
    dtm = NULL
    
    q = strsplit( gsub('[[:space:]]+', ' ', tolower(qtext) ), " ")[[1]]
    vec = vector( mode="numeric", length(termlist) )
    for ( word in q ) {
        if (stemming) word = wordStem(word, language)
        if (word != "") {
            vec[ match(word,termlist) ] = vec[ match(word,termlist) ] + 1
        }
    }
    
    dtm = as.matrix(vec)
    colnames(dtm) = toupper(qtext)
    rownames(dtm) = termlist
    
    environment(dtm) = new.env()
    class(dtm) = "textmatrix"
    
    return ( dtm )
    
}

