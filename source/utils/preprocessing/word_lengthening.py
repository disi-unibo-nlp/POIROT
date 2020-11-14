import re

#' Rip offs repeated characters more than 2.
#'
#' @param message The string to analyze
#' @return The string with a maximum of two repeated characters
#' @examples
#' reduceLengthening("faaantasticooooo")
#' "faantasticoo"
def reduceLengthening(message):
  pattern = re.compile(r"(.)\1{2,}")
  return pattern.sub(r"\1\1", message)

def reduceAllLengthening(messages):
  if isinstance(messages, str):
    messages = [messages]
  reduced_messages = map(lambda m: reduceLengthening(m), messages)
  return list(reduced_messages)