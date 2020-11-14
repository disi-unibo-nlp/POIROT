# Download italian resources for lemmatization
# pip install spacy
# python -m spacy download it

import spacy
import re

# Initialize spacy 'it' model, keeping only tagger component (for efficiency)
nlp_it = spacy.load('it', disable=['parser', 'ner'])
nlp_en = spacy.load('en', disable=['parser', 'ner'])

def lemmatize(doc, lang, pattern_to_ignore = None):
	lemmatized_words = []
	nlp = nlp_it if (lang == "it") else nlp_en
	if pattern_to_ignore is not None:
		pattern = re.compile(pattern_to_ignore)
	for word in doc.split(" "):
		word_res = nlp(word)
		lemmatized_words.append(word if ((pattern_to_ignore is not None and pattern.match(word)) or len(word_res) == 0) else word_res[0].lemma_)
	lemmatized_doc = " ".join(lemmatized_words)
	return lemmatized_doc
