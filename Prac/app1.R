library(pdftools)
library(stringr)
#library(tm)
#library(coreNLP)
#library(pdftables)
#library(wordcloud)
#library(Rstem)
#library(tokenizers)
#library(openNLPmodels.en)
#library(NLP) 
#library(openNLP)
#library(rentrez)

#install.packages(c('pdftools','tm','coreNLP','pdftables','wordcloud','Rstem','tokenizers','NLP','openNLP','rentrez'))

#Converting pdf to text file
text <- pdf_text("ordjud.pdf")
head(text[[1]])

#Preprocessing in the text
# clean up sentences with R's regex-driven global substitute, gsub():
sentence = gsub('[[:punct:]]', '', text)
sentence = gsub('[[:cntrl:]]', '', sentence)
sentence = gsub('\\d+', '', sentence)
sentence = gsub("@\\w+ *", "", sentence)
print(sentence[[1]])
# and convert to lower case:
sentence = tolower(sentence)
print(sentence[[1]])

# split into words. str_split is in the stringr package
word.list = str_split(sentence, '\\s+')
print(word.list) #Page wise list of words

words = unlist(word.list)
print(words[1:50])

#Tokenize the words in the text
words <- tokenize_words(text)
print(words[[1]])

head(strsplit(text[[1]], "\n")[[1]])
print(text)
write.table(text, "ordjud.txt")

#tokenizing the text
text_tokenize = tokenize_words(text)
print(text_tokenize)
sentence_tokenize = tokenize_sentences(text)
print(sentence_tokenize)
 corpus <- Corpus(VectorSource(text))
 print(corpus)

  tagPOS <-  function(x, ...) {
   s <- as.String(text)
   word_token_annotator <- Maxent_Word_Token_Annotator()
   a2 <- Annotation(1L, "sentence", 1L, nchar(s))
   a2 <- annotate(s, word_token_annotator, a2)
   a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
   a3w <- a3[a3$type == "word"]
   POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
   POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
   list(POStagged = POStagged, POStags = POStags)}
 str <- "this is a the first sentence."
 tagged_str <-  tagPOS(text)
print(tagged_str$POStagged)


results <- entrez_search(text[1], term = "jurisdiction", retmax = 10)

is.String(text)
text <- as.String(text)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
print(sent_token_annotator)
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(text,
               list(sent_token_annotator,
                    word_token_annotator,
                    pos_tag_annotator))
print(a3)
annotate(text, Maxent_Chunk_Annotator(), a3)
annotate(text, Maxent_Chunk_Annotator(probs = TRUE), a3)

a2 <- annotate(text, list(sent_token_annotator, word_token_annotator))
## Entity recognition for persons.
entity_annotator <- Maxent_Entity_Annotator()
entity_annotator
annotate(text, entity_annotator, a2)
## Directly:
entity_annotator(text, a2)
## And slice ...
text[entity_annotator(text, a2)]
## Variant with sentence probabilities as features.
annotate(text, Maxent_Entity_Annotator(probs = TRUE), a2)

a3 <- annotate(text, pos_tag_annotator, a2)
a3
## Variant with POS tag probabilities as (additional) features.
head(annotate(text, Maxent_POS_Tag_Annotator(probs = TRUE), a2))
## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)
## Extract token/POS pairs (all of them): easy.
sprintf("%s/%s", text[a3w], tags)
## Extract pairs of word tokens and POS tags for second sentence:
a3ws2 <- annotations_in_spans(subset(a3, type == "word"),
                              subset(a3, type == "sentence")[2L])[[1L]]
sprintf("%s/%s", text[a3ws2], sapply(a3ws2$features, `[[`, "POS"))

install.packages('magrittr')
library(magrittr)

text <- as.String(text)
text_annotations <- annotate(text, list(sent_token_annotator, word_token_annotator))
class(text_annotations)
head(text_annotations)
text_doc <- AnnotatedPlainTextDocument(text, text_annotations)
sents(text_doc) %>% head(2)
words(text_doc) %>% head(10)
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
pipeline <- list(sent_token_annotator,
                 word_token_annotator,
                 person_ann,
                 location_ann,
                 organization_ann)
text_annotations <- annotate(text, pipeline)
text_doc <- AnnotatedPlainTextDocument(text, text_annotations)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(text_doc, kind = "person")
entities(text_doc, kind = "location")
entities(text_doc, kind = "organization")


text <- as.String(text)
# Before POS tagging, we need to do Sentence annotation followed by word annotation
wordAnnotation <- annotate(text, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))

# POS tag the words & extract the "words" from the output
POSAnnotation <- annotate(text, Maxent_POS_Tag_Annotator(), wordAnnotation)
POSwords <- subset(POSAnnotation, type == "word")
# Extract the tags from the words
tags <- sapply(POSwords$features, '[[', "POS")

# Create a data frame with words and tags
tokenizedAndTagged <- data.frame(Tokens = text[POSwords], Tags = tags)

# Define a flag(tags_mod) for pos tags - Flag set to 1 if it contains the POS tag we are interested in else 0
# In this case we only want Noun and Adjective tags (NN, JJ)
# Note that this will also capture variations such as NNP, NNPS etc
tokenizedAndTagged$Tags_mod = grepl("NN|JJ", tokenizedAndTagged$Tags)

# Initialize a vector to store chunk indexes
chunk = vector()  

# Iterate thru each word and assign each one to a group
# if the word doesn’t belong to NN|JJ tags (i.e. tags_mod flag is 0) assign it to the default group (0)
# If the ith tag is in “NN|JJ” (i.e. tags_mod flag is 1) assign it to group i-1 if the (i-1)th tag_mod flag is also 1; else assign it to a new group
chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
for (i in 2:nrow(tokenizedAndTagged)) {
  
  if(!tokenizedAndTagged$Tags_mod[i]) {
    chunk[i] = 0
  } else if (tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]) {
    chunk[i] = chunk[i-1]
  } else {
    chunk[i] = max(chunk) + 1
  }
  
}
# Split and chunk words
text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
names(text_chunk) <- sapply(tag_pattern, function(text) paste(text, collapse = "-"))

# Extract chunks matching pattern
# We will extract JJ-NN chunks and two or more continuous NN tags 
# "NN.-NN" -> The "." in this regex will match all variants of NN: NNP, NNS etc
res = text_chunk[grepl("JJ-NN|NN.-NN", names(text_chunk))]
res = sapply(res, function(x) paste(x, collapse =  " "))
print(res)
