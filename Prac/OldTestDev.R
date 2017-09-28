library(pdftools)
library(stringr)
#library(tm)
library(coreNLP)
#library(pdftables)
#library(wordcloud)
#library(Rstem)
#library(tokenizers)
library(openNLPmodels.en)
library(NLP) 
library(openNLP)
#library(rentrez)

install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
                 repos=NULL, type="source")
#install.packages(c('pdftools','tm','coreNLP','pdftables','wordcloud','Rstem','tokenizers','NLP','openNLP','rentrez'))

#Converting pdf to text file
text <- pdf_text("ordjud.pdf")
head(text[[1]])
"
Clean_String <- function(string){
# Lowercase
temp <- tolower(string)
#' Remove everything that is not a number or letter (may want to keep more 
#' stuff in your actual analyses). 
temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
# Shrink down to just one white space
temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
# Split it
temp <- stringr::str_split(temp, " ")
# Get rid of trailing "" if necessary
indexes <- which(temp == "")
if(length(indexes) > 0){
temp <- temp[-indexes]
} 
return(temp)
}
"
#cText <- Clean_String(text)

#print(cText)
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
print(word.list[[1]]) #Page wise list of words

words = unlist(word.list)
#print(words[1:50])

word.list = as.character(word.list)
print(word.list[[1]])
#First Task: Finding the name of the Court
for(i in 1:length(words)) {
  if(words[i] == 'judicature'){
    courtName = words[i+2]
    break
  }
}
courtName

#Second Task: Finding Party types and their names
for(i in 1:length(words)){
  if( words[[i]] == 'applicant'){
    min = i-5
    max = i-1
    applicantParty = words[min:max]
    break
  }
}
#Tag the Part of Speech annotators into tokenized Sentence

text <- as.String(words)
applicantParty <- as.String(applicantParty)
# Before POS tagging, we need to do Sentence annotation followed by word annotation
wordAnnotation <- annotate(applicantParty, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))

# POS tag the words & extract the "words" from the output
POSAnnotation <- annotate(applicantParty, Maxent_POS_Tag_Annotator(), wordAnnotation)


pos_tag_annotator <- Maxent_POS_Tag_Annotator()
POSedSentence <- annotate(s, pos_tag_annotator, tokenizedSent)

sentWords <- subset(POSAnnotation, type == "word")
print(sentWords)
sentWordsWithTags <- sapply(sentWords$features, `[[`, "POS")
sentWordsWithTags
sentWordsWithTags <- sprintf("%s/%s", applicantParty[sentWords], sentWordsWithTags)
print(sentWordsWithTags[1:5])

print(partiesPara)
"
stringSentence <- as.String(sentence)
print(stringSentence)
#Import the Tokenizers
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
tokenizedSent <- annotate(stringSentence, list(sent_token_annotator, word_token_annotator))
print(tokenizedSent)
#Tag the Part of Speech annotators into tokenized Sentence
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
POSedSentence <- annotate(s, pos_tag_annotator, tokenizedSent)
sentWords <- subset(POSedSentence, type == "word")
print(sentWords)
sentWordsWithTags <- sapply(sentWords$features, `[[`, "POS")
sentWordsWithTags
sentWordsWithTags <- sprintf("%s/%s", stringSentence[sentWords], sentWordsWithTags)
print(sentWordsWithTags[1:10])