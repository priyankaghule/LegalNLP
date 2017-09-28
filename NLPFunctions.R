library(pdftools)
library(stringr)
library(dplyr)
library(coreNLP)
library(openNLPmodels.en)
library(NLP) 
library(openNLP)

#Preprocessing the Text
preprocessText <- function(text){
  # clean up sentences with R's regex-driven global substitute, gsub():
  sentence = gsub('[[:punct:]]', '', text);
  sentence = gsub('[[:cntrl:]]', '', sentence);
  #sentence = gsub('\\d+', '', sentence);
  sentence = gsub("@\\w+ *", "", sentence);
  # and convert to lower case:
  sentence = tolower(sentence);
  return(sentence)
}
#Split the words
getSplitWords <- function(words){
  words = str_split(words, '\\s+');
  words = unlist(words);
  return(words)
}

#Get Tokenized
getTokensandTags <- function(sentence){
  
  #Converting it to a string
  sentence <- as.String(sentence);
  
  # Before POS tagging, we need to do Sentence annotation followed by word annotation
  wordAnnotation <- annotate(sentence, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()));
  
  # POS tag the words & extract the "words" from the output
  POSAnnotation <- annotate(sentence, Maxent_POS_Tag_Annotator(), wordAnnotation);
  POSwords <- subset(POSAnnotation, type == "word");
  # Extract the tags from the words
  tags <- sapply(POSwords$features, '[[', "POS");
  
  # Create a data frame with words and tags
  tokenizedAndTagged <- data.frame(Tokens = sentence[POSwords], Tags = tags);
  
  return (tokenizedAndTagged)
}

# Tag Part of Speech to Sentences
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

#Chunking and Extraction
chunkingAndExtraction <- function(tokenizedAndTagged,chunkToExtract,chunkToSelect){
  
  # Define a flag(tags_mod) for pos tags - Flag set to 1 if it contains the POS tag we are interested in else 0
  # In this case we only want Noun and Adjective tags (NN, JJ)
  # Note that this will also capture variations such as NNP, NNPS etc
  tokenizedAndTagged$Tags_mod = grepl(chunkToExtract, tokenizedAndTagged$Tags);
  
  # Initialize a vector to store chunk indexes
  chunk = vector();
  
  # Iterate thru each word and assign each one to a group
  # if the word doesn’t belong to 'chunkToExtract' tags (i.e. tags_mod flag is 0) assign it to the default group (0)
  # If the ith tag is in “chunkToExtract” (i.e. tags_mod flag is 1) assign it to group i-1 if the (i-1)th tag_mod flag is also 1; else assign it to a new group
  chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1]);
  for (i in 2:nrow(tokenizedAndTagged)) {
    
    if(!tokenizedAndTagged$Tags_mod[i]) {
      chunk[i] = 0;
    } else if (tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]) {
      chunk[i] = chunk[i-1];
    } else {
      chunk[i] = max(chunk) + 1;
    }
    
  }
  
  # Split and chunk words
  text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk);
  tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk);
  names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"));
  #print(len(names(text_chunk)))
  # Extract chunks matching pattern
  # We will extract JJ-NN chunks and two or more continuous NN tags 
  # "NN.-NN" -> The "." in this regex will match all variants of NN: NNP, NNS etc
  res = text_chunk[grepl(chunkToSelect, names(text_chunk))];
  return(res)
}

## Extract PETITIONER and RESPONDENT
getPartiesGroup <- function(words,pChunk,rChunk){
  
  petitionerType = ''
  respondentType = ''
  
  ### Get Petitioner and Respondent Type
  for(w in words){
    if(w == 'plaintiff' || w == 'apellant' || w == 'petitioner' || w == 'applicant') {
      petitionerType = w;
    }
    if(w == 'respondent' || w == 'defendant') {
      respondentType = w;
    }
  }
  
  ### Get Petitioner Name
  dd_V1 <- tagPOS(words)$POStagged
  
  # Get the constrol sequence IDs (probably with RegEx nicer to do...)
  tags <- sapply(strsplit(strsplit(dd_V1,"/")[[1]][-1]," "),"[",1)
  #Get the length
  totalTags <- length(tags)
  searchLength <- length(pChunk)
  
  # Loop through all subvectors and store starting points of possible matches
  startPoints <- c()
  for(i in 1:(totalTags-searchLength)){
    if(identical(tags[i:(i+searchLength-1)], pChunk)) startPoints <- c(startPoints,i)
  }
  
  #Petitioner Name
  petitionerName <- words[(startPoints+1):(startPoints+searchLength)]
  petitionerName <- paste(petitionerName, collapse = ' ')
  
  #Get Respondant NAME
  searchLength <- length(rChunk)
  
  # Loop through all subvectors and store starting points of possible matches
  startPoints <- c()
  for(i in 1:(totalTags-searchLength)){
    if(identical(tags[i:(i+searchLength-1)], rChunk)) startPoints <- c(startPoints,i)
  }
  
  #Petitioner Name
  respondentName <- words[(startPoints+1):(startPoints+searchLength)]
  respondentName <- paste(respondentName, collapse = ' ')
  
  partyNames <- c(petitionerName,respondentName)
  
  output <- list(petitionerType,respondentType,partyNames)
  return(output)
}

# Get Petitioner/Respondent Paragraphs
getCounselParaWithTag <- function(words,phrase) {
  matches <- which(words=="for")
  
  if(phrase == 'petitioner'){
    index <- matches[1]
    counselPara = words[(index-22):(index-1)]
  }
  if(phrase == 'respondent'){
    index <- matches[2]
    counselPara = words[(index-16):(index-1)]
  }
  return(counselPara)
}

# Get Advocates Group for both Petitioner and Respondent
getCounselGroup <- function(paraTagged,advocates){
  ### Get POS Tagged
  dd_V1 <- tagPOS(paraTagged)$POStagged
  advNames = c()
  
  # Get the constrol sequence IDs (probably with RegEx nicer to do...)
  tags <- sapply(strsplit(strsplit(dd_V1,"/")[[1]][-1]," "),"[",1)
  #Get the length
  totalTags <- length(tags)
  for(a in advocates){
    # Loop through all subvectors and store starting points of possible matches
    startPoints <- c()
    for(i in 1:(totalTags)){
      if(identical(tags[i:(i+(length(a))-1)], a)) { 
        startPoints <- c(startPoints,i) 
        break
        }
    }
    #Petitioner Name
    advName <- paraTagged[(startPoints):(startPoints+(length(a))-1)]
    advName <- paste(advName, collapse = ' ')
    advNames <- c(advNames,advName)
  }
  return(advNames)
}

getCoramGroup <- function(words){
  judgePosition = ''
  judgeName = ''
  
  matches <- which(words=="coram")
  coramDetails = words[(matches+1):(matches+3)]
  judgeName = coramDetails[1:2]
  judgeName = paste(judgeName, collapse = ' ')
  judgePosition = coramDetails[3]
  details = c(judgeName, judgePosition)
  return(details)
}

getWordChunksFromPara <- function(words,chunks,type){
  ### Get POS Tagged
  wordsTagged <- tagPOS(words)$POStagged
  
  # Get the constrol sequence IDs (probably with RegEx nicer to do...)
  tags <- sapply(strsplit(strsplit(wordsTagged,"/")[[1]][-1]," "),"[",1)
  #Get the length
  totalTags <- length(tags)
  extract <- c()
  for(a in chunks){
    # Loop through all subvectors and store starting points of possible matches
    startPoints <- c()
    for(i in 1:(totalTags)){
      if(identical(tags[i:(i+(length(a))-1)], a)) { 
        startPoints <- c(startPoints,i) 
        break
      }
    }
    #Join together
    if(type == 'day'){
      chunk <- words[(startPoints):(startPoints+length(a)-1)]
      date <- chunk[1]
      if(date == '10th'){
        date = '10'
      }
      if(date == '13th'){
        date = '13'
      }
      month <- chunk[2]
      year <- chunk[3]
      chunk <- paste(chunk, collapse = ' ')
      chunk <- c(date,month,year,chunk)
      extract <- c(extract, chunk)
      
    } else {
      chunk <- words[(startPoints):(startPoints+length(a)-1)]
      chunk <- paste(chunk, collapse = ' ')
      extract <- c(extract, chunk)
    }
  }
  return(extract)
}

## Extract Dates

getDateDetail <- function(words){
  date = c()
  month = c()
  year = c()
  type = c()
  day = c()
  
  matches <- which(words=="order")
  dateDetailPara = words[(matches+1):(matches+11)]
  
  #Define what to extract for Date Type
  res = c('VBN')
  pro = c('VBD')
  type = list(res,pro)
  dateType = getWordChunksFromPara(dateDetailPara,type,'type')
  #print(type[1])
  #Define what to extract for date
  res = c('JJ','NNP','CD')
  pro = c('JJ','NN','CD')
  type = list(res,pro)
  day = getWordChunksFromPara(dateDetailPara,type,'day')
  total <- c(dateType,day)
  return(total)
}

getJudgementDetails <- function(words){
  judgementType = c()
  citePara = c()
  citeTitle = c()
  citation = c()
  actId = ''
  actTitle = c()
  actPara = c()
  sectionId = ''
  sectionTitle = c()
  everything = c()
  actDetail = c()

  for(w in words){
    if(w == 'judgement' || w == 'order' || w == 'p c'){
      judgementType = w
    }
  }
  #Cite title and citation
  matches <- which(words=="cited")
  #print(matches)
  citePara = words[(matches-25):(matches)]
  #print(tagPOS(citePara)$POStagged)
  
  chunkCiteTitle <- c('DT','JJ','NN','IN','NN','NNS','RB')
  chunkCitation <- c('JJ','NN','IN','JJ','NN')
  type <- list(chunkCiteTitle,chunkCitation)
  citeationDetail = getWordChunksFromPara(citePara,type,'cited')
  
  #ACT and Section
  matches <- which(words=="1999")
  actPara = words[(matches-10):(matches+1)]
  actTitle <- c('NN','NNS','VBP','CD')
  section <- c('CD','NN')
  type <- list(actTitle,section)
  actAndSection <- getWordChunksFromPara(actPara,type,'act')
  #Get Act and Sec
  if(actAndSection[1] == "trade marks act 1999"){
    actId = 'tma-1999'
  }
  if(actAndSection[2] == "9 c"){
    sectionId = 'tma-1999-9c'
  }
  Ids <- c(actId,sectionId)
  
  #Return everything
  everything <- c(judgementType,citeationDetail,actAndSection,Ids)
  return(everything)
}
