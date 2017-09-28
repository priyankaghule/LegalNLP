posText<- "The VeriFone is not working, when customers slide card nothing happens. The screen is frozen. We rebooted but it did not help."
posText1<- c("The VeriFone is not working","scanner is not scanning","printer offline","when customers slide card nothing happens. The screen is frozen. We rebooted but it did not help.")

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
sent <- as.String(sentence[[1]])
dd1 <- do.call(rbind, strsplit(as.character(sentence[[1]]), ' '))
print(dd1)
dd_V1 <- tagPOS(dd1)$POStagged
dd_V1

# Get the constrol sequence IDs (probably with RegEx nicer to do...)
tags <- sapply(strsplit(strsplit(dd_V1,"/")[[1]][-1]," "),"[",1)
print(tags)
# Define constants
matchSeq <- c('NN', 'NNS','VBN')
matchSeq <- c('JJ','JJ','NN','NNS','VBN')
totalTags <- length(tags)
searchLength <- length(matchSeq)

# Loop through all subvectors and store starting points of possible matches
startPoints <- c()
for(i in 1:(totalTags-searchLength)){
  if(identical(tags[i:(i+searchLength-1)], matchSeq)) startPoints <- c(startPoints,i)
}
print(startPoints)
print(str_split(sent,' '))

# Print results, if there are any
if(!is.null(startPoints)) paste(dd1[startPoints:(startPoints+searchLength-1)], collapse=" ")