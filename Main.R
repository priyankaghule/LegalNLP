source("NLPFunctions.R")
library(XML)

# 1. Get the Text :: Converting pdf to text file
pdfToText <- pdf_text("Docs/ordjud.pdf")

# 1.2 Preprocess the text
sentence <- preprocessText(pdfToText)
words <- getSplitWords(sentence[1])

# 2. Get Court Name
matches <- which(words=="bombay")
if(!is.null(matches)) {
  courtName = 'bombay'
}

## 3.1 Extract PETITIONER and RESPONDENT
print(tagPOS(words)$POStagged)
petitionerChunk <- c('NN', 'NNS','VBN')
respondantChunk <- c('JJ','JJ','NN','NNS','VBN')
parties <- getPartiesGroup(words,
                           petitionerChunk,
                           respondantChunk
                           )
petitionerType <- as.String(parties[1])
respondentType <- as.String(parties[2])

petitionerName <- as.String(parties[[3]][1])
respondentName <- as.String(parties[[3]][2])

#Get Counsel Group

## FOR PETITIONER
petitionerCounselPara <- getCounselParaWithTag(words,'petitioner')
print(tagPOS(petitionerCounselPara)$POStagged)
adv1Chunk <- c('NN','NN','NN')
adv2Chunk <- c('JJ','JJ','NNS')
adv3Chunk <- c(',','NN','NNS')
adv4Chunk <- c('JJ','NN','NN')
adv5Chunk <- c('NNS','VBG','NNS')
petitionerAdvs <- list(adv1Chunk,adv2Chunk,adv3Chunk,adv4Chunk,adv5Chunk)
petitionerAdvocates <- getCounselGroup(petitionerCounselPara,petitionerAdvs)

## FOR RESPONDENT
respondentCounselPara <- getCounselParaWithTag(words,'respondent')
print(tagPOS(respondentCounselPara)$POStagged)
adv1Chunk <- c('NN','NN','NN')
adv2Chunk <- c('JJ','JJ','NN')
adv3Chunk <- c('RB','JJ')
respondentAdvs <- list(adv1Chunk,adv2Chunk,adv3Chunk)
respondentAdvocates <- getCounselGroup(respondentCounselPara,respondentAdvs)


## Get Coram Details
coramDetails <- getCoramGroup(words)

##Get Dates Detail
dateDetail <- getDateDetail(words)
print(dateDetail)
rType <- dateDetail[1]
pType <- dateDetail[2]

rDate<- dateDetail[3]
rMonth <- dateDetail[4]
rYear <- dateDetail[5]
rDay <- dateDetail[6]

pDate <- dateDetail[7]
pMonth <- dateDetail[8]
pYear <- dateDetail[9]
pDay <- dateDetail[10]

#Get judgement detail
words <- getSplitWords(sentence)

judgementDetail <- getJudgementDetails(words)
jTitle <- judgementDetail[1]
citeTitle <- judgementDetail[2]
citation <- judgementDetail[3]
actTitle <- judgementDetail[4]
secTitle <- judgementDetail[5]
actId <- judgementDetail[6]
secId <- judgementDetail[7]


##CREATING XML FILE
#Generate XML File
xmlCase = newXMLNode('Case')

#Insert Court Details
xmlCourt = newXMLNode('Court', parent = xmlCase)
xmlCourtName = newXMLNode('CourtName', courtName, parent = xmlCourt)

#Insert Parties group
xmlPartiesGroup = newXMLNode('PartiesGroup', parent = xmlCase)
xmlParties = newXMLNode('Parties', parent = xmlPartiesGroup)
xmlPetitionerGroup = newXMLNode('PetitionerGroup', parent= xmlParties)
xmlPetitioner = newXMLNode('Petitioner', petitionerName, attrs = c(Type=petitionerType), parent = xmlPetitionerGroup)

xmlRespondentGroup = newXMLNode('RespondentGroup', parent = xmlParties)
xmlRespondent = newXMLNode('Respondent', respondentName , attrs = c(Type=respondentType), parent = xmlRespondentGroup)

#Insert Counsel group
xmlCounselGroup = newXMLNode('CounselGroup', parent = xmlCase)

xmlforPetitioner = newXMLNode('forPetitioner', parent = xmlCounselGroup)
for (adv in petitionerAdvocates) {
  xmlCounselName = newXMLNode('CounselName', adv , parent = xmlforPetitioner) 
}

xmlforRespondent = newXMLNode('forRespondent', parent = xmlCounselGroup)
for (adv in respondentAdvocates) {
  xmlCounselName = newXMLNode('CounselName', adv, parent = xmlforRespondent) 
}

#Insert Coram group
xmlCoramGroup = newXMLNode('CoramGroup', parent = xmlCase)
xmlJudge = newXMLNode('Judge', coramDetails[1] ,attrs = c(Position = coramDetails[2]), parent = xmlCoramGroup)

#Insert Date:
xmlDate = newXMLNode('Date', pDay, attrs = c(Month = pMonth, Date = pDate, Year = pYear, Type = pType) , parent = xmlCase)
xmlDate = newXMLNode('Date', rDay, attrs = c(Month = rMonth, Date = rDate, Year = rYear, Type = rType) , parent = xmlCase)

#Insert Judgement Group:
xmlJudgementGroup = newXMLNode('JudgementGroup', attrs = c(Title=jTitle), parent = xmlCase)
xmlPara = newXMLNode('Para', parent = xmlJudgementGroup)
xmlCite = newXMLNode('Cite', parent = xmlPara)
xmlCiteTitle = newXMLNode('Title', citeTitle, parent = xmlCite)
xmlCitation = newXMLNode('Citation', citation ,parent = xmlCite)

xmlAct = newXMLNode('Act', parent = xmlPara)
xmlActTitle = newXMLNode('Title' , actTitle,attrs = c(id = actId), parent = xmlAct)

xmlSec = newXMLNode('SecRef', parent = xmlPara)
xmlSecTitle = newXMLNode('Title' , secTitle,attrs = c(id = secId), parent = xmlSec)

saveXML(xmlCase, file="ordjud.xml")

