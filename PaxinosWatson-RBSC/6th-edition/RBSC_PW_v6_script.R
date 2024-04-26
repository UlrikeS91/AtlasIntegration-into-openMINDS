setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS")

library("jsonlite")
library("writexl")
library("readxl")
library("tibble")
library("tidyr")
library("dplyr")
library("stringr")

#########################################################################################################################################
###SKIP IF COMBINED TERMINOLOGY HAS BEEN GENERATED!
#########################################################################################################################################
{
#########################################################################################################################################
###Read excel sheets:
#Terminology from "Index of Structures" from Paxinos & Watson RBSC 6th edition:
PAX_v6_Terms.S <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/RBSC_PW_v6_terminology-IndexOfStructures.xlsx")

#Terminology from "Index of Abbreviations" from Paxinos & Watson RBSC 6th edition:
PAX_v6_Terms.A <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/RBSC_PW_v6_terminology-IndexOfAbbreviations.xlsx")

#########################################################################################################################################

#########################################################################################################################################
###Terminology file applies to the entire 6th edition of the atlas. Script only generates files for the CORONAL atlas plates!
#########################################################################################################################################

#########################################################################################################################################
###Split abbreviation and name from terminology files:
###For structure list:

PAX_v6_Terms.S_labels <- data.frame("nameAbbrev" = PAX_v6_Terms.S$name, "name" = PAX_v6_Terms.S$name, "lookup" = NA, "abbreviation" = NA)

for(i in 1:length(PAX_v6_Terms.S_labels$nameAbbrev)){
  PAX_v6_Terms.S_labels$abbreviation[i] <- strsplit(PAX_v6_Terms.S_labels$nameAbbrev[i], " ")[[1]][length(strsplit(PAX_v6_Terms.S_labels$nameAbbrev[i], " ")[[1]])]
  PAX_v6_Terms.S_labels$name[i] <- substr(PAX_v6_Terms.S_labels$name[i], 1, (nchar(PAX_v6_Terms.S_labels$name[i])-nchar(PAX_v6_Terms.S_labels$abbreviation[i])-1))
  PAX_v6_Terms.S_labels$lookup[i] <- PAX_v6_Terms.S_labels$name[i]
  PAX_v6_Terms.S_labels$name[i] <- str_trim(PAX_v6_Terms.S_labels$name[i])
}

count_Terms.S <- data.frame(table(PAX_v6_Terms.S_labels$name))
###region names "stratum lucidum of the hippocampus" and "subiculum" have been listed twice in "List of Structures"
count_Terms.S <- data.frame(table(PAX_v6_Terms.S_labels$abbreviation))
###abbreviation count confirms "stratum lucidum of the hippocampus" and "subiculum" as duplicates
###abbreviation "STS" exists twice in "List of Structures" - one as "bed nucleus of the stria terminalis, supracapsular division" and 
#####another as "bed nucleus of stria terminalis, supracapsular division" (with and without "the")

###For abbreviation list:
PAX_v6_Terms.A_labels <- data.frame("nameAbbrev" = PAX_v6_Terms.A$name, "name" = PAX_v6_Terms.A$name, "lookup" = NA, "abbreviation" = NA)

for(i in 1:length(PAX_v6_Terms.A_labels$nameAbbrev)){
  PAX_v6_Terms.A_labels$abbreviation[i] <- strsplit(PAX_v6_Terms.A_labels$nameAbbrev[i], " ")[[1]][1]
  PAX_v6_Terms.A_labels$name[i] <- substr(PAX_v6_Terms.A_labels$name[i], nchar(PAX_v6_Terms.A_labels$abbreviation[i])+2, nchar(PAX_v6_Terms.A_labels$name[i]))
  PAX_v6_Terms.A_labels$lookup[i] <- PAX_v6_Terms.A_labels$name[i]
  PAX_v6_Terms.A_labels$name[i] <- str_trim(PAX_v6_Terms.A_labels$name[i])
}

count_Terms.A <- data.frame(table(PAX_v6_Terms.A_labels$name))
###no duplicated region names in "List of Abbreviations"
count_Terms.A <- data.frame(table(PAX_v6_Terms.A_labels$abbreviation))
###abbreviation count confirms this again

###Comparison between "List of Structures" and "List of Abbreviations":
###Names:
PAX_v6_Terms.A <- data.frame("name" = PAX_v6_Terms.A_labels$name)
PAX_v6_Terms.S <- data.frame("name" = PAX_v6_Terms.S_labels$name)
Terms.comp.name <- rbind(PAX_v6_Terms.A, PAX_v6_Terms.S)
count.name <- data.frame(table(Terms.comp.name$name))

names(count.name)[1] <- "regionName"
names(count.name)[2] <- "occurance"

count.name$source <- NA
for(i in 1:length(PAX_v6_Terms.A$name)){
  for(j in 1:length(count.name$regionName)){
    if(PAX_v6_Terms.A$name[i] == count.name$regionName[j]){
      count.name$source[j] <- "ListOfAbbreviations"
    }
  }
}
for(i in 1:length(PAX_v6_Terms.S$name)){
  for(j in 1:length(count.name$regionName)){
    if(PAX_v6_Terms.S$name[i] == count.name$regionName[j]){
      count.name$source[j] <- "ListOfStructures"
    }
  }
}
for(i in 1:length(count.name$regionName)){
  if(count.name$occurance[i] == 2){
    count.name$source[i] <- "both" 
  }
}
for(i in 1:length(count.name$regionName)){
  if(count.name$regionName[i] == "subiculum"){
    count.name$source[i] <- "twice in ListOfStructures" 
  }
}
for(i in 1:length(count.name$regionName)){
  if(count.name$occurance[i] == 3){
    count.name$source[i] <- "twice in ListOfStructures & once in ListOfAbbreviations" 
  }
}

###Abbreviations:
PAX_v6_Terms.A <- data.frame("abbrev" = PAX_v6_Terms.A_labels$abbreviation)
PAX_v6_Terms.S <- data.frame("abbrev" = PAX_v6_Terms.S_labels$abbreviation)
Terms.comp.abbrev <- rbind(PAX_v6_Terms.A, PAX_v6_Terms.S)
count.abbrev <- data.frame(table(Terms.comp.abbrev$abbrev))

names(count.abbrev)[1] <- "regionAbbrev"
names(count.abbrev)[2] <- "occurance"

count.abbrev$source <- NA
for(i in 1:length(PAX_v6_Terms.A$abbrev)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.A$abbrev[i] == count.abbrev$regionAbbrev[j]){
      count.abbrev$source[j] <- "ListOfAbbreviations"
    }
  }
}
for(i in 1:length(PAX_v6_Terms.S$abbrev)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.S$abbrev[i] == count.abbrev$regionAbbrev[j]){
      count.abbrev$source[j] <- "ListOfStructures"
    }
  }
}
for(i in 1:length(count.abbrev$regionAbbrev)){
  if(count.abbrev$occurance[i] == 2){
    count.abbrev$source[i] <- "both" 
  }
}
for(i in 1:length(count.abbrev$regionAbbrev)){
  if(count.abbrev$regionAbbrev[i] == "S"){
    count.abbrev$source[i] <- "twice in ListOfStructures" 
  }
}
for(i in 1:length(count.abbrev$regionAbbrev)){
  if(count.abbrev$occurance[i] == 3){
    count.abbrev$source[i] <- "twice in ListOfStructures & once in ListOfAbbreviations" 
  }
}

###Add information to summary files:
###List of Structures:
PAX_v6_Terms.S_labels$source <- NA

for(i in 1:length(PAX_v6_Terms.S_labels$name)){
  for(j in 1:length(count.name$regionName)){
    if(PAX_v6_Terms.S_labels$name[i] == count.name$regionName[j] && count.name$source[j] == "ListOfStructures"){
      PAX_v6_Terms.S_labels$source[i] <- "NameOnlyHere"
    }else if(PAX_v6_Terms.S_labels$name[i] == count.name$regionName[j] && count.name$source[j] == "both"){
      PAX_v6_Terms.S_labels$source[i] <- "NameInBoth"
    }else if(PAX_v6_Terms.S_labels$name[i] == count.name$regionName[j] && count.name$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"){
      PAX_v6_Terms.S_labels$source[i] <- "NameInBothTwiceHere"
    }else if(PAX_v6_Terms.S_labels$name[i] == count.name$regionName[j] && count.name$source[j] == "twice in ListOfStructures"){
      PAX_v6_Terms.S_labels$source[i] <- "NameOnlyHereTwice"
    }
  }
}

for(i in 1:length(PAX_v6_Terms.S_labels$abbreviation)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "ListOfStructures"
       && PAX_v6_Terms.S_labels$source[i] == "NameOnlyHere"){
      PAX_v6_Terms.S_labels$source[i] <- "OnlyHere"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "ListOfStructures"
             && PAX_v6_Terms.S_labels$source[i] == "NameInBoth"){
      PAX_v6_Terms.S_labels$source[i] <- "NameInBoth-CheckAbbrev"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "ListOfStructures"
             && PAX_v6_Terms.S_labels$source[i] == "NameInBothTwiceHere"){
      PAX_v6_Terms.S_labels$source[i] <- "mismatch"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "ListOfStructures"
             && PAX_v6_Terms.S_labels$source[i] == "NameOnlyHereTwice"){
      PAX_v6_Terms.S_labels$source[i] <- "mismatch"
    }
  }
}

for(i in 1:length(PAX_v6_Terms.S_labels$abbreviation)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "both"
       && PAX_v6_Terms.S_labels$source[i] == "NameOnlyHere"){
      PAX_v6_Terms.S_labels$source[i] <- "NameOnlyHere-CheckAbbrev"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "both"
             && PAX_v6_Terms.S_labels$source[i] == "NameInBoth"){
      PAX_v6_Terms.S_labels$source[i] <- "BothInBoth"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "both"
             && PAX_v6_Terms.S_labels$source[i] == "NameInBothTwiceHere"){
      PAX_v6_Terms.S_labels$source[i] <- "mismatch"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "both"
             && PAX_v6_Terms.S_labels$source[i] == "NameOnlyHereTwice"){
      PAX_v6_Terms.S_labels$source[i] <- "mismatch"
    }
  }
}

for(i in 1:length(PAX_v6_Terms.S_labels$abbreviation)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures"
       && PAX_v6_Terms.S_labels$source[i] == "NameOnlyHere"){
      PAX_v6_Terms.S_labels$source[i] <- "NameOnceAbbrevTwice-CheckAbbrev"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures"
             && PAX_v6_Terms.S_labels$source[i] == "NameInBoth"){
      PAX_v6_Terms.S_labels$source[i] <- "NameInBothAbbrevTwiceHere-CheckAbbrev"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures"
             && PAX_v6_Terms.S_labels$source[i] == "NameInBothTwiceHere"){
      PAX_v6_Terms.S_labels$source[i] <- "NameInBothTwiceHere-AbbrevTwice-CheckAbbrev"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures"
             && PAX_v6_Terms.S_labels$source[i] == "NameOnlyHereTwice"){
      PAX_v6_Terms.S_labels$source[i] <- "NameOnlyHereTwice-AbbrevTwice-CheckAbbrev"
    }
  }
}

for(i in 1:length(PAX_v6_Terms.S_labels$abbreviation)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"
       && PAX_v6_Terms.S_labels$source[i] == "NameOnlyHere"){
      PAX_v6_Terms.S_labels$source[i] <- "NameOnceAbbrevTwice-CheckAbbrev"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"
             && PAX_v6_Terms.S_labels$source[i] == "NameInBoth"){
      PAX_v6_Terms.S_labels$source[i] <- "NameInBothAbbrevTwiceHere-CheckAbbrev"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"
             && PAX_v6_Terms.S_labels$source[i] == "NameInBothTwiceHere"){
      PAX_v6_Terms.S_labels$source[i] <- "NameInBothTwiceHere-AbbrevTwice-CheckAbbrev"
    }else if(PAX_v6_Terms.S_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"
             && PAX_v6_Terms.S_labels$source[i] == "NameOnlyHereTwice"){
      PAX_v6_Terms.S_labels$source[i] <- "NameOnlyHereTwice-AbbrevTwice-CheckAbbrev"
    }
  }
}

names(PAX_v6_Terms.S_labels)[5] <- "sourceNote"
PAX_v6_Terms.S_labels$source <-- NA
PAX_v6_Terms.S_labels$source <- "ListOfStructures"

###List of Abbreviations:
PAX_v6_Terms.A_labels$source <- NA

for(i in 1:length(PAX_v6_Terms.A_labels$name)){
  for(j in 1:length(count.name$regionName)){
    if(PAX_v6_Terms.A_labels$name[i] == count.name$regionName[j] && count.name$source[j] == "ListOfAbbreviations"){
      PAX_v6_Terms.A_labels$source[i] <- "NameOnlyHere"
    }else if(PAX_v6_Terms.A_labels$name[i] == count.name$regionName[j] && count.name$source[j] == "both"){
      PAX_v6_Terms.A_labels$source[i] <- "NameInBoth"
    }else if(PAX_v6_Terms.A_labels$name[i] == count.name$regionName[j] && count.name$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"){
      PAX_v6_Terms.A_labels$source[i] <- "NameInBoth-CheckName"
    }
  }
}

for(i in 1:length(PAX_v6_Terms.A_labels$abbreviation)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "ListOfAbbreviations"
       && PAX_v6_Terms.A_labels$source[i] == "NameOnlyHere"){
      PAX_v6_Terms.A_labels$source[i] <- "OnlyHere"
    }else if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "ListOfAbbreviations"
             && PAX_v6_Terms.A_labels$source[i] == "NameInBoth"){
      PAX_v6_Terms.A_labels$source[i] <- "NameInBoth-CheckAbbrev"
    }else if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "ListOfAbbreviations"
             && PAX_v6_Terms.A_labels$source[i] == "NameInBoth-CheckName"){
      PAX_v6_Terms.A_labels$source[i] <- "NameInBothAbbrevOnce-CheckName"
    }
  }
}

for(i in 1:length(PAX_v6_Terms.A_labels$abbreviation)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "both"
       && PAX_v6_Terms.A_labels$source[i] == "NameOnlyHere"){
      PAX_v6_Terms.A_labels$source[i] <- "NameOnlyHere-CheckAbbrev"
    }else if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "both"
             && PAX_v6_Terms.A_labels$source[i] == "NameInBoth"){
      PAX_v6_Terms.A_labels$source[i] <- "BothInBoth"
    }else if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "both"
             && PAX_v6_Terms.A_labels$source[i] == "NameInBoth-CheckName"){
      PAX_v6_Terms.A_labels$source[i] <- "NameInBothAbbrevOnce-CheckName"
    }
  }
}

for(i in 1:length(PAX_v6_Terms.A_labels$abbreviation)){
  for(j in 1:length(count.abbrev$regionAbbrev)){
    if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"
       && PAX_v6_Terms.A_labels$source[i] == "NameOnlyHere"){
      PAX_v6_Terms.A_labels$source[i] <- "NameOnlyHereAbbrevInBoth-CheckAbbrev"
    }else if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"
             && PAX_v6_Terms.A_labels$source[i] == "NameInBoth"){
      PAX_v6_Terms.A_labels$source[i] <- "NameInBothAbbrevInBoth-CheckAbbrev"
    }else if(PAX_v6_Terms.A_labels$abbreviation[i] == count.abbrev$regionAbbrev[j] && count.abbrev$source[j] == "twice in ListOfStructures & once in ListOfAbbreviations"
             && PAX_v6_Terms.A_labels$source[i] == "NameInBoth-CheckName"){
      PAX_v6_Terms.A_labels$source[i] <- "NameInBothAbbrevInBoth-CheckName-CheckAbbrev"
    }
  }
}

names(PAX_v6_Terms.A_labels)[5] <- "sourceNote"
PAX_v6_Terms.A_labels$source <-- NA
PAX_v6_Terms.A_labels$source <- "ListOfAbbreviations"

PAX_v6_Terms_labels <- rbind(PAX_v6_Terms.A_labels, PAX_v6_Terms.S_labels)

###Export as excel for visual inspection and annotation of which terms to use in next steps:
write_xlsx(PAX_v6_Terms_labels,"Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles/00_forInspectionOnly/RBSC_PW_v6_terminology_combined.xlsx")

#########################################################################################################################################
}
#########################################################################################################################################
###START HERE IF COMBINED TERMINOLOGY HAS BEEN GENERATED!
#########################################################################################################################################

#########################################################################################################################################
###Read excel sheets:
#Combined terminology from "Index of Structures" and "Index of Abbreviations" from Paxinos & Watson RBSC 6th edition
#Annotated with "use" or "no" indicating which to use in next step, and comments that need to be corrected manually: 
PAX_v6_Terms <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/RBSC_PW_v6_terminology_combined_use.xlsx")

PAX_v6_Terms <- PAX_v6_Terms[PAX_v6_Terms$use == "use",]

###Combined list of brain regions:
PAX_v6_Terms_labels <- data.frame("name" = PAX_v6_Terms$name, "lookup" = PAX_v6_Terms$lookup, "abbreviation" = PAX_v6_Terms$abbreviation,
                                  "source" = PAX_v6_Terms$source, "sourceNote" = PAX_v6_Terms$sourceNote, "comment" = PAX_v6_Terms$comment)

##Removes punctuation (!"#$%&???()*+,-./:;<=>?@[]^_`{|}~):
PAX_v6_Terms_labels$lookup <- gsub("([[:punct:]])", " ", PAX_v6_Terms_labels$lookup)
##Removes special punctuation (e.g., "medial forebrain bundle, ???a??? component"; NB: is not stored correctly, needs to be updated before running the script):
PAX_v6_Terms_labels$lookup <- gsub("\\???", "", PAX_v6_Terms_labels$lookup)
PAX_v6_Terms_labels$lookup <- gsub("\\???", "", PAX_v6_Terms_labels$lookup)
##Replaces language specific special characters and letters (e.g., Lorente de N?? to Lorente de No):
PAX_v6_Terms_labels$lookup <- iconv(PAX_v6_Terms_labels$lookup, from = 'UTF-8', to = 'ASCII//TRANSLIT')
##Produces lowerCamelCase, but keeps UpperCamelCase for words that should be capitalised (e.g., Ammon's):
PAX_v6_Terms_labels$lookup <- gsub(" ([[:alpha:]])", "\\U\\1", PAX_v6_Terms_labels$lookup, perl = TRUE)
##Removes any additional white spaces:
PAX_v6_Terms_labels$lookup <- gsub(" ", "", PAX_v6_Terms_labels$lookup, perl = TRUE)

#########################################################################################################################################

#########################################################################################################################################
###PARCELLATION ENTITY VERSIONS: 
###(from terminology)
PAX_v6_PEV <- data.frame("at_id" = paste("https://openminds.ebrains.eu/instances/parcellationEntityVersion/PW-RBSC-cor_6th-ed_", PAX_v6_Terms_labels$lookup, sep = ""),
                             "at_type" = rep("https://openminds.ebrains.eu/sands/ParcellationEntityVersion", length(PAX_v6_Terms_labels$name)),
                             "abbreviation" = PAX_v6_Terms_labels$abbreviation,
                             "additionalRemarks" = rep(NA, length(PAX_v6_Terms_labels$name)),
                             "alternateName" = rep(NA, length(PAX_v6_Terms_labels$name)),
                             "correctedName" = rep(NA, length(PAX_v6_Terms_labels$name)),
                             "hasAnnotation" = rep(NA, length(PAX_v6_Terms_labels$name)),
                             "hasParent" = rep(NA, length(PAX_v6_Terms_labels$name)),
                             "lookupLabel" = paste("PW-RBSC-cor_6th-ed_", PAX_v6_Terms_labels$lookup, sep = ""),
                             "name" = PAX_v6_Terms_labels$name,
                             "ontologyIdentifier" = rep(NA, length(PAX_v6_Terms_labels$name)),
                             "relationAssessment" = rep(NA, length(PAX_v6_Terms_labels$name)), 
                             "versionIdentifier" = rep("6th ed.", length(PAX_v6_Terms_labels$name)),
                             "versionInnovation" = rep(NA, length(PAX_v6_Terms_labels$name)))

###Add origin of brain region and if brain region was listed in both lists:
for(i in 1:length(PAX_v6_PEV$at_id)){
  for(j in 1:length(PAX_v6_Terms_labels$name)){
    if(PAX_v6_PEV$name[i] == PAX_v6_Terms_labels$name[j] && PAX_v6_Terms_labels$sourceNote[j] == "OnlyHere" 
       && PAX_v6_Terms_labels$source[j] == "ListOfStructures"){
      PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation only listed in chapter 'List of Structures' of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from this chapter."
    }else if(PAX_v6_PEV$name[i] == PAX_v6_Terms_labels$name[j] && PAX_v6_Terms_labels$sourceNote[j] == "OnlyHere" 
             && PAX_v6_Terms_labels$source[j] == "ListOfAbbreviations"){
      PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation only listed in chapter 'List of Abbreviations' of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from this chapter."
    }else if(PAX_v6_PEV$name[i] == PAX_v6_Terms_labels$name[j] && PAX_v6_Terms_labels$sourceNote[j] == "BothInBoth" 
             && PAX_v6_Terms_labels$source[j] == "ListOfStructures"){
      PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation listed in both chapter 'List of Abbreviations' and chapter 'List of Structures' of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from chapter 'List of Structures'."
    }else if(PAX_v6_PEV$name[i] == PAX_v6_Terms_labels$name[j] && PAX_v6_Terms_labels$sourceNote[j] == "BothInBoth" 
             && PAX_v6_Terms_labels$source[j] == "ListOfAbbreviations"){
      PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation listed in both chapter 'List of Abbreviations' and chapter 'List of Structures' of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from chapter 'List of Abbreviations'."
    }
  }
}

###Manual error correction:
###AHiPL - amygdalohippocampal area, posterolateral part:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "AHiPL"){
    PAX_v6_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/PW-RBSC-cor_6th-ed_amygdalohippocampalAreaPosterolateralPart"
    PAX_v6_PEV$additionalRemarks[i] <- paste(PAX_v6_PEV$additionalRemarks[i], "Region name was extended with 'part' in accordance with the name of other subregions from the same parent structure. The assumed correct name 'amygdalohippocampal area, posterolateral part' has been added under 'correctedName'.", sep = " ")
    PAX_v6_PEV$correctedName[i] <- "amygdalohippocampal area, posterolateral part"
    PAX_v6_PEV$lookupLabel[i] <- "PW-RBSC-cor_6th-ed_amygdalohippocampalAreaPosterolateralPart"
  }
}

###InCSh - interstitial nucleus of Cajal, shell region:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "InCSh"){
    PAX_v6_PEV$additionalRemarks[i] <- paste(PAX_v6_PEV$additionalRemarks[i], "Region name has a spelling mistake (dot instead of comma). The assumed correct name 'interstitial nucleus of Cajal, shell region' has been added under 'correctedName'.", sep = " ")
    PAX_v6_PEV$correctedName[i] <- "interstitial nucleus of Cajal, shell region"
  }
}

###La - lateral amygdaloid nucleus:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "La"){
    PAX_v6_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/PW-RBSC-cor_6th-ed_lateralAmygdaloidNucleus"
    PAX_v6_PEV$additionalRemarks[i] <- paste(PAX_v6_PEV$additionalRemarks[i], "Region name has a spelling mistake ('lat' instead of 'lateral'). The assumed correct name 'lateral amygdaloid nucleus' has been added under 'correctedName'.", sep = " ")
    PAX_v6_PEV$correctedName[i] <- "lateral amygdaloid nucleus"
    PAX_v6_PEV$lookupLabel[i] <- "PW-RBSC-cor_6th-ed_lateralAmygdaloidNucleus"
  }
}

###MeAD - medial amygdaloid nucleus, anterodorsal part:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "MeAD"){
    PAX_v6_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/PW-RBSC-cor_6th-ed_medialAmygdaloidNucleusAnterodorsalPart"
    PAX_v6_PEV$additionalRemarks[i] <- paste(PAX_v6_PEV$additionalRemarks[i], "Region name has a spelling mistake ('ant dorsal' instead of 'anterodorsal part'). The assumed correct name 'medial amygdaloid nucleus, anterodorsal part' has been added under 'correctedName'.", sep = " ")
    PAX_v6_PEV$correctedName[i] <- "medial amygdaloid nucleus, anterodorsal part"
    PAX_v6_PEV$lookupLabel[i] <- "PW-RBSC-cor_6th-ed_medialAmygdaloidNucleusAnterodorsalPart"
  }
}

###STSL - bed nucleus of stria terminalis, supracapsular division, lateral part:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "STSM"){
    PAX_v6_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/PW-RBSC-cor_6th-ed_bedNucleusOfStriaTerminalisSupracapsularDivisionLateralPart"
    PAX_v6_PEV$additionalRemarks[i] <- paste(PAX_v6_PEV$additionalRemarks[i], "Both the name of the region and its abbreviation have spelling mistakes (name: 'Lateralpart' instead of 'Lateral part'; abbreviation: 'STSL' instead of 'STSM'). The assumed correct name 'bed nucleus of stria terminalis, supracapsular division, lateral part' has been added under 'correctedName'. The assumed correct abbreviation 'STSL' has been added under 'abbreviation'. 'STSM' has been added under 'alternateName'.", sep = " ")
    PAX_v6_PEV$correctedName[i] <- "bed nucleus of stria terminalis, supracapsular division, lateral part"
    PAX_v6_PEV$abbreviation[i] <- "STSL"
    PAX_v6_PEV$alternateName[i] <- "STSM"
    PAX_v6_PEV$lookupLabel[i] <- "PW-RBSC-cor_6th-ed_bedNucleusOfStriaTerminalisSupracapsularDivisionLateralPart"
  }
}

###STSM - bed nucleus of stria terminalis, supracapsular division, medial part:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "STSL" & is.na(PAX_v6_PEV$alternateName[i]) == TRUE){
    PAX_v6_PEV$additionalRemarks[i] <- paste(PAX_v6_PEV$additionalRemarks[i], "The abbreviation of the brain region has a spelling mistake ('STSL' instead of 'STSM'). The assumed correct abbreviation 'STSM' has been added under 'abbreviation'. 'STSL' has been added under 'alternateName'.", sep = " ")
    PAX_v6_PEV$abbreviation[i] <- "STSM"
    PAX_v6_PEV$alternateName[i] <- "STSL"
  }
}

###FrA - frontal association cortex:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "FrA"){
    PAX_v6_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/PW-RBSC-cor_6th-ed_frontalAssociationCortex"
    PAX_v6_PEV$additionalRemarks[i] <- paste(PAX_v6_PEV$additionalRemarks[i], "Region name has a spelling mistake ('assocn' instead of 'association'). The assumed correct name 'frontal association cortex' has been added under 'correctedName'.", sep = " ")
    PAX_v6_PEV$correctedName[i] <- "frontal association cortex"
    PAX_v6_PEV$lookupLabel[i] <- "PW-RBSC-cor_6th-ed_frontalAssociationCortex"
  }
}

###Pa - paraventricular hypothalamic nucleus:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "Pa"){
    PAX_v6_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/PW-RBSC-cor_6th-ed_paraventricularHypothalamicNucleus"
    PAX_v6_PEV$additionalRemarks[i] <- paste(PAX_v6_PEV$additionalRemarks[i], "Region name has a spelling mistake ('hypoth' instead of 'hypothalamic'). The assumed correct name 'paraventricular hypothalamic nucleus' has been added under 'correctedName'.", sep = " ")
    PAX_v6_PEV$correctedName[i] <- "paraventricular hypothalamic nucleus"
    PAX_v6_PEV$lookupLabel[i] <- "PW-RBSC-cor_6th-ed_paraventricularHypothalamicNucleus"
  }
}

###Manual comment and error correction for more complicated cases:
###STS - bed nucleus of stria terminalis, supracapsular division:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "STS"){
    PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation listed both in chapter 'List of Abbreviations' and twice in chapter 'List of Structures' of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from chapter 'List of Abbreviations'. In 'List of Structures', one of the region names is an alternate version of the region name ('bed nucleus of the stria [...]' instead of 'bed nucleus of stria [...]'). The alternate name 'bed nucleus of the stria terminalis, supracapsular division' has been added under 'alternateName'."
    PAX_v6_PEV$alternateName[i] <- "bed nucleus of the stria terminalis, supracapsular division"
  }
}

###SLu - stratum lucidum of the hippocampus:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "SLu"){
    PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation listed both in chapter 'List of Abbreviations' and twice in chapter 'List of Structures' of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from chapter 'List of Abbreviations'."
  }
}

###KF - K??lliker-Fuse nucleus:
###NOTE 1: "K??lliker" is wrongly translated into the additionalRemarks - corrected the error manually in the JSON-LD instead
###NOTE 2: "K??lliker-Fuse nucleus" was added manually to the generated JSON-LD under "alternateName" because we decided that this should be included for openMINDS; compare with the corresponding openMINDS instance 
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "KF"){
    PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation listed in both chapter 'List of Abbreviations' and chapter 'List of Structures' of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from chapter 'List of Structures'. In 'List of Abbreviations', region name has a spelling mistake ('K??lliker' instead of 'K??lliker'). The original name 'K??lliker-Fuse nucleus' has been added under 'alternateName'."
    PAX_v6_PEV$alternateName[i] <- "K??lliker-Fuse nucleus"
  }
}


###MT - medial terminal nucleus of the accessory optic tract:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "MT"){
    PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation listed in both chapter 'List of Abbreviations' and chapter 'List of Structures' of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from chapter 'List of Abbreviations'. In 'List of Structures', region name has a spelling mistake ('optic trac' instead of 'optic tract'). The original name 'medial terminal nucleus of the accessory optic trac' has been added under 'alternateName'."
    PAX_v6_PEV$alternateName[i] <- "medial terminal nucleus of the accessory optic trac"
  }
}

###S - subiculum:
for(i in 1:length(PAX_v6_PEV$abbreviation)){
  if(PAX_v6_PEV$abbreviation[i] == "S"){
    PAX_v6_PEV$additionalRemarks[i] <- "Brain region name and abbreviation listed twice in chapter 'List of Structures', but not in chapter 'List of Abbreviations', of the 6th edition of The Rat Brain in Stereotaxic Coordinates (ISBN: 0-12-547612-4). Brain region name and abbreviation were taken from this chapter."
  }
}


###Export PEVs as excel for visual inspection:
write_xlsx(PAX_v6_PEV,"Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles/00_forInspectionOnly/PAX_v6_PEV.xlsx")


###Generate JSON-LDs: 
###Note that abbreviation list from atlas contains 'subiculum' and 'stratum lucidum of the hippocampus'. The total number of files will be 2 shorter than number of objects in the dataframe (809 vs. 811). 
setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles/PEV_jsonlds")

PAX_v6_PEV <-PAX_v6_PEV[order(PAX_v6_PEV$lookupLabel),]

PAX_v6_PEV$at_id <- as.character(PAX_v6_PEV$at_id)
PAX_v6_PEV$at_type <- as.character(PAX_v6_PEV$at_type)
PAX_v6_PEV$abbreviation <- as.character(PAX_v6_PEV$abbreviation)
PAX_v6_PEV$additionalRemarks <- as.character(PAX_v6_PEV$additionalRemarks)
PAX_v6_PEV$alternateName <- as.character(PAX_v6_PEV$alternateName)
PAX_v6_PEV$correctedName <- as.character(PAX_v6_PEV$correctedName)
PAX_v6_PEV$hasAnnotation <- as.character(PAX_v6_PEV$hasAnnotation)
PAX_v6_PEV$hasParent <- as.character(PAX_v6_PEV$hasParent)
PAX_v6_PEV$lookupLabel <- as.character(PAX_v6_PEV$lookupLabel)
PAX_v6_PEV$name <- as.character(PAX_v6_PEV$name)
PAX_v6_PEV$ontologyIdentifier <- as.character(PAX_v6_PEV$ontologyIdentifier)
PAX_v6_PEV$relationAssessment <- as.character(PAX_v6_PEV$relationAssessment)
PAX_v6_PEV$versionIdentifier <- as.character(PAX_v6_PEV$versionIdentifier)
PAX_v6_PEV$versionInnovation <- as.character(PAX_v6_PEV$versionInnovation)

for(i in 1:length(PAX_v6_PEV$abbreviation)){
  PAX_v6_PEV_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                              at_id = PAX_v6_PEV$at_id[i],
                              at_type = PAX_v6_PEV$at_type[i],
                              abbreviation = PAX_v6_PEV$abbreviation[i],
                              additionalRemarks = PAX_v6_PEV$additionalRemarks[i],
                              alternateName = ifelse(is.na(PAX_v6_PEV$alternateName[i]) == TRUE, NA, list(PAX_v6_PEV$alternateName[i])),
                              correctedName = PAX_v6_PEV$correctedName[i],
                              hasAnnotation = PAX_v6_PEV$hasAnnotation[i],
                              hasParent = PAX_v6_PEV$hasParent[i],
                              lookupLabel = PAX_v6_PEV$lookupLabel[i],
                              name = PAX_v6_PEV$name[i],
                              ontologyIdentifier = PAX_v6_PEV$ontologyIdentifier[i],
                              relationAssessment = PAX_v6_PEV$relationAssessment[i],
                              versionIdentifier = PAX_v6_PEV$versionIdentifier[i],
                              versionInnovation = PAX_v6_PEV$versionInnovation[i])
  
  json.PEV <- toJSON(PAX_v6_PEV_list, pretty = 2, auto_unbox = TRUE)
  json.PEV.at <- gsub("\"at_", "\"@", json.PEV)  
  write(json.PEV.at, file = paste(PAX_v6_PEV$lookupLabel[i], ".jsonld", sep = ""))
}

#########################################################################################################################################

#########################################################################################################################################
###PARCELLATION ENTITIES:
###(from terminology - same as PEVs without versionID, uses same labels as PEVs)

###ParcellationEntity table:
PAX_v6_PE <- data.frame("at_id" = paste("https://openminds.ebrains.eu/instances/parcellationEntity/PW-RBSC-cor_", PAX_v6_Terms_labels$lookup, sep = ""),
                            "at_type" = rep("https://openminds.ebrains.eu/sands/ParcellationEntity", length(PAX_v6_Terms_labels$name)),
                            "abbreviation" = PAX_v6_Terms_labels$abbreviation,
                            "alternateName" = rep(NA, length(PAX_v6_Terms_labels$name)),
                            "definition" = rep(NA, length(PAX_v6_Terms_labels$name)),
                            "hasParent" = rep(NA, length(PAX_v6_Terms_labels$name)),
                            "hasVersion" = rep(NA, length(PAX_v6_Terms_labels$name)),
                            "lookupLabel" = paste("PW-RBSC-cor_", PAX_v6_Terms_labels$lookup, sep = ""),
                            "name" = PAX_v6_Terms_labels$name,
                            "ontologyIdentifier" = rep(NA, length(PAX_v6_Terms_labels$name)),
                            "relatedUBERONTerm" = rep(NA, length(PAX_v6_Terms_labels$name)))


###Manual error correction:
###AHiPL - amygdalohippocampal area, posterolateral part:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "AHiPL"){
    PAX_v6_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/PW-RBSC-cor_amygdalohippocampalAreaPosterolateralPart"
    PAX_v6_PE$name[i] <- "amygdalohippocampal area, posterolateral part"
    PAX_v6_PE$lookupLabel[i] <- "PW-RBSC-cor_amygdalohippocampalAreaPosterolateralPart"
  }
}

###InCSh - interstitial nucleus of Cajal, shell region:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "InCSh"){
    PAX_v6_PE$name[i] <- "interstitial nucleus of Cajal, shell region"
  }
}

###La - lateral amygdaloid nucleus:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "La"){
    PAX_v6_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/PW-RBSC-cor_lateralAmygdaloidNucleus"
    PAX_v6_PE$name[i] <- "lateral amygdaloid nucleus"
    PAX_v6_PE$lookupLabel[i] <- "PW-RBSC-cor_lateralAmygdaloidNucleus"
  }
}

###MeAD - medial amygdaloid nucleus, anterodorsal part:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "MeAD"){
    PAX_v6_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/PW-RBSC-cor_medialAmygdaloidNucleusAnterodorsalPart"
    PAX_v6_PE$name[i] <- "medial amygdaloid nucleus, anterodorsal part"
    PAX_v6_PE$lookupLabel[i] <- "PW-RBSC-cor_medialAmygdaloidNucleusAnterodorsalPart"
  }
}

###STSL - bed nucleus of stria terminalis, supracapsular division, lateral part:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "STSM"){
    PAX_v6_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/PW-RBSC-cor_bedNucleusOfStriaTerminalisSupracapsularDivisionLateralPart"
    PAX_v6_PE$name[i] <- "bed nucleus of stria terminalis, supracapsular division, lateral part"
    PAX_v6_PE$abbreviation[i] <- "STSL"
    PAX_v6_PE$alternateName[i] <- "STSM"
    PAX_v6_PE$lookupLabel[i] <- "PW-RBSC-cor_bedNucleusOfStriaTerminalisSupracapsularDivisionLateralPart"
  }
}

###STSM - bed nucleus of stria terminalis, supracapsular division, medial part:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "STSL" & is.na(PAX_v6_PE$alternateName[i]) == TRUE){
    PAX_v6_PE$abbreviation[i] <- "STSM"
    PAX_v6_PE$alternateName[i] <- "STSL"
  }
}

###FrA - frontal association cortex:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "FrA"){
    PAX_v6_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/PW-RBSC-cor_frontalAssociationCortex"
    PAX_v6_PE$name[i] <- "frontal association cortex"
    PAX_v6_PE$lookupLabel[i] <- "PW-RBSC-cor_frontalAssociationCortex"
  }
}

###Pa - paraventricular hypothalamic nucleus:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "Pa"){
    PAX_v6_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/PW-RBSC-cor_paraventricularHypothalamicNucleus"
    PAX_v6_PE$name[i] <- "paraventricular hypothalamic nucleus"
    PAX_v6_PE$lookupLabel[i] <- "PW-RBSC-cor_paraventricularHypothalamicNucleus"
  }
}

###STS - bed nucleus of stria terminalis, supracapsular division:
for(i in 1:length(PAX_v6_PE$abbreviation)){
  if(PAX_v6_PE$abbreviation[i] == "STS"){
    PAX_v6_PE$alternateName[i] <- "bed nucleus of the stria terminalis, supracapsular division"
  }
}


for(i in 1:length(PAX_v6_PE$at_id)){
  for(j in 1:length(PAX_v6_PEV$at_id)){
    if(PAX_v6_PE$abbreviation[i] == PAX_v6_PEV$abbreviation[j]){
      PAX_v6_PE$hasVersion[i] <- PAX_v6_PEV$at_id[j]
    }
  }
  
}

###Export PEs as excel for visual inspection:
write_xlsx(PAX_v6_PE,"Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles/00_forInspectionOnly/PAX_v6_PE-only-v6.xlsx")

###Generate JSON-LDs: 
setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles/PE_jsonlds")

PAX_v6_PE <-PAX_v6_PE[order(PAX_v6_PE$lookupLabel),]

PAX_v6_PE$at_id <- as.character(PAX_v6_PE$at_id)
PAX_v6_PE$at_type <- as.character(PAX_v6_PE$at_type)
PAX_v6_PE$abbreviation <- as.character(PAX_v6_PE$abbreviation)
PAX_v6_PE$alternateName <- as.character(PAX_v6_PE$alternateName)
PAX_v6_PE$definition <- as.character(PAX_v6_PE$definition)
PAX_v6_PE$hasParent <- as.character(PAX_v6_PE$hasParent)
PAX_v6_PE$hasVersion <- as.character(PAX_v6_PE$hasVersion)
PAX_v6_PE$lookupLabel <- as.character(PAX_v6_PE$lookupLabel)
PAX_v6_PE$name <- as.character(PAX_v6_PE$name)
PAX_v6_PE$ontologyIdentifier <- as.character(PAX_v6_PE$ontologyIdentifier)
PAX_v6_PE$relatedUBERONTerm <- as.character(PAX_v6_PE$relatedUBERONTerm)

for(i in 1:length(PAX_v6_PE$abbreviation)){
  PAX_v6_PE_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                             at_id = PAX_v6_PE$at_id[i],
                             at_type = PAX_v6_PE$at_type[i],
                             abbreviation = PAX_v6_PE$abbreviation[i],
                             alternateName = ifelse(is.na(PAX_v6_PE$alternateName[i]) == TRUE, NA, list(PAX_v6_PE$alternateName[i])),
                             definition = PAX_v6_PE$definition[i],
                             hasParent = PAX_v6_PE$hasParent[i],
                             hasVersion = list(list(at_id = PAX_v6_PE$hasVersion[i])),
                             lookupLabel = PAX_v6_PE$lookupLabel[i],
                             name = PAX_v6_PE$name[i],
                             ontologyIdentifier = PAX_v6_PE$ontologyIdentifier[i],
                             relatedUBERONTerm = PAX_v6_PE$relatedUBERONTerm[i])
  
  json.PE <- toJSON(PAX_v6_PE_list, pretty = 2, auto_unbox = TRUE)
  json.PE.at <- gsub("\"at_", "\"@", json.PE)  
  write(json.PE.at, file = paste(PAX_v6_PE$lookupLabel[i], ".jsonld", sep = ""))
}

#########################################################################################################################################

#########################################################################################################################################
###PARCELLATION TERMINOLOGY VERSION:
###(will be embedded in brain atlas version)
setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles/00_forInspectionOnly")

###Generate list used in JSON-LD:
entity.list <- list(list(at_id = PAX_v6_PEV$at_id[1]))
for(i in 2:length(PAX_v6_PEV$at_id)) {
  entity.list <- c(entity.list,list(list(at_id = PAX_v6_PEV$at_id[i])))
}

PAX_v6_PTV_list <- list(at_type = "https://openminds.ebrains.eu/sands/ParcellationTerminologyVersion",
                            dataLocation = NA,
                            hasEntity = entity.list,
                            ontologyIdentifier = NA)

###Generate JSON-LD for visual inspection:
json.PTV <- toJSON(PAX_v6_PTV_list, pretty = 2, auto_unbox = TRUE)
json.PTV.at <- gsub("\"at_", "\"@", json.PTV)  
write(json.PTV.at, file = paste("PAX_v6_PTV", ".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###PARCELLATION TERMINOLOGY:
###(will be embedded in brain atlas)
setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles/00_forInspectionOnly")

entity.list <- list(list(at_id = PAX_v6_PE$at_id[1]))
for(i in 2:length(PAX_v6_PE$at_id)) {
  entity.list <- c(entity.list,list(list(at_id = PAX_v6_PE$at_id[i])))
}

PAX_v6_PT_list <- list(at_type = "https://openminds.ebrains.eu/sands/ParcellationTerminology",
                           dataLocation = NA,
                           hasEntity = entity.list,
                           ontologyIdentifier = NA)

json.PT <- toJSON(PAX_v6_PT_list, pretty = 2, auto_unbox = TRUE)
json.PT.at <- gsub("\"at_", "\"@", json.PT)  
write(json.PT.at, file = paste("PAX_v6_PT-only-v6", ".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###In openMINDS prepared brain atlas & version, common coordinate space & version, person:
#BA & BAV:
PAX_v6_cor_BA <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_BA_coronal.xlsx")
PAX_v6_cor_BAV_BrLe <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_BAV_6th_cor-Br-le.xlsx")
PAX_v6_cor_BAV_BrRi <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_BAV_6th_cor-Br-ri.xlsx")
PAX_v6_cor_BAV_IALe <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_BAV_6th_cor-IA-le.xlsx")
PAX_v6_cor_BAV_IARi <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_BAV_6th_cor-IA-ri.xlsx")

#CCS & CCSV:
PAX_v6_cor_CCS <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_CCS_coronal.xlsx")
PAX_v6_cor_CCSV_BrLe <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_CCSV_v2004_cor-Br-le.xlsx")
PAX_v6_cor_CCSV_BrRi <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_CCSV_v2004_cor-Br-ri.xlsx")
PAX_v6_cor_CCSV_IALe <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_CCSV_v2004_cor-IA-le.xlsx")
PAX_v6_cor_CCSV_IARi <- read_excel("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/Pax_v6_CCSV_v2004_cor-IA-ri.xlsx")

#########################################################################################################################################

#########################################################################################################################################
###COMMON COORDINATE SPACE VERSION:
setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles")

###BREGMA LEFT:
PAX_v6_CCSV_BrLe_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                             at_id = paste("https://openminds.ebrains.eu/instances/commonCoordinateSpaceVersion/",PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "abbreviation"],"_v2004-Bregma-LIA", sep = ""),
                             at_type = "https://openminds.ebrains.eu/sands/CommonCoordinateSpaceVersion",
                             abbreviation = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "abbreviation"]),
                             accessibility = if(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "accessibility"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "accessibility"])},
                             anatomicalAxesOrientation = if(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "anatomicalAxesOrientation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "anatomicalAxesOrientation"])},
                             author = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "author"]))),
                             axesOrigin = NA,
                             copyright = NA,
                             custodian = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "custodian"]))),
                             defaultImage = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "defaultImage"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "defaultImage"]))),
                             description = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "description"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "description"]),
                             digitalIdentifier = if(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "digitalIdentifier"])},
                             fullDocumentation = if(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "fullDocumentation"])},
                             fullName = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "fullName"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "fullName"]),
                             funding = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "funding"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "funding"]))),
                             homepage = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "homepage"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "homepage"]),
                             howToCite = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "howToCite"]),
                             isAlternativeVersionOf = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "isAlternativeVersionOf"]))),
                             isNewVersionOf = if(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "isNewVersionOf"])},
                             keyword = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "keyword"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "keyword"]))), 
                             license = if(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "license"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "license"])}, 
                             nativeUnit = if(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "nativeUnit"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "nativeUnit"])},
                             ontologyIdentifier = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "ontologyIdentifier"]),
                             otherContribution = NA,
                             relatedPublication = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "relatedPublication"]))), 
                             releaseDate = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "releaseDate"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "releaseDate"]),
                             repository = if(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "repository"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "repository"])},
                             shortName = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "shortName"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "shortName"]),
                             supportChannel = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "supportChannel"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "supportChannel"]),
                             usedSpecimen = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "usedSpecimen"]))),
                             versionIdentifier = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "versionIdentifier"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "versionIdentifier"]),
                             versionInnovation = ifelse(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "versionInnovation"] == "NA", NA, PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "versionInnovation"]))

json.CCSV <- toJSON(PAX_v6_CCSV_BrLe_list, pretty = 2, auto_unbox = TRUE)
json.CCSV.at <- gsub("\"at_", "\"@", json.CCSV)  
write(json.CCSV.at, file = paste(PAX_v6_cor_CCSV_BrLe$VALUE[PAX_v6_cor_CCSV_BrLe$KEY == "abbreviation"],"_v2004-Bregma-LIA",".jsonld", sep = ""))

###BREGMA RIGHT:
PAX_v6_CCSV_BrRi_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                              at_id = paste("https://openminds.ebrains.eu/instances/commonCoordinateSpaceVersion/",PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "abbreviation"],"_v2004-Bregma-RIA", sep = ""),
                              at_type = "https://openminds.ebrains.eu/sands/CommonCoordinateSpaceVersion",
                              abbreviation = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "abbreviation"]),
                              accessibility = if(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "accessibility"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "accessibility"])},
                              anatomicalAxesOrientation = if(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "anatomicalAxesOrientation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "anatomicalAxesOrientation"])},
                              author = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "author"]))),
                              axesOrigin = NA,
                              copyright = NA,
                              custodian = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "custodian"]))),
                              defaultImage = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "defaultImage"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "defaultImage"]))),
                              description = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "description"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "description"]),
                              digitalIdentifier = if(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "digitalIdentifier"])},
                              fullDocumentation = if(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "fullDocumentation"])},
                              fullName = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "fullName"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "fullName"]),
                              funding = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "funding"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "funding"]))),
                              homepage = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "homepage"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "homepage"]),
                              howToCite = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "howToCite"]),
                              isAlternativeVersionOf = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "isAlternativeVersionOf"]))),
                              isNewVersionOf = if(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "isNewVersionOf"])},
                              keyword = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "keyword"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "keyword"]))), 
                              license = if(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "license"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "license"])}, 
                              nativeUnit = if(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "nativeUnit"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "nativeUnit"])},
                              ontologyIdentifier = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "ontologyIdentifier"]),
                              otherContribution = NA,
                              relatedPublication = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "relatedPublication"]))), 
                              releaseDate = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "releaseDate"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "releaseDate"]),
                              repository = if(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "repository"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "repository"])},
                              shortName = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "shortName"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "shortName"]),
                              supportChannel = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "supportChannel"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "supportChannel"]),
                              usedSpecimen = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "usedSpecimen"]))),
                              versionIdentifier = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "versionIdentifier"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "versionIdentifier"]),
                              versionInnovation = ifelse(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "versionInnovation"] == "NA", NA, PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "versionInnovation"]))

json.CCSV <- toJSON(PAX_v6_CCSV_BrRi_list, pretty = 2, auto_unbox = TRUE)
json.CCSV.at <- gsub("\"at_", "\"@", json.CCSV)  
write(json.CCSV.at, file = paste(PAX_v6_cor_CCSV_BrRi$VALUE[PAX_v6_cor_CCSV_BrRi$KEY == "abbreviation"],"_v2004-Bregma-RIA",".jsonld", sep = ""))

###INTERAURAL LEFT:
PAX_v6_CCSV_IALe_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                              at_id = paste("https://openminds.ebrains.eu/instances/commonCoordinateSpaceVersion/",PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "abbreviation"],"_v2004-Interaural-LSA", sep = ""),
                              at_type = "https://openminds.ebrains.eu/sands/CommonCoordinateSpaceVersion",
                              abbreviation = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "abbreviation"]),
                              accessibility = if(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "accessibility"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "accessibility"])},
                              anatomicalAxesOrientation = if(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "anatomicalAxesOrientation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "anatomicalAxesOrientation"])},
                              author = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "author"]))),
                              axesOrigin = NA,
                              copyright = NA,
                              custodian = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "custodian"]))),
                              defaultImage = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "defaultImage"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "defaultImage"]))),
                              description = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "description"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "description"]),
                              digitalIdentifier = if(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "digitalIdentifier"])},
                              fullDocumentation = if(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "fullDocumentation"])},
                              fullName = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "fullName"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "fullName"]),
                              funding = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "funding"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "funding"]))),
                              homepage = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "homepage"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "homepage"]),
                              howToCite = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "howToCite"]),
                              isAlternativeVersionOf = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "isAlternativeVersionOf"]))),
                              isNewVersionOf = if(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "isNewVersionOf"])},
                              keyword = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "keyword"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "keyword"]))), 
                              license = if(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "license"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "license"])}, 
                              nativeUnit = if(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "nativeUnit"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "nativeUnit"])},
                              ontologyIdentifier = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "ontologyIdentifier"]),
                              otherContribution = NA,
                              relatedPublication = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "relatedPublication"]))), 
                              releaseDate = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "releaseDate"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "releaseDate"]),
                              repository = if(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "repository"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "repository"])},
                              shortName = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "shortName"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "shortName"]),
                              supportChannel = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "supportChannel"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "supportChannel"]),
                              usedSpecimen = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "usedSpecimen"]))),
                              versionIdentifier = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "versionIdentifier"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "versionIdentifier"]),
                              versionInnovation = ifelse(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "versionInnovation"] == "NA", NA, PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "versionInnovation"]))

json.CCSV <- toJSON(PAX_v6_CCSV_IALe_list, pretty = 2, auto_unbox = TRUE)
json.CCSV.at <- gsub("\"at_", "\"@", json.CCSV)  
write(json.CCSV.at, file = paste(PAX_v6_cor_CCSV_IALe$VALUE[PAX_v6_cor_CCSV_IALe$KEY == "abbreviation"],"_v2004-Interaural-LSA",".jsonld", sep = ""))

###INTERAURAL RIGHT:
PAX_v6_CCSV_IARi_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                              at_id = paste("https://openminds.ebrains.eu/instances/commonCoordinateSpaceVersion/",PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "abbreviation"],"_v2004-Interaural-RSA", sep = ""),
                              at_type = "https://openminds.ebrains.eu/sands/CommonCoordinateSpaceVersion",
                              abbreviation = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "abbreviation"]),
                              accessibility = if(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "accessibility"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "accessibility"])},
                              anatomicalAxesOrientation = if(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "anatomicalAxesOrientation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "anatomicalAxesOrientation"])},
                              author = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "author"]))),
                              axesOrigin = NA,
                              copyright = NA,
                              custodian = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "custodian"]))),
                              defaultImage = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "defaultImage"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "defaultImage"]))),
                              description = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "description"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "description"]),
                              digitalIdentifier = if(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "digitalIdentifier"])},
                              fullDocumentation = if(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "fullDocumentation"])},
                              fullName = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "fullName"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "fullName"]),
                              funding = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "funding"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "funding"]))),
                              homepage = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "homepage"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "homepage"]),
                              howToCite = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "howToCite"]),
                              isAlternativeVersionOf = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "isAlternativeVersionOf"]))),
                              isNewVersionOf = if(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "isNewVersionOf"])},
                              keyword = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "keyword"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "keyword"]))), 
                              license = if(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "license"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "license"])}, 
                              nativeUnit = if(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "nativeUnit"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "nativeUnit"])},
                              ontologyIdentifier = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "ontologyIdentifier"]),
                              otherContribution = NA,
                              relatedPublication = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "relatedPublication"]))), 
                              releaseDate = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "releaseDate"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "releaseDate"]),
                              repository = if(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "repository"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "repository"])},
                              shortName = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "shortName"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "shortName"]),
                              supportChannel = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "supportChannel"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "supportChannel"]),
                              usedSpecimen = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "usedSpecimen"]))),
                              versionIdentifier = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "versionIdentifier"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "versionIdentifier"]),
                              versionInnovation = ifelse(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "versionInnovation"] == "NA", NA, PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "versionInnovation"]))

json.CCSV <- toJSON(PAX_v6_CCSV_IARi_list, pretty = 2, auto_unbox = TRUE)
json.CCSV.at <- gsub("\"at_", "\"@", json.CCSV)  
write(json.CCSV.at, file = paste(PAX_v6_cor_CCSV_IARi$VALUE[PAX_v6_cor_CCSV_IARi$KEY == "abbreviation"],"_v2004-Interaural-RSA",".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###Preparation:
###(for linkage of CCSVs)

PAX_CCSVs <- c(list(list(at_id = PAX_v6_CCSV_BrLe_list$at_id)),
               list(list(at_id = PAX_v6_CCSV_BrRi_list$at_id)),
               list(list(at_id = PAX_v6_CCSV_IALe_list$at_id)),
               list(list(at_id = PAX_v6_CCSV_IARi_list$at_id)))

#########################################################################################################################################

#########################################################################################################################################
###COMMON COORDINATE SPACE:
setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles")

PAX_v6_cor_CCS_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                            at_id = paste("https://openminds.ebrains.eu/instances/commonCoordinateSpace/",PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "abbreviation"], sep = ""),
                            at_type = "https://openminds.ebrains.eu/sands/CommonCoordinateSpace",
                            abbreviation = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "abbreviation"]),
                            author = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "author"]))),
                            custodian = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "custodian"]))),
                            description = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "description"] == "NA", NA, PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "description"]),
                            digitalIdentifier = if(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "digitalIdentifier"])},
                            fullName = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "fullName"] == "NA", NA, PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "fullName"]),
                            hasVersion = PAX_CCSVs,
                            homepage = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "homepage"] == "NA", NA, PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "homepage"]),
                            howToCite = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "howToCite"]),
                            ontologyIdentifier = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "ontologyIdentifier"]),
                            shortName = ifelse(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "shortName"] == "NA", NA, PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "shortName"]),
                            usedSpecies = if(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "usedSpecies"] == "NA"){NA}else{list(at_id = PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "usedSpecies"])})

json.CCS <- toJSON(PAX_v6_cor_CCS_list, pretty = 2, auto_unbox = TRUE)
json.CCS.at <- gsub("\"at_", "\"@", json.CCS)  
write(json.CCS.at, file = paste(PAX_v6_cor_CCS$VALUE[PAX_v6_cor_CCS$KEY == "abbreviation"],".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###BRAIN ATLAS VERSION:
setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles")

###BREGMA LEFT:
PAX_v6_cor_BAV_BrLe_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                            at_id = paste("https://openminds.ebrains.eu/instances/brainAtlasVersion/",PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "abbreviation"],"_6th-ed-Bregma-LIA", sep = ""),
                            at_type = "https://openminds.ebrains.eu/sands/BrainAtlasVersion",
                            abbreviation = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "abbreviation"]),
                            accessibility = if(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "accessibility"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "accessibility"])},
                            author = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "author"]))),
                            coordinateSpace = if(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "coordinateSpace"] == "NA"){NA}else{list(at_id = PAX_v6_CCSV_BrLe_list$at_id)},
                            copyright = NA,
                            custodian = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "custodian"]))),
                            description = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "description"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "description"]),
                            digitalIdentifier = if(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = paste("https://openminds.ebrains.eu/instances/ISBN/", PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "digitalIdentifier"], sep = ""))},
                            fullDocumentation = if(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "fullDocumentation"])},
                            fullName = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "fullName"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "fullName"]),
                            funding = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "funding"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "funding"]))),
                            hasTerminology = PAX_v6_PTV_list,
                            homepage = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "homepage"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "homepage"]),
                            howToCite = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "howToCite"]),
                            isAlternativeVersionOf = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "isAlternativeVersionOf"]))),
                            isNewVersionOf = if(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "isNewVersionOf"])},
                            keyword = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "keyword"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "keyword"]))), 
                            license = if(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "license"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "license"])},
                            majorVersionIdentifier = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "majorVersionIdentifier"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "majorVersionIdentifier"]))), 
                            ontologyIdentifier = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "ontologyIdentifier"]),
                            otherContribution = NA,
                            relatedPublication = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "relatedPublication"]))), 
                            releaseDate = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "releaseDate"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "releaseDate"]),
                            repository = if(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "repository"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "repository"])},
                            shortName = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "shortName"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "shortName"]),
                            supportChannel = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "supportChannel"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "supportChannel"]),
                            type = if(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "type"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "type"])},
                            usedSpecimen = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "usedSpecimen"]))),
                            versionIdentifier = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "versionIdentifier"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "versionIdentifier"]),
                            versionInnovation = ifelse(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "versionInnovation"] == "NA", NA, PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "versionInnovation"]))

json.BAV <- toJSON(PAX_v6_cor_BAV_BrLe_list, pretty = 2, auto_unbox = TRUE)
json.BAV.at <- gsub("\"at_", "\"@", json.BAV)  
write(json.BAV.at, file = paste(PAX_v6_cor_BAV_BrLe$VALUE[PAX_v6_cor_BAV_BrLe$KEY == "abbreviation"], "_6th-ed-Bregma-LIA",".jsonld", sep = ""))

###BREGMA RIGHT:
PAX_v6_cor_BAV_BrRi_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                                 at_id = paste("https://openminds.ebrains.eu/instances/brainAtlasVersion/",PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "abbreviation"],"_6th-ed-Bregma-RIA", sep = ""),
                                 at_type = "https://openminds.ebrains.eu/sands/BrainAtlasVersion",
                                 abbreviation = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "abbreviation"]),
                                 accessibility = if(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "accessibility"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "accessibility"])},
                                 author = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "author"]))),
                                 coordinateSpace = if(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "coordinateSpace"] == "NA"){NA}else{list(at_id = PAX_v6_CCSV_BrRi_list$at_id)},
                                 copyright = NA,
                                 custodian = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "custodian"]))),
                                 description = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "description"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "description"]),
                                 digitalIdentifier = if(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = paste("https://openminds.ebrains.eu/instances/ISBN/", PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "digitalIdentifier"], sep = ""))},
                                 fullDocumentation = if(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "fullDocumentation"])},
                                 fullName = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "fullName"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "fullName"]),
                                 funding = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "funding"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "funding"]))),
                                 hasTerminology = PAX_v6_PTV_list,
                                 homepage = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "homepage"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "homepage"]),
                                 howToCite = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "howToCite"]),
                                 isAlternativeVersionOf = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "isAlternativeVersionOf"]))),
                                 isNewVersionOf = if(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "isNewVersionOf"])},
                                 keyword = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "keyword"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "keyword"]))), 
                                 license = if(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "license"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "license"])},
                                 majorVersionIdentifier = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "majorVersionIdentifier"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "majorVersionIdentifier"]))), 
                                 ontologyIdentifier = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "ontologyIdentifier"]),
                                 otherContribution = NA,
                                 relatedPublication = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "relatedPublication"]))), 
                                 releaseDate = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "releaseDate"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "releaseDate"]),
                                 repository = if(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "repository"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "repository"])},
                                 shortName = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "shortName"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "shortName"]),
                                 supportChannel = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "supportChannel"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "supportChannel"]),
                                 type = if(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "type"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "type"])},
                                 usedSpecimen = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "usedSpecimen"]))),
                                 versionIdentifier = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "versionIdentifier"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "versionIdentifier"]),
                                 versionInnovation = ifelse(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "versionInnovation"] == "NA", NA, PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "versionInnovation"]))

json.BAV <- toJSON(PAX_v6_cor_BAV_BrRi_list, pretty = 2, auto_unbox = TRUE)
json.BAV.at <- gsub("\"at_", "\"@", json.BAV)  
write(json.BAV.at, file = paste(PAX_v6_cor_BAV_BrRi$VALUE[PAX_v6_cor_BAV_BrRi$KEY == "abbreviation"], "_6th-ed-Bregma-RIA",".jsonld", sep = ""))

###INTERAURAL LEFT:
PAX_v6_cor_BAV_IALe_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                                 at_id = paste("https://openminds.ebrains.eu/instances/brainAtlasVersion/",PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "abbreviation"],"_6th-ed-Interaural-LSA", sep = ""),
                                 at_type = "https://openminds.ebrains.eu/sands/BrainAtlasVersion",
                                 abbreviation = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "abbreviation"]),
                                 accessibility = if(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "accessibility"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "accessibility"])},
                                 author = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "author"]))),
                                 coordinateSpace = if(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "coordinateSpace"] == "NA"){NA}else{list(at_id = PAX_v6_CCSV_IALe_list$at_id)},
                                 copyright = NA,
                                 custodian = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "custodian"]))),
                                 description = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "description"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "description"]),
                                 digitalIdentifier = if(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = paste("https://openminds.ebrains.eu/instances/ISBN/", PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "digitalIdentifier"], sep = ""))},
                                 fullDocumentation = if(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "fullDocumentation"])},
                                 fullName = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "fullName"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "fullName"]),
                                 funding = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "funding"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "funding"]))),
                                 hasTerminology = PAX_v6_PTV_list,
                                 homepage = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "homepage"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "homepage"]),
                                 howToCite = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "howToCite"]),
                                 isAlternativeVersionOf = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "isAlternativeVersionOf"]))),
                                 isNewVersionOf = if(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "isNewVersionOf"])},
                                 keyword = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "keyword"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "keyword"]))), 
                                 license = if(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "license"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "license"])},
                                 majorVersionIdentifier = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "majorVersionIdentifier"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "majorVersionIdentifier"]))), 
                                 ontologyIdentifier = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "ontologyIdentifier"]),
                                 otherContribution = NA,
                                 relatedPublication = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "relatedPublication"]))), 
                                 releaseDate = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "releaseDate"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "releaseDate"]),
                                 repository = if(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "repository"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "repository"])},
                                 shortName = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "shortName"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "shortName"]),
                                 supportChannel = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "supportChannel"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "supportChannel"]),
                                 type = if(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "type"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "type"])},
                                 usedSpecimen = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "usedSpecimen"]))),
                                 versionIdentifier = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "versionIdentifier"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "versionIdentifier"]),
                                 versionInnovation = ifelse(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "versionInnovation"] == "NA", NA, PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "versionInnovation"]))

json.BAV <- toJSON(PAX_v6_cor_BAV_IALe_list, pretty = 2, auto_unbox = TRUE)
json.BAV.at <- gsub("\"at_", "\"@", json.BAV)  
write(json.BAV.at, file = paste(PAX_v6_cor_BAV_IALe$VALUE[PAX_v6_cor_BAV_IALe$KEY == "abbreviation"], "_6th-ed-Interaural-LSA",".jsonld", sep = ""))

###INTERAURAL RIGHT:
PAX_v6_cor_BAV_IARi_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                                 at_id = paste("https://openminds.ebrains.eu/instances/brainAtlasVersion/",PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "abbreviation"],"_6th-ed-Interaural-RSA", sep = ""),
                                 at_type = "https://openminds.ebrains.eu/sands/BrainAtlasVersion",
                                 abbreviation = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "abbreviation"]),
                                 accessibility = if(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "accessibility"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "accessibility"])},
                                 author = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "author"]))),
                                 coordinateSpace = if(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "coordinateSpace"] == "NA"){NA}else{list(at_id = PAX_v6_CCSV_IARi_list$at_id)},
                                 copyright = NA,
                                 custodian = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "custodian"]))),
                                 description = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "description"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "description"]),
                                 digitalIdentifier = if(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = paste("https://openminds.ebrains.eu/instances/ISBN/", PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "digitalIdentifier"], sep = ""))},
                                 fullDocumentation = if(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "fullDocumentation"])},
                                 fullName = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "fullName"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "fullName"]),
                                 funding = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "funding"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "funding"]))),
                                 hasTerminology = PAX_v6_PTV_list,
                                 homepage = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "homepage"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "homepage"]),
                                 howToCite = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "howToCite"]),
                                 isAlternativeVersionOf = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "isAlternativeVersionOf"]))),
                                 isNewVersionOf = if(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "isNewVersionOf"])},
                                 keyword = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "keyword"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "keyword"]))), 
                                 license = if(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "license"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "license"])},
                                 majorVersionIdentifier = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "majorVersionIdentifier"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "majorVersionIdentifier"]))), 
                                 ontologyIdentifier = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "ontologyIdentifier"]),
                                 otherContribution = NA,
                                 relatedPublication = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "relatedPublication"]))), 
                                 releaseDate = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "releaseDate"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "releaseDate"]),
                                 repository = if(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "repository"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "repository"])},
                                 shortName = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "shortName"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "shortName"]),
                                 supportChannel = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "supportChannel"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "supportChannel"]),
                                 type = if(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "type"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "type"])},
                                 usedSpecimen = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "usedSpecimen"]))),
                                 versionIdentifier = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "versionIdentifier"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "versionIdentifier"]),
                                 versionInnovation = ifelse(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "versionInnovation"] == "NA", NA, PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "versionInnovation"]))

json.BAV <- toJSON(PAX_v6_cor_BAV_IARi_list, pretty = 2, auto_unbox = TRUE)
json.BAV.at <- gsub("\"at_", "\"@", json.BAV)  
write(json.BAV.at, file = paste(PAX_v6_cor_BAV_IARi$VALUE[PAX_v6_cor_BAV_IARi$KEY == "abbreviation"], "_6th-ed-Interaural-RSA",".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###Preparation:
###(for linkage of BAVs)

PAX_BAVs <- c(list(list(at_type = PAX_v6_cor_BAV_BrLe_list$at_id)),
              list(list(at_type = PAX_v6_cor_BAV_BrRi_list$at_id)),
              list(list(at_type = PAX_v6_cor_BAV_IALe_list$at_id)),
              list(list(at_type = PAX_v6_cor_BAV_IARi_list$at_id)))

#########################################################################################################################################

#########################################################################################################################################
###BRAIN ATLAS:
setwd("Z:/ULRIKE/openMINDS/SANDS/PAX_v6_into-SANDS/generatedFiles")

PAX_v6_cor_BA_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                           at_id = paste("https://openminds.ebrains.eu/instances/brainAtlas/",PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "abbreviation"], sep = ""),
                           at_type = "https://openminds.ebrains.eu/sands/BrainAtlas",
                           abbreviation = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "abbreviation"] == "NA", NA, PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "abbreviation"]),
                           author = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "author"] == "NA", NA, list(list(at_id = PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "author"]))),
                           custodian = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "custodian"] == "NA", NA, list(list(at_id = PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "custodian"]))),
                           description = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "description"] == "NA", NA, PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "description"]),
                           digitalIdentifier = if(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "digitalIdentifier"])},
                           fullName = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "fullName"] == "NA", NA, PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "fullName"]),
                           hasTerminology = PAX_v6_PT_list,
                           hasVersion = PAX_BAVs,
                           homepage = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "homepage"] == "NA", NA, PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "homepage"]),
                           howToCite = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "howToCite"] == "NA", NA, PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "howToCite"]),
                           ontologyIdentifier = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "ontologyIdentifier"] == "NA", NA, PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "ontologyIdentifier"]),
                           shortName = ifelse(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "shortName"] == "NA", NA, PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "shortName"]),
                           usedSpecies = if(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "usedSpecies"] == "NA"){NA}else{list(at_id = PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "usedSpecies"])})

json.BA <- toJSON(PAX_v6_cor_BA_list, pretty = 2, auto_unbox = TRUE)
json.BA.at <- gsub("\"at_", "\"@", json.BA)  
write(json.BA.at, file = paste(PAX_v6_cor_BA$VALUE[PAX_v6_cor_BA$KEY == "abbreviation"],".jsonld", sep = ""))

