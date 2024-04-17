setwd("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS")

library("jsonlite")
library("writexl")
library("readxl")
library("tibble")
library("tidyr")
library("dplyr")
library("stringr")

#########################################################################################################################################
###Read excel sheets:
#Terminology with abbreviations:
Swanson_v3_Terms <- read_excel("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/Swanson-v3_terminology.xlsx")

#########################################################################################################################################

#########################################################################################################################################
###PARCELLATION ENTITY VERSIONS: 
###(from terminology)

###lookup/at_id:
Swanson_v3_PEV_labels <- data.frame("name" = Swanson_v3_Terms$name)

##Removes punctuation (!"#$%&???()*+,-./:;<=>?@[]^_`{|}~):
Swanson_v3_PEV_labels$name <- gsub("([[:punct:]])", "", Swanson_v3_PEV_labels$name)
##Removes special punctuation (e.g., "Ammon???s horn"; NB: is not stored correctly, needs to be updated before running the script):
Swanson_v3_PEV_labels$name <- gsub("\\???", "", Swanson_v3_PEV_labels$name)
##Replaces language specific special characters and letters (e.g., Lorente de N?? to Lorente de No):
Swanson_v3_PEV_labels$name <- iconv(Swanson_v3_PEV_labels$name, from = 'UTF-8', to = 'ASCII//TRANSLIT')
##Produces lowerCamelCase, but keeps UpperCamelCase for words that should be capitalised (e.g., Ammon's):
Swanson_v3_PEV_labels$name <- gsub(" ([[:alpha:]])", "\\U\\1", Swanson_v3_PEV_labels$name, perl = TRUE)
##Removes any additional white spaces:
Swanson_v3_PEV_labels$name <- gsub(" ", "", Swanson_v3_PEV_labels$name, perl = TRUE)

###ParcellationEntityVersion table:
Swanson_v3_PEV <- data.frame("at_id" = paste("https://openminds.ebrains.eu/instances/parcellationEntityVersion/SwansonBM_3rd-ed_", Swanson_v3_PEV_labels$name, sep = ""),
                         "at_type" = rep("https://openminds.ebrains.eu/sands/ParcellationEntityVersion", length(Swanson_v3_Terms$name)),
                         "abbreviation" = Swanson_v3_Terms$abbreviation,
                         "additionalRemarks" = rep(NA, length(Swanson_v3_Terms$name)),
                         "alternateName" = rep(NA, length(Swanson_v3_Terms$name)),
                         "correctedName" = rep(NA, length(Swanson_v3_Terms$name)),
                         "hasAnnotation" = rep(NA, length(Swanson_v3_Terms$name)),
                         "hasParent" = rep(NA, length(Swanson_v3_Terms$name)),
                         "lookupLabel" = paste("SwansonBM_3rd-ed_", Swanson_v3_PEV_labels$name, sep = ""),
                         "name" = Swanson_v3_Terms$name,
                         "ontologyIdentifier" = rep(NA, length(Swanson_v3_Terms$name)),
                         "relationAssessment" = rep(NA, length(Swanson_v3_Terms$name)), 
                         "versionIdentifier" = rep("3rd ed.", length(Swanson_v3_Terms$name)),
                         "versionInnovation" = rep(NA, length(Swanson_v3_Terms$name)))

###Manual error correction:
###phpv - ventral propriohypothalamic pathways:
for(i in 1:length(Swanson_v3_PEV$abbreviation)){
  if(Swanson_v3_PEV$abbreviation[i] == "phpv"){
    Swanson_v3_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/SwansonBM_3rd-ed_ventralPropriohypothalamicPathways"
    Swanson_v3_PEV$additionalRemarks[i] <- "Region name does not match the abbreviation (medial propriohypothalamic pathways vs. phpv). The assumed correct name 'ventral propriohypothalamic pathways' has been added under 'correctedName'."
    Swanson_v3_PEV$correctedName[i] <- "ventral propriohypothalamic pathways"
    Swanson_v3_PEV$lookupLabel[i] <- "SwansonBM_3rd-ed_ventralPropriohypothalamicPathways"
  }
}

###REcp - nucleus reuniens, caudal division, posterior part:
for(i in 1:length(Swanson_v3_PEV$abbreviation)){
  if(Swanson_v3_PEV$abbreviation[i] == "REcp"){
    Swanson_v3_PEV$abbreviation[i] <- "REp"
    Swanson_v3_PEV$alternateName[i] <- "REcp"
    Swanson_v3_PEV$additionalRemarks[i] <- "Corrected abbreviation based on ['Errata for Brain maps: structure of the rat brain, 3rd edition'](https://larrywswanson.com/wp-content/uploads/2015/03/BM3-Errata.pdf). 'REcp' has been added under 'alternateName'."
  }
}

###REc - nucleus reuniens, caudal division:
for(i in 1:length(Swanson_v3_PEV$abbreviation)){
  if(Swanson_v3_PEV$abbreviation[i] == "REc"){
    Swanson_v3_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/SwansonBM_3rd-ed_nucleusReuniensCaudalDivision"
    Swanson_v3_PEV$additionalRemarks[i] <- "Corrected name based on ['Errata for Brain maps: structure of the rat brain, 3rd edition'](https://larrywswanson.com/wp-content/uploads/2015/03/BM3-Errata.pdf). The 'nucleus reuniens, caudal division, caudal part' should not exist, but 'nucleus reuniens, caudal division' should. Abbreviation remained. Correct name has been added under 'correctedName'."
    Swanson_v3_PEV$correctedName[i] <- "nucleus reuniens, caudal division"
    Swanson_v3_PEV$lookupLabel[i] <- "SwansonBM_3rd-ed_nucleusReuniensCaudalDivision"
  }
}

#LHAsfa - lateral hypothalamic area, subfornical region, anterior zone:
for(i in 1:length(Swanson_v3_PEV$abbreviation)){
  if(Swanson_v3_PEV$abbreviation[i] == "LHAsfa"){
    Swanson_v3_PEV$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntityVersion/SwansonBM_3rd-ed_lateralHypothalamicAreaSubfornicalRegionAnteriorZone"
    Swanson_v3_PEV$additionalRemarks[i] <- "Corrected name based on ['Errata for Brain maps: structure of the rat brain, 3rd edition'](https://larrywswanson.com/wp-content/uploads/2015/03/BM3-Errata.pdf). The correct name 'lateral hypothalamic area, subfornical region, anterior zone' has been added under 'correctedName'."
    Swanson_v3_PEV$correctedName[i] <- "lateral hypothalamic area, subfornical region, anterior zone"
    Swanson_v3_PEV$lookupLabel[i] <- "SwansonBM_3rd-ed_lateralHypothalamicAreaSubfornicalRegionAnteriorZone"
  }
}

###Manual addition of: 
###REr - nucleus reuniens, rostral division
Swanson_v3_PEV.add <- data.frame("at_id" = "https://openminds.ebrains.eu/instances/parcellationEntityVersion/SwansonBM_3rd-ed_nucleusReuniensRostralDivision",
                                   "at_type" = "https://openminds.ebrains.eu/sands/ParcellationEntityVersion",
                                   "abbreviation" = "REr",
                                   "additionalRemarks" = "Manually added based on ['Errata for Brain maps: structure of the rat brain, 3rd edition'](https://larrywswanson.com/wp-content/uploads/2015/03/BM3-Errata.pdf).",
                                   "alternateName" = NA,
                                   "correctedName" = NA,
                                   "hasAnnotation" = NA,
                                   "hasParent" = NA,
                                   "lookupLabel" = "SwansonBM_3rd-ed_nucleusReuniensRostralDivision",
                                   "name" = "nucleus reuniens, rostral division",
                                   "ontologyIdentifier" = NA,
                                   "relationAssessment" = NA, 
                                   "versionIdentifier" = "3rd ed.",
                                   "versionInnovation" = NA)

Swanson_v3_PEV <- rbind(Swanson_v3_PEV, Swanson_v3_PEV.add)
 
###Export PEVs as excel for visual inspection:
write_xlsx(Swanson_v3_PEV,"Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/generatedFiles/00_forInspectionOnly/Swanson-v3_PEV.xlsx")

###Generate JSON-LDs: 
setwd("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/generatedFiles/PEV_jsonlds")

Swanson_v3_PEV <-Swanson_v3_PEV[order(Swanson_v3_PEV$lookupLabel),]

Swanson_v3_PEV$at_id <- as.character(Swanson_v3_PEV$at_id)
Swanson_v3_PEV$at_type <- as.character(Swanson_v3_PEV$at_type)
Swanson_v3_PEV$abbreviation <- as.character(Swanson_v3_PEV$abbreviation)
Swanson_v3_PEV$additionalRemarks <- as.character(Swanson_v3_PEV$additionalRemarks)
Swanson_v3_PEV$alternateName <- as.character(Swanson_v3_PEV$alternateName)
Swanson_v3_PEV$correctedName <- as.character(Swanson_v3_PEV$correctedName)
Swanson_v3_PEV$hasAnnotation <- as.character(Swanson_v3_PEV$hasAnnotation)
Swanson_v3_PEV$hasParent <- as.character(Swanson_v3_PEV$hasParent)
Swanson_v3_PEV$lookupLabel <- as.character(Swanson_v3_PEV$lookupLabel)
Swanson_v3_PEV$name <- as.character(Swanson_v3_PEV$name)
Swanson_v3_PEV$ontologyIdentifier <- as.character(Swanson_v3_PEV$ontologyIdentifier)
Swanson_v3_PEV$relationAssessment <- as.character(Swanson_v3_PEV$relationAssessment)
Swanson_v3_PEV$versionIdentifier <- as.character(Swanson_v3_PEV$versionIdentifier)
Swanson_v3_PEV$versionInnovation <- as.character(Swanson_v3_PEV$versionInnovation)

for(i in 1:length(Swanson_v3_PEV$abbreviation)){
  Swanson_v3_PEV_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                              at_id = Swanson_v3_PEV$at_id[i],
                              at_type = Swanson_v3_PEV$at_type[i],
                              abbreviation = Swanson_v3_PEV$abbreviation[i],
                              additionalRemarks = Swanson_v3_PEV$additionalRemarks[i],
                              alternateName = ifelse(is.na(Swanson_v3_PEV$alternateName[i]) == TRUE, NA, list(Swanson_v3_PEV$alternateName[i])),
                              correctedName = Swanson_v3_PEV$correctedName[i],
                              hasAnnotation = Swanson_v3_PEV$hasAnnotation[i],
                              hasParent = Swanson_v3_PEV$hasParent[i],
                              lookupLabel = Swanson_v3_PEV$lookupLabel[i],
                              name = Swanson_v3_PEV$name[i],
                              ontologyIdentifier = Swanson_v3_PEV$ontologyIdentifier[i],
                              relationAssessment = Swanson_v3_PEV$relationAssessment[i],
                              versionIdentifier = Swanson_v3_PEV$versionIdentifier[i],
                              versionInnovation = Swanson_v3_PEV$versionInnovation[i])
  
  json.PEV <- toJSON(Swanson_v3_PEV_list, pretty = 2, auto_unbox = TRUE)
  json.PEV.at <- gsub("\"at_", "\"@", json.PEV)  
  write(json.PEV.at, file = paste(Swanson_v3_PEV$lookupLabel[i], ".jsonld", sep = ""))
}

#########################################################################################################################################

#########################################################################################################################################
###PARCELLATION ENTITIES:
###(from terminology - same as PEVs without versionID, uses same labels as PEVs)

###ParcellationEntity table:
Swanson_v3_PE <- data.frame("at_id" = paste("https://openminds.ebrains.eu/instances/parcellationEntity/SwansonBM_", Swanson_v3_PEV_labels$name, sep = ""),
                         "at_type" = rep("https://openminds.ebrains.eu/sands/ParcellationEntity", length(Swanson_v3_Terms$name)),
                         "abbreviation" = Swanson_v3_Terms$abbreviation,
                         "alternateName" = rep(NA, length(Swanson_v3_Terms$name)),
                         "definition" = rep(NA, length(Swanson_v3_Terms$name)),
                         "hasParent" = rep(NA, length(Swanson_v3_Terms$name)),
                         "hasVersion" = rep(NA, length(Swanson_v3_Terms$name)),
                         "lookupLabel" = paste("SwansonBM_", Swanson_v3_PEV_labels$name, sep = ""),
                         "name" = Swanson_v3_Terms$name,
                         "ontologyIdentifier" = rep(NA, length(Swanson_v3_Terms$name)),
                         "relatedUBERONTerm" = rep(NA, length(Swanson_v3_Terms$name)))

###Manual error correction:
###phpv - ventral propriohypothalamic pathways:
for(i in 1:length(Swanson_v3_PE$abbreviation)){
  if(Swanson_v3_PE$abbreviation[i] == "phpv"){
    Swanson_v3_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/SwansonBM_ventralPropriohypothalamicPathways"
    Swanson_v3_PE$name[i] <- "ventral propriohypothalamic pathways"
    Swanson_v3_PE$lookupLabel[i] <- "SwansonBM_ventralPropriohypothalamicPathways"
  }
}

###REcp - nucleus reuniens, caudal division, posterior part:
for(i in 1:length(Swanson_v3_PE$abbreviation)){
  if(Swanson_v3_PE$abbreviation[i] == "REcp"){
    Swanson_v3_PE$abbreviation[i] <- "REp"
    Swanson_v3_PE$alternateName[i] <- "REcp"
  }
}

###REc - nucleus reuniens, caudal division:
for(i in 1:length(Swanson_v3_PE$abbreviation)){
  if(Swanson_v3_PE$abbreviation[i] == "REc"){
    Swanson_v3_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/SwansonBM_nucleusReuniensCaudalDivision"
    Swanson_v3_PE$name[i] <- "nucleus reuniens, caudal division"
    Swanson_v3_PE$lookupLabel[i] <- "SwansonBM_nucleusReuniensCaudalDivision"
  }
}

#LHAsfa - lateral hypothalamic area, subfornical region, anterior zone:
for(i in 1:length(Swanson_v3_PE$abbreviation)){
  if(Swanson_v3_PE$abbreviation[i] == "LHAsfa"){
    Swanson_v3_PE$at_id[i] <- "https://openminds.ebrains.eu/instances/parcellationEntity/SwansonBM_lateralHypothalamicAreaSubfornicalRegionAnteriorZone"
    Swanson_v3_PE$name[i] <- "lateral hypothalamic area, subfornical region, anterior zone"
    Swanson_v3_PE$lookupLabel[i] <- "SwansonBM_lateralHypothalamicAreaSubfornicalRegionAnteriorZone"
  }
}

###Manual addition of: 
###REr - nucleus reuniens, rostral division
Swanson_v3_PE.add <- data.frame("at_id" = "https://openminds.ebrains.eu/instances/parcellationEntity/SwansonBM_nucleusReuniensRostralDivision",
                                 "at_type" = "https://openminds.ebrains.eu/sands/ParcellationEntity",
                                 "abbreviation" = "REr",
                                 "alternateName" = NA,
                                 "definition" = NA,
                                 "hasParent" = NA,
                                 "hasVersion" = NA,
                                 "lookupLabel" = "SwansonBM_nucleusReuniensRostralDivision",
                                 "name" = "nucleus reuniens, rostral division",
                                 "ontologyIdentifier" = NA,
                                 "relatedUBERONTerm" = NA)

Swanson_v3_PE <- rbind(Swanson_v3_PE, Swanson_v3_PE.add)

for(i in 1:length(Swanson_v3_PE$at_id)){
  for(j in 1:length(Swanson_v3_PEV$at_id)){
    if(Swanson_v3_PE$abbreviation[i] == Swanson_v3_PEV$abbreviation[j]){
      Swanson_v3_PE$hasVersion[i] <- Swanson_v3_PEV$at_id[j]
    }
  }

}

###Export PEs as excel for visual inspection:
write_xlsx(Swanson_v3_PE,"Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/generatedFiles/00_forInspectionOnly/Swanson_PE-only-v3.xlsx")

###Generate JSON-LDs: 
setwd("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/generatedFiles/PE_jsonlds")

Swanson_v3_PE <-Swanson_v3_PE[order(Swanson_v3_PE$lookupLabel),]

Swanson_v3_PE$at_id <- as.character(Swanson_v3_PE$at_id)
Swanson_v3_PE$at_type <- as.character(Swanson_v3_PE$at_type)
Swanson_v3_PE$abbreviation <- as.character(Swanson_v3_PE$abbreviation)
Swanson_v3_PE$alternateName <- as.character(Swanson_v3_PE$alternateName)
Swanson_v3_PE$definition <- as.character(Swanson_v3_PE$definition)
Swanson_v3_PE$hasParent <- as.character(Swanson_v3_PE$hasParent)
Swanson_v3_PE$hasVersion <- as.character(Swanson_v3_PE$hasVersion)
Swanson_v3_PE$lookupLabel <- as.character(Swanson_v3_PE$lookupLabel)
Swanson_v3_PE$name <- as.character(Swanson_v3_PE$name)
Swanson_v3_PE$ontologyIdentifier <- as.character(Swanson_v3_PE$ontologyIdentifier)
Swanson_v3_PE$relatedUBERONTerm <- as.character(Swanson_v3_PE$relatedUBERONTerm)

for(i in 1:length(Swanson_v3_PE$abbreviation)){
  Swanson_v3_PE_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                             at_id = Swanson_v3_PE$at_id[i],
                             at_type = Swanson_v3_PE$at_type[i],
                             abbreviation = Swanson_v3_PE$abbreviation[i],
                             alternateName = ifelse(is.na(Swanson_v3_PE$alternateName[i]) == TRUE, NA, list(Swanson_v3_PE$alternateName[i])),
                             definition = Swanson_v3_PE$definition[i],
                             hasParent = Swanson_v3_PE$hasParent[i],
                             hasVersion = list(list(at_id = Swanson_v3_PE$hasVersion[i])),
                             lookupLabel = Swanson_v3_PE$lookupLabel[i],
                             name = Swanson_v3_PE$name[i],
                             ontologyIdentifier = Swanson_v3_PE$ontologyIdentifier[i],
                             relatedUBERONTerm = Swanson_v3_PE$relatedUBERONTerm[i])
  
  json.PE <- toJSON(Swanson_v3_PE_list, pretty = 2, auto_unbox = TRUE)
  json.PE.at <- gsub("\"at_", "\"@", json.PE)  
  write(json.PE.at, file = paste(Swanson_v3_PE$lookupLabel[i], ".jsonld", sep = ""))
}

#########################################################################################################################################

#########################################################################################################################################
###PARCELLATION TERMINOLOGY VERSION:
###(will be embedded in brain atlas version)
setwd("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/generatedFiles/00_forInspectionOnly")

###Generate list used in JSON-LD:
entity.list <- list(list(at_id = Swanson_v3_PEV$at_id[1]))
for(i in 2:length(Swanson_v3_PEV$at_id)) {
  entity.list <- c(entity.list,list(list(at_id = Swanson_v3_PEV$at_id[i])))
}

Swanson_v3_PTV_list <- list(at_type = "https://openminds.ebrains.eu/sands/ParcellationTerminologyVersion",
                           dataLocation = NA,
                           hasEntity = entity.list,
                           ontologyIdentifier = NA)

###Generate JSON-LD for visual inspection:
json.PTV <- toJSON(Swanson_v3_PTV_list, pretty = 2, auto_unbox = TRUE)
json.PTV.at <- gsub("\"at_", "\"@", json.PTV)  
write(json.PTV.at, file = paste("Swanson_v3_PTV", ".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###PARCELLATION TERMINOLOGY:
###(will be embedded in brain atlas)
setwd("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/generatedFiles/00_forInspectionOnly")

entity.list <- list(list(at_id = Swanson_v3_PE$at_id[1]))
for(i in 2:length(Swanson_v3_PE$at_id)) {
  entity.list <- c(entity.list,list(list(at_id = Swanson_v3_PE$at_id[i])))
}

Swanson_v3_PT_list <- list(at_type = "https://openminds.ebrains.eu/sands/ParcellationTerminology",
                            dataLocation = NA,
                            hasEntity = entity.list,
                            ontologyIdentifier = NA)

json.PT <- toJSON(Swanson_v3_PT_list, pretty = 2, auto_unbox = TRUE)
json.PT.at <- gsub("\"at_", "\"@", json.PT)  
write(json.PT.at, file = paste("Swanson_PT-only-v3", ".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###In openMINDS prepared brain atlas & version, common coordinate space & version, person:
#BA & BAV:
Swanson_v3_BA <- read_excel("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/Swanson-v3_BA.xlsx")
Swanson_v3_BAV <- read_excel("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/Swanson-v3_BAV.xlsx")

#CCS & CCSV:
Swanson_v3_CCS <- read_excel("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/Swanson-v3_CCS.xlsx")
Swanson_v3_CCSV <- read_excel("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/Swanson-v3_CCSV_v1992.xlsx")


#########################################################################################################################################

#########################################################################################################################################
###Preparation:
###AXES ORIGIN:
setwd("Z:/ULRIKE/openMINDS/SANDS/Swanson-v3_into-SANDS/generatedFiles/00_forInspectionOnly")

Swanson_AxesOr <- c(list(list(at_type = "https://openminds.ebrains.eu/core/QuantitativeValue",
                              typeOfUncertainty = NA,
                              uncertainty = NA,
                              unit =  if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "nativeUnit"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "nativeUnit"])},
                              value = 0)),
                    list(list(at_type = "https://openminds.ebrains.eu/core/QuantitativeValue",
                              typeOfUncertainty = NA,
                              uncertainty = NA,
                              unit =  if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "nativeUnit"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "nativeUnit"])},
                              value = 12)),
                    list(list(at_type = "https://openminds.ebrains.eu/core/QuantitativeValue",
                              typeOfUncertainty = NA,
                              uncertainty = NA,
                              unit =  if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "nativeUnit"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "nativeUnit"])},
                              value = 8.11)))

###Generate JSON-LD for visual inspection:
json.AxesOr <- toJSON(Swanson_AxesOr, pretty = 2, auto_unbox = TRUE)
json.AxesOr.at <- gsub("\"at_", "\"@", json.AxesOr)  
write(json.AxesOr.at, file = paste("Swanson_AxesOr", ".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###COMMON COORDINATE SPACE VERSION:
setwd("Z:/ULRIKE/openMINDS/Swanson-v3_into-SANDS/generatedFiles")

Swanson_v3_CCSV_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                             at_id = paste("https://openminds.ebrains.eu/instances/commonCoordinateSpaceVersion/",Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "abbreviation"],"_v1992", sep = ""),
                             at_type = "https://openminds.ebrains.eu/core/CommonCoordinateSpaceVersion",
                             abbreviation = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "abbreviation"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "abbreviation"]),
                             accessibilty = if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "accessibility"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "accessibility"])},
                             anatomicalAxesOrientation = if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "anatomicalAxesOrientation"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "anatomicalAxesOrientation"])},
                             author = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "author"] == "NA", NA, list(list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "author"]))),
                             axesOrigin = Swanson_AxesOr,
                             copyright = NA,
                             custodian = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "custodian"] == "NA", NA, list(list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "custodian"]))),
                             defaultImage = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "defaultImage"] == "NA", NA, list(list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "defaultImage"]))),
                             description = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "description"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "description"]),
                             digitalIdentifier = if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "digitalIdentifier"])},
                             fullDocumentation = if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "fullDocumentation"])},
                             fullName = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "fullName"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "fullName"]),
                             funding = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "funding"] == "NA", NA, list(list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "funding"]))),
                             homepage = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "homepage"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "homepage"]),
                             howToCite = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "howToCite"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "howToCite"]),
                             isAlternativeVersionOf = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "isAlternativeVersionOf"]))),
                             isNewVersionOf = if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "isNewVersionOf"])},
                             keyword = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "keyword"] == "NA", NA, list(list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "keyword"]))), 
                             license = if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "license"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "license"])}, 
                             nativeUnit = if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "nativeUnit"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "nativeUnit"])},
                             ontologyIdentifier = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "ontologyIdentifier"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "ontologyIdentifier"]),
                             otherContribution = NA,
                             relatedPublication = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "relatedPublication"]))), 
                             releaseDate = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "releaseDate"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "releaseDate"]),
                             repository = if(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "repository"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "repository"])},
                             shortName = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "shortName"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "shortName"]),
                             supportChannel = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "supportChannel"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "supportChannel"]),
                             usedSpecimen = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "usedSpecimen"]))),
                             versionIdentifier = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "versionIdentifier"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "versionIdentifier"]),
                             versionInnovation = ifelse(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "versionInnovation"] == "NA", NA, Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "versionInnovation"]))

json.CCSV <- toJSON(Swanson_v3_CCSV_list, pretty = 2, auto_unbox = TRUE)
json.CCSV.at <- gsub("\"at_", "\"@", json.CCSV)  
write(json.CCSV.at, file = paste(Swanson_v3_CCSV$VALUE[Swanson_v3_CCSV$KEY == "abbreviation"],"_v1992",".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###COMMON COORDINATE SPACE:
setwd("Z:/ULRIKE/openMINDS/Swanson-v3_into-SANDS/generatedFiles")

Swanson_v3_CCS_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                            at_id = paste("https://openminds.ebrains.eu/instances/commonCoordinateSpace/",Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "abbreviation"], sep = ""),
                            at_type = "https://openminds.ebrains.eu/core/CommonCoordinateSpace",
                            abbreviation = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "abbreviation"] == "NA", NA, Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "abbreviation"]),
                            author = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "author"] == "NA", NA, list(list(at_id = Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "author"]))),
                            custodian = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "custodian"] == "NA", NA, list(list(at_id = Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "custodian"]))),
                            description = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "description"] == "NA", NA, Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "description"]),
                            digitalIdentifier = if(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "digitalIdentifier"])},
                            fullName = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "fullName"] == "NA", NA, Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "fullName"]),
                            hasVersion = list(list(at_id = Swanson_v3_CCSV_list$at_id)),
                            homepage = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "homepage"] == "NA", NA, Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "homepage"]),
                            howToCite = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "howToCite"] == "NA", NA, Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "howToCite"]),
                            ontologyIdentifier = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "ontologyIdentifier"] == "NA", NA, Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "ontologyIdentifier"]),
                            shortName = ifelse(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "shortName"] == "NA", NA, Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "shortName"]),
                            usedSpecies = if(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "usedSpecies"] == "NA"){NA}else{list(at_id = Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "usedSpecies"])})

json.CCS <- toJSON(Swanson_v3_CCS_list, pretty = 2, auto_unbox = TRUE)
json.CCS.at <- gsub("\"at_", "\"@", json.CCS)  
write(json.CCS.at, file = paste(Swanson_v3_CCS$VALUE[Swanson_v3_CCS$KEY == "abbreviation"],".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###BRAIN ATLAS VERSION:
setwd("Z:/ULRIKE/openMINDS/Swanson-v3_into-SANDS/generatedFiles")

Swanson_v3_BAV_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                             at_id = paste("https://openminds.ebrains.eu/instances/brainAtlasVersion/",Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "abbreviation"],"_3rd-ed", sep = ""),
                             at_type = "https://openminds.ebrains.eu/core/BrainAtlasVersion",
                             abbreviation = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "abbreviation"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "abbreviation"]),
                             accessibility = if(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "accessibility"] == "NA"){NA}else{list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "accessibility"])},
                             author = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "author"] == "NA", NA, list(list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "author"]))),
                             coordinateSpace = if(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "coordinateSpace"] == "NA"){NA}else{list(at_id = Swanson_v3_CCSV_list$at_id)},
                             copyright = NA,
                             custodian = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "custodian"] == "NA", NA, list(list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "custodian"]))),
                             description = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "description"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "description"]),
                             digitalIdentifier = if(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = paste("https://openminds.ebrains.eu/instances/ISBN/", Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "digitalIdentifier"], sep = ""))},
                             fullDocumentation = if(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "fullDocumentation"] == "NA"){NA}else{list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "fullDocumentation"])},
                             fullName = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "fullName"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "fullName"]),
                             funding = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "funding"] == "NA", NA, list(list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "funding"]))),
                             hasTerminology = Swanson_v3_PTV_list,
                             homepage = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "homepage"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "homepage"]),
                             howToCite = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "howToCite"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "howToCite"]),
                             isAlternativeVersionOf = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "isAlternativeVersionOf"] == "NA", NA, list(list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "isAlternativeVersionOf"]))),
                             isNewVersionOf = if(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "isNewVersionOf"] == "NA"){NA}else{list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "isNewVersionOf"])},
                             keyword = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "keyword"] == "NA", NA, list(list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "keyword"]))), 
                             license = if(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "license"] == "NA"){NA}else{list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "license"])},
                             majorVersionIdentifier = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "majorVersionIdentifier"] == "NA", NA, list(list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "majorVersionIdentifier"]))), 
                             ontologyIdentifier = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "ontologyIdentifier"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "ontologyIdentifier"]),
                             otherContribution = NA,
                             relatedPublication = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "relatedPublication"] == "NA", NA, list(list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "relatedPublication"]))), 
                             releaseDate = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "releaseDate"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "releaseDate"]),
                             repository = if(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "repository"] == "NA"){NA}else{list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "repository"])},
                             shortName = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "shortName"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "shortName"]),
                             supportChannel = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "supportChannel"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "supportChannel"]),
                             type = if(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "type"] == "NA"){NA}else{list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "type"])},
                             usedSpecimen = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "usedSpecimen"] == "NA", NA, list(list(at_id = Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "usedSpecimen"]))),
                             versionIdentifier = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "versionIdentifier"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "versionIdentifier"]),
                             versionInnovation = ifelse(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "versionInnovation"] == "NA", NA, Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "versionInnovation"]))

json.BAV <- toJSON(Swanson_v3_BAV_list, pretty = 2, auto_unbox = TRUE)
json.BAV.at <- gsub("\"at_", "\"@", json.BAV)  
write(json.BAV.at, file = paste(Swanson_v3_BAV$VALUE[Swanson_v3_BAV$KEY == "abbreviation"], "_3rd-ed",".jsonld", sep = ""))

#########################################################################################################################################

#########################################################################################################################################
###BRAIN ATLAS:
setwd("Z:/ULRIKE/openMINDS/Swanson-v3_into-SANDS/generatedFiles")

Swanson_v3_BA_list <- list(at_context = list(at_vocab = "https://openminds.ebrains.eu/vocab/"),
                            at_id = paste("https://openminds.ebrains.eu/instances/brainAtlas/",Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "abbreviation"], sep = ""),
                            at_type = "https://openminds.ebrains.eu/core/BrainAtlas",
                            abbreviation = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "abbreviation"] == "NA", NA, Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "abbreviation"]),
                            author = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "author"] == "NA", NA, list(list(at_id = Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "author"]))),
                            custodian = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "custodian"] == "NA", NA, list(list(at_id = Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "custodian"]))),
                            description = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "description"] == "NA", NA, Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "description"]),
                            digitalIdentifier = if(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "digitalIdentifier"] == "NA"){NA}else{list(at_id = Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "digitalIdentifier"])},
                            fullName = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "fullName"] == "NA", NA, Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "fullName"]),
                            hasTerminology = Swanson_v3_PT_list,
                            hasVersion = list(list(at_id = Swanson_v3_BAV_list$at_id)),
                            homepage = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "homepage"] == "NA", NA, Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "homepage"]),
                            howToCite = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "howToCite"] == "NA", NA, Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "howToCite"]),
                            ontologyIdentifier = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "ontologyIdentifier"] == "NA", NA, Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "ontologyIdentifier"]),
                            shortName = ifelse(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "shortName"] == "NA", NA, Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "shortName"]),
                            usedSpecies = if(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "usedSpecies"] == "NA"){NA}else{list(at_id = Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "usedSpecies"])})

json.BA <- toJSON(Swanson_v3_BA_list, pretty = 2, auto_unbox = TRUE)
json.BA.at <- gsub("\"at_", "\"@", json.BA)  
write(json.BA.at, file = paste(Swanson_v3_BA$VALUE[Swanson_v3_BA$KEY == "abbreviation"],".jsonld", sep = ""))


