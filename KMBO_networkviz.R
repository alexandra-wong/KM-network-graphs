#Import libraries
library(tidyr)
library(dplyr)
library(stringr)


## Read csv file in as a dataframe
df <- read.csv('/Users/wongale/OneDrive - York University/KMbO - EDF/Shruti_EDF_Database.csv')
head(db)

# NODES
## End result: Create a dataframe with "Id", "Label", "Size", "Category", "Subcategory"
## Create a list of nodes given "KMbO", "KMbO2" PrimaryDepartmentalAffiliation", "SecondaryDepartmentalAffiliation", 
##    "PrimaryHospitalAffiliation" columns
dfWorking <- df[c('Name', 'KMbO', 'KMbO2', 'PrimaryDepartmentalAffiliation', 'SecondaryDepartmentalAffiliation', 'PrimaryHospitalAffiliation', 'PrimIntegratedHospAffiliation')]

## Retrieve all unique values from these columns = Node Label
dfWorking <- dfWorking %>% separate(KMbO2, c("KMbO2", "KMbO3"), sep="/")
dfNodesKMBOWorking <- c(as.character(dfWorking$KMbO), as.character(dfWorking$KMbO2), as.character(dfWorking$KMbO3))
dfNodesKMBOWorking <- as.data.frame(dfNodesKMBOWorking)
colnames(dfNodesKMBOWorking) <- ("Label")
dfNodesDept <- unique(data.frame(Label=union(dfWorking$PrimaryDepartmentalAffiliation, dfWorking$SecondaryDepartmentalAffiliation)))
dfNodesHosp <- unique(data.frame(Label=dfWorking$PrimIntegratedHospAffiliation))

## Remove unncessary values like "None"
dfNodesKMBOWorking <- subset(dfNodesKMBOWorking, Label!="None")
dfNodesDept <- subset(dfNodesDept, Label!="None")
dfNodesHosp <- subset(dfNodesHosp, Label!="None")

## Add Hospitals and Departments that are not linked but are part of TAHSN or the Temerty Faculty Departments
dfDeptToAdd <- data.frame(Label = c("Biochemistry", "Immunology", "Medical Imaging", "Molecular Genetics", "Nutritional Sciences", "Ophthalmology and Vision Sciences", "Otolaryngology - Head and Neck Surgery", "Pharmacology and Toxicology"))
dfNodesDept <- rbind(dfNodesDept, dfDeptToAdd)

## Sum up how often each KMbO value is mentioned for Size
dfNodesKMBO <- dfNodesKMBOWorking %>% count(Label)
colnames(dfNodesKMBO) <- c("Label", "Size")

## Assign Size = 1 for departments, hospitals, hospital affiliations
dfNodesDept["Size"] <- 1
dfNodesHosp["Size"] <- 1

## Add a category to each node - "KMbO", "Department", "Hospital", "Hospital affiliation"
dfNodesKMBO["Category"] <- "KMbO"
dfNodesKMBO["Subcategory"] <- "N/A"
dfNodesDept["Category"] <- "Department"
dfNodesHosp["Category"] <- "Hospital"

## Add subcategories for Departments and Hospitals
dfNodesHosp <- dfNodesHosp %>% mutate(Subcategory = ifelse(dfNodesHosp$Label == "UHN" | dfNodesHosp$Label == "Unity Health", "Hospital Network", "Hospital"))
dfNodesDept <- dfNodesDept %>% mutate(Subcategory = ifelse(dfNodesDept$Label == "Dalla Lana School of Public Health" | dfNodesDept$Label == "Faculty of Pharmacy" | dfNodesDept$Label == "Faculty of Dentistry", "Faculty", "Department"))


## Add if Departments and Hospitals have Research Units
dfNodesKMBO["ResearchUnitName"] <- "N/A"
dfNodesKMBO["ResearchUnit"] <- "N/A"
dfNodesDept <- dfNodesDept %>% mutate(ResearchUnitName = ifelse(dfNodesDept$Label == "Medicine", "CREST", "N/A"))
dfNodesDept[dfNodesDept$Label=="Family and Community Medicine", "ResearchUnitName"] <- "OES"
dfNodesDept <- dfNodesDept %>% mutate(ResearchUnit = ifelse(dfNodesDept$ResearchUnitName == "N/A", FALSE, TRUE))

dfNodesHosp["ResearchUnitName"] <- "N/A"
dfNodesHosp <- dfNodesHosp %>% mutate(ResearchUnit = ifelse(dfNodesHosp$Label == "Women's College Hospital" | dfNodesHosp$Label == "Sinai Health" | dfNodesHosp$Label == "Baycrest", FALSE, TRUE))
dfNodesHosp[dfNodesHosp$Label == "Centre for Addiction & Mental Health", "ResearchUnitName"] <- "Unnamed"
dfNodesHosp[dfNodesHosp$Label == "Holland Bloorview Kids Rehabilitation Hospital", "ResearchUnitName"] <- "Teaching & Learning Institute"
dfNodesHosp[dfNodesHosp$Label == "Hospital for Sick Children", "ResearchUnitName"] <- "SickKids Learning Institute"
dfNodesHosp[dfNodesHosp$Label == "Unity Health", "ResearchUnitName"] <- "Applied Education Research Operatives (AERO) - St. Michael's"
dfNodesHosp[dfNodesHosp$Label == "Sunnybrook Health Sciences Centre", "ResearchUnitName"] <- "ERU"
dfNodesHosp[dfNodesHosp$Label == "UHN", "ResearchUnitName"] <- "TIER; CEEP"

## Add affiliations for Hospitals, N/A columns for KMBO and Dept when binding
dfNodesHosp <- dfNodesHosp %>% mutate(AffiliationType = ifelse(dfNodesHosp$Label == "Michael Garron Hospital" | 
                                                                 dfNodesHosp$Label == "North York General Hospital" | 
                                                                 dfNodesHosp$Label == "Trillium Health Partners", 
                                                            "Associate Affiliated", "Fully Affiliated"))
dfNodesKMBO["AffiliationType"] <- "N/A"
dfNodesDept["AffiliationType"] <- "N/A"

## Combine dataframes of KMbO, Department, Hospital, Hospital Affiliation into one large dataframe
dfNodes <- rbind(dfNodesKMBO, dfNodesDept, dfNodesHosp)
## Assign a number ID to each node, 0-n
dfNodes["Id"] <- 0:(length(dfNodes$Label)-1)
## Reorder columns as needed (ID needs to be first for Gephi, then Label, then others)
dfNodes <- dfNodes[, c(8, 1, 2, 3, 4, 5, 6, 7)]

#EDGES
## Create a new dataframe where all Labels are converted to Ids 
dfEdgesWorking <- dfWorking
lookup <- dfNodes[c("Id", "Label")]
dfEdgesWorking$KMbO <- lapply(dfEdgesWorking$KMbO, function(x) lookup$Id[match(x, lookup$Label)])
dfEdgesWorking$KMbO2 <- lapply(dfEdgesWorking$KMbO2, function(x) lookup$Id[match(x, lookup$Label)])
dfEdgesWorking$KMbO3 <- lapply(dfEdgesWorking$KMbO3, function(x) lookup$Id[match(x, lookup$Label)])
dfEdgesWorking$PrimaryDepartmentalAffiliation <- lapply(dfEdgesWorking$PrimaryDepartmentalAffiliation, function(x) lookup$Id[match(x, lookup$Label)])
dfEdgesWorking$SecondaryDepartmentalAffiliation <- lapply(dfEdgesWorking$SecondaryDepartmentalAffiliation, function(x) lookup$Id[match(x, lookup$Label)])
dfEdgesWorking$PrimIntegratedHospAffiliation <- lapply(dfEdgesWorking$PrimIntegratedHospAffiliation, function(x) lookup$Id[match(x, lookup$Label)])

## Create a new Working dataframe for creating edges 
dfEdgesWorking2 <- dfEdgesWorking
dfEdgesWorking2["ParticipantId"] <- 1:(length(dfEdgesWorking2$Name))

## Create a dataframe for edges with columns "Source", "Target", "Type", "Category", and "ParticipantID"
dfEdges = data.frame(matrix(
  vector(), 0, 5, dimnames=list(c(), c("Source", "Target", "Type", "Category", "ParticipantID"))),
  stringsAsFactors=F)


## Add all KMbO/KMbO edges
for(i in 1:nrow(dfEdgesWorking2)) { 
  if(is.na(dfEdgesWorking2[i, 3]) == FALSE) {
    dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[i, 2] #Set Source
    dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[i, 3] #Set Target
    dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[i, 9] #Set ParticipantID
  }
  if(is.na(dfEdgesWorking2[i, 4]) == FALSE) {
    dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[i, 2] #Set Source
    dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[i, 4] #Set Target
    dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[i, 9] #Set ParticipantID
    
    dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[i, 3] #Set Source
    dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[i, 4] #Set Target
    dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[i, 9] #Set ParticipantID
  }
}
dfEdges[, 4] <- "KMbO/KMbO" #Set Category

## Add all KMbO/Department edges
rows = nrow(dfEdges) + 1
for(j in 1:nrow(dfEdgesWorking2)) {  
  if(is.na(dfEdgesWorking2[j, 5]) == FALSE) { #PrimaryDepartment, 1st KMbO
    dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 2] #Set Source
    dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 5] #Set Target
    dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
    
    if(is.na(dfEdgesWorking2[j, 3]) == FALSE) {#PrimaryDepartment, 2nd KMbO
      dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 3] #Set Source
      dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 5] #Set Target
      dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
      
      if(is.na(dfEdgesWorking2[j, 4]) == FALSE) { #PrimaryDepartment, 3rd KMbO
        dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 4] #Set Source
        dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 5] #Set Target
        dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
      }
    }
  }

  if(is.na(dfEdgesWorking2[j, 6]) == FALSE) { #SecondaryDepartment, 1st KMbO
    dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 2] #Set Source
    dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 6] #Set Target
    dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
  
    if(is.na(dfEdgesWorking2[j, 3]) == FALSE) {#SecondaryDepartment, 2nd KMbO
      dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 3] #Set Source
      dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 6] #Set Target
      dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
      
      if(is.na(dfEdgesWorking2[j, 4]) == FALSE) { #SecondaryDepartment, 3rd KMbO
        dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 4] #Set Source
        dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 6] #Set Target
        dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
      }
    }
  }
}
dfEdges[rows:nrow(dfEdges), 4] <- "KMbO/Department" #Set Category

## Add all KMbO/Hospital edges
rows2 = nrow(dfEdges) + 1
for(j in 1:nrow(dfEdgesWorking2)) {  
  if(is.na(dfEdgesWorking2[j, 8]) == FALSE) { #Hospital, 1st KMbO
    dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 2] #Set Source
    dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 8] #Set Target
    dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
    
    if(is.na(dfEdgesWorking2[j, 3]) == FALSE) {#Hospital, 2nd KMbO
      dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 3] #Set Source
      dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 8] #Set Target
      dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
      
      if(is.na(dfEdgesWorking2[j, 4]) == FALSE) { #Hospital, 3rd KMbO
        dfEdges[nrow(dfEdges)+1, 1] <- dfEdgesWorking2[j, 4] #Set Source
        dfEdges[nrow(dfEdges), 2] <- dfEdgesWorking2[j, 8] #Set Target
        dfEdges[nrow(dfEdges), 5] <- dfEdgesWorking2[j, 9] #Set ParticipantID
      }
    }
  }
}
dfEdges[rows2:nrow(dfEdges), 4] <- "KMbO/Hospital" #Set Category

## Set Type Column for all rows
dfEdges[, 3] <- "Undirected" 


# WRITE TO CSV 
write.csv(dfNodes, file = '/Users/wongale/OneDrive - York University/KMbO - EDF/Nodes-20220506.csv', row.names=FALSE)
write.csv(dfEdges, file = '/Users/wongale/OneDrive - York University/KMbO - EDF/Edges-20220506.csv', row.names=FALSE)
