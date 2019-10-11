#####################
## Data extraction ##
#####################

rm(list = ls())
# Installs and loads packages
paket <- function(pak){
  new_pak <- pak[!(pak %in% rownames(installed.packages()))]
  if (length(new_pak))
    install.packages(new_pak, dependencies = TRUE,repos='http://cran.us.r-project.org')
  sapply(pak, library, character.only = TRUE)
}
listOfPackages = c("data.table","stringr")
paket(listOfPackages)

###################
## dbGap studies ##
###################

#https://www.ncbi.nlm.nih.gov/projects/gapsolr/facets.html
#Click on Save results as CSV

file_path = "/Users/alba/Desktop/biobankPaper/Biobanks/"

#read the csv file
dbgap <- read.delim(paste0(file_path,"dbGAP_All.csv"), sep = ",")
colnames(dbgap)

#select the columns needed
dbgapSelection <- dbgap[, c("name", "Study.Design", "Study.Disease.Focus","Study.Molecular.Data.Type", 
                   "Study.Markerset", "Study.Consent", "Ancestry..computed.", "accession")]

#select those studies with molecular data type provided
#dbgapSelection <- dbgapSelection[ dbgapSelection$Study.Molecular.Data.Type != "Not Provided", ]
dbgapSelection$link <- paste0( "https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=", dbgapSelection$accession)


## extract the number of samples and subjects for each study
dbgapStudyContent <- data.table(dbgap[, c("accession", "Study.Content")])
dbgapStudyContent <- dbgapStudyContent[ dbgapStudyContent$accession %in% dbgapSelection$accession, ]
dbgapStudyContent <- dbgapStudyContent[,Study.Content:=as.character(Study.Content)]
# Add columns 
add_cols <- c("phenotype.dataset","variables","documents","analyses","molecular.datasets","subjects","samples")
dbgapStudyContent = dbgapStudyContent[,c(add_cols):=list(NA)]


for(i in 1:nrow( dbgapStudyContent ) ){
  print(i)
  mySudyContentData <- do.call("rbind", strsplit(dbgapStudyContent$Study.Content[i], ","))
  if(any(grep( "phenotype", mySudyContentData)) == TRUE){
    dbgapStudyContent$phenotype.dataset[i] <- gsub( " phenotype datasets", "", mySudyContentData[grep( "phenotype", mySudyContentData)])
  }
  if(any(grep( "variables", mySudyContentData)) == TRUE){
    dbgapStudyContent$variables[i] <- gsub( " variables", "", mySudyContentData[grep( "variables", mySudyContentData)])
  }
  if(any(grep( "documents", mySudyContentData)) == TRUE){
    dbgapStudyContent$documents[i] <- gsub( " documents", "", mySudyContentData[grep( "documents", mySudyContentData)])
  }
  if(any(grep( "analyses", mySudyContentData)) == TRUE){
    dbgapStudyContent$analyses[i] <- gsub( " analyses", "", mySudyContentData[grep( "analyses", mySudyContentData)])
  }
  if(any(grep( "molecular", mySudyContentData)) == TRUE){
    dbgapStudyContent$molecular.datasets[i] <- gsub( " molecular datasets", "", mySudyContentData[grep( "molecular", mySudyContentData)])
  }
  if(any(grep( "subjects", mySudyContentData)) == TRUE){
    dbgapStudyContent$subjects[i] <- gsub( " subjects", "", mySudyContentData[grep( "subjects", mySudyContentData)])
  }
  if(any(grep( "samples", mySudyContentData)) == TRUE){
    dbgapStudyContent$samples[i] <- gsub( " samples", "", mySudyContentData[grep( "samples", mySudyContentData)])
  }
}

dbgapStudyContent <- dbgapStudyContent[, c("accession", "variables", "subjects", "samples")]
finalSet <- merge( dbgapSelection, dbgapStudyContent, by="accession")

##add the columns that we need and sort the table
finalSetFilter <- data.table(finalSet)
# Add columns 
add_cols <- c("Country","Patients.Age","Notes","PubMedLink","Phenotypic.Data.Type")
finalSetFilter = finalSetFilter[,c(add_cols):=list(NA)]

finalSetFilterSort <- finalSetFilter[, c("name", "Country", "samples", "subjects", "Study.Design",
                                     "variables", "Phenotypic.Data.Type","Study.Molecular.Data.Type", "Study.Markerset", 
                                     "Study.Disease.Focus", "Patients.Age", "Ancestry..computed.", 
                                     "Study.Consent", "accession", "link", "PubMedLink", "Notes")]

finalSetFilterSort$dbGaP.TOPMed.Study.Accession <-  sapply(strsplit( as.character(finalSetFilterSort$accession), "[.]"), '[', 1)

##select the ones that are in TopMED freeze 5b
topmed <- read.delim(paste0(file_path,"topmedFreeze5b.csv"), sep=',', colClasses = "character")
topmedphs <- unique( c(topmed$Study.Accession, topmed$Parent.Study.Accession))
topmedphs <- topmedphs[topmedphs != "" ]

##check that all of them are in the dbgap downloaded information
if( length(topmedphs[ topmedphs %in% finalSetFilterSort$dbGaP.TOPMed.Study.Accession]) < length(topmedphs) ){
  print(paste0( "There are ", length(topmedphs[ topmedphs %in% finalSetFilterSort$dbGaP.TOPMed.Study.Accession]), " out the ", 
                length(topmedphs), " present in dbGap"))
}
if( length(topmedphs[! topmedphs %in% finalSetFilterSort$dbGaP.TOPMed.Study.Accession ]) != 0){
  print( paste0("The ones that are not present are: ", topmedphs[! topmedphs %in% finalSetFilterSort$dbGaP.TOPMed.Study.Accession ]))
}

finalData <- as.data.frame( matrix( ncol = ncol(finalSetFilterSort)))
colnames(finalData) <- colnames(finalSetFilterSort)

for( i in 1:nrow(topmed)){
  if( topmed$Parent.Study.Accession[i] == "" ){
    selection <- finalSetFilterSort[ finalSetFilterSort$dbGaP.TOPMed.Study.Accession == topmed$Study.Accession[i], ]
    selection$name <- topmed[ i, "Study.Cohort.Abbreviation"]
    selection$Notes <- "TOPMed Freeze 5b"
    finalData <- rbind( finalData, selection)
  }else{
    clinical <- finalSetFilterSort[ finalSetFilterSort$dbGaP.TOPMed.Study.Accession == topmed$Parent.Study.Accession[i], ]
    genomic <- finalSetFilterSort[ finalSetFilterSort$dbGaP.TOPMed.Study.Accession == topmed$Study.Accession[i], ]
    name <-  topmed[ i, "Study.Cohort.Abbreviation"]
    Country <- NA
    samples  <- ifelse(as.numeric(genomic$samples) < as.numeric(clinical$samples), as.numeric(clinical$samples), as.numeric(genomic$samples))
    subjects <- ifelse(as.numeric(genomic$samples) < as.numeric(clinical$samples), as.numeric(clinical$subjects), as.numeric(genomic$subjects))
    studyDesing <- ifelse( as.character(genomic$Study.Design) != as.character(clinical$Study.Design), paste0("Genomic study desing: ", as.character(genomic$Study.Design), "; Clinical study design: ", as.character(clinical$Study.Design)), as.character(genomic$Study.Design))
    phenoVariables <- ifelse(as.numeric(genomic$variables) < as.numeric(clinical$variables), as.numeric(clinical$variables), paste0("Clinical variables are present in both studies: ", as.numeric(clinical$variables), " and ", as.numeric(genomic$variables), "respectively" ))
    phenoData <- NA
    molecularData <- as.character( genomic$Study.Molecular.Data.Type)
    markerset <- as.character( genomic$Study.Markerset)
    disease <- ifelse( as.character(genomic$Study.Disease.Focus) != as.character(clinical$Study.Disease.Focus), paste0("Genomic disease focus: ", as.character(genomic$Study.Disease.Focus), "; Clinical disease focus: ", as.character(clinical$Study.Disease.Focus)), as.character(genomic$Study.Disease.Focus))
    age <- NA
    ancestry <- ifelse(as.numeric(genomic$samples) < as.numeric(clinical$samples), as.character(clinical$Ancestry..computed.), as.character(genomic$Ancestry..computed.))
    consent <- ifelse( as.character(genomic$Study.Consent) != as.character(clinical$Study.Consent), paste0("Genomic study consent: ", as.character(genomic$Study.Consent), "; Clinical study consent: ", as.character(clinical$Study.Consent)), as.character(genomic$Study.Consent))
    # accession <- paste0( "Genomic study: ", as.character( genomic$accession ), "; Clinical study:", as.character(clinical$accession))
    links <- paste0( as.character( genomic$link ), "; ", as.character( clinical$link ) )
    pubmed <- NA
    Notes <- "TOPMed Freeze 5b"
    hps <- paste0( as.character( genomic$accession ), "; ", as.character( clinical$accession ) )
    newrow <- c(name, Country, samples, subjects, studyDesing, phenoVariables, phenoData, molecularData,markerset , disease, age, ancestry,consent, accession, links, pubmed, Notes, hps)
    finalData <- rbind( finalData, newrow)
    }
}

finalSetFilterSort <- finalData[, c(1:17)]
# Names with underscores
colnames(finalSetFilterSort) <- c("Name","Country","Sample_Size","Subject_Count","Study_Design","Phenotypic_Variables",
                                  "Phenotypic_Data_Type","Molecular_Data_Type","Markerset","Disease_Focus",
                                  "Patients_Age","Ancestry","Consent","Accession","Link","PubMedLink","Notes")

finalSetFilterSort$Name <- gsub( ",", ";", finalSetFilterSort$Name)
finalSetFilterSort$Molecular_Data_Type <- gsub( ",", ";", finalSetFilterSort$Molecular_Data_Type)
finalSetFilterSort$Disease_Focus <- gsub( ",", ";", finalSetFilterSort$Disease_Focus)
finalSetFilterSort$Markerset <- gsub( ",", ";", finalSetFilterSort$Markerset)
finalSetFilterSort$Ancestry <- gsub( ",", ";", finalSetFilterSort$Ancestry)
finalSetFilterSort$Consent <- gsub(  ",", ";", finalSetFilterSort$Consent)

# Function to hard-code set values 
setValue <- function(BB_name, select_col, value){ 
  set(f5b, which(f5b[,Name==BB_name]), which(names(f5b)==select_col), value)
}

f5b = data.table(finalSetFilterSort) # freeze5b
f5b = f5b[!is.na(Name),][!is.na(Phenotypic_Variables),]
# Change columns to integers
f5b = f5b[,Sample_Size:=as.integer(Sample_Size)][,Subject_Count:=as.integer(Subject_Count)][,Phenotypic_Variables:=as.integer(Phenotypic_Variables)]
setValue("GeneSTAR","Phenotypic_Variables", 157)
# Filter to PhenoVars>=100 & Remove String values from PhenoVars 
f5b = f5b[str_length(Phenotypic_Variables)<6,][Phenotypic_Variables>99,][order(-Phenotypic_Variables)]
f5b = f5b[,Country:="USA"][, Phenotypic_Data_Type:="Patient/Disease registries"] 
f5b = f5b[, Accession:="<a href='https://dbgap.ncbi.nlm.nih.gov/aa/wga.cgi', target='_blank'>Request access through dbGAP</a>"]
pubMed="<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term="

# FHS - phs000974 (geno) ; phs000007 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=974&version=3&exp1=1&exp2=1&exp3=1
setValue("FHS","Patients_Age", "28-62")
setValue("FHS", "PubMedLink", paste0(pubMed,"5921755[PMID]', target='_blank'>5921755</a>"))
# setValue("FHS", "Phenotypic_Data_Type", "TBD")

# MESA - phs001416 (geno) ; phs000209 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1416&version=1&exp1=1&exp2=1&exp3=1
setValue("MESA","Patients_Age", "45-84")
setValue("MESA", "PubMedLink",  paste0(pubMed,"12397006[PMID]', target='_blank'>12397006</a>"))
# setValue("MESA", "Phenotypic_Data_Type", "TBD")

# ARIC - phs001211 (geno) ; phs000280 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1211&version=2&exp1=1&exp2=1&exp3=1
setValue("ARIC","Patients_Age", "45-64")
setValue("ARIC", "PubMedLink",  paste0(pubMed,"2646917[PMID]', target='_blank'> </a>"))
# setValue("ARIC", "Phenotypic_Data_Type", "TBD")

# CHS - phs001368 (geno) ; phs000287 (pheno)
# Ancestry: Not available for the initial cohort of 5,201 participants
setValue("CHS","Patients_Age", ">65")
setValue("CHS", "PubMedLink",  paste0(pubMed,"1669507[PMID]', target='_blank'> </a>"))
# setValue("CHS", "Phenotypic_Data_Type", "TBD") # See Table 2 : https://reader.elsevier.com/reader/sd/pii/104727979190005W?token=52A6D06132598482C9EFD4EA215DBCAB80A92424DC3CD16B7DAB25823823D1BBE9C9F30FCA0FFB1BD78566ADE54DCF18

# WHI - phs001237 (geno) ; phs000200 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1237&version=1&exp1=1&exp2=1&exp3=1
setValue("WHI","Patients_Age", "50-79") # https://www.whi.org/SitePages/WHI%20Home.aspx
setValue("WHI", "PubMedLink", "https://doi.org/10.1089/jwh.1995.4.519" ) # No PubMed Link; https://www.whi.org/researchers/bibliography/SitePages/Advanced%20Search.aspx
# setValue("WHI", "Phenotypic_Data_Type", "TBD")

# JHS - phs000964 (geno) ; phs000286 (pheno)
setValue("JHS","Ancestry", "African American")
setValue("JHS","Patients_Age", "35-84")
setValue("JHS", "PubMedLink",  paste0(pubMed,"10100686[PMID]', target='_blank'> </a>"))
# setValue("JHS", "Phenotypic_Data_Type", "TBD")

# CFS - phs000954 (geno) ; phs000284 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=284&version=2&exp1=1&exp2=1&exp3=1
setValue("CFS","Patients_Age", " 2-18")
setValue("CFS", "PubMedLink",  paste0(pubMed,"10228121[PMID]', target='_blank'> </a>"))
# setValue("CFS", "Phenotypic_Data_Type", "TBD")

# GENOA - phs001345 (geno) ; phs001238 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=954&version=2&exp1=1&exp2=1&exp3=1
setValue("GENOA","Patients_Age", "<60")
setValue("GENOA", "PubMedLink",  paste0(pubMed,"15121494[PMID]', target='_blank'> </a>"))
# setValue("GENOA", "Phenotypic_Data_Type", "TBD")

# COPDGene - phs000951 (geno) ; phs000179 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=179&version=6&exp1=1&exp2=1&exp3=1
setValue("COPDGene","Patients_Age", "45-80")
setValue("COPDGene", "PubMedLink",  paste0(pubMed,"20214461[PMID]', target='_blank'> </a>"))
# setValue("COPDGene", "Phenotypic_Data_Type", "TBD")

# SAS - phs000972 (geno) ; phs000914 (pheno)
setValue("SAS","Country", "Samoa")
setValue("SAS","Ancestry", "Samoan")
setValue("SAS","Patients_Age", "23-70")
setValue("SAS", "PubMedLink",  paste0(pubMed,"24799123[PMID]', target='_blank'> </a>"))
# setValue("SAS", "Phenotypic_Data_Type", "TBD")

# HyperGEN - phs001293 (geno) 
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=954&version=2&exp1=1&exp2=1&exp3=1
setValue("HyperGEN","Patients_Age", "<60")
setValue("HyperGEN", "PubMedLink",  paste0(pubMed,"11115379[PMID]', target='_blank'> </a>"))
# setValue("HyperGEN", "Phenotypic_Data_Type", "TBD")

# GeneSTAR - phs001218 (geno) ; phs001074 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1218&version=1&exp1=1&exp2=1&exp3=1
setValue("GeneSTAR","Patients_Age", "<60")
setValue("GeneSTAR", "PubMedLink",  paste0(pubMed,"16551714[PMID]', target='_blank'> </a>"))
# setValue("GeneSTAR", "Phenotypic_Data_Type", "TBD")

# GOLDN - phs001218 (geno) ; phs001074 (pheno)
setValue("GOLDN","Ancestry", "Caucasian")
setValue("GOLDN","Patients_Age", ">18")
setValue("GOLDN", "PubMedLink",  paste0(pubMed,"22228203[PMID]', target='_blank'> </a>"))
# setValue("GOLDN", "Phenotypic_Data_Type", "TBD")


colnames(f5b) <- c("Name","Country","Sample Size","Subject Count","Study Design","Phenotypic Variables",
                        "Phenotypic Data Type","Molecular Data Type","Markerset","Disease/Focus",
                        "Patients Age (yrs)","Ancestry","Consent","Accession","Link","PubMed Link","Notes") 
fwrite(f5b, file=paste0(file_path,"freeze5b.csv"), sep = ',', col.names = TRUE)





### add biobank information from Paul excel sheet
biobanks <- read.delim(paste0(file_path,"Biobank Paper - Master Spreadsheet.csv"), sep = ",")
biobanks <- biobanks[, c(1,2,5,7,10:12,14, 16)]




biobanks$Subject.Count <- NA
biobanks$Phenotypic.Variables <- NA
biobanks$Markerset <- NA
biobanks$Disease.Focus <- NA
biobanks$Patients.Age <- NA
biobanks$Consent <- NA
biobanks$Notes <- NA
biobanks$PubMedLink <- NA


biobanks <- biobanks[, c(1, 2, 4, 10, 3, 11,6, 7,12,13,14,5,15,8,9,17,16)]
# Names with spaces
colnames(biobanks) <- c("Name","Country","Sample Size","Subject Count","Study Design","Phenotypic Variables",
                        "Phenotypic Data Type","Molecular Data Type","Markerset","Disease/Focus","Patients Age (yr)",
                        "Ancestry","Consent","Accession","Link","PubMedLink","Notes")

biobanks$`Sample Size` <- gsub( "," , ";", biobanks$`Sample Size`)
biobanks$Ancestry <- gsub( "," , ";", biobanks$Ancestry)
biobanks$`Phenotypic Data Type` <- gsub( "," , ";", biobanks$`Phenotypic Data Type`)
biobanks$`Molecular Data Type` <- gsub( "," , ";", biobanks$`Molecular Data Type`)
biobanks$`Study Design` <- gsub( "," , ";", biobanks$`Study Design`)
biobanks$`Molecular Data Type` <- gsub( "\n" , "", biobanks$`Molecular Data Type`)
biobanks$Accession <- gsub( "\n" , "", biobanks$Accession)
biobanks$Name <- gsub( "\n" , "", biobanks$Name)
biobanks <- biobanks[ biobanks$Name != "TOPMed", ]


write.table( biobanks, file="/Users/alba/Desktop/biobankPaper/Biobanks/biobanksInfoUpdated.csv", 
             sep = ',', 
             col.names = TRUE, 
             row.names = FALSE, 
             quote = FALSE)


dbgapAndBiobanks <- rbind( finalSetFilterSort, biobanks)


write.table( dbgapAndBiobanks, file="/Users/alba/Desktop/biobankPaper/Biobanks/test2.csv", 
             sep = ',', 
             col.names = TRUE, 
             row.names = FALSE, 
             quote = FALSE)
