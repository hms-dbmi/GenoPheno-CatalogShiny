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
# Go to  https://www.ncbi.nlm.nih.gov/projects/gapsolr/facets.html, click on Save Results and save as: dbGAP_All.csv
#read the csv file 
file_path = "/full-path/GenoPheno-CatalogShiny/csv/"
dbgap <- read.delim(paste0(file_path,"dbGAP_All.csv"), sep = ",")
colnames(dbgap)
colnames(dbgap)[2] <- "Name"

#select the columns needed
dbgapSelection <- dbgap[, c("Name", "Study.Design", "Study.Disease.Focus","Study.Molecular.Data.Type", 
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

finalSetFilterSort <- finalSetFilter[, c("Name", "Country", "samples", "subjects", "Study.Design",
                                     "variables", "Phenotypic.Data.Type","Study.Molecular.Data.Type", "Study.Markerset", 
                                     "Study.Disease.Focus", "Patients.Age", "Ancestry..computed.", 
                                     "Study.Consent", "accession", "link", "PubMedLink", "Notes")]

finalSetFilterSort$dbGaP.TOPMed.Study.Accession <-  sapply(strsplit( as.character(finalSetFilterSort$accession), "[.]"), '[', 1)

# From https://www.nhlbiwgs.org/topmed-whole-genome-sequencing-project-freeze-5b-phases-1-and-2
# Copy and save Table 1 into: topmedFreeze5b.csv
topmed <- read.delim(paste0(file_path,"topmedFreeze5b.csv"), sep=',', colClasses = "character")
topmedphs <- unique( c(topmed$Study.Accession, topmed$Parent.Study.Accession))
topmedphs <- topmedphs[topmedphs != "" ]

##check that all of them are in the dbgap downloaded information
topmedphs_parent <- topmed$Parent.Study.Accession
topmedphs_parent <- topmedphs_parent[topmedphs_parent != "" ]
topmedphs_children <- topmed$Study.Accession
topmedphs_children <- topmedphs_children[topmedphs_children != "" ]

if( length(topmedphs_parent[! topmedphs_parent %in% finalSetFilterSort$dbGaP.TOPMed.Study.Accession ]) != 0){
  not_present=topmedphs_parent[! topmedphs_parent %in% finalSetFilterSort$dbGaP.TOPMed.Study.Accession ]
  print( paste0("Parent phs not present in dbGAP: ", not_present  ))
  topmed$Parent.Study.Accession = gsub(not_present, "", topmed$Parent.Study.Accession)
}
if( length(topmedphs_children[! topmedphs_children %in% finalSetFilterSort$dbGaP.TOPMed.Study.Accession ]) != 0){
  not_present=topmedphs_children[! topmedphs_children %in% finalSetFilterSort$dbGaP.TOPMed.Study.Accession ]
  print( paste0("Parent phs not present in dbGAP: ", not_present  ))
  topmed$Study.Accession = gsub(not_present, "", topmed$Study.Accession)
}

finalData <- as.data.frame( matrix( ncol = ncol(finalSetFilterSort)))
colnames(finalData) <- colnames(finalSetFilterSort)
finalData$extraLink <- NA

for( i in 1:nrow(topmed)){
  if( topmed$Study.Accession[i] == "" )
    next()
  if( topmed$Parent.Study.Accession[i] == "" ){
    selection <- finalSetFilterSort[ finalSetFilterSort$dbGaP.TOPMed.Study.Accession == topmed$Study.Accession[i], ]
    selection$Name <- topmed[ i, "Name"]
    selection$Notes <- paste0( "TOPMed Freeze 5b: <a href='https://www.biorxiv.org/content/10.1101/563866v1.full', target='_blank'>Joint Variant Calling</a>,  <a href='https://www.nhlbiwgs.org/topmed-whole-genome-sequencing-project-freeze-5b-phases-1-and-2', target='_blank'>Overview</a>")
    selection$link <- paste0("Genomic and clinical: <a href='", as.character( selection$link ), "', target='_blank'>", topmed$Study.Accession[i] ,"</a>")
    selection$extralink <- NA
    finalData <-  rbindlist(list( finalData, selection), use.names=F )
  }else{
    clinical <- finalSetFilterSort[ finalSetFilterSort$dbGaP.TOPMed.Study.Accession == topmed$Parent.Study.Accession[i], ]
    genomic <- finalSetFilterSort[ finalSetFilterSort$dbGaP.TOPMed.Study.Accession == topmed$Study.Accession[i], ]
    Name <- topmed[ i, "Name"]
    Country <- NA
    #samples  <- ifelse(as.numeric(genomic$samples) < as.numeric(clinical$samples), as.numeric(clinical$samples), as.numeric(genomic$samples))
    samples  <- as.numeric(genomic$samples)
    phenoVariables <- ifelse(as.numeric(genomic$variables) < as.numeric(clinical$variables), as.numeric(clinical$variables), as.numeric(genomic$variables))
    subjects <- ifelse(as.numeric(genomic$variables) > as.numeric(clinical$variables), as.numeric(genomic$subjects), min(c(as.numeric(genomic$subjects), as.numeric(clinical$subjects))))
    studyDesing <- ifelse( as.character(genomic$Study.Design) != as.character(clinical$Study.Design), paste0("Genomic study desing: ", as.character(genomic$Study.Design), "; Clinical study design: ", as.character(clinical$Study.Design)), as.character(genomic$Study.Design))
    phenoData <- NA
    molecularData <- as.character( genomic$Study.Molecular.Data.Type)
    markerset <- as.character( genomic$Study.Markerset)
    disease <- ifelse( as.character(genomic$Study.Disease.Focus) != as.character(clinical$Study.Disease.Focus), paste0("Genomic disease focus: ", as.character(genomic$Study.Disease.Focus), "; Clinical disease focus: ", as.character(clinical$Study.Disease.Focus)), as.character(genomic$Study.Disease.Focus))
    age <- NA
    ancestry <- ifelse(as.numeric(genomic$samples) < as.numeric(clinical$samples), as.character(clinical$Ancestry..computed.), as.character(genomic$Ancestry..computed.))
    consent <- ifelse( as.character(genomic$Study.Consent) != as.character(clinical$Study.Consent), paste0("Genomic study consent: ", as.character(genomic$Study.Consent), "; Clinical study consent: ", as.character(clinical$Study.Consent)), as.character(genomic$Study.Consent))
    accession <- paste0( "<a href='https://dbgap.ncbi.nlm.nih.gov', target='_blank'>Request access through dbGAP</a>")
    #link <- ifelse(as.numeric(genomic$variables) > as.numeric(clinical$variables), paste0("Genomic and clinical: <a href='", as.character( genomic$link ), "', target='_blank'>", topmed$Study.Accession[i] ,"</a>"),paste0( paste0("Genomic: <a href='", as.character( genomic$link ), "', target='_blank'>", topmed$Study.Accession[i] ,"</a>"), "; Clinical: ", paste0("<a href='", as.character( clinical$link ), "', target='_blank'>", topmed$Parent.Study.Accession[i] ,"</a>") ))
    if(as.numeric(genomic$variables) > as.numeric(clinical$variables)){
      link <- paste0("Clinical: <a href='", as.character( clinical$link ), "', target='_blank'>", topmed$Parent.Study.Accession[i] ,"</a>")
      extralink <- paste0("Genomic: <a href='", as.character( genomic$link ), "', target='_blank'>", topmed$Study.Accession[i] ,"</a>")
      }else{
      link <- paste0("Genomic and clinical: <a href='", as.character( genomic$link ), "', target='_blank'>", topmed$Study.Accession[i] ,"</a>")
      extralink <- NA
    
      }
       
    pubmed <- NA
    Notes <- ifelse(as.numeric(genomic$variables) > as.numeric(clinical$variables), paste0( "TOPMed Freeze 5b: <a href='https://www.biorxiv.org/content/10.1101/563866v1.full', target='_blank'>Joint Variant Calling</a>,  <a href='https://www.nhlbiwgs.org/topmed-whole-genome-sequencing-project-freeze-5b-phases-1-and-2', target='_blank'>Overview</a>", "; Additional information in the parent study: <a href='", as.character( clinical$link ), "', target='_blank'>", topmed$Parent.Study.Accession[i] ,"</a>"),paste0( "TOPMed Freeze 5b: <a href='https://www.biorxiv.org/content/10.1101/563866v1.full', target='_blank'>Joint Variant Calling</a>,  <a href='https://www.nhlbiwgs.org/topmed-whole-genome-sequencing-project-freeze-5b-phases-1-and-2', target='_blank'>Overview</a>"))
    hps <- paste0( as.character( genomic$accession ), "; ", as.character( clinical$accession ) )
    newrow <- c(Name, Country, samples, subjects, studyDesing, phenoVariables, phenoData, molecularData,markerset , disease, age, ancestry,consent, accession, link, pubmed, Notes, hps, extralink)
    finalData <- rbindlist( list(finalData, data.table(t(newrow))),  use.names=F )
  }
}

finalSetFilterSort <- finalData[, c(1:17,19)]

##### add dbgap studies that are not in TOPMed but that meet the inclusion criteria
# select those studies in dbGAP that meet the inclusion criteria:
finalSetDbgap <- finalSet[ as.numeric(finalSet$subjects) >= 500 & as.numeric(finalSet$variables) >= 100, ]
finalSetDbgap <- na.omit(finalSetDbgap)
finalSetDbgap$phs <- sapply(strsplit( as.character(finalSetDbgap$accession), "[.]"), '[', 1)
finalSetDbgap <- finalSetDbgap[! finalSetDbgap$phs %in% c(topmed$Study.Accession, topmed$Parent.Study.Accession), ]
# Select genomic data 
totalSelection <- unique(grep( "NGS|WXS|WES|WGS", finalSetDbgap$Study.Molecular.Data.Type))
finalSetDbgap <- finalSetDbgap[ totalSelection, ]

##add the columns needed to sort the table
finalSetDbgap <- data.table(finalSetDbgap)
# Add columns 
add_cols <- c("Country","Patients.Age","Notes","PubMedLink","Phenotypic.Data.Type")
finalSetDbgap = finalSetDbgap[,c(add_cols):=list(NA)]
finalSetDbgap$link <- paste0("Genomic and clinical: <a href='", as.character( finalSetDbgap$link ), "', target='_blank'>", finalSetDbgap$phs ,"</a>")
finalSetDbgap <- finalSetDbgap[, c("Name", "Country", "samples", "subjects", "Study.Design",
                                   "variables", "Phenotypic.Data.Type","Study.Molecular.Data.Type", "Study.Markerset", 
                                   "Study.Disease.Focus", "Patients.Age", "Ancestry..computed.", 
                                   "Study.Consent", "accession", "link", "PubMedLink", "Notes")]
finalSetDbgap$accession <- paste0( "<a href='https://dbgap.ncbi.nlm.nih.gov', target='_blank'>Request access through dbGAP</a>")
finalSetDbgap$extraLink <- NA

# Merge both
finalSetFilterSort <- rbindlist( list(finalSetFilterSort, finalSetDbgap), use.names = T)

# Names with underscores
colnames(finalSetFilterSort) <- c("Name","Country","Sample_Size","Subject_Count","Study_Design","Phenotypic_Variables",
                                  "Phenotypic_Data_Type","Molecular_Data_Type","Markerset","Disease_Focus",
                                  "Patients_Age","Ancestry","Consent","Accession","LinkClinicalAndGenomic","PubMedLink","Notes", "LinkGenomic")

finalSetFilterSort$Name <- gsub( ",", ";", finalSetFilterSort$Name)
finalSetFilterSort$Molecular_Data_Type <- gsub( ",", ";", finalSetFilterSort$Molecular_Data_Type)
finalSetFilterSort$Disease_Focus <- gsub( ",", ";", finalSetFilterSort$Disease_Focus)
finalSetFilterSort$Markerset <- gsub( ",", ";", finalSetFilterSort$Markerset)
finalSetFilterSort$Ancestry <- gsub( ",", ";", finalSetFilterSort$Ancestry)
finalSetFilterSort$Consent <- gsub(  ",", ";", finalSetFilterSort$Consent)

finalSetFilterSort <- finalSetFilterSort[, c ("Name","Country","Sample_Size","Subject_Count","Study_Design","Phenotypic_Variables",
                                          "Phenotypic_Data_Type","Molecular_Data_Type","Markerset","Disease_Focus",
                                          "Patients_Age","Ancestry","Consent","Accession","LinkClinicalAndGenomic",  "LinkGenomic", "PubMedLink","Notes") ]

# Function to hard-code set values 
setValue <- function(BB_name, select_col, value){ 
  set(f5b, which(f5b[,Name==BB_name]), which(names(f5b)==select_col), value)
}

f5b = data.table(finalSetFilterSort) # freeze5b
f5b = f5b[!is.na(Name),][!is.na(Phenotypic_Variables),]
# Change columns to integers
f5b = f5b[,Sample_Size:=as.integer(Sample_Size)][,Subject_Count:=as.integer(Subject_Count)][,Phenotypic_Variables:=as.integer(Phenotypic_Variables)]
# Filter to PhenoVars>=100 & Remove String values from PhenoVars 
f5b = f5b[str_length(Phenotypic_Variables)<6,][Phenotypic_Variables>99,][order(-Phenotypic_Variables)]
f5b = f5b[,Country:="USA"][, Phenotypic_Data_Type:="Patient/Disease registries"] 
f5b = f5b[, Accession:="<a href='https://dbgap.ncbi.nlm.nih.gov', target='_blank'>Request access through dbGAP</a>"]
pubMed="<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term="

# FHS - phs000974 (geno) ; phs000007 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=974&version=3&exp1=1&exp2=1&exp3=1
setValue("Framingham Heart Study (FHS)","Patients_Age", "28-62")
setValue("Framingham Heart Study (FHS)", "PubMedLink", paste0(pubMed,"5921755[PMID]', target='_blank'>5921755</a>; ", pubMed,"17372189[PMID]', target='_blank'>17372189</a>; ", pubMed,"24084292[PMID]', target='_blank'>24084292</a>"))
setValue("Framingham Heart Study (FHS)", "Sample_Size", 8326)

# MESA - phs001416 (geno) ; phs000209 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1416&version=1&exp1=1&exp2=1&exp3=1
setValue("Multi-Ethnic Study of Atherosclerosis (MESA)","Patients_Age", "45-84")
setValue("Multi-Ethnic Study of Atherosclerosis (MESA)", "PubMedLink",  paste0(pubMed,"12397006[PMID]', target='_blank'>12397006</a>"))
setValue("Multi-Ethnic Study of Atherosclerosis (MESA)", "Subject_Count", 4875)
setValue("Multi-Ethnic Study of Atherosclerosis (MESA)", "Sample_Size", 4875 )

# ARIC - phs001211 (geno) ; phs000280 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1211&version=2&exp1=1&exp2=1&exp3=1
setValue("Atherosclerosis Risk in Communities (ARIC)","Patients_Age", "45-64")
setValue("Atherosclerosis Risk in Communities (ARIC)", "PubMedLink",  paste0(pubMed,"2646917[PMID]', target='_blank'>2646917</a>"))
setValue("Atherosclerosis Risk in Communities (ARIC)", "Sample_Size",  6667)
setValue("Atherosclerosis Risk in Communities (ARIC)", "Subject_Count", 3612)
# setValue("ARIC", "Phenotypic_Data_Type", "TBD")

# CHS - phs001368 (geno) ; phs000287 (pheno)
# Ancestry: Not available for the initial cohort of 5,201 participants
setValue("Cardiovascular Health Study (CHS)","Patients_Age", ">65")
setValue("Cardiovascular Health Study (CHS)", "PubMedLink",  paste0(pubMed,"1669507[PMID]', target='_blank'>1669507</a>"))
# setValue("CHS", "Phenotypic_Data_Type", "TBD") # See Table 2 : https://reader.elsevier.com/reader/sd/pii/104727979190005W?token=52A6D06132598482C9EFD4EA215DBCAB80A92424DC3CD16B7DAB25823823D1BBE9C9F30FCA0FFB1BD78566ADE54DCF18

# WHI - phs001237 (geno) ; phs000200 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1237&version=1&exp1=1&exp2=1&exp3=1
setValue("Women's Health Initiative (WHI)","Patients_Age", "50-79") # https://www.whi.org/SitePages/WHI%20Home.aspx
setValue("Women's Health Initiative (WHI)", "PubMedLink",  paste0("<a href='https://doi.org/10.1089/jwh.1995.4.519', target='_blank'>doi:10.1089</a>") ) # No PubMed Link; https://www.whi.org/researchers/bibliography/SitePages/Advanced%20Search.aspx
setValue("Women's Health Initiative (WHI)", "Subject_Count", 10000)
setValue("Women's Health Initiative (WHI)", "Sample_Size",  20010)
# setValue("WHI", "Phenotypic_Data_Type", "TBD")

# JHS - phs000964 (geno) ; phs000286 (pheno)
setValue("The Jackson Heart Study (JHS)","Ancestry", "African American")
setValue("The Jackson Heart Study (JHS)","Patients_Age", "35-84")
setValue("The Jackson Heart Study (JHS)", "Subject_Count", 3406)
setValue("The Jackson Heart Study (JHS)", "Sample_Size", 6812)
setValue("The Jackson Heart Study (JHS)", "PubMedLink",  paste0(pubMed,"10100686[PMID]', target='_blank'>10100686</a>; ", pubMed,"16317983[PMID]', target='_blank'>16317983</a>; ",pubMed,"16320381[PMID]', target='_blank'>16320381</a>"))
# setValue("JHS", "Phenotypic_Data_Type", "TBD")

# CFS - phs000954 (geno) ; phs000284 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=284&version=2&exp1=1&exp2=1&exp3=1
setValue("The Cleveland Family Study (CFS)","Patients_Age", " 2-18")
setValue("The Cleveland Family Study (CFS)","Sample_Size", 1988)
setValue("The Cleveland Family Study (CFS)", "PubMedLink",  paste0(pubMed,"10228121[PMID]', target='_blank'>10228121</a>"))
# setValue("CFS", "Phenotypic_Data_Type", "TBD")

# GENOA - phs001345 (geno) ; phs001238 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=954&version=2&exp1=1&exp2=1&exp3=1
setValue("Genetic Epidemiology Network of Arteriopathy (GENOA)","Patients_Age", "<60")
setValue("Genetic Epidemiology Network of Arteriopathy (GENOA)", "Subject_Count", 1143)
setValue("Genetic Epidemiology Network of Arteriopathy (GENOA)", "Sample_Size", 1143)
setValue("Genetic Epidemiology Network of Arteriopathy (GENOA)", "PubMedLink",  paste0(pubMed,"15121494[PMID]', target='_blank'>15121494</a>"))
# setValue("GENOA", "Phenotypic_Data_Type", "TBD")

# COPDGene - phs000951 (geno) ; phs000179 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=179&version=6&exp1=1&exp2=1&exp3=1
setValue("Genetic Epidemiology of Chronic Obstructive Pulmonary Disease (COPDGene) ","Patients_Age", "45-80")
setValue("Genetic Epidemiology of Chronic Obstructive Pulmonary Disease (COPDGene) ", "PubMedLink",  paste0(pubMed,"20214461[PMID]', target='_blank'>20214461</a>"))
setValue("Genetic Epidemiology of Chronic Obstructive Pulmonary Disease (COPDGene)", "Subject_Count", 8843)
setValue("Genetic Epidemiology of Chronic Obstructive Pulmonary Disease (COPDGene)", "Sample_Size",  19460)

# SAS - phs000972 (geno) ; phs000914 (pheno)
setValue("Study of Adiposity in Samoans (SAS)","Country", "Samoa")
setValue("Study of Adiposity in Samoans (SAS)", "Subject_Count", 1222)
setValue("Study of Adiposity in Samoans (SAS)", "Sample_Size", 1232)
setValue("Study of Adiposity in Samoans (SAS)","Ancestry", "Samoan")
setValue("Study of Adiposity in Samoans (SAS)","Patients_Age", "23-70")
setValue("Study of Adiposity in Samoans (SAS)", "PubMedLink",  paste0(pubMed,"24799123[PMID]', target='_blank'>24799123</a>"))
# setValue("SAS", "Phenotypic_Data_Type", "TBD")

# HyperGEN - phs001293 (geno) 
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=954&version=2&exp1=1&exp2=1&exp3=1
setValue("Genetics of Left Ventricular (LV) Hypertrophy (HyperGEN)","Patients_Age", "<60")
setValue("Genetics of Left Ventricular (LV) Hypertrophy (HyperGEN)", "Subject_Count", 1773)
setValue("Genetics of Left Ventricular (LV) Hypertrophy (HyperGEN)", "Sample_Size", 1776)
setValue("Genetics of Left Ventricular (LV) Hypertrophy (HyperGEN)", "PubMedLink",  paste0(pubMed,"11115379[PMID]', target='_blank'>11115379</a>"))
# setValue("HyperGEN", "Phenotypic_Data_Type", "TBD")

# GeneSTAR - phs001218 (geno) ; phs001074 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1218&version=1&exp1=1&exp2=1&exp3=1
setValue("Genetic Study of Atherosclerosis Risk (GeneSTAR)","Patients_Age", "<60")
setValue("Genetic Study of Atherosclerosis Risk (GeneSTAR)", "Subject_Count", 1636)
setValue("Genetic Study of Atherosclerosis Risk (GeneSTAR)", "Sample_Size", 3420)
setValue("Genetic Study of Atherosclerosis Risk (GeneSTAR)", "PubMedLink",  paste0(pubMed,"16551714[PMID]', target='_blank'>16551714</a>"))
# setValue("GeneSTAR", "Phenotypic_Data_Type", "TBD")

# GOLDN - phs001218 (geno) ; phs001074 (pheno)
setValue("Genetics of Lipid Lowering Drugs and Diet Network (GOLDN)","Ancestry", "Caucasian")
setValue("Genetics of Lipid Lowering Drugs and Diet Network (GOLDN)","Patients_Age", ">18")
setValue("Genetics of Lipid Lowering Drugs and Diet Network (GOLDN)", "Subject_Count", 898)
setValue("Genetics of Lipid Lowering Drugs and Diet Network (GOLDN)", "Sample_Size", 1859)
setValue("Genetics of Lipid Lowering Drugs and Diet Network (GOLDN)", "PubMedLink",  paste0(pubMed,"22228203[PMID]', target='_blank'>22228203</a>"))
# setValue("GOLDN", "Phenotypic_Data_Type", "TBD")

##function to add new rows
newRows <- function( newEntries, data){
  data =  rbindlist(list(data, data.table( Name=c(newEntries) )) , use.names=T, fill=T  )
  return( data )
}

f5b <- newRows( newEntries = c("UK Biobank", "Undiagnosed Disease Network (UDN)", "Boston Children's Biobank", 
                               "Genomics Research and Innovation Network (GRIN)", 
                               "All of Us", "Simons Simplex Collection (SSC)"), data = f5b)


# UK Biobank
setValue("UK Biobank", "Country", "UK")
setValue("UK Biobank", "Sample_Size", 50050)
setValue("UK Biobank", "Subject_Count", 50050)
setValue("UK Biobank", "Study_Design", "Prospective study")
# https://biobank.ndph.ox.ac.uk/showcase/browse.cgi?tk=OZMjnzmOfzLus1OfQ3AKFrr5cSCtYbZS66549
setValue("UK Biobank", "Phenotypic_Variables", 7291)
setValue("UK Biobank", "Phenotypic_Data_Type", "Health Record Data; Questionnaires; Physical Measures; Lifestyle")
setValue("UK Biobank", "Molecular_Data_Type", "WGS; WES")
setValue("UK Biobank", "Markerset", "Exome: IDT xGen Exome Research Panel v1.0")
setValue("UK Biobank", "Disease_Focus", "General")
setValue("UK Biobank","Patients_Age", "40-69")
setValue("UK Biobank", "Ancestry", "White (95%); Other (5%)")
setValue("UK Biobank", "Consent", "<a href='https://www.ukbiobank.ac.uk/gdpr/', target='_blank'>Consent for health-related research, for their health to be followed over many years through medical and other health-related records, as well as by being re-contacted by UK Biobank</a>")
setValue("UK Biobank", "Accession", "<a href='http://www.ukbiobank.ac.uk/wp-content/uploads/2012/09/Access-Procedures-2011.pdf', target='_blank'>UK Biobank Access Procedures</a>") 
setValue("UK Biobank", "LinkClinicalAndGenomic", "<a href='https://www.ukbiobank.ac.uk', target='_blank'>UK Biobank</a>")
setValue("UK Biobank", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=25826379[PMID]', target='_blank'>25826379</a> ; <a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=30305743[PMID]', target='_blank'>30305743</a>" )
setValue("UK Biobank", "Notes", "<a href='http://biobank.ctsu.ox.ac.uk/crystal/browse.cgi', target='_blank'>UK Biobank Data Showcase</a>. A fee of 2,250£ is required to access the data.")



# Undiagnosed_Disease_Network_(UDN)
setValue("Undiagnosed Disease Network (UDN)", "Country", "USA")
setValue("Undiagnosed Disease Network (UDN)", "Sample_Size", 462)
setValue("Undiagnosed Disease Network (UDN)", "Subject_Count", 1042)
setValue("Undiagnosed Disease Network (UDN)", "Study_Design", "Prospective Longitudinal Cohort")
setValue("Undiagnosed Disease Network (UDN)", "Phenotypic_Variables", 3965)
setValue("Undiagnosed Disease Network (UDN)", "Phenotypic_Data_Type", "EMR; Primary symptom reported by patient or caregiver or by clinical experts and HPO terms")
setValue("Undiagnosed Disease Network (UDN)", "Molecular_Data_Type", "NGS; WGS; WES")
setValue("Undiagnosed Disease Network (UDN)", "Markerset", "GRCh37(hg19)")
setValue("Undiagnosed Disease Network (UDN)", "Disease_Focus", "Undiagnosed Diseases/Rare Diseases")
setValue("Undiagnosed Disease Network (UDN)","Patients_Age", "Pediatric population: 0-17 and adult population > 18")
setValue("Undiagnosed Disease Network (UDN)", "Ancestry", "White (81%); Asian (6%); American Indian or Alaska Native (<1%)'; Black or African American (5%); Native Hawaiian Pacific Islander (<1%); Other (8%)")
setValue("Undiagnosed Disease Network (UDN)", "Consent", "NHGRI GRU --- General research use")
setValue("Undiagnosed Disease Network (UDN)", "Accession", "<a href='https://dbgap.ncbi.nlm.nih.gov', target='_blank'>Request access through dbGAP</a>")
setValue("Undiagnosed Disease Network (UDN)", "LinkClinicalAndGenomic", "<a href='https://undiagnosed.hms.harvard.edu/research/', target='_blank'>Undiagnosed Disease Network (UDN)</a>")
setValue("Undiagnosed Disease Network (UDN)", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=26220576[PMID]', target='_blank'>26220576</a>")
setValue("Undiagnosed Disease Network (UDN)", "Notes", "<a href='http://undiagnosed.hms.harvard.edu/wp-content/uploads/UDN-Manual-of-Operations_February-2016.pdf', target='_blank'>UDN Manual of Operations</a>")

# Boston Children's Biobank
setValue("Boston Children's Biobank", "Country", "USA")
setValue("Boston Children's Biobank", "Sample_Size", 500)
setValue("Boston Children's Biobank", "Subject_Count", 500 )
setValue("Boston Children's Biobank", "Study_Design", "Prospective longitudinal cohort")
setValue("Boston Children's Biobank", "Phenotypic_Variables", 73077)
setValue("Boston Children's Biobank", "Phenotypic_Data_Type", "EHR")
setValue("Boston Children's Biobank", "Molecular_Data_Type", "WES")
#setValue("Boston Children's Biobank", "Markerset", "")
setValue("Boston Children's Biobank", "Disease_Focus", "General")
setValue("Boston Children's Biobank","Patients_Age", "0-18 (80.5%); >18 (19.5%)")
setValue("Boston Children's Biobank", "Ancestry", "White (75%); Black (9.6%); Asian (3.4%); Navite American/Alaska Native (0.2%); Native Hawaiian/Pacific Islander (0.1%); Other (4.3%); Prefere not to answer (1.0%); Unknown (6.3%)")
setValue("Boston Children's Biobank", "Consent", "<a href='http://www.mdpi.com/2075-4426/7/4/21/s1', target='_blank'>Research Consent</a>")
setValue("Boston Children's Biobank", "Accession", "<a href='http://biobank.childrens.harvard.edu', target='_blank'>Boston Children's Biobank</a>")
setValue("Boston Children's Biobank", "LinkClinicalAndGenomic", "<a href='http://biobank.childrens.harvard.edu', target='_blank'>Boston Children's Biobank</a>")
setValue("Boston Children's Biobank", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=29244735[PMID]', target='_blank'>29244735</a>")
setValue("Boston Children's Biobank", "Notes", "The parents provide consent until participants are 18")

# Genomics Research and Innovation Network (GRIN)
setValue("Genomics Research and Innovation Network (GRIN)", "Country", "USA")
setValue("Genomics Research and Innovation Network (GRIN)", "Sample_Size", 500 )
setValue("Genomics Research and Innovation Network (GRIN)", "Subject_Count", 500)
setValue("Genomics Research and Innovation Network (GRIN)", "Study_Design", "Pediatric Network")
setValue("Genomics Research and Innovation Network (GRIN)", "Phenotypic_Variables", 19649)
setValue("Genomics Research and Innovation Network (GRIN)", "Phenotypic_Data_Type", "EHR")
setValue("Genomics Research and Innovation Network (GRIN)", "Molecular_Data_Type", "WES")
#setValue("Genomics Research and Innovation Network (GRIN)", "Markerset", "")
setValue("Genomics Research and Innovation Network (GRIN)", "Disease_Focus", "General")
#setValue("Genomics Research and Innovation Network (GRIN)","Patients_Age", "")
#setValue("Genomics Research and Innovation Network (GRIN)", "Ancestry", "")
setValue("Genomics Research and Innovation Network (GRIN)", "Consent", "Research <a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=31481752[PMID]', target='_blank'>(More details in: IRB protocol section)</a>")
setValue("Genomics Research and Innovation Network (GRIN)", "Accession", "<a href='https://www.grinnetwork.org/', target='_blank'>PIC-SURE application programming interface (API) is used to access genotype and phenotype data from databases at each site of care</a>")
setValue("Genomics Research and Innovation Network (GRIN)", "LinkClinicalAndGenomic", "<a href='https://www.grinnetwork.org/', target='_blank'>Genomics Research and Innovation Network (GRIN)</a>")
setValue("Genomics Research and Innovation Network (GRIN)", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=31481752[PMID]', target='_blank'>31481752</a>")
#setValue("Genomics Research and Innovation Network (GRIN)", "Notes", "")

# H3Africa 
# setValue("H3Africa", "Country", "Africa") # https://h3africa.org/h3africa.old/images/ConsortiumMeetingDoc/8thConsortiumMeeting/PIpresentations/Mulder_session4_final.pdf
# #setValue("H3Africa", "Sample_Size", ) 
# setValue("H3Africa", "Subject_Count", 324) #https://www.ebi.ac.uk/ega/dacs/EGAC00001000648 - Does not fulfill inclusion criteria #1 
# setValue("H3Africa", "Study_Design", "Population-based cohort")
# setValue("H3Africa", "Phenotypic_Variables", 776) # Using PhenX Protocols 
# setValue("H3Africa", "Phenotypic_Data_Type", "Demographic data, clinical measurements")
# setValue("H3Africa", "Molecular_Data_Type", "WGS")
# #setValue("H3Africa", "Markerset", "")
# setValue("H3Africa", "Disease_Focus", "Diseases affecting people in Africa")
# setValue("H3Africa","Patients_Age", ">18")
# setValue("H3Africa", "Ancestry", "African")
# setValue("H3Africa", "Consent", "<a href='https://h3africa.org/wp-content/uploads/2018/05/H3A%202017%20Revised%20IC%20guideline%20for%20SC%2020_10_2017.pdf', target='_blank'>H3Africa Guideline for Informed Consent</a>")
# setValue("H3Africa", "Accession", "<a href='https://h3africa.org/index.php/contacts/', target='_blank'>H3Africa_contact</a> ; <a href='https://www.h3abionet.org/resources/h3africa-archive', target='_blank'>H3Africa_archive</a>")
# setValue("H3Africa", "LinkClinicalAndGenomic", "<a href='https://h3africa.org/', target='_blank'>H3Africa</a>")
# setValue("H3Africa", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=29692621[PMID]', target='_blank'>29692621</a>; <a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=28470782[PMID]', target='_blank'>28470782</a>")
# #setValue("H3Africa", "Notes", "")

# All of Us
# https://www.researchallofus.org/data/data-snapshots/
setValue("All of Us", "Country", "USA")
setValue("All of Us", "Sample_Size", 219000)
setValue("All of Us", "Subject_Count", 80000) # https://databrowser.researchallofus.org/
setValue("All of Us", "Study_Design", "Population-based cohort")
# https://www.nejm.org/doi/full/10.1056/NEJMsr1809937 - EHR Data
setValue("All of Us", "Phenotypic_Variables", 48682)
setValue("All of Us", "Phenotypic_Data_Type", "EHR; Surver Questions; Program Physiscal Measurements")
setValue("All of Us", "Molecular_Data_Type", "WGS")
#setValue("All of Us", "Markerset", "")
setValue("All of Us", "Disease_Focus", "General")
setValue("All of Us","Patients_Age", ">18")
setValue("All of Us", "Ancestry", "White(46.2%); Asian(2.9); Black,African American(21.9%); ispanic,Latino(17.7%); More than one(6.7%); Other(3.0%); Prefer not to say(0.7%)")
setValue("All of Us", "Consent", "<a href='https://allofus.nih.gov/about/protocol/all-us-consent-process#all-us-consent-process-videos-1', target='_blank'>Consent Process</a>")
setValue("All of Us", "Accession", "<a href='https://www.researchallofus.org/data/workbench/', target='_blank'>All of Us: Data Browser</a>")
setValue("All of Us", "LinkClinicalAndGenomic", "<a href='https://www.researchallofus.org/', target='_blank'>All of Us</a>")
setValue("All of Us", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=31412182[PMID]', target='_blank'>31412182</a>")
#setValue("All of Us", "Notes", "")

# Simons Simplex Collection (SSC)
setValue("Simons Simplex Collection (SSC)", "Country", "USA")
setValue("Simons Simplex Collection (SSC)", "Sample_Size", 4784 )
setValue("Simons Simplex Collection (SSC)", "Subject_Count", 2392)
setValue("Simons Simplex Collection (SSC)", "Study_Design", "Family cohort (simplex families with an affected sibling)")
setValue("Simons Simplex Collection (SSC)", "Phenotypic_Variables", 5459)
setValue("Simons Simplex Collection (SSC)", "Phenotypic_Data_Type", "Questionnaires; Autism Diagnostic Interview – Revised (ADI-R) ; Autism Diagnostic Observation Schedule (ADOS)")
setValue("Simons Simplex Collection (SSC)", "Molecular_Data_Type", "WGS; WES")
setValue("Simons Simplex Collection (SSC)", "Markerset", "Genome Reference Consortium Human Build 37 (GRCh37 - hg19)")
setValue("Simons Simplex Collection (SSC)", "Disease_Focus", "Autism")
setValue("Simons Simplex Collection (SSC)","Patients_Age", "4-18 (ASD Children)")
setValue("Simons Simplex Collection (SSC)", "Ancestry", "White (6773 ); Hispanic (724); African american (357); Asian (361); Native american (15); Native hawaiian (11); More than one race (516); Other (147); Not Provided (64)")
setValue("Simons Simplex Collection (SSC)", "Consent", "<a href='http://simonsfoundation.s3.amazonaws.com/share/Policies_and_forms/2014/ssc/UMACC_Adult_Consent_03-2011.pdf', target='_blank'>Consent form</a>")
setValue("Simons Simplex Collection (SSC)", "Accession", "<a href='https://www.sfari.org/resource/sfari-base/', target='_blank'>Only principal investigators can request access to phenotypic, genetic or imaging data</a>")
setValue("Simons Simplex Collection (SSC)", "LinkClinicalAndGenomic", "<a href='https://www.sfari.org/resource/simons-simplex-collection/', target='_blank'>Simons Simplex Collection (SSC)</a>")
setValue("Simons Simplex Collection (SSC)", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed/?term=20955926%5BPMID%5D', target='_blank'>20955926</a>")
setValue("Simons Simplex Collection (SSC)", "Notes", "Simplex families which have one child affected with an autism spectrum disorder, unaffected parents and in some cases unnafected siblings. Genomic data is available for all family members.")

# # Genome-Wide Association Study of Amyotrophic Lateral Sclerosis 
# setValue("Genome-Wide Association Study of Amyotrophic Lateral Sclerosis", "Patients_Age", ">18")
# setValue("Genome-Wide Association Study of Amyotrophic Lateral Sclerosis", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=25023141[PMID]', target='_blank'>25023141</a>")
# Does not fullfil inclusion criteria #1
f5b=f5b[Name!="Genome-Wide Association Study of Amyotrophic Lateral Sclerosis",]

# OPPERA prospective cohort study of first-onset TMJD
setValue("OPPERA prospective cohort study of first-onset TMJD", "Subject_Count", 2866)
setValue("OPPERA prospective cohort study of first-onset TMJD",  "PubMedLink", paste0(pubMed,"24275221[PMID]', target='_blank'>24275221</a> ;", pubMed,"24275219[PMID]', target='_blank'>24275219</a>"))
setValue("OPPERA prospective cohort study of first-onset TMJD", "Ancestry", "White(1448); Black/African American(766); Asian(256); Hispanic(178); Other or unstated(89)")
setValue("OPPERA prospective cohort study of first-onset TMJD", "Patients_Age", "18-44")

# Sequencing of Targeted Genomic Regions Associated with Smoking
setValue("Sequencing of Targeted Genomic Regions Associated with Smoking", "Sample_Size", 6196)
setValue("Sequencing of Targeted Genomic Regions Associated with Smoking", "PubMedLink",  paste0(pubMed,"18629733[PMID]', target='_blank'>18629733</a>"))
setValue("Sequencing of Targeted Genomic Regions Associated with Smoking", "Patients_Age", "25-44")
setValue("Sequencing of Targeted Genomic Regions Associated with Smoking", "Ancestry", "White(2925); Black(582)")

# OPPERA baseline case-control study of chronic TMJD
setValue("OPPERA baseline case-control study of chronic TMJD", "Subject_Count", 1608)
setValue("OPPERA baseline case-control study of chronic TMJD", "Sample_Size", 1608)
setValue("OPPERA baseline case-control study of chronic TMJD",  "PubMedLink", paste0(pubMed,"22074749[PMID]', target='_blank'>22074749</a>"))
setValue("OPPERA baseline case-control study of chronic TMJD", "Ancestry", "White-NH(984); Black/African American(533); Hispanic(108); Other or unstated(193)")
setValue("OPPERA baseline case-control study of chronic TMJD", "Patients_Age", "18-44")

# Whole Exome Sequencing in Tourette Disorder in Simplex Trios-TIC Genetics
setValue("Whole Exome Sequencing in Tourette Disorder in Simplex Trios-TIC Genetics", "Subject_Count", 1104)
setValue("Whole Exome Sequencing in Tourette Disorder in Simplex Trios-TIC Genetics", "Sample_Size", 1104)
setValue("Whole Exome Sequencing in Tourette Disorder in Simplex Trios-TIC Genetics",  "PubMedLink", paste0(pubMed,"24771252[PMID]', target='_blank'>24771252</a>"))
setValue("Whole Exome Sequencing in Tourette Disorder in Simplex Trios-TIC Genetics", "Patients_Age", "4-83")
setValue("Whole Exome Sequencing in Tourette Disorder in Simplex Trios-TIC Genetics", "Ancestry", "White(91.5%); Black(1.5%); Asian(7.6%); Mixed or unknown race (3.3%)") 

#T2D-GENES Project 2: San Antonio Mexican American Family Studies(SAMAFS); Substudy 2: Whole genome sequencing in pedigrees
setValue("T2D-GENES Project 2: San Antonio Mexican American Family Studies(SAMAFS); Substudy 2: Whole genome sequencing in pedigrees","Patients_Age", "40-60 (probands age)")
setValue("T2D-GENES Project 2: San Antonio Mexican American Family Studies(SAMAFS); Substudy 2: Whole genome sequencing in pedigrees", "PubMedLink",  paste0(pubMed,"8901667[PMID]', target='_blank'>8901667</a>; ", pubMed,"23487405[PMID]', target='_blank'>23487405</a>"))
setValue("T2D-GENES Project 2: San Antonio Mexican American Family Studies(SAMAFS); Substudy 2: Whole genome sequencing in pedigrees", "Notes", "Substudy of <a href='https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs000280.v5.p1', target='_blank'>phs000847.v2.p1</a>")
setValue("T2D-GENES Project 2: San Antonio Mexican American Family Studies(SAMAFS); Substudy 2: Whole genome sequencing in pedigrees", "Ancestry", "Mexican-American") 
setValue("T2D-GENES Project 2: San Antonio Mexican American Family Studies(SAMAFS); Substudy 2: Whole genome sequencing in pedigrees", "Sample_Size", 1197)


#NHGRI ClinSeq
setValue("HGRI ClinSeq","Patients_Age", "45-65")
setValue("HGRI ClinSeq","Subject_Count", 1000)
setValue("HGRI ClinSeq","Sample_Size", 1000)
setValue("HGRI ClinSeq", "PubMedLink",  paste0(pubMed,"19602640[PMID]', target='_blank'>19602640</a> ;", pubMed,"26186621[PMID]', target='_blank'>26186621</a>")) 
setValue("HGRI ClinSeq", "Ancestry", "European (747); African (27); East Asian (4); African American (467); Hispanic1 (150); Hispanic2 (9); Other Asian or Pacific Islander (37); South Asian (18); Other (23)")

#Genotype-Tissue Expression (GTEx)
setValue("Genotype-Tissue Expression (GTEx)","Patients_Age", "21-70")
setValue("Genotype-Tissue Expression (GTEx)","Sample_Size", 3155)
setValue("Genotype-Tissue Expression (GTEx)", "PubMedLink",  paste0(pubMed,"23715323[PMID]', target='_blank'>23715323</a>; ", pubMed,"25954001[PMID]', target='_blank'>25954001</a>")) 
setValue("Genotype-Tissue Expression (GTEx)", "Ancestry", "European (153); East Asian (2); African American (25); Hispanic1 (3); Hispanic2 (2)") 

#NHLBI GO-ESP: Heart Cohorts Exome Sequencing Project (ARIC)
setValue("NHLBI GO-ESP: Heart Cohorts Exome Sequencing Project (ARIC)", "Patients_Age", "45-64")
setValue("NHLBI GO-ESP: Heart Cohorts Exome Sequencing Project (ARIC)", "Sample_Size", 1686)
# For pubmed link https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs000398.v5.p1
setValue("NHLBI GO-ESP: Heart Cohorts Exome Sequencing Project (ARIC)", "Notes", "Substudy of <a href='https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs000280.v5.p1', target='_blank'>ARIC (phs000280.v5.p1)</a>; GO-ESP is comprised by: 3 cohort consortia - HeartGO, LungGO, and WHISP - and 2 sequencing centers - BroadGO and SeattleGO [<a href='http://evs.gs.washington.edu/EVS/', target='_blank'>NHLBI GO Exome Sequencing Project (ESP)</a>]")

# UIC ACE Exome Sequencing Analysis
setValue("UIC ACE Exome Sequencing Analysis", "Subject_Count", 523)
setValue("UIC ACE Exome Sequencing Analysis", "Sample_Size", 1066)
setValue("UIC ACE Exome Sequencing Analysis","Patients_Age", "3-50")
setValue("UIC ACE Exome Sequencing Analysis", "PubMedLink",  paste0(pubMed,"23956104[PMID]', target='_blank'>23956104</a>"))
setValue("UIC ACE Exome Sequencing Analysis", "Ancestry", "European(59.5%); Hispanic(17.4%); African American(15.4%); Asian(4.1%); More than one(3.6%)") 

# Molecular Data-types and study design
setValue("Genome-Wide Association Study of Amyotrophic Lateral Sclerosis", "Molecular_Data_Type", "SNP Genotypes (NGS); WES")
setValue("Atherosclerosis Risk in Communities (ARIC)", "Study_Design", "Prospective longitudinal cohort")
setValue("Sequencing of Targeted Genomic Regions Associated with Smoking", "Molecular_Data_Type", "SNP Genotypes (NGS); WES")
setValue("Simons Simplex Collection (SSC)", "Study_Design", "Family/Twin/Trios")
setValue("Genetic Epidemiology Network of Arteriopathy (GENOA)", "Study_Design", "Prospective longitudinal cohort; Family/Twin/Trios")
setValue("OPPERA baseline case-control study of chronic TMJD", "Molecular_Data_Type", "SNP Genotypes (NGS)")
setValue("Whole Exome Sequencing in Tourette Disorder in Simplex Trios-TIC Genetics", "Molecular_Data_Type", "WES")
setValue("T2D-GENES Project 2: San Antonio Mexican American Family Studies (SAMAFS); Substudy 2: Whole genome sequencing in pedigrees", "Molecular_Data_Type", "SNP Genotypes (NGS); WGS")
setValue("NHGRI ClinSeq", "Molecular_Data_Type", "WES")
setValue("Genotype-Tissue Expression (GTEx)", "Molecular_Data_Type", "SNP/CNV Genotypes (NGS); WGS; WES")
setValue("Genotype-Tissue Expression (GTEx)", "Markerset", "markerset_grc37; Markerset_grc38")
setValue("NHLBI GO-ESP: Heart Cohorts Exome Sequencing Project (ARIC)", "Molecular_Data_Type", "SNP Genotypes (NGS); WES")
setValue("UIC ACE Exome Sequencing Analysis", "Molecular_Data_Type", "SNP/CNV Genotypes (NGS); WES")


f5b <- f5b[ order(-Subject_Count) ]

colnames(f5b) <- c("Name","Country","Sample Size","Subject Count with Genomic and Clinical Data","Study Design","# Phenotypic Variables Per Patient",
                        "Phenotypic Data Type","Molecular Data Type","Markerset","Disease/Focus",
                        "Patients Age (yrs)","Ancestry","Consent","Accession","LinkClinicalAndGenomic", "LinkGenomic","PubMed Link","Notes") 
# Re-Order columns
f5b <- f5b[ ,  c("Name","Country", "Subject Count with Genomic and Clinical Data","Study Design","Disease/Focus","# Phenotypic Variables Per Patient",
                 "Phenotypic Data Type","Sample Size","Molecular Data Type","Markerset",
                 "Patients Age (yrs)","Ancestry","Consent","Accession","LinkClinicalAndGenomic", "LinkGenomic","PubMed Link","Notes") ]

fwrite(f5b, file=paste0(file_path,"tableData.csv"), sep = ',', col.names = TRUE)

# NEW
setValue("NEW", "Country", "")
setValue("NEW", "Sample_Size", )
setValue("NEW", "Subject_Count", )
setValue("NEW", "Study_Design", "")
setValue("NEW", "Phenotypic_Variables", )
setValue("NEW", "Phenotypic_Data_Type", "")
setValue("NEW", "Molecular_Data_Type", "")
setValue("NEW", "Markerset", "")
setValue("NEW", "Disease_Focus", "")
setValue("NEW","Patients_Age", "")
setValue("NEW", "Ancestry", "")
setValue("NEW", "Consent", "")
setValue("NEW", "Accession", "<a href='http://', target='_blank'>NEW</a>")
setValue("NEW", "Link", "<a href='http://', target='_blank'>NEW</a>")
setValue("NEW", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=[PMID]', target='_blank'></a>")
setValue("NEW", "Notes", "")
