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

for( i in 1:nrow(topmed)){
  if( topmed$Study.Accession[i] == "" )
    next()
  if( topmed$Parent.Study.Accession[i] == "" ){
    selection <- finalSetFilterSort[ finalSetFilterSort$dbGaP.TOPMed.Study.Accession == topmed$Study.Accession[i], ]
    selection$name <- topmed[ i, "Study.Cohort.Abbreviation"]
    selection$Notes <- paste0( "<a href='https://www.biorxiv.org/content/10.1101/563866v1.full', target='_blank'>TOPMed Freeze 5b</a>")
    finalData <-  rbindlist(list( finalData, selection), use.names=F )
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
    accession <- paste0( "<a href='https://dbgap.ncbi.nlm.nih.gov/aa/wga.cgi', target='_blank'>Request access through dbGAP</a>")
    links <- paste0( paste0("<a href='", as.character( genomic$link ), "', target='_blank'>", topmed$Study.Accession[i] ,"</a>"), "; ", paste0("<a href='", as.character( clinical$link ), "', target='_blank'>", topmed$Parent.Study.Accession[i] ,"</a>") )
    pubmed <- NA
    Notes <- "TOPMed Freeze 5b"
    hps <- paste0( as.character( genomic$accession ), "; ", as.character( clinical$accession ) )
    newrow <- c(name, Country, samples, subjects, studyDesing, phenoVariables, phenoData, molecularData,markerset , disease, age, ancestry,consent, accession, links, pubmed, Notes, hps)
    finalData <- rbindlist( list(finalData, data.table(t(newrow))),  use.names=F )
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
setValue("ARIC", "PubMedLink",  paste0(pubMed,"2646917[PMID]', target='_blank'>2646917</a>"))
# setValue("ARIC", "Phenotypic_Data_Type", "TBD")

# CHS - phs001368 (geno) ; phs000287 (pheno)
# Ancestry: Not available for the initial cohort of 5,201 participants
setValue("CHS","Patients_Age", ">65")
setValue("CHS", "PubMedLink",  paste0(pubMed,"1669507[PMID]', target='_blank'>1669507</a>"))
# setValue("CHS", "Phenotypic_Data_Type", "TBD") # See Table 2 : https://reader.elsevier.com/reader/sd/pii/104727979190005W?token=52A6D06132598482C9EFD4EA215DBCAB80A92424DC3CD16B7DAB25823823D1BBE9C9F30FCA0FFB1BD78566ADE54DCF18

# WHI - phs001237 (geno) ; phs000200 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1237&version=1&exp1=1&exp2=1&exp3=1
setValue("WHI","Patients_Age", "50-79") # https://www.whi.org/SitePages/WHI%20Home.aspx
setValue("WHI", "PubMedLink",  paste0("<a href='https://doi.org/10.1089/jwh.1995.4.519', target='_blank'>doi:10.1089</a>") ) # No PubMed Link; https://www.whi.org/researchers/bibliography/SitePages/Advanced%20Search.aspx
# setValue("WHI", "Phenotypic_Data_Type", "TBD")

# JHS - phs000964 (geno) ; phs000286 (pheno)
setValue("JHS","Ancestry", "African American")
setValue("JHS","Patients_Age", "35-84")
setValue("JHS", "PubMedLink",  paste0(pubMed,"10100686[PMID]', target='_blank'>10100686</a>"))
# setValue("JHS", "Phenotypic_Data_Type", "TBD")

# CFS - phs000954 (geno) ; phs000284 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=284&version=2&exp1=1&exp2=1&exp3=1
setValue("CFS","Patients_Age", " 2-18")
setValue("CFS", "PubMedLink",  paste0(pubMed,"10228121[PMID]', target='_blank'>10228121</a>"))
# setValue("CFS", "Phenotypic_Data_Type", "TBD")

# GENOA - phs001345 (geno) ; phs001238 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=954&version=2&exp1=1&exp2=1&exp3=1
setValue("GENOA","Patients_Age", "<60")
setValue("GENOA", "PubMedLink",  paste0(pubMed,"15121494[PMID]', target='_blank'>15121494</a>"))
# setValue("GENOA", "Phenotypic_Data_Type", "TBD")

# COPDGene - phs000951 (geno) ; phs000179 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=179&version=6&exp1=1&exp2=1&exp3=1
setValue("COPDGene","Patients_Age", "45-80")
setValue("COPDGene", "PubMedLink",  paste0(pubMed,"20214461[PMID]', target='_blank'>20214461</a>"))
# setValue("COPDGene", "Phenotypic_Data_Type", "TBD")

# SAS - phs000972 (geno) ; phs000914 (pheno)
setValue("SAS","Country", "Samoa")
setValue("SAS","Ancestry", "Samoan")
setValue("SAS","Patients_Age", "23-70")
setValue("SAS", "PubMedLink",  paste0(pubMed,"24799123[PMID]', target='_blank'>24799123</a>"))
# setValue("SAS", "Phenotypic_Data_Type", "TBD")

# HyperGEN - phs001293 (geno) 
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=954&version=2&exp1=1&exp2=1&exp3=1
setValue("HyperGEN","Patients_Age", "<60")
setValue("HyperGEN", "PubMedLink",  paste0(pubMed,"11115379[PMID]', target='_blank'>11115379</a>"))
# setValue("HyperGEN", "Phenotypic_Data_Type", "TBD")

# GeneSTAR - phs001218 (geno) ; phs001074 (pheno)
# Ancestry: https://www.ncbi.nlm.nih.gov/projects/gap/population/cgi-bin/StudySubjectAncestry.cgi?phs=1218&version=1&exp1=1&exp2=1&exp3=1
setValue("GeneSTAR","Patients_Age", "<60")
setValue("GeneSTAR", "PubMedLink",  paste0(pubMed,"16551714[PMID]', target='_blank'>16551714</a>"))
# setValue("GeneSTAR", "Phenotypic_Data_Type", "TBD")

# GOLDN - phs001218 (geno) ; phs001074 (pheno)
setValue("GOLDN","Ancestry", "Caucasian")
setValue("GOLDN","Patients_Age", ">18")
setValue("GOLDN", "PubMedLink",  paste0(pubMed,"22228203[PMID]', target='_blank'>22228203</a>"))
# setValue("GOLDN", "Phenotypic_Data_Type", "TBD")



##function to add new rows
newRows <- function( newEntries, data){
  data =  rbindlist(list(data, data.table( Name=c(newEntries) )) , use.names=T, fill=T  )
  return( data )
}

f5b <- newRows( newEntries = c("UK_Biobank", "Undiagnosed_Disease_Network_(UDN)"), data = f5b)


# UK_Biobank
setValue("UK_Biobank", "Country", "UK")
setValue("UK_Biobank", "Sample_Size", 538050)
setValue("UK_Biobank", "Subject_Count", 500000)
setValue("UK_Biobank", "Study_Design", "Prospective study")
setValue("UK_Biobank", "Phenotypic_Variables", 0)
setValue("UK_Biobank", "Phenotypic_Data_Type", "Health Record Data; Questionnaires; Physical Measures; Lifestyle")
setValue("UK_Biobank", "Molecular_Data_Type", "WGS; WES")
setValue("UK_Biobank", "Markerset", "Exome: IDT xGen Exome Research Panel v1.0")
setValue("UK_Biobank", "Disease_Focus", "General")
setValue("UK_Biobank","Patients_Age", "40-69")
setValue("UK_Biobank", "Ancestry", "White (95%); Other (5%)")
setValue("UK_Biobank", "Consent", "<a href='https://www.ukbiobank.ac.uk/gdpr/', target='_blank'>Consent for health-related research, for their health to be followed over many years through medical and other health-related records, as well as by being re-contacted by UK Biobank</a>")
setValue("UK_Biobank", "Accession", "<a href='http://www.ukbiobank.ac.uk/wp-content/uploads/2012/09/Access-Procedures-2011.pdf', target='_blank'>UK Biobank Access Procedures</a>") 
setValue("UK_Biobank", "Link", "<a href='https://www.ukbiobank.ac.uk', target='_blank'>UK Biobank</a>")
setValue("UK_Biobank", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=25826379[PMID]', target='_blank'>25826379</a>")
setValue("UK_Biobank", "Notes", "<a href='http://biobank.ctsu.ox.ac.uk/crystal/browse.cgi', target='_blank'>UK Biobank Data Showcase</a>")

# Undiagnosed_Disease_Network_(UDN)
setValue("Undiagnosed_Disease_Network_(UDN)", "Country", "USA")
setValue("Undiagnosed_Disease_Network_(UDN)", "Sample_Size", 462)
setValue("Undiagnosed_Disease_Network_(UDN)", "Subject_Count", 1042)
setValue("Undiagnosed_Disease_Network_(UDN)", "Study_Design", "Prospective Longitudinal Cohort")
setValue("Undiagnosed_Disease_Network_(UDN)", "Phenotypic_Variables", 3965)
setValue("Undiagnosed_Disease_Network_(UDN)", "Phenotypic_Data_Type", "EMR; Primary symptom reported by patient or caregiver or by clinical experts and HPO terms")
setValue("Undiagnosed_Disease_Network_(UDN)", "Molecular_Data_Type", "NGS; WGS; WES")
setValue("Undiagnosed_Disease_Network_(UDN)", "Markerset", "GRCh37(hg19)")
setValue("Undiagnosed_Disease_Network_(UDN)", "Disease_Focus", "Undiagnosed Diseases/Rare Diseases")
setValue("Undiagnosed_Disease_Network_(UDN)","Patients_Age", "Pediatric population: 0-17 and adult population > 18")
setValue("Undiagnosed_Disease_Network_(UDN)", "Ancestry", "White (81%); Asian (6%); American Indian or Alaska Native (<1%)'; Black or African American (5%); Native Hawaiian Pacific Islander (<1%); Other (8%)")
setValue("Undiagnosed_Disease_Network_(UDN)", "Consent", "NHGRI GRU --- General research use")
setValue("Undiagnosed_Disease_Network_(UDN)", "Accession", "<a href='https://undiagnosed.hms.harvard.edu/research/', target='_blank'>Undiagnosed Disease Network (UDN)</a>")
setValue("Undiagnosed_Disease_Network_(UDN)", "Link", "<a href='https://undiagnosed.hms.harvard.edu/research/', target='_blank'>Undiagnosed Disease Network (UDN)</a>")
setValue("Undiagnosed_Disease_Network_(UDN)", "PubMedLink",  "<a href='https://www.ncbi.nlm.nih.gov/pubmed?cmd=DetailsSearch&term=26220576[PMID]', target='_blank'>26220576</a>")
setValue("Undiagnosed_Disease_Network_(UDN)", "Notes", "<a href='http://undiagnosed.hms.harvard.edu/wp-content/uploads/UDN-Manual-of-Operations_February-2016.pdf', target='_blank'>UDN Manual of Operations</a>")

f5b <- f5b[ order( f5b$Subject_Count, decreasing = TRUE), ]

colnames(f5b) <- c("Name","Country","Sample Size","Subject Count with Genomic and Clinical Data","Study Design","# Phenotypic Variables",
                        "Phenotypic Data Type","Molecular Data Type","Markerset","Disease/Focus",
                        "Patients Age (yrs)","Ancestry","Consent","Accession","Link","PubMed Link","Notes") 

f5b <- f5b[ ,  c("Name","Country", "Subject Count with Genomic and Clinical Data","Study Design","Disease/Focus","# Phenotypic Variables",
                 "Phenotypic Data Type","Sample Size","Molecular Data Type","Markerset",
                 "Patients Age (yrs)","Ancestry","Consent","Accession","Link","PubMed Link","Notes") ]

fwrite(f5b, file=paste0(file_path,"freeze5b.csv"), sep = ',', col.names = TRUE)
fwrite(f5b, file="/Users/alba/Desktop/testingApp/test2.csv", sep = ',', col.names = TRUE)
