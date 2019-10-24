library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(rdrop2)
library(shinyalert)
library(shinyBS)
library(DT)

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
outputDir <- "responses"

######################drop################################################################
# Define global #
######################################################################################
fieldsMandatory <- c("email", "dataset_submit", "country_submit", "subjects_submit", 
                        "disease_submit", "phenoVars_submit", "phenoType_submit", 
                        "sample_submit", "molecularType_submit", "consent_submit", "accession_submit")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


appCSS <- ".mandatory_star { color: red; }
   #error { color: red; }"

formCSS <- ".notbold{
    font-weight:normal  
}"

fieldsAll <-  c("email", "dataset_submit", "country_submit", "phenoType_submit", "design_submit",  "disease_submit", 
                "subjects_submit", "sample_submit", "molecularType_submit","markerset_submit","age_submit",
                "ancestry_submit", "consent_submit", "phenoVars_submit", "pubmed_submit", 
                   "accession_submit", "linkClinicalGenomic_submit", "linkGenomic_submit","notes_submit")

responsesDir <- file.path("responses")
responesesBackup <- file.path("responsesBackup")
epochTime <- function() {
  as.integer(Sys.time())
}


##Remotely saved in dropbox
outputDir <- "responses"
outputDirBckup <- "responsesBackup"


######################################################################################
# Define UI #
######################################################################################

ui <- fluidPage(

  shinyjs::useShinyjs(), 
  shinyjs::inlineCSS(appCSS), 
  shinyjs::inlineCSS(formCSS), 
  
    tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: lightyellow !important;}')),
  
  navbarPage(
    title = "", id="main_panel",
    theme = shinythemes::shinytheme("cosmo"),
    ##first the table
    tabPanel( value="catalog",
              p("GenoPheno Catalog"),
              
              sidebarLayout(
                # Sidebar panel for inputs ----
                wellPanel(
                  fluidRow(
                    column( 3, 
                            uiOutput("countryValue")),
                    column( 3, 
                            uiOutput("radioButtonsDiseaseType")),
                    column( 3, 
                            uiOutput("radioButtonsGenomicType")),
                    column( 3,
                            uiOutput("patientValue"))
                  
                ))
                ,
                # Main panel for displaying outputs ----
                div(DT::dataTableOutput("mytable1", width = 1700), style = "font-size: 75%")
                
              )
    ), 
    tabPanel( value="submit",
              p("Submit a new dataset"),
              div(
                id = "form",
                column(3, textInput("email", labelMandatory(HTML("<b>Contributor e-mail</b>  <br/>  <span class='notbold'>(e.g., myemail@...)</span>")), "")),
                column(3, textInput("dataset_submit", labelMandatory(HTML("Data Set Name  <br/>  <span class='notbold'>(e.g., UK Biobank)</span>")), "")),
                column(3, textInput("country_submit", labelMandatory(HTML("Country  <br/>  <span class='notbold'>(e.g., UK)</span>")), "")),
                column(3, textInput("disease_submit", labelMandatory(HTML("Disease/Focus <br/>  <span class='notbold'>(e.g, general, cancer)</span>")), "")), 
                
                br(),
                
                column(3, textInput("subjects_submit", labelMandatory(HTML("Subjects with Genomic and Clinical Data <br/>  <span class='notbold'>(numeric value no commas, e.g., 1000)</span>")), "")),
                column(3, textInput("phenoVars_submit", labelMandatory(HTML("Number Of Phenotypic Variables Per Patient <br/>  <span class='notbold'>(numeric value without commas, e.g., 1000)</span>")), "")), 
                column(3, textInput("phenoType_submit", labelMandatory(HTML("Phenotypic Data Type <br/>  <span class='notbold'>(e.g., EHR, questionnaires)</span>")), "")),
                column(3, textInput("sample_submit", labelMandatory(HTML("Sample Size <br/>  <span class='notbold'>(numeric value without commas,e.g., 1000)</span>")))), 
                
                br(),
                
                column(3, textInput("molecularType_submit", labelMandatory(HTML("Molecular Data Type <br/>  <span class='notbold'> (e.g., WGS, whole genome sequencing)</span>")))), 
                column(3,  textInput("consent_submit", labelMandatory(HTML("Consent groups present in the data set <br/>  <span class='notbold'> (e.g., biomedical research)</span>")))), 
                column(3,  textInput("accession_submit", labelMandatory(HTML("Accession Link to the dataset/webpage <br/>  <span class='notbold'>(e.g., https://dbgap.ncbi.nlm.nih.gov)</span>")))), 
                column(3, textInput("design_submit", HTML("Study Design <br/>  <span class='notbold'> (e.g., Case Control Study, Prospective Study)</span>"), "")), 
                
                br(),
                
                column(3, textInput("age_submit", HTML("Patients Age (yrs) <br/> <span class='notbold'> (numeric range, e.g., 40-59, >18)</span>"), "")), 
                column(3, textInput("ancestry_submit", HTML("Ancestry <br/>  <span class='notbold'>(e.g., european(XX) or european(XX%)...)</span>"), "")), 
                column(3, textInput("pubmed_submit", HTML("PubMedID to study infrastructure publication <br/>  <span class='notbold'>(e.g., 25826379)</span>"), "")), 
                column(3, textInput("linkClinicalGenomic_submit", HTML("Link to clinical/genomic study <br/>  <span class='notbold'>(e.g., https://www.ukbiobank.ac.uk/)</span>"), "")), 
                column(3, textInput("linkGenomic_submit", HTML("Link to genomic study if different then clinical one <br/>  <span class='notbold'>(e.g., https://www.ukbiobank.ac.uk/)</span>"), "")), 

                br(),
                
                column(3, textInput("markerset_submit", HTML("Markerset <br/>  <span class='notbold'>(e.g, grc37, grc38)</span>", ""))), 
                column(3, textInput("notes_submit", HTML("Notes <br/> <span class='notbold'> (additional information)</span>", ""))), 
                
                br(), 
                
                actionButton("submit", "Submit", class = "btn-primary")
                
              ),
              DT::dataTableOutput("responsesTable"),
              shinyjs::hidden(
                div(
                  id = "thankyou_msg",
                  h3("Thanks, your response was submitted successfully!"),
                  actionLink("submit_another", "Submit another response")
                )
              ), 
              shinyjs::hidden(
                span(id = "submit_msg", "Submitting..."),
                div(id = "error",
                    div(br(), tags$b("Error: "), span(id = "error_msg"))
                )
              )
              
    ),
    tabPanel(value="main",
             title = p("Suggest a modification on the current catalog data"),
             fluidRow( 
               #column ( 6,
               sidebarLayout(
                 sidebarPanel(
                   tags$p(HTML("The Shiny App of this on-line catalog is automatically generated based on  the CSV file <a href='https://github.com/hms-dbmi/geno-pheno-CatalogShiny/blob/master/csv/tableData.csv', target='_blank'>\"tableData.csv\"</a> available at the GitHub repo: <a href='https://github.com/hms-dbmi/geno-pheno-CatalogShiny', target='_blank'>geno-pheno-CatalogShiny</a>" )),
                   tags$p(HTML( "To propose any correction, please:")),
                   p(
                     HTML("<ol>
                                <li>Fork the GitHub repo <a href='https://github.com/hms-dbmi/geno-pheno-CatalogShiny', target='_blank'>geno-pheno-CatalogShiny</a></li>
                                <li>Propose the changes in the CSV file <a href='https://github.com/hms-dbmi/geno-pheno-CatalogShiny/blob/master/csv/tableData.csv', target='_blank'>\"tableData.csv\"</a></li>
                                <li>Submit a pull request</li>
                                </ol>")
                    ),
                   br(),
                   h3( "Thank you for your contribution to update and improve the GenoPheno Catalog!" ),
                   br(),
                   tags$p(HTML( "Your proposed changes will be reivewed in the following days")),
                   br(),
                   width = 12
                   ),
                 mainPanel(img(src = 'logo.png', align = "center", height="30px")), 
                 
               ))
    ), 
    tabPanel(value="main",
             title = p("About"),
             fluidRow( 
               #column ( 6,
               sidebarLayout(
                 sidebarPanel(
                   h3( "Welcome to the GenoPheno Catalog Shiny App!" ),
                   br(),
                   tags$p(HTML( "The objective of this Shiny App is to provide a dynamic online dataset catalog. We welcome the community to correct and complete it." ) ),
                   tags$h5(HTML("<u>Inclusion Criteria</u>")),
                   p(
                     HTML("<ol>
                                <li>Over five hundred (500) human subjects</li>
                                <li>Contain both genotype and phenotype data of the same subjects</li>
                                <li>Include Whole Genome Sequencing (WGS) or Whole Exome Sequencing (WES) data as part of their genomic data content</li>
                                <li>Include at least one hundred (100) recorded phenotypic variables per subject</li>
                                <li>The dataset has to be accessible through a website or via a way of collaboration with investigators.</li>
                                </ol>")
                     
                     
                     
                     
                     
                   ),
                   br(),
                   tags$h5(HTML("<u>All five criteria must be meet</u>")),
                   
                   tags$p(HTML( "The GenoPheno catalog contains:
                                        <li>Dataset name (long name and acronym if any)</li>
                                        <li>Country (where does the research take place)</li>
                                        <li>Subject count with both genomic and clinical data</li>
                                        <li>Study design (e.g., cohort, prospective, longitudinal)</li>
                                        <li>Number of phenotypic variables per patient</li>
                                        <li>Phenotypic data type (e.g., electronic health records -EHR-, questionnaires, clinical notes)</li>
                                        <li>Sample size (total number of genomic samples [e.g., # of WGS samples + # of WES samples])</li>
                                        <li>Molecular data type (e.g., SNP array, whole genome sequencing data -WGS -, whole exome sequencing data -WES- )</li>
                                        <li>Study Markerset</li>
                                        <li>Disease/Focus (e.g., general or disease specific)</li>
                                        <li>Patients age in years</li>
                                        <li>Ancestry</li>
                                        <li>Consent groups present in the dataset (e.g., biomedical, disease-specific)</li>
                                        <li>Accession link to the dataset (link to the website or contact information to obtain data access)</li>
                                        <li>Link to clinical/genomic study</li>
                                        <li>Link to genomic study if different than the clinical one</li>
                                        <li>Pubmed identifier number to key study infrastructure publication</li>" ) ),
                   br(),
                   
                   tags$p(HTML("For further details see the <a href=\"\">manuscript</a>.")),
                   width = 12
                 ),
                 mainPanel(img(src = 'logo.png', align = "center", height="30px")), 
                 
                 # )
               ))
    )
  )
)

######################################################################################
# Define server #
######################################################################################

server <- function(input, output, session) {
  
  attr(input, "readonly") <- FALSE
  dataValues <- reactiveValues()
  biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/geno-pheno-CatalogShiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

  observeEvent(input$confirm0, {
    
    
    if( input$dataset == ""){
      biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/geno-pheno-CatalogShiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
    }else{
      biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/geno-pheno-CatalogShiny/master/csv/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
      
      output$mytable1 <- DT::renderDataTable(DT::datatable({
        data <- biobanks
        if (input$dataset %in% data$Name) {
          data <- data[data$Name == input$dataset,]
          colnames(data) <- gsub("_", " ", colnames(data))
          data
        }else{
          
          # Show a modal when the button is pressed
          shinyalert("Oops!", "There is not any biobank/datase with that name in the catalog", type = "error")
        }
        
        
      }))
      
    }
    
    
  })
  
  output$countryValue <- renderUI({
    selectInput(inputId = "studydesign", 
                label = "Study Design:", 
                choices =  c("All", unique(tolower(biobanks$Study.Design)))
    )
    
  })
  
  output$patientValue <-
    renderUI({
      sliderInput("patientValue", "Subject count with genomic and clinical data:",
                  min = min(biobanks$Subject, na.rm = TRUE),
                  max = max(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE),
                  value = c(min(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE), max = max(as.numeric(as.character(biobanks$Subject.Count)), na.rm = TRUE))
      )
    })
  
  
  output$radioButtonsDiseaseType <- renderUI({
    radioButtons("diseasetype", "Disease/Focus:",
                 c("All" = "all",
                   "General" = "general", 
                   "Disease specific" = "specific"
                 )
    )
    
  })
  
  output$radioButtonsGenomicType <- renderUI({
    radioButtons("genomictype", "Molecular Data Type:",
                 c("All" = "all",
                   "WGS" = "wgs", 
                   "WES" = "wes"
                 )
    )
    
  })
  
  output$mytable1 <- DT::renderDataTable(
    DT::datatable({
    data <- biobanks
    if (input$diseasetype != "all") {
      if (input$diseasetype == "general") {
        data <- data[data$Disease.Focus == "General" & !is.na(data$Disease.Focus),]
      }
      if (input$diseasetype != "general") {
        data <- data[data$Disease.Focus != "General"  & !is.na(data$Disease.Focus),]
      }
    }
    if (input$genomictype != "all") {
      if (input$genomictype == "wgs") {
        data <- data[ grep( "WGS", data$Molecular.Data.Type), ]
      }
      if (input$genomictype == "wes") {
        data <- data[ grep( "WES", data$Molecular.Data.Type), ]
      }
    }
    if (input$studydesign != "All") {
      data <- data[tolower(data$Study.Design) == tolower(input$studydesign),]
    }
    if( ! is.null(input$patientValue) ){
      data <- data[ as.numeric(as.character(data$Subject.Count)) >= input$patientValue[1] &
                      as.numeric(as.character(data$Subject.Count)) <= input$patientValue[2] , ]
    }

    colnames(data) <- c("Name","Country", "Subject Count with Genomic and Clinical Data","Study Design","Disease/Focus","Number Of Phenotypic Variables Per Patient",
                         "Phenotypic Data Type","Sample Size","Molecular Data Type","Markerset",
                         "Patients Age (yrs)","Ancestry","Consent","Accession Link to the Dataset","Link to Clinical And Genomic Study", "Link to Genomic Study (if different than the clinical)","PubMed ID","Notes")
    data
    
  }, escape = FALSE, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 30), callback = JS("
var tips = ['Dataset name (long name and acronym if any)', 'Country (where does the research take place)', 'Subject count with both genomic and clinical data',
'Study design (e.g., cohort, prospective, longitudinal)','Disease/Focus (e.g., general or disease specific)','Number Of Phenotypic Variables Per Patient',
'Phenotypic data type (e.g., electronic health records -EHR-, questionnaires, clinical notes)',
'Sample size (total number of genomic samples [e.g., # of WGS samples + # of WES samples])','Molecular data type (e.g., SNP array, whole genome sequencing data -WGS -, whole exome sequencing data -WES- )','Markerset',
'Patients Age in years','Ancestry','Consent groups present in the dataset (e.g., biomedical, disease-specific)','Accession link to the dataset (link to the website or contact information to obtain data access)', 
'Link to clinical/genomic study','Link to genomic study if different than the clinical one','Pubmed identifier number to the key study infrastructure publication/s','Notes'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);
}
")
)
  )

  
  
  
  observeEvent(input$confirm1, {
    
    
    updateTabsetPanel(session, "main_panel",
                      selected = "submission")
    
  })
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
  
  formData <- reactive({
    dataSubmission <- sapply(fieldsAll, function(x) input[[x]])
    dataSubmission <- c(dataSubmission, timestamp = epochTime())
    dataSubmission$status <- "Validation Pending"
    dataSubmission <- t(dataSubmission)
    dataSubmission
  })
  
  outputDir <- "responses"
  outputDirBckup <- "responsesBackup"

  saveData <- function(dataSubmission) {
    dataSubmission <- t(dataSubmission)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(dataSubmission))
    dataSubmission <- gsub(",", ";", dataSubmission)
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(dataSubmission, filePath, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir)
    drop_upload(filePath, path = outputDirBckup)

  }
  
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })   
  
  
  
  
}


# Create Shiny app ----
shinyApp(ui, server)