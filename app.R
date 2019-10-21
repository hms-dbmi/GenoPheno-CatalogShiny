library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(rdrop2)
library(shinyalert)
library(shinyBS)
library(DT)

##Remotely saved in dropbox
#token <- drop_auth(new_user = TRUE)
#Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
#saveRDS(token, "droptoken.rds")
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

fieldsAll <-  c("email", "dataset_submit", "country_submit", "phenoType_submit", "design_submit",  "disease_submit", 
                "subjects_submit", "sample_submit", "molecularType_submit","markerset_submit","age_submit",
                "ancestry_submit", "consent_submit", "phenoVars_submit", "pubmed_submit", 
                   "accession_submit", "link_submit", "notes_submit")

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
                column(3, textInput("email", labelMandatory("Contributor e-mail"), "")),
                column(3, textInput("dataset_submit", labelMandatory("Data Set Name (e.g., UK Biobank)"), "")),
                column(3, textInput("country_submit", labelMandatory("Country"), "")),
                column(3, textInput("disease_submit", labelMandatory("Disease/Focus (e.g, general, cancer)"), "")), 
                
                br(),
                
                column(3, textInput("subjects_submit", labelMandatory("Subject Count with Genomic and Clinical Data (numeric value no commas, e.g., 1000)"), "")),
                column(3, textInput("phenoVars_submit", labelMandatory("Number Of Phenotypic Variables Per Patient (numeric value without commas, e.g., 1000)"), "")), 
                column(3, textInput("phenoType_submit", labelMandatory("Phenotypic Data Type (e.g., EHR, questionnaires)"), "")),
                column(3, textInput("sample_submit", labelMandatory("Sample Size (numeric value without commas, e.g., 1000)"))), 
                
                br(),
                
                column(3, textInput("molecularType_submit", labelMandatory("Molecular Data Type (e.g., WGS, whole genome sequencing)"))), 
                column(3,  textInput("consent_submit", labelMandatory("Consent groups present in the data set (e.g., biomedical research)"))), 
                column(3,  textInput("accession_submit", labelMandatory("Accession (Link to the dataset/webpage e.g., https://dbgap.ncbi.nlm.nih.gov/aa/wga.cgi)"))), 
                column(3, textInput("design_submit", "Study Design (e.g., Case Control Study, Prospective Study)", "")), 
                
                br(),
                
                column(3, textInput("age_submit", "Patients Age (yrs) (numeric range, e.g., 40-59, >18)", "")), 
                column(3, textInput("ancestry_submit", "Ancestry (e.g., european(XX) or european(XX%)...)", "")), 
                column(3, textInput("pubmed_submit", "PubMed ID of the papers describing the dataset (e.g., 25826379)", "")), 
                column(3, textInput("link_submit", "Link (Link to the webpage of the dataset e.g., https://www.ukbiobank.ac.uk/)", "")), 
                
                br(),
                
                column(3, textInput("markerset_submit", "Markerset (e.g, grc37, grc38)", "")), 
                column(3, textInput("notes_submit", "Notes (additional information)", "")), 
                
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
                   tags$p(HTML("The Shiny App of this on-line catalog is automatically generated based on  the CSV file <a href='https://github.com/hms-dbmi/geno-pheno-CatalogShiny/blob/master/tableData.csv', target='_blank'>\"tableData.csv\"</a> available at the GitHub repo: <a href='>https://github.com/hms-dbmi/geno-pheno-CatalogShiny', target='_blank'>geno-pheno-CatalogShiny</a>" )),
                   tags$p(HTML( "To propose any correction, please:")),
                   p(
                     HTML("<ol>
                                <li>Fork the GitHub repo <a href='>https://github.com/hms-dbmi/geno-pheno-CatalogShiny', target='_blank'>geno-pheno-CatalogShiny</a></li>
                                <li>Propose the changes in the CSV file <a href='https://github.com/hms-dbmi/geno-pheno-CatalogShiny/blob/master/tableData.csv', target='_blank'>\"tableData.csv\"</a></li>
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
                   tags$h5(HTML("<u>Inclusion Criterias</u>")),
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
                   tags$h5(HTML("<u>All five criterias must be meet</u>")),
                   
                   tags$p(HTML( "The GenoPheno catalog contains:
                                        <li>Dataset name</li>
                                        <li>Country</li>
                                        <li>Sample size</li>
                                        <li>Subject count</li>
                                        <li>Study design</li>
                                        <li>Number of phenotypic variables</li>
                                        <li>Phenotypic data type (e.g., electronic health records -EHR-, questionnaires, clinical notes)</li>
                                        <li>Molecular data type (e.g., SNP array, whole genome sequencing data -WGS -, whole exome sequencing data -WES- )</li>
                                        <li>Markerset</li>
                                        <li>Disease/Focus (e.g., general or disease specific)</li>
                                        <li>Patients age</li>
                                        <li>Ancestry</li>
                                        <li>Consent</li>
                                        <li>Accession</li>
                                        <li>Link</li>
                                        <li>PubMed Link</li>" ) ),
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
  biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/geno-pheno-CatalogShiny/master/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

  observeEvent(input$confirm0, {
    
    
    if( input$dataset == ""){
      biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/geno-pheno-CatalogShiny/master/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
    }else{
      biobanks <- read.delim( "https://raw.githubusercontent.com/hms-dbmi/geno-pheno-CatalogShiny/master/tableData.csv", nrows=-1L, sep=",", header=T, stringsAsFactors=FALSE)

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
                         "Patients Age (yrs)","Ancestry","Consent","Accession","Link","PubMed Link","Notes")
    data
    
  }, escape = FALSE, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 30), callback = JS("
var tips = ['Data Set Name', 'Country', 'Subject Count with Genomic and Clinical Data','Study Design','Disease/Focus','Number Of Phenotypic Variables Per Patient',
                         'Phenotypic Data Type','Sample Size','Molecular Data Type','Markerset',
                         'Patients Age (yrs)','Ancestry','Consent groups present in the data set','Accession','Link','PubMed Link to papers describing the dataset','Notes'],
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
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(dataSubmission, filePath, row.names = FALSE, quote = TRUE)
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