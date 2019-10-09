library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(rdrop2)
library(shinyalert)


######################################################################################
# Define global #
######################################################################################


######################################################################################
# Define UI #
######################################################################################

ui <- fluidPage(
  
  navbarPage(
    title = "", id="main_panel",
    theme = shinythemes::shinytheme("cosmo"),
   ##first the table
    tabPanel( value="catalog",
              p("Biobank Catalog"),
              
              sidebarLayout(
                # Sidebar panel for inputs ----
                wellPanel(
                  fluidRow(
                    column( 3, 
                            uiOutput("countryValue")),
                    column( 3, 
                            uiOutput("radioButtonsDiseaseType")),
                    column( 3,
                            uiOutput("patientValue")),
                    column( 3, 
                            uiOutput("sampleValue")))
                  
                )
                ,
                # Main panel for displaying outputs ----
                DT::dataTableOutput("mytable1")
              )
    ),
   tabPanel(value="main",
            title = p("About"),
            fluidRow( 
              #column ( 6,
                       sidebarLayout(
                         sidebarPanel(
                           h3( "Welcome to the Biobank Catalog Shiny App!" ),
                           br(),
                           tags$p(HTML( "The objective of this Shiny App is to provide a dynamic online biobank catalog. We welcome the community to correct and complete it." ) ),
                           tags$h5(HTML("<u>Inclusion Criteria</u>")),
                           p(
                             HTML("<ol>
                                <li>Contain both genotype and phenotype data of the same patients</li>
                                <li>Include at least one hundred (100) recorded clinical variables</li>
                                <li>Include Whole Genome Sequencing (WGS) or Whole Exome Sequencing (WES) data as part of their genomic data content</li>
                               <li>Over 1,000 patients (unless the dataset is focused on rare diseases)</li>
                               <li>The data has to be accessible to the public, either through an open-access license</li>
                                  </ol>")
                           ),
                           br(),
                           tags$p(HTML( "The biobank catalog contains:
                                        <li>Biobank/dataset name</li>
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
                           tags$p(HTML( "Shiny app GitHub repo: <a href=\"\">https://github.com/hms-dbmi/biobankCatalogShiny</a>.")),
                           tags$p(HTML( "To submit new entries or correct the existing ones, update the CSV file \"xxxx\" available at the GitHub repo: <a href=\"\">https://github.com/hms-dbmi/biobankCatalogShiny</a>.")),
                           width = 12
                         ),
                         #mainPanel(img(src = 'testing.png', align = "center", width="1000px", height="500px"))
                         mainPanel(img(src = 'logo.png', align = "center", height="30px"))
                         
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
  
  #biobanks <- read.csv("https://raw.githubusercontent.com/aGutierrezSacristan/testingApp/master/BiobankList_test.csv")
  #biobanks <- read.csv("/Users/alba/Desktop/biobankPaper/Biobanks/catalogBiobanks6oct.csv")
  biobanks <- fread( "/Users/alba/Desktop/biobankPaper/Biobanks/test2.csv", nrows=-1L, verbose=getOption("datatable.verbose"),header=T, stringsAsFactors=FALSE)
  
  observeEvent(input$confirm0, {
    
    #biobanks <- read.delim( "BiobankList_test.csv", 
    #                        sep = ",", 
    #                        header = TRUE)
    if( input$dataset == ""){
      #biobanks <- read.csv("https://raw.githubusercontent.com/aGutierrezSacristan/testingApp/master/BiobankList_test.csv")
      #biobanks <- read.csv("/Users/alba/Desktop/biobankPaper/Biobanks/catalogBiobanks6oct.csv")
      biobanks <- fread( "/Users/alba/Desktop/biobankPaper/Biobanks/test2.csv", nrows=-1L, verbose=getOption("datatable.verbose"),header=T, stringsAsFactors=FALSE)
      
      updateTabsetPanel(session, "main_panel",
                        selected = "catalog")
    }else{
      #biobanks <- read.csv("https://raw.githubusercontent.com/aGutierrezSacristan/testingApp/master/BiobankList_test.csv")
      #biobanks <- read.csv("/Users/alba/Desktop/biobankPaper/Biobanks/catalogBiobanks6Oct.csv")
      #biobanks <- read.csv("/Users/alba/Desktop/biobankPaper/Biobanks/catalogBiobanks6oct.csv")
      biobanks <- fread( "/Users/alba/Desktop/biobankPaper/Biobanks/test2.csv", nrows=-1L, verbose=getOption("datatable.verbose"),header=T, stringsAsFactors=FALSE)
      
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
    selectInput(inputId = "country", 
                label = "Country:", 
                choices =  c("All", unique(biobanks$Country))
    )
    
  })
  
  output$patientValue <-
    renderUI({
      sliderInput("patientValue", "Subject count:",
                  min = min(biobanks$Subject, na.rm = TRUE),
                  max = max(as.numeric(as.character(biobanks$`Subject Count`)), na.rm = TRUE),
                    value = c(min(as.numeric(as.character(biobanks$`Subject Count`)), na.rm = TRUE), max = max(as.numeric(as.character(biobanks$`Subject Count`)), na.rm = TRUE))
      )
    })
  
  output$sampleValue <-
    renderUI({
      sliderInput("sampleValue", "Sample size:",
                  min = min(as.numeric(as.character(biobanks$`Sample Size`)), na.rm = TRUE),
                  max = max(as.numeric(as.character(biobanks$`Sample Size`)), na.rm = TRUE),
                  value = c(min(as.numeric(as.character(biobanks$`Sample Size`)), na.rm = TRUE), max = max(as.numeric(as.character(biobanks$`Sample Size`)), na.rm = TRUE))
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
  
  
  
  # output$mytable1 <- DT::renderDataTable({
  #   DT::datatable(biobanks, rownames = FALSE)
  # })
  output$mytable1 <- DT::renderDataTable(DT::datatable({
    data <- biobanks
    if (input$diseasetype != "all") {
      if (input$diseasetype == "general") {
        data <- data[data$`Disease/Focus` == "General" & !is.na(data$`Disease/Focus`),]
      }
      if (input$diseasetype != "general") {
        data <- data[data$`Disease/Focus` != "General"  & !is.na(data$`Disease/Focus`),]
      }
    }
    if (input$country != "All") {
      data <- data[data$Country == input$country,]
    }
    if( ! is.null(input$patientValue) ){
      data <- data[ as.numeric(as.character(data$`Subject Count`)) >= input$patientValue[1] &
                      as.numeric(as.character(data$`Subject Count`)) <= input$patientValue[2] , ]
    }
    if( ! is.null(input$sampleValue) ){
      data <- data[ as.numeric(as.character(data$`Sample Size`)) >= input$sampleValue[1] &
                      as.numeric(as.character(data$`Sample Size`)) <= input$sampleValue[2] , ]
    }
    data
  }))
  
  observeEvent(input$confirm1, {
    
    
    updateTabsetPanel(session, "main_panel",
                      selected = "submission")
    
  })
  
  
}


# Create Shiny app ----
shinyApp(ui, server)