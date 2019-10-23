library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(rdrop2)
library(shinyalert)
library(shinyBS)
library(DT)
library(V8)


responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}


token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

outputDir <- "responses"


loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  dataNotValidated <- lapply(filePaths, drop_read_csv, blank.lines.skip = FALSE, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  dataNotValidated <- data.frame(matrix(unlist(dataNotValidated), nrow=length(dataNotValidated), byrow=T))
  colnames( dataNotValidated ) <- c("email", "Name","Country", "Phenotypic Data Type","Study Design",
                                    "Disease/Focus", "Subject Count with Genomic and Clinical Data",
                                    "Sample Size","Molecular Data Type", "Markerset", "Patients Age (yrs)",
                                    "Ancestry","Consent","Number Of Phenotypic Variables Per Patient",
                                    "PubMed Link", "Accession","LinkBoth","LinkGenomic","Notes", "ID", "Status")
  
  
  dataNotValidated <- dataNotValidated[, c("email", "Name","Country", "Subject Count with Genomic and Clinical Data",
                                           "Study Design","Disease/Focus","Number Of Phenotypic Variables Per Patient",
                                           "Phenotypic Data Type","Sample Size","Molecular Data Type","Markerset",
                                           "Patients Age (yrs)","Ancestry","Consent","Accession","LinkBoth","LinkGenomic","PubMed Link","Notes", "ID", "Status")]
  
  dataNotValidated$Accession <- as.character(dataNotValidated$Accession)
  dataNotValidated$LinkBoth <- as.character(dataNotValidated$LinkBoth)
  dataNotValidated$LinkGenomic <- as.character(dataNotValidated$LinkGenomic)
  dataNotValidated$`PubMed Link`  <- as.character(dataNotValidated$`PubMed Link`)
  
  for( i in 1:nrow(dataNotValidated)){
    if( any(grep( "<a href=",  dataNotValidated$Accession[i])) == FALSE ){
      dataNotValidated$Accession[i] <- sprintf(paste0("<a href='", dataNotValidated$Accession[i], "', target='_blank'>", dataNotValidated$Accession[i], "</a>" ))
    }
    if( dataNotValidated$LinkBoth[i] != ""  &  any(grep( "<a href=",  dataNotValidated$LinkBoth[i])) == FALSE ){
      dataNotValidated$LinkBoth[i] <- sprintf(paste0("<a href='", dataNotValidated$LinkBoth[i], "', target='_blank'>", dataNotValidated$Name[i], "</a>" ))
    }
    if( dataNotValidated$LinkGenomic[i] != ""  &  any(grep( "<a href=",  dataNotValidated$LinkGenomic[i])) == FALSE ){
      dataNotValidated$LinkGenomic[i] <- sprintf(paste0("<a href='", dataNotValidated$LinkGenomic[i], "', target='_blank'>", dataNotValidated$Name[i], "</a>" ))
    }
    if( dataNotValidated$`PubMed Link`[i] != ""  &  any(grep( "<a href=",  dataNotValidated$`PubMed Link`[i])) == FALSE ){
      dataNotValidated$`PubMed Link`[i] <- sprintf(paste0("<a href='https://www.ncbi.nlm.nih.gov/pubmed/", dataNotValidated$`PubMed Link`[i],"', target='_blank'>", dataNotValidated$`PubMed Link`[i], "</a>" ))
    }
  }
  
  data.table::setDT(dataNotValidated)
  dataNotValidated[, (colnames(dataNotValidated)) := lapply(.SD, as.character), .SDcols = colnames(dataNotValidated)]
  dataNotValidated <- as.data.frame( dataNotValidated)
}

this_table <- loadData()

ui <- fluidPage(
  
  titlePanel("Validation App"),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  wellPanel(
    fluidRow(
      column( 3, 
              uiOutput("validationStatus"))
  ),
  actionButton("delete_btn", "Delete"), 
  actionButton("save_btn", "Save")),
  dataTableOutput("shiny_table")

)

server <- function(input, output) {
  
  #this_table <- reactiveVal(this_table)
  rvs <- reactiveValues(
    data = this_table, 
    dbdata = NA, 
    dataSame = TRUE, 
    editedInfo = NA 
  )
  
  
  observeEvent(input$delete_btn, {
   
    if (!is.null(input$shiny_table_rows_selected)) {
      outputDir <- "responses"
      filesInfo <- drop_dir(outputDir)
      filePaths <- filesInfo$path_display
      print(as.numeric(input$shiny_table_rows_selected))
      print(input$shiny_table_rows_selected)
      idToDelete <- rvs$data[input$shiny_table_rows_selected, "ID"]
      print(idToDelete)
      drop_delete(filePaths[grep(idToDelete, filePaths)])
      rvs$data <- rvs$data[-as.numeric(input$shiny_table_rows_selected),]
    }
    
  })
  
 
  
  output$shiny_table = DT::renderDT(
    rvs$data,editable = TRUE,rownames = FALSE,  escape = FALSE, extensions="Buttons",options = list(dom = 'Bfrtip',
                                                             buttons = c('csv', 'excel')
    ))
  
  proxy3 = dataTableProxy('shiny_table')
  
  observeEvent(input$shiny_table_cell_edit, {
    
    info = input$shiny_table_cell_edit
    
    i = info$row
    j = info$col = info$col + 1  # column index offset by 1
    v = info$value
    print(info$value)

    print(i)
    print(j)
    rvs$data[i, j] <<- info$value
    #print( purrr::flatten_dbl(rvs$data[i, j]))
    #rvs$data[i, j] <<- DT::coerceValue(v, purrr::flatten_dbl(rvs$data[i, j]))
    
    replaceData(proxy3, rvs$data, resetPaging = FALSE, rownames = FALSE)
    rvs$dataSame <- identical(rvs$data, rvs$dbdata)
    
    observeEvent(input$save_btn, {
      t = rvs$data
      if (!is.null(input$shiny_table_rows_selected)) {
        outputDir <- "responses"
        filesInfo <- drop_dir(outputDir)
        filePaths <- filesInfo$path_display
        print(as.numeric(input$shiny_table_rows_selected))
        rowToModify <- t(t[as.numeric(input$shiny_table_rows_selected), ])
        idToModify <- as.character(t[as.numeric(input$shiny_table_rows_selected), "ID"])
        fileName <- unlist(strsplit(filePaths[grep(idToModify, filePaths)], "responses/" ))[2]
        filePath <- file.path(tempdir(), fileName)
        rowToModify <-  rowToModify[ c("email", "Name","Country", "Phenotypic Data Type","Study Design",
                          "Disease/Focus", "Subject Count with Genomic and Clinical Data",
                          "Sample Size","Molecular Data Type", "Markerset", "Patients Age (yrs)",
                          "Ancestry","Consent","Number Of Phenotypic Variables Per Patient",
                          "PubMed Link", "Accession","LinkBoth","LinkGenomic", "Notes", "ID", "Status"),]
        write.csv(rowToModify, filePath, row.names = FALSE, quote = TRUE)
        # Upload the file to Dropbox
        drop_upload(filePath, path = outputDir)
      }
      
      
    })
    
    
  })

}

shinyApp(ui = ui, server = server)