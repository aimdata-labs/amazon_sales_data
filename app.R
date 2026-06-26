# Shiny app for generating reports on Amazon sales data

library(shiny)
library(rmarkdown)
library(tidyverse)

linebreaks <- function(n){HTML(strrep(br(), n))}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sales report generator"),
    
    linebreaks(1),
    
    fileInput("file", NULL, accept = c(".csv")),
    
    linebreaks(1),
    
    downloadButton("download_report", label = "Generate report")
           
       )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  uploaded_data <- reactive({
    req(input$file)
    read_csv(input$file$datapath, show_col_types = FALSE)
  })
  
  output$download_report <- downloadHandler(
      filename = paste0("report", format(Sys.Date(), "%Y%m%d"), ".html"), 
      
      content = function(file) {
        
        # Copy qmd to a temp folder so it can be rendered outside the app dir
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(dataset = uploaded_data())
        
        id <- showNotification(
          "Rendering report...", 
          duration = NULL,
          closeButton = FALSE
        )
        
        on.exit(removeNotification(id), add = TRUE)
        
        # According to Hadley, it's best to create a function and use callr
        # But I haven't figured out how to get it to work yet
        rmarkdown::render(
          "report.rmd",
          output_file = file, 
          params = params, 
          envir = new.env(parent = globalenv())
        )
      }
    )

}


# Run the application 
shinyApp(ui = ui, server = server)
