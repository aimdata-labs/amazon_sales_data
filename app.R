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
    
    h3("This is an example of a report generator. It accepts one file upload."), 
    
    p("The file to be uploaded for this example may be downloaded here:"),
    
    downloadButton("amazon_raw_data", label = "Download data"),
    
    # Doubtful that clients will know how to interact with github
    # linebreaks(1), 
    # 
    # p("It may also be copied from ", 
    #   a(href = "https://github.com/aimdata-labs/amazon_sales_data/raw/refs/heads/main/data/amazon.csv", "here", 
    #     .noWS = "outside"), 
    #   ".", 
    #   .noWS = c("after-begin", "before-end")),
    
    # https://aimdata.shinyapps.io/amazon_sales_data/
    
    linebreaks(3), 
    
    h3("Upload the file you downloaded above."),
    
    p("As long as the uploaded file remains in the same format i.e. with the same number of columns and with column names unchanged, updated versions may be uploaded in place of the original, generating a new version of the report with the updated data."),
    
    fileInput("file", NULL, accept = c(".csv")),
    
    linebreaks(1),
    
    h3("This button generates the report."), 
    
    p("This may take a few minutes."), 
    
    downloadButton("download_report", label = "Generate report"), 
    
    linebreaks(3), 
    
    p("NOTE: The download produced by the button above is an .html file, which allows for interactive elements. This file, once saved, will open in your web browser. However, Microsoft word or PDF documents can also be produced, if preferred. The report generated shows a range of charts that should serve as an example of what you might request in your own reports. There are two interactive elements towards report -- an interactive scatterplot and a searchable reference table; neither of these elements will be available in static Word or PDF documents."),
    
    linebreaks(1), 
    
    p("This interface can also be customised: allowing for uploads and downloads. For instance, if you just wanted to upload raw data and download a cleaned dataset, that would also be possible. This is just a simple interface to run a script or even multiple scripts; what each script does is fairly flexible.")
           
       )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  amazon_raw_data <- read_csv("./data/amazon.csv")
  
  output$amazon_raw_data <- downloadHandler(
    filename = "amazon.csv", 
    content = function(file) {
      write_csv(amazon_raw_data, file)
    }
  )
  
  uploaded_data <- reactive({
    req(input$file)
    read_csv(input$file$datapath, show_col_types = FALSE)
  })
  
  output$download_report <- downloadHandler(
      filename = paste0("report", format(Sys.Date(), "%Y%m%d"), ".html"), 
      
      content = function(file) {

        # Copy qmd to a temp folder so it can be rendered outside the app dir
        # Not sure which one works properly 
        tempReport <- file.path(tempdir(), "report.Rmd")
        # tempReport <- tempfile(fileext = ".Rmd")
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
          "report.Rmd",
          output_file = file, 
          params = params, 
          envir = new.env(parent = globalenv()), 
          quiet = TRUE
        )
      }
    )

}


# Run the application 
shinyApp(ui = ui, server = server)
