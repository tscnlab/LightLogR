pkgload::load_all(".")

ui <- shiny::fluidPage(
  shiny::fileInput('file1', 'Select a data file'),
  shiny::tableOutput('results')
)

server <- function(input, output) {
  output$results <- shiny::renderTable({
    inFile <- input$file1
    if (is.null(inFile)) {
      return()
    }
    
    lines <- readLines(inFile$datapath, n = 1000)
    temp_file <- tempfile()
    writeLines(lines, temp_file)
    
    read_methods <- list(
      LYS = import.LYS,
      ActLumus = import.ActLumus,
      read_csv = function(x) readr::read_csv(x)
    )
    
    result <- read_methods %>%
      purrr::imap_dfr(function(func, name) {
        tryCatch({
          func(temp_file)
          tibble::tibble(Method = name, Success = TRUE, Error = "")
        }, error = function(e) {
          tibble::tibble(Method = name, Success = FALSE, Error = as.character(e))
        })
      })
    
    unlink(temp_file)
    
    result
  })
}

shiny::shinyApp(ui, server)

