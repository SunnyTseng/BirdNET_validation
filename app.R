
# preset ------------------------------------------------------------------

library(shiny) 
library(bslib)
library(shinyWidgets) 
library(shinyFiles)

library(tidyverse)
library(DT)
library(praise)

library(tuneR)
library(seewave)


# Function to play audio based on filepath, start, and end columns
play_audio <- function(filepath) { 
  song <- readWave(filepath, units = "seconds") #
  play(song, ... = "/play /close") 
}


# Function to view spectrogram
view_spectrogram <- function(filepath, flim, wl) {
  # Read in the specific segment of the audio file
  song <- readWave(filepath, units = "seconds")
  
  # Plot the spectrogram
  seewave::spectro(song, f = song@samp.rate, flim = flim, ovlp = 50,
                   collevels = seq(-40, 0, 1), wl = wl, scale = FALSE,
                   main = str_extract(filepath, "[^/]+(?=\\.wav$)"))
}



# ui ----------------------------------------------------------------------

ui <- page_sidebar(
  
  # add title
  title = "Bird Audio Validation for BirdNET Threshold Setting",
  
  sidebar_width = 350,
  
  # add sidebar
  sidebar = sidebar(
    card("Upload Files", 
         ## input: validation_file
         fileInput("detection_list", 
                   label = "Metadata csv file", 
                   multiple = FALSE, 
                   accept = c("text/csv", 
                              "text/comma-separated-values,text/plain",
                              ".csv"),
                   width = "100%"),
         
         ## input: validation folder
         p("Recording folder"),
         shinyDirButton(id = "dir", 
                        label = "Select", 
                        title = "Select the species recording folder"),
         
         verbatimTextOutput("dir", placeholder = TRUE),
        
         ## output: target_species
         textOutput("target_species")
    ),
    
    card("Settings",
         ## input: flim, wl
         numericRangeInput("flim", "Frequency range:",
                           min = 1, max = 10, value = c(0, 6)),
         
         numericInput("wl", "Window length:", value = 512),
    ),
    
    card("Actions",
         # ## ui: progress_bar
         # uiOutput("progress_bar"),
         
         ## button: praise_me
         actionButton("praise_me", "Praise me"), 
         
         # ## button: download_data
         # downloadButton("download_data", "Download")
    )
    
    
  ),
  
  # add main panel
  card(
    ## output: main_table
    DTOutput("main_table")
  ),
  card(
    ## output: spectrogram
    plotOutput("spectrogram")
  )
)



# server  -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # reactive values --------------------------------------------------------
  
  roots <- if (.Platform$OS.type == "windows") {
    # List all available drives on Windows
    drives <- system("wmic logicaldisk get name", intern = TRUE)
    drives <- gsub("\\s", "", drives[-1])  # Clean up drive names
    
    c(Desktop = file.path(Sys.getenv("USERPROFILE"), "Desktop"),
      Documents = file.path(Sys.getenv("USERPROFILE"), "Documents"),
      Working_dir = ".",
      setNames(drives, drives)) # Add drives with their names as keys
  } else {
    # For Unix-like systems, include common mount points
    external_drives <- list.files("/Volumes", full.names = TRUE)
    
    c(Desktop = "~/Desktop",
      Documents = "~/Documents",
      Working_dir = ".",
      setNames(external_drives, basename(external_drives))) # Use volume names
  }
  
  
  shinyDirChoose(
    input,
    'dir',
    roots = roots
  )
  
  dir_path <- reactive({
    # Convert the shinyDirChoose output to a usable path
    parseDirPath(roots, input$dir)
  })
  
  output$dir <- renderText({
    dir_path()
  })
  
  
  
  # Create a reactiveValues object to store the data
  rv <- reactiveValues()
  
  # Observe the detection_list input and read the data file
  observeEvent(input$detection_list, {
    tryCatch({
      rv$data_display <- read_csv(input$detection_list$datapath)
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  
  # making all the elements on the UI ---------------------------------------
  
  # render the main table
  output$main_table <- renderDT({
    # Ensure file has been uploaded before trying to render the table
    req(rv$data_display)
    
    # Render the data table with a play button
    rv$data_display %>%
      select("datetime", "common_name", "id") %>%
      mutate(`Spectrogram` = '<button class="spectrogram">Spectrogram</button>',
             `Audio` = '<button class="play-audio">Audio</button>') %>%
      datatable(editable = TRUE,
                escape = FALSE,
                selection = "none",
                options = list(pageLength = 10,
                               columDefs = list(list(targets = ncol(rv$data_display) + 1, orderable = FALSE),
                                                list(targets = ncol(rv$data_display) + 2, orderable = FALSE)))
      )
  })
  
  
  # print out target species
  output$target_species <- renderText({
    # Ensure file has been uploaded before trying to render the table
    req(rv$data_display)
    
    # find out the target species
    paste0("Target: ", rv$data_display %>%
             pull(common_name) %>%
             unique())
  })
  
  
  
  # # progress bar
  # output$progress_bar <- renderUI({
  #   # Ensure file has been uploaded before trying to render the table
  #   req(rv$data_display)
  #   
  #   # Calculate the percentage of T/F values in the data
  #   percentage <- rv$data_display %>%
  #     select(validation) %>%
  #     map_df(~ sum(.x %in% c("T", "F")) / length(.x)) %>%
  #     pull() %>%
  #     mean() * 100
  #   
  #   # Create a progress bar
  #   progressBar(title = "Progress bar:", id = "bar_id", value = percentage)
  # })
  
  
  
  # define what will happen for actions -------------------------------------
  
  ## praise the user when the praise_me button is clicked
  observeEvent(input$praise_me, {
    showNotification(praise(), type = "message")
  })
  
  
  # ## download the data when the download_data button is clicked
  # output$download_data <- downloadHandler(
  # 
  #   filename = function() {
  #     paste0(rv$data_display %>%
  #              pull(common_name) %>%
  #              unique(), "_validated.csv")
  #   },
  #   
  #   content = function(file) {
  #     write_csv(rv$data_display, file)
  #     showNotification("Download successfully.", type = "message")
  #   }
  # )
  
  
  
  # ## update the reactive object when data is edited
  # observeEvent(input$main_table_cell_edit, {
  #   
  #   info <- input$main_table_cell_edit
  #   
  #   showNotification(paste0("You made a value change with ", info$value), type = "message")
  #   
  #   rv$data_display <<- editData(rv$data_display, info, 'main_table')
  #   
  # })
  
  
  ## show spectrogram when the spectrogram button click
  observeEvent(input$main_table_cell_clicked, {
    
    info <- input$main_table_cell_clicked
    
    # Check if the click was on the spectrogram button
    if (is.null(info$value) || info$col != 4) return()
    
    # Retrieve the file path and start/end times for the selected row
    selected_row <- rv$data_display[info$row, ]
    filepath <- paste0(dir_path(), "/", 
                       selected_row$common_name, "_", selected_row$id, ".wav")
    
    # show spectrogram
    if (file.exists(filepath)) {
      output$spectrogram <- renderPlot({
        
        view_spectrogram(filepath = filepath,
                         flim = input$flim,
                         wl = input$wl)
      })
    } else {
      showNotification("Audio file not found.", type = "error")
    }
  })
  
  
  ## play audio when the play-audio button click
  observeEvent(input$main_table_cell_clicked, {
    
    info <- input$main_table_cell_clicked
    
    # Check if the click was on the play-audio button
    if (is.null(info$value) || info$col != 5) return()
    
    # Retrieve the file path and start/end times for the selected row
    selected_row <- rv$data_display[info$row, ]
    filepath <- paste0(dir_path(), "/", 
                       selected_row$common_name, "_", selected_row$id, ".wav")

    
    # Play the audio file
    if (file.exists(filepath)) {
      play_audio(filepath = filepath)
    } else {
      showNotification("Audio file not found.", type = "error")
    }
  })
  
}



# deploy the Shiny App ----------------------------------------------------

shinyApp(ui = ui, server = server)
