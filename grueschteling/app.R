library(shiny)
library(bs4Dash)
library(tidyverse)
library(brickr)
library(png)
library(shinycssloaders)

ui <- dashboardPage(
  header = dashboardHeader(title = "Lego Mosaic Builder"),
  sidebar = dashboardSidebar(
    fileInput("image_upload", "Upload Image", accept = c("image/png", "image/jpeg")),
    sliderInput("image_scaling", "Scale Image (%)", value=0.5, min = 0, max = 1, step = 0.05),
    numericInput("mosaic_size", "Mosaic Size (Bricks)", value = 36, min = 1),
    actionButton("create_mosaic", "Create Mosaic"),
  ),
  body = dashboardBody(
    fluidRow(
    ),
    fluidRow(
      valueBoxOutput("total_pieces_box", width = 4),
      valueBoxOutput("size_in_box", width = 4),
      valueBoxOutput("size_ft_box", width = 4)
    ),
    fluidRow(
      bs4Card(title = "Uploaded Image", status = "info", width = 6, collapsible = TRUE, imageOutput("uploaded_image")), # Collapsible Card
      box(title = "Build Image", status = "success", width = 6, plotOutput("build_plot") %>% withSpinner(color="#0dcaf0")),
    ),
    fluidRow(
      box(title = "Piece List", status = "primary", width = 6, plotOutput("piece_list") %>% withSpinner(color="#0dcaf0")),
      box(title = "Instructions", status = "primary", width = 6, plotOutput("instructions") %>% withSpinner(color="#0dcaf0"))
    ),
    fluidRow(
      box(title = "Piece Table", status = "warning", width = 6, tableOutput("piece_table") %>% withSpinner(color="#0dcaf0"))
    )
  ),
  controlbar = dashboardControlbar(),
  title = "Dashboard"
)

server <- function(input, output) {
  
  output$uploaded_image <- renderImage({
    req(input$image_upload)
    list(src = input$image_upload$datapath,
         alt = "Uploaded Image",
         width = paste0(input$image_scaling*100,"%")# "25%")
    )
  }, deleteFile = FALSE)
  
  mosaic_data <- eventReactive(input$create_mosaic, {
    # Ensure image is uploaded and size input is there
    req(input$image_upload, input$mosaic_size)
    
    image <- png::readPNG(input$image_upload$datapath)
    
    # run the brickr mosaic function
    mosaic <- image %>%
      brickr::image_to_mosaic(img_size = input$mosaic_size)
      list(mosaic = mosaic)
  })

  output$build_plot <- renderPlot({
    req(mosaic_data()$mosaic)
    mosaic_data()$mosaic %>% build_mosaic() %>% plot()
  })
  output$piece_table <- renderTable({
    req(mosaic_data()$mosaic)
    mosaic_data()$mosaic %>% build_pieces_table()
  })

  
  output$instructions <- renderPlot({
    req(mosaic_data()$mosaic)
    mosaic_data()$mosaic %>% build_instructions(9) %>% plot()
  })
  
  output$piece_list <- renderPlot({
    req(mosaic_data()$mosaic)
    mosaic_data()$mosaic %>% build_pieces() %>% plot()
  })
  
  output$piece_summary_table <- renderTable({
    req(mosaic_data()$mosaic)
    mosaic_data()$mosaic %>% build_pieces_table() %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
  })

  output$total_pieces_box <- renderValueBox({
    req(mosaic_data()$mosaic)
    tbl <- mosaic_data()$mosaic %>% build_pieces_table()
    tbl_piece_summary <- tbl %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
    total_pieces <- rowSums(tbl_piece_summary)
    valueBox(
      value = total_pieces,
      subtitle = "Total Pieces",
      icon = icon("cubes"),
      color = "primary"
    )
  })
  output$size_in_box <- renderValueBox({
    req(mosaic_data()$mosaic)
    size <- input$mosaic_size
    size_in <- size * 8 / 10 / 2.54 # 8 mm between lego studs
    valueBox(
      value = paste0(round(size_in,2)," in"),
      subtitle = "Mosaic Size (Inches)",
      icon = icon("ruler-horizontal"),
      color = "success"
    )
  })
  output$size_ft_box <- renderValueBox({
    req(mosaic_data()$mosaic)
    size <- input$mosaic_size
    size_in <- size * 8 / 10 / 2.54 # 8 mm between lego studs
    size_ft <- size_in / 12
    valueBox(
      value = paste0(round(size_ft,2)," ft"),
      subtitle = "Mosaic Size (Feet)",
      icon = icon("ruler-horizontal"),
      color = "warning"
    )
  })
}

shinyApp(ui = ui, server = server)