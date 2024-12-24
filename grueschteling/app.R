library(shiny)
library(bs4Dash)
library(tidyverse)
library(brickr)
library(png)
library(shinycssloaders)
library(shinyjqui)

ui <- dashboardPage(
  header = dashboardHeader(title = "Lego Mosaic Builder"),
  sidebar = dashboardSidebar(
    fileInput("image_upload", "Upload Image", accept = c("image/png", "image/jpeg")),
    hr(h4("Create Mosaic")),
    sliderInput("image_scaling", "Scale Image (%)", value=0.5, min = 0, max = 1, step = 0.05),
    numericInput("mosaic_size", "Mosaic Size (Bricks)", value = 36, min = 1),
    actionButton("create_mosaic", "Create Mosaic"),
    hr(h4("Advanced Options")),
    sliderInput("brightness", "Brightness", min=0, max=3, value=1, step=0.1),
    selectInput("color_palette", "Color Palette", selected=c("universal", "generic", "special"), choices=c("universal", "generic", "special"), multiple=TRUE),
    selectInput("use_bricks", "Bricks to Use", selected=c('4x2', '2x2', '3x1', '2x1', '1x1'), choices=c('4x2', '2x2', '3x1', '2x1', '1x1'), multiple=TRUE),
    orderInput("warhol", "Warhol Effect", items=c(1,2,3)),
    
    magnified = FALSE
  ),

# body --------------------------------------------------------------------
  
  body = dashboardBody(
    fluidRow(
      valueBoxOutput("total_pieces_box", width = 4),
      valueBoxOutput("size_in_box", width = 4),
      valueBoxOutput("price_estimate", width = 4)
    ),
    fluidRow(
      bs4Card(title = "Uploaded Image", status = "success", width = 6, collapsible = TRUE, imageOutput("uploaded_image")), # Collapsible Card
      box(title = "Build Image", status = "success", width = 6, plotOutput("build_plot") %>% withSpinner(color="#0dcaf0")),
    ),
    fluidRow(
      box(title = "Piece List", status = "primary", width = 6, plotOutput("piece_list") %>% withSpinner(color="#0dcaf0")),
      box(title = "Instructions", status = "primary", width = 6, plotOutput("instructions") %>% withSpinner(color="#0dcaf0"))
    ),
    fluidRow(
      box(title = "Piece Table", status = "warning", width = 6, tableOutput("piece_table") %>% withSpinner(color="#0dcaf0")),
      box(title = "Price Estimate", status = "warning", width = 6, tableOutput("piece_summary_table") %>% withSpinner(color="#0dcaf0"))
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
  

# reactives ---------------------------------------------------------------

  mosaic_data <- eventReactive(input$create_mosaic, {
    # Ensure image is uploaded and size input is there
    req(input$image_upload, input$mosaic_size)
    
    image <- png::readPNG(input$image_upload$datapath)
    
    # run the brickr mosaic function
    mosaic <- image %>%
      brickr::image_to_mosaic(
        img_size = input$mosaic_size,
        brightness=input$brightness,
        color_palette=input$color_palette,
        use_bricks=input$use_bricks,
        warhol=input$warhol
      )
      list(mosaic = mosaic)
  })
  
  pieces_table <- reactive({
    mosaic_data()$mosaic %>%
      build_pieces_table()
  })
  
  piece_summary <- reactive({
    
    # total pieces by shape
    pieces_table <- pieces_table() %>%
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
    
    # transpose total by shape
    piece_table_summary <- as_tibble(cbind(shape = names(pieces_table), t(pieces_table))) %>%
      rename(count="V2")
    
    # join totals by shape with unit prices and add row-wise subtotal
    piece_summary <- tibble(
        shape = c("1 x 1", "2 x 1", "2 x 2", "3 x 1", "4 x 1", "4 x 2"),
        # prices estimated from build-a-brick website on 12/23/24
        price = c(0.05, 0.07, 0.09, 0.07, 0.1, 0.14)
      ) %>%
      left_join(piece_table_summary) %>%
      mutate(subtotal = price * as.numeric(count)) %>%
      drop_na(count)

  })


# outputs -----------------------------------------------------------------

  output$build_plot <- renderPlot({
    req(mosaic_data()$mosaic)
    mosaic_data()$mosaic %>% build_mosaic() %>% plot()
  })
  
  output$piece_table <- renderTable({
    req(mosaic_data()$mosaic)
    pieces_table()
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
    piece_summary()
  })

  output$total_pieces_box <- renderValueBox({
    req(mosaic_data()$mosaic)

    total_pieces <- sum(as.numeric(piece_summary()$count), na.rm=TRUE)
    valueBox(
      value = formatC(total_pieces, big.mark = ","),
      subtitle = "Total Pieces",
      icon = icon("cubes"),
      color = "primary"
    )
  })
  output$size_in_box <- renderValueBox({
    req(mosaic_data()$mosaic)

    size_in <- round(input$mosaic_size * 8 / 10 / 2.54, 1) # 8 mm between lego studs
    size_ft <- round(size_ft <- size_in / 12, 1)
    valueBox(
      value = paste0(size_in," x ", size_in, " in. (", size_ft, " x ", size_ft, " ft.)"),
      subtitle = "Approximate Physical Dimensions",
      icon = icon("ruler-horizontal"),
      color = "purple"
    )
  })
  output$price_estimate <- renderValueBox({
    req(mosaic_data()$mosaic)
    
    cost <- sum(piece_summary()$subtotal, na.rm=TRUE)
    valueBox(
      value = paste0("$", cost),
      subtitle = tags$a(
        href = "https://www.lego.com/en-us/pick-and-build/pick-a-brick",
        "Estimated Cost from Lego's Pick-a-Brick Website",
        style = "color: white;"
      ),
      icon = icon("dollar-sign"),
      color = "success"
      # subtitle = "Estimated Cost from Lego's Pick-a-Brick Website",
      # icon = icon("ruler-horizontal"),
      # color = "success"
    )
  })
}

shinyApp(ui = ui, server = server)