library(shiny)
library(bs4Dash)
library(tidyverse)
library(brickr)
library(png)
library(shinycssloaders)
library(shinyjqui)
library(shinyBS)

ui <- dashboardPage(
  header = dashboardHeader(title = dashboardBrand("Mosaic Builder", image="https://assets.lego.com/logos/v4.5.0/brand-lego.svg")),
  sidebar = dashboardSidebar(
    actionLink("info", "What's going on here?"),
    hr(),
    fileInput("image_upload", "Upload PNG file", accept = c("image/png")),
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
  footer = dashboardFooter(
    right=a("Copyright Jonathan Eller, 2024",
            href = "https://github.com/jawneller"
    )
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
  
  observeEvent(input$info, {
    showModal(modalDialog(
      id = "infoModal", # ID of the modal
      title = "Merry Christmas Randall!!",
      img(
        src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Frandalleller.com%2Fwp-content%2Fuploads%2F2016%2F05%2Fcorpbanner-e1466137029610.jpg&f=1&nofb=1&ipt=115122eaeaec07e7a5f8a2f48d3d9698758b6053aa20a7b69da6d53c13daeef1&ipo=images",
        align="right",
        width="250px"
      ),
      p("Now you can create your own Lego mosaics from your own images using this app."),
      p("Upload a png image file, adjust the mosaic size, and click 'Create Mosaic'."),
      tags$ul(
        tags$li("Build Image: Shows the mosaic as it would be built."),
        tags$li("Piece Table: A table listing the number of each type of brick needed."),
        tags$li("Piece List: A visual representation of the pieces needed."),
        tags$li("Instructions (Plot): A visual instruction guide for building the mosaic."),
        tags$li("Uploaded Image: The original uploaded image.")
      ),
      p("There are quite a few options to play around with the coloring, resolution,
        and which bricks are used for the mosaic. See the Advanced Options for these."),
      p("This app uses the 'brickr' package."),
      tags$a(href="https://github.com/koen-hufkens/brickr", "brickr github page"),
    ))
  })


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