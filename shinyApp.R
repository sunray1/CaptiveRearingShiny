library(shiny)
library(ggplot2)
library(dplyr)
library(raster)

# Load data from github
#data_url <- "https://raw.githubusercontent.com/yourusername/yourrepository/master/yourdata.csv"
data_url <- "all_filtered_cleaned.csv"
df <- read.csv(data_url)

css <- "
#reverseSlider .irs-bar {
    border-top: 1px solid #ddd;
    border-bottom: 1px solid #ddd;
    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
}
#reverseSlider .irs-bar-edge {
    border: 1px solid #ddd;
    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
    border-right: 0;
}
#reverseSlider .irs-line {
    background: #428bca;
    border: 1px solid #428bca;
}
"


# Convert date column to proper format
df$eventdate <- as.Date(df$eventdate, format = "%Y-%m-%d")


ui <- fluidPage(
  titlePanel("Species Locations"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "species",
                  label = "Choose a Species",
                  choices = sort(unique(df$cleanedname))),
      sliderInput(inputId = "uncertainty",
                  label = "Uncertainty Cutoff",
                  min = min(df$coordinateUncertaintyInMeters, na.rm = TRUE), max = max(df$coordinateUncertaintyInMeters, na.rm = TRUE), value = max(df$coordinateUncertaintyInMeters, na.rm = TRUE)),
      tags$style(type='text/css', css),
      div(id = "reverseSlider",
        sliderInput(inputId = "date",
                    label = "Date Cutoff",
                    min = min(df$eventdate, na.rm = TRUE), max = max(df$eventdate, na.rm = TRUE), value = min(df$eventdate, na.rm = TRUE))
      ),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      plotOutput(outputId = "map"),
    )
  )
)

server <- function(input, output) {

  
  # Filter data based on user input
  filtered_data <- reactive({
    if (input$date == min(df$eventdate, na.rm = TRUE) && input$uncertainty == max(df$coordinateUncertaintyInMeters, na.rm = TRUE)) {
      df %>% 
        filter(cleanedname == input$species)
    } else if (input$date != min(df$eventdate, na.rm = TRUE) && input$uncertainty == max(df$coordinateUncertaintyInMeters, na.rm = TRUE)) {
      df %>% 
        filter(cleanedname == input$species,
               eventdate >= input$date)
    } else if (input$date == min(df$eventdate, na.rm = TRUE) && input$uncertainty != max(df$coordinateUncertaintyInMeters, na.rm = TRUE)) {
      df %>% 
        filter(cleanedname == input$species,
               coordinateUncertaintyInMeters <= input$uncertainty)
    } else {
        df %>% 
          filter(cleanedname == input$species,
                 coordinateUncertaintyInMeters <= input$uncertainty,
                 eventdate >= input$date)  
    }
  })
  
  # Create map
  output$map <- renderPlot({
    # Plot filtered data on shapefile
    sat_map <- get_map(location = c(min(filtered_data()$decimallongitude)-.05, min(filtered_data()$decimalLatitude)-.05, max(filtered_data()$decimallongitude)+.05, max(filtered_data()$decimalLatitude)+.05), 
                       maptype = c("satellite"))
    ggmap(sat_map, extent="device", legend="none") + 
      geom_point(data=filtered_data(),  aes(x=decimallongitude, y=decimalLatitude), fill="red", shape=23, alpha=0.8)
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", input$species, "-", input$uncertainty, "meters-", input$date, ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)


