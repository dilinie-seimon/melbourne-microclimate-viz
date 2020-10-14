library(readr)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(lubridate)
library(plotly)
library(leaflet)

sensor_locations <- read_csv(here::here("Data/Microclimate_Sensor_Locations.csv"))
sensor_readings <- read_csv(here::here("Data/Microclimate_Sensor_Readings.csv"))


# converting date time from char to dttm format
sensor_readings <- sensor_readings %>%
  mutate(date_time = parse_date_time(local_time,'%Y %m %d %I:%M:%S %p')) %>%
  select(-local_time) %>%
  left_join(sensor_locations, by = c("site_id"="site_id"))
  

#extracting only quarter hourly readings
quarter_hour_readings <- sensor_readings %>%
  filter(!grepl("EPA", sensor_id)) %>%
  select(-id, -units, -type) %>%
  pivot_wider(names_from = sensor_id,
              values_from = value) %>%
  unnest(c("5a", "5b", "5c", "6", "0a", "0b")) %>%
  rename("ambient_air_temperature" ="5a",
         "relative_humidy" = "5b",
         "barometric_pressure" = "5c",
         "particulate_density_2.5" = "0a",
         "particulate_density_10" = "0b",
         "average_wind_speed" = "6")

hourly_avg <- sensor_readings %>%
  filter(!grepl("EPA", sensor_id)) %>%
  mutate(datetime = parse_date_time(format(date_time, "%Y-%m-%d %H:00"),'%Y %m %d %H:%M')) %>%
  select(-id, -units, -type) %>%
  group_by(site_id, sensor_id, datetime, description) %>%
  summarise(hourly_avg = mean(value))


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    leafletOutput("carltonmap"),
    selectInput("sensor","Select Sensor",
                sensor_locations$description, selected = NULL, multiple = TRUE),
    plotlyOutput("trendplot")#, hover = "trendplot_hover")
    )
)
    
  
server <- function(input, output, session) {
  
  output$carltonmap <- renderLeaflet({
    leaflet(sensor_locations) %>%
      addTiles() %>%
      addCircleMarkers(
        layerId = ~description,
        label = ~site_id)
  })
  
  # observeEvent(input$carltonmap_marker_click, {
  #   click <- input$carltonmap_marker_click
  #   print(click)
  #   updateSelectInput(session, "sensor", selected = c(input$sensor, click$id))
  # })
  
  # observeEvent(input$trendplot_hover, {
  #   print(input$trendplot_hover)
  # })
  
  output$trendplot <- renderPlotly ({
    ggplotly(
      hourly_avg %>%
        #filter(description %in% input$sensor) %>%
        ggplot(aes(x = datetime, y = hourly_avg, color = site_id)) +
        geom_line() +
        facet_wrap(~sensor_id, scales = "free", ncol = 1) +
        theme_minimal()
    )
  })
  
  }

shinyApp(ui = ui, server = server)