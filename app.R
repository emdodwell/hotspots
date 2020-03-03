library(shiny)
library(data.table)
library(absmapsdata)
library(ggplot2)
library(sf)
library(plotly)

data <- readRDS("data/data_20200101_20200110_test.RDS")
focus <- data[date(h) == ymd("2020-01-01")]

focus[, cluster := as.factor(cluster)]
str(data)
states <- absmapsdata::state2016 %>% 
  filter(state_code_2016 %in% c("2"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Victoria Hotspots Prototype: 2020-01-01"),
  
  
  fluidRow(
    column(width = 6,
      plotlyOutput(outputId = "vicPlot")
    )
  ),
  
  fluidRow(
    column(width = 12,
      plotlyOutput(outputId = "firepowerPlot")
    )
  )
)

server <- function(input, output) {
  
  sd <- highlight_key(focus, ~cluster)
  
  output$vicPlot <- renderPlotly({
    
    p <- ggplot(states) +
      geom_sf() + 
      geom_point(aes(x = lon, y = lat, color = cluster, frame = ind), shape = 4, sd) # frame = ind in aes
    
    ggplotly(p) %>% highlight("plotly_selected", off = "plotly_doubleclick") %>% 
      animation_opts(transition = 0)

  })
  
  output$firepowerPlot <- renderPlotly({
    q <- ggplot(sd, aes(x = ind, y = firepower, group = cluster)) +
        geom_line(aes(color = cluster)) + geom_point(aes(color = cluster))
   
    ggplotly(q) %>% highlight("plotly_selected")
   
  })
  
}

shinyApp(ui = ui, server = server)
