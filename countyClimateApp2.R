library(tidyverse)
library(shiny)
library(plotly)
library(ggthemes)
library(forcats)
library(leaflet)

setwd("C:\\Users\\lfcar\\Documents\\Junior\\DataExpo 2019-2020\\summer2020")
load("countySums.Rdata")
load("countySums2.Rdata")

countySums1 <- countySums1 %>%
  select(year:daysUnder30, climateRegion, binary_status)
names(countySums1) <- c("year","county","p90_total","lro90_total","do90_total",
                        "p30_total","lro30_total","du30_total",'climateRegion',"binary_status")

county_full <- county_full %>%
  mutate(state = gsub(".*,", "", county))

counties <- tigris::counties()
counties_map <- sp::merge(counties, county_full, by.x = "GEOID", by.y = "geoid")

temp_choices <- c("90th Percentile of Temps"="p90_total",
                  "Number of Days Over 90 Degrees F"="do90_total",
                  "Max Consecutive Days Over 90 Degrees F"="lro90_total",
                  "10th Percentile of Temps"="p10_total",
                  "Number of Days Under 30 Degrees F"="do30_total",
                  "Max Consecutive Days Under 30 Degrees F"="llru30_total")

survey_choices <- c("% who think global warming is happening"="happening",
                    "% who think global warming is caused mostly by human activities"="human",
                     "% who think global warming is affecting weather in the US"="affectweather",
                    "% who are worried about global warming"="worried",
                     "% who think global warmig will harm people in the US"="harmUS",
                     "% who think global warming will harm them personally"="personal")

### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "Changes in U.S. County Climate (1970-2019)"),
  
  sidebarLayout(
    
    # Sidebar typically used to house input controls
    sidebarPanel(
      
      ##this conditional panel is for map tab (tab 1)
      conditionalPanel(tags$h4("Choose Mapping Variable"),
                       condition = "input.tabs == 'Map Overview' ",
                       
                       selectInput(
                         inputId = "map_type",
                         label = "Select Map Type",
                         choices = c("Climate Variables","Survey Variables", "Urban/Rural")
                       ),
                       
                       uiOutput(outputId = "yvar_choices"),
                       
                       actionButton(inputId = "make_map",
                                    label = "Build Map")
      ),
      
      ##this conditional panel is for county comparison/detail tabs (tab 2&3)
      conditionalPanel(tags$h4("Choose Climate and Survey Variables"),
                       condition = "input.tabs != 'Map Overview' ",
                       
                         selectInput(
                           inputId = "climate_var",
                           label = "Select Climate Variable of Interest",
                           choices = temp_choices
                         ),
                         selectInput(
                           inputId = "survey_var",
                           label = "Select Survey Response Variable",
                           choices = survey_choices
                         ),
                       #the state drop down is rendered as a UI in the server
                       uiOutput(outputId = "state")
                       
      ),
      conditionalPanel(condition = "input.tabs == 'County Detail' ",
                       
                       #the county drop down is rendered as a UI in the server
                       uiOutput(outputId = "county") 
                       
      )
    ),
    
    # Main panel typically used to display outputs
    mainPanel(
      
      tabsetPanel(id = "tabs",
                  tabPanel("Map Overview", leafletOutput(outputId="mapplot")),
                  tabPanel("County Comparison", plotlyOutput(outputId="barplot")),
                  tabPanel("County Detail", plotlyOutput(outputId="tempplot"))
      )
    )
  )
)

### Define server behavior for application here
server <- function(input, output) {
  
  output$yvar_choices <- renderUI({
    
    if(input$map_type == "Climate Variables") {
      selectInput(
        inputId = "yvar",
        label = "Select Climate Variable of Interest",
        choices = temp_choices
      )
    }
    else if(input$map_type == "Survey Variables") {
      selectInput(
        inputId = "yvar",
        label = "Select Survey Response Variable",
        choices = survey_choices
      )
    }
    else {
      selectInput(
        inputId = "yvar",
        label = "Select Urban/Rural Variable",
        choices = c("Percent Urban" = "percent_urban",
                    "Urban Population"="urban_pop")
      )
    }
  })
  
  output$state <- renderUI({
    
    selectInput(
      inputId = "state",
      label = "Select Specific States",
      choices = unique(county_full$state),
      multiple = F
    )
    
  })
  
  output$county <- renderUI({
    
    county_options <- county_full %>%
      filter(state %in% input$state) %>%
      select(county) %>%
      unique()
    
      selectInput(
        inputId = "county",
        label = "Select Specific Counties",
        choices = county_options$county,
        multiple = F)
  
  })
  
  new_map <- eventReactive(input$make_map,{
    
    pal <- colorNumeric(palette="Blues", domain = counties_map[[input$yvar]])
    palData <-counties_map[[input$yvar]]
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g percent urban",
      counties_map$county, counties_map$percent_urban
    ) %>% lapply(htmltools::HTML)
    
    leaflet(counties_map) %>%
      setView(-96, 37.8, 4) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(palData),
        weight = 0.5,
        opacity = 1,
        color = "white",
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~palData, opacity = 0.7, title = NULL,
                position = "bottomright")
    
  })
  
  output$mapplot <- renderLeaflet({ 
    
   new_map()
    
  })
  
  
  output$barplot <- renderPlotly({ 
    
    region_subset <- county_full %>%
      filter(state %in% input$state) 
    
    region_subset$county <- factor(region_subset$county, 
                                   levels = unique(region_subset$county)[order(region_subset[[input$climate_var]],
                                                                               decreasing = FALSE)])
    # PLOT BUILDING
    p1 <- region_subset %>%
      group_by(county) %>%
      plot_ly(
        type = 'bar',
        orientation = 'h',
        y=~county,
        x = as.formula(paste0("~",input$climate_var)),
       #x = ~p90_total,
        color= ~binary_status,
        #colors=region_colors,
        hoverinfo = 'text',
        text = ~paste0(county,": ",round(p90_total,2))) 
    
    # PLOT BUILDING
    p2 <- region_subset %>%
      group_by(county) %>%
      plot_ly(
        type = 'bar',
        orientation = 'h',
        y=~county,
        x = as.formula(paste0("~",input$survey_var)),
        #x = ~p90_total,
        color= ~binary_status,
        #colors=region_colors,
        hoverinfo = 'text',
        text = ~paste0(county,": ",round(p90_total,2))) 
    
    subplot(p1,p2)
      
 
    # p1 <- region_subset %>%
    #   mutate(county = fct_reorder(county, get(input$climate_var))) %>%
    #   ggplot() + 
    #   geom_bar(aes_string(x="county", 
    #                       y=input$climate_var,
    #                       fill="binary_status"), 
    #            stat = "identity") + 
    #   coord_flip() +
    #   theme_minimal() +
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    # 
    # 
    # p2 <- region_subset %>%
    #   mutate(county = fct_reorder(county, get(input$climate_var))) %>%
    #   ggplot() + 
    #   geom_bar(aes_string(x="county", 
    #                       y=input$survey_var,
    #                       fill="binary_status"), 
    #            stat = "identity") + 
    #   coord_flip() +
    #   theme_minimal() +
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    # 
    # gridExtra::grid.arrange(p1,p2, ncol=1)
  })
  
  output$tempplot <- renderPlotly({ 
    
    region_subset <- countySums1 %>%
      filter(county %in% input$county) %>%
      na.omit()
    
    # PLOT BUILDING
    region_subset %>%
      group_by(county) %>%
      plot_ly(
        type = 'scatter', mode = 'lines+markers',
        x = ~year,
        y = as.formula(paste0("~",input$yvar)),
        color= ~binary_status,
        #colors=region_colors,
        hoverinfo = 'text',
        text = ~paste0(county,", ",input$yvar)) %>%
      layout(title = paste0("Change in ",names(temp_choices)[temp_choices==input$yvar]," from 1970-2019"),
             xaxis=list(title="Year"))
    
  })
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

