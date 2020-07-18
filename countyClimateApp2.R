library(tidyverse)
library(shiny)
library(plotly)
library(ggthemes)
library(forcats)
library(leaflet)
library(ggpubr)

#setwd("C:\\Users\\lfcar\\Documents\\Junior\\DataExpo 2019-2020\\summer2020")
setwd("C:/Users/Matthew/Documents/_My Actual Documents/Miami/STA Project/7-13-2020")
load("countySums.Rdata")
load("countySums2.Rdata")

countySums1 <- countySums1 %>%
  select(year:daysUnder30, climateRegion, binary_status)
names(countySums1) <- c("year","county","p90_total","lro90_total","do90_total",
                        "p30_total","lro30_total","du30_total",'climateRegion',"binary_status")

county_full <- county_full %>%
  mutate(state = gsub(".*,", "", county))

#counties <- tigris::counties()
#counties_map <- sp::merge(counties, county_full, by.x = "GEOID", by.y = "geoid")
counties_map <- county_full %>% 
  mutate(countyName = gsub(",.*$", "", county),
         state = gsub(".*,", "", county)) %>% 
  full_join(map_data("county") %>% mutate(region = gsub(" ", "", region),
                                          subregion = gsub(" ", "", subregion)), by=c("state"="region", "countyName"="subregion"))

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
ui <- navbarPage("Changes in U.S. County Climate (1970-2019)",
  
  tabPanel("Map Overview",
    fluidRow(
      column(6,
             plotOutput(outputId="climatemap", height = 600),
             
             selectInput(inputId = "climate_yvar",
                         label = "Select Climate Variable of Interest",
                         choices = temp_choices,
                         width="90%")),
      column(6, 
             plotOutput(outputId="surveymap", height = 600),
             
             selectInput(inputId = "survey_yvar",
                      label = "Select Survey Response Variable",
                      choices = survey_choices,
                      width="90%"))
    )         
  ),
  
  tabPanel("County Comparison",
    sidebarLayout(
      sidebarPanel(width=3, 
        tags$h4("Choose Climate and Survey Variables"),

        selectInput(inputId = "climate_var",
                    label = "Select Climate Variable of Interest",
                    choices = temp_choices),
        selectInput(inputId = "survey_var",
                    label = "Select Survey Response Variable",
                    choices = survey_choices),
        #the state drop down is rendered as a UI in the server
        uiOutput(outputId = "state")),
      
      mainPanel(plotlyOutput(outputId="barplot"))
    )
  ),
  
  tabPanel("County Detail",
    sidebarLayout(
      sidebarPanel(uiOutput(outputId = "county")),
      
      mainPanel(plotlyOutput(outputId="tempplot"))
    )
  )
)
    


### Define server behavior for application here
server <- function(input, output) {
  
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
  
  
  output$climatemap <- renderPlot({ 
    
    ggplot(counties_map) +
      geom_polygon(aes_string(x="long", y="lat", group="group", fill=input$climate_yvar), color="black") + 
      scale_fill_gradientn(colors=c("#390099", "#edf2f4", "#d90429")) +
      coord_quickmap() +
      theme_map() +
      theme(legend.position=c(.87, 0.25))
  })
  
  output$surveymap <- renderPlot({ 
    ggplot(counties_map) +
      geom_polygon(aes_string(x="long", y="lat", group="group", fill=input$survey_yvar), color="black") + 
      scale_fill_gradientn(colors=c("#390099", "#edf2f4", "#d90429")) +
      coord_quickmap() +
      theme_map() +
      theme(legend.position=c(.87, 0.25))
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

