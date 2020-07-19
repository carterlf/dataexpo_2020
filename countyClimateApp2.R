library(tidyverse)
library(shiny)
library(plotly)
library(ggthemes)
library(forcats)
library(leaflet)
library(ggpubr)
library(mathjaxr)

setwd("C:\\Users\\lfcar\\Documents\\Junior\\DataExpo 2019-2020\\summer2020")
#setwd("C:/Users/Matthew/Documents/_My Actual Documents/Miami/STA Project/7-13-2020")
load("countySums3.Rdata")
load("countySums2.Rdata")

##delete after we get updated county full processed
tmp <- countySums3 %>%
  select(county, choice) %>%
  unique()
county_full <- left_join(county_full, tmp, by = "county")

countySums3 <- countySums3 %>%
  select(year:daysUnder30, state, perc_rep) %>%
  mutate(party_status = ifelse(perc_rep > 0.5, "Trump", "Clinton"))
names(countySums3) <-
  c(
    "year",
    "county",
    "p90_total",
    "lro90_total",
    "do90_total",
    "p10_total",
    "lru30_total",
    "do30_total",
    "state",
    "perc_rep",
    "party_status"
  )

county_full <- county_full %>%
  mutate(state = gsub(".*,", "", county))

counties_map <- county_full %>%
  mutate(countyName = gsub(",.*$", "", county),
         state = gsub(".*,", "", county)) %>%
  full_join(
    map_data("county") %>% mutate(
      region = gsub(" ", "", region),
      subregion = gsub(" ", "", subregion)
    ),
    by = c("state" = "region", "countyName" = "subregion")
  )

temp_choices <- c(
  "90th Percentile (\u00B0F)" = "p90_total",
  "Number of Days Over 90\u00B0F" = "do90_total",
  "Max Consecutive Days Over 90\u00B0F" = "lro90_total",
  "10th Percentile of Temps (\u00B0F)" = "p10_total",
  "Number of Days Under 30\u00B0F" = "do30_total",
  "Max Consecutive Days Under 30\u00B0F" = "lru30_total"
)

survey_choices <-
  c(
    "% who think global warming is happening" = "happening",
    "% who think global warming is caused mostly by human activities" =
      "human",
    "% who think global warming is affecting weather in the US" =
      "affectweather",
    "% who are worried about global warming" = "worried",
    "% who think global warmig will harm people in the US" =
      "harmUS",
    "% who think global warming will harm them personally" =
      "personal"
  )

choice_colors <- c("Clinton" = "blue", "Trump" = "red")

state_choices <- unique(county_full$state)

### Define UI for application
ui <- navbarPage(
  "Changes in U.S. County Climate (1900-2019)",
  
  tabPanel("Project Background",
           titlePanel("An Analysis of Climate Change Impact on Public Opinion"),
           fluidPage(
            h4("Authors: Lydia Carter and Matthew Snyder, Faculty Advisor: Dr. Thomas Fisher"),
             h3("Research Question", align = "left"),
             p("In recent years climate change has become a key political issue and the subject of 
               frequent debate in the United States.  However, the percentage of Americans who believe 
               that climate change is a growing threat varies based on location. The goal of this application
               is to evaluate whether public perception of climate change is related to the realities of
               climate change in a given area."),
             h3("Finding Climate Index", align = "left"),
             p("We used Sen's Slope to calculate a one-number summary for each county in the United States.
               We chose Sen's Slope (formula shown below) because it is a nonparametric method that can be
               used for time series data."),
            p(withMathJax(helpText("$$median[{\\frac{x_j-x_i}{j-i}; i<j}]$$"))),
             h3("Results", align = "left"),
             h3("Data Sources and Links", align = "left"),
            p("Global Historical Climatology Network: https://www.ncdc.noaa.gov/data-access/land-based-station-data/
              land-based-datasets/global-historical-climatology-network-ghcn"),
            p("Yale Climate Opinion Maps 2019: https://climatecommunication.yale.edu/visualizations-data/ycom-us/"),
            p("U.S. Election Data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ")
           )
           ),
  
  tabPanel(
    "Map Overview",
    
    titlePanel("Compare Maps"),
    
    fluidRow(
      column(
        6,
        selectInput(
          inputId = "climate_yvar",
          label = "Select Climate Variable",
          choices = temp_choices,
          width = "90%"
        ),
        plotOutput(outputId = "climatemap", height = 600)
      ),
      column(
        6,
        selectInput(
          inputId = "survey_yvar",
          label = "Select Survey Response Variable",
          choices = survey_choices,
          width = "90%"
        ),
        plotOutput(outputId = "surveymap", height = 600)
      )
    )
  ),
  
  tabPanel(
    "County Comparison",
    titlePanel("Compare Bar Plots"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h4("Choose Climate and Survey Variables"),
        
        selectInput(
          inputId = "climate_var",
          label = "Select Climate Variable",
          choices = temp_choices
        ),
        selectInput(
          inputId = "survey_var",
          label = "Select Survey Response Variable",
          choices = survey_choices
        ),
        selectInput(
          inputId = "order",
          label = "Select Variable Type to Order By",
          choices = c("Climate Variable", "Survey Variable")
        ),
        selectInput(
          inputId = "state1",
          label = "Select State",
          choices = state_choices,
          multiple = F
        )
      ),
      
      mainPanel(plotlyOutput(outputId = "barplot"))
    )
  ),
  
  tabPanel(
    "County Detail",
    titlePanel("View Individual Counties"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "yvar",
          label = "Select Climate Variable of Interest",
          choices = temp_choices
        ),
        uiOutput(outputId = "state"),
        uiOutput(outputId = "county")
      ),
      mainPanel(plotlyOutput(outputId = "tempplot"))
    )
  )
)



### Define server behavior for application here
server <- function(input, output) {
  output$state <- renderUI({
    selectInput(
      inputId = "state",
      label = "Select State",
      choices = state_choices,
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
      label = "Select County",
      choices = county_options$county,
      multiple = F
    )
    
  })
  
  
  output$climatemap <- renderPlot({
    ggplot(counties_map) +
      geom_polygon(aes_string(
        x = "long",
        y = "lat",
        group = "group",
        fill = input$climate_yvar
      ),
      color = "black") +
      labs(fill = "Climate Variable") +
      scale_fill_gradientn(colors = c("#390099", "#edf2f4", "#d90429")) +
      coord_quickmap() +
      theme_map() +
      theme(legend.position = c(.87, 0.25))
  })
  
  output$surveymap <- renderPlot({
    ggplot(counties_map) +
      geom_polygon(aes_string(
        x = "long",
        y = "lat",
        group = "group",
        fill = input$survey_yvar
      ),
      color = "black") +
      labs(fill = "Survey Variable") +
      scale_fill_gradientn(colors = c("white", "#1b9425")) +
      coord_quickmap() +
      theme_map() +
      theme(legend.position = c(.87, 0.25))
  })
  
  
  output$barplot <- renderPlotly({
    region_subset <- county_full %>%
      filter(state %in% input$state1)
    
    
    if (input$order == "Climate Variable") {
      region_subset$county <- factor(region_subset$county,
                                     levels = unique(region_subset$county)[order(region_subset[[input$climate_var]],
                                                                                 decreasing = FALSE)])
    } else {
      region_subset$county <- factor(region_subset$county,
                                     levels = unique(region_subset$county)[order(region_subset[[input$survey_var]],
                                                                                 decreasing = FALSE)])
    }
    
    # PLOT BUILDING
    p1 <- region_subset %>%
      group_by(county) %>%
      plot_ly(
        type = 'bar',
        orientation = 'h',
        y =  ~ county,
        x = as.formula(paste0("~", input$climate_var)),
        color = ~ choice,
        showlegend = F,
        colors = choice_colors,
        hoverinfo = 'text',
        text = ~ paste0(county, "; ", round(get(
          input$climate_var
        ), 3))
      ) %>%
      layout(xaxis = list(title = names(temp_choices)[temp_choices == input$climate_var]),
             yaxis = list(title = "County"))
    
    # PLOT BUILDING
    p2 <- region_subset %>%
      group_by(county) %>%
      plot_ly(
        type = 'bar',
        orientation = 'h',
        y =  ~ county,
        x = as.formula(paste0("~", input$survey_var)),
        color = ~ choice,
        showlegend = T,
        colors = choice_colors,
        hoverinfo = 'text',
        text = ~ paste0(county, "; ", round(get(
          input$survey_var
        ), 2))
      ) %>%
      layout(
        xaxis = list(
          title = names(survey_choices)[survey_choices == input$survey_var],
          ticksuffix = "%",
          range = c(0, 100)
        ),
        yaxis = list(title = "County")
      )
    
    subplot(p1, p2, shareY = T, titleX = T) %>%
      layout(title = paste0(
        "Compare ",
        names(temp_choices)[temp_choices == input$climate_var],
        " with ",
        names(survey_choices)[survey_choices == input$survey_var]
      ))
    
  })
  
  output$tempplot <- renderPlotly({
    region_subset <- countySums3 %>%
      filter(state %in% input$state & county %in% input$county) %>%
      na.omit()
    
    # PLOT BUILDING
    region_subset %>%
      group_by(county) %>%
      plot_ly(
        type = 'scatter',
        mode = 'lines+markers',
        x = ~ year,
        y = as.formula(paste0("~", input$yvar)),
        color = ~ party_status,
        colors = choice_colors,
        hoverinfo = 'text',
        text = ~ paste0(county, "; ", round(get(
          input$climate_var
        ), 3))
      ) %>%
      layout(
        title = paste0("Change in ", names(temp_choices)[temp_choices == input$yvar], " from 1900-2019"),
        xaxis = list(title = "Year"),
        yaxis = list(title = names(temp_choices)[temp_choices == input$yvar])
      )
    
  })
  
}

### specify the ui and server objects to be combined to make App
shinyApp(ui = ui, server = server)
