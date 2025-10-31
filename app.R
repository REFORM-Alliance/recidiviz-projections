rm(list = ls())

#### Read in Libraries ####
library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(DBI)
library(RPostgres)
library(dbplyr)
library(scales)
library(shiny)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(sf)
library(tigris)
library(bslib)
library(RColorBrewer)
library(plotly)
library(readr)


####Read in Data####
##Raw Recidiviz Data
recidiviz_df <- 
  "data-raw" %>% 
  here("recidiviz-projections-total.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(across(c("life_years", "people_impacted", 
                  starts_with("lower_ci"), 
                  starts_with("upper_ci")), 
                ~.x %>% 
                  str_replace_all(",", "") %>% 
                  as.numeric())) %>% 
  mutate(across(where(is.character),
                ~.x %>% 
                  na_if(""))) %>%
  rename("lower_ci_life_years" = "lower_ci", 
         "upper_ci_life_years" = "upper_ci",
         "lower_ci_people_impacted" = "lower_ci_2",
         "upper_ci_people_impacted" = "upper_ci_2") %>% 
  pivot_longer(cols = c(starts_with("lower_ci"), 
                        starts_with("upper_ci"), 
                        "life_years", "people_impacted"), 
               names_to = "names", 
               values_to = "values") %>% 
  mutate(ci_flag = 
           names %>% 
           str_extract("lower_ci|upper_ci") %>% 
           replace_na("estimate"), 
         names = 
           names %>% 
           str_extract("life_years|people_impacted")) %>% 
  pivot_wider(names_from = ci_flag, 
              values_from = values) %>% 
  rename("estimate_type" = "names") %>% 
  relocate("estimate", .before = "lower_ci") %>% 
  filter(is.na(estimate_time_frame) == FALSE) %>% 
  mutate(estimate_time_frame = 
           estimate_time_frame %>% 
           str_replace_all("ten years", "10-Year"))

##States Spatial DF
states_sf <- 
  states(cb = TRUE) %>% 
  st_transform(4326) %>% 
  clean_names() %>%
  rename("state" = "name")

##My Themes
my_theme <- 
  bs_theme(version = 4, 
           bg = "#2C2C2C", 
           fg = "#F5F5F5", 
           primary = "#4A90E2", 
           secondary = "#7FB3D5")


####App####
##UI
ui <- navbarPage("Recidiviz Impact Projections Dashboard", theme = my_theme,
                 
                 tabPanel("Main Dashboard",
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              selectInput("estimate_time_frame", "Projection Timeframe", 
                                          choices = unique(recidiviz_df$estimate_time_frame), 
                                          selected = "September 2025", multiple = FALSE),
                              selectInput("impact_type", "Reform Type",
                                          choices = unique(recidiviz_df$impact_type), 
                                          selected = NULL, multiple = TRUE),
                              selectInput("state", "State",
                                          choices = unique(recidiviz_df$state), multiple = TRUE, selected = NULL),
                              selectInput("bill_name", "Bill",
                                          choices = unique(recidiviz_df$bill_name), multiple = TRUE),
                              selectInput("estimate_type", "Estimate Type", 
                                          choices = c("Life Years" = "life_years", "People Impacted" = "people_impacted"),
                                          selected = "people_impacted")
                            ),
                            mainPanel(
                              width = 10,
                              fluidRow(
                                box(width = 12, leafletOutput("main_map", height = 600))
                              ),
                              fluidRow(
                                box(width = 12, DTOutput("main_table"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Custom Visualization",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              selectInput("custom_chart_type", "Chart Type",
                                          choices = c("Bar Chart" = "bar",
                                                      "Line Chart" = "line",
                                                      "Scatterplot" = "scatter",
                                                      "Boxplot" = "boxplot")),
                              
                              selectInput("custom_group", "Group By",
                                          choices = c("State" = "state", 
                                                      "Reform Type" = "impact_type", 
                                                      "Projection Timeframe" = "estimate_time_frame"),
                                          selected = "state"),
                              selectInput("custom_facet", "Facet By (max 2)",
                                          choices = c("State" = "state", 
                                                      "Reform Type" = "impact_type", 
                                                      "Projection Timeframe" = "estimate_time_frame"),
                                          multiple = TRUE,
                                          selectize = TRUE),
                              
                              selectInput("custom_projection_time", "Projection Time Frame(s)",
                                          choices = unique(recidiviz_df$estimate_time_frame),
                                          multiple = TRUE,
                                          selected = unique(recidiviz_df$estimate_time_frame)),
                              
                              selectInput("custom_state", "State",
                                          choices = unique(recidiviz_df$state),
                                          multiple = TRUE),
                              selectInput("custom_bill_name", "Bill",
                                          choices = unique(recidiviz_df$bill_name),
                                          multiple = TRUE),
                              selectInput("custom_impact_type", "Reform Type",
                                          choices = unique(recidiviz_df$impact_type),
                                          multiple = TRUE),
                              
                              actionButton("generate_plot", "Generate Plot"),
                              downloadButton("download_custom_plot", "Download Plot"),
                              downloadButton("download_custom_data", "Download Data")
                            ),
                            mainPanel(
                              width = 9,
                              plotOutput("custom_plot", height = 600),
                              DTOutput("custom_table")
                            )
                          )
                 )
)

##Server
server <- function(input, output, session){
  
  observeEvent(input$state, {
    bills_df <- recidiviz_df %>%
      filter(is.null(input$state) | state %in% input$state) %>%
      pull(bill_name) %>% unique() %>% sort()
    updateSelectInput(session, "bill_name", choices = bills_df, selected = NULL)
  })
  
  filtered_df <- reactive({
    df <- recidiviz_df
    if(!is.null(input$state) && length(input$state) > 0) df <- df %>% filter(state %in% input$state)
    if(!is.null(input$bill_name) && length(input$bill_name) > 0) df <- df %>% filter(bill_name %in% input$bill_name)
    if(!is.null(input$estimate_type) && length(input$estimate_type) > 0) df <- df %>% filter(estimate_type %in% input$estimate_type)
    if(!is.null(input$impact_type) && length(input$impact_type) > 0) df <- df %>% filter(impact_type %in% input$impact_type)
    if(!is.null(input$estimate_time_frame) && length(input$estimate_time_frame) > 0) df <- df %>% filter(estimate_time_frame %in% input$estimate_time_frame)
    df
  })
  
  output$main_map <- renderLeaflet({
    df <- filtered_df()
    req(nrow(df) > 0)
    
    map_df <- df %>%
      group_by(state, estimate_type) %>%
      filter(estimate_type %in% input$estimate_type) %>% 
      summarise(estimate = sum(estimate, na.rm = TRUE)) %>%
      ungroup()
    
    states_map <- states_sf %>%
      left_join(map_df, by = c("state"))
    
    pal <- colorNumeric(palette = "Blues", domain = states_map$estimate, na.color = "transparent")
    
    leaflet(states_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98, lat = 39, zoom = 4) %>%   
      addPolygons(
        fillColor = ~pal(estimate),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~paste0(state, ": ", scales::comma(estimate)),
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = pal, values = ~estimate, position = "bottomright",
                title = input$estimate_type %>% str_replace_all("_", " ") %>% str_to_title())
  })
  
  output$main_table <- renderDT({
    df <- 
      filtered_df() %>% 
      dplyr::select(-c(modelability_score)) %>% 
      mutate(
        estimate_type = estimate_type %>% 
          str_replace_all("people_impacted", "People Impacted") %>% 
          str_replace_all("life_years", "Life Years"), 
        across(c("estimate", ends_with("_ci")), ~comma(.x))
      ) %>% 
      rename_with(~.x %>% 
                    str_replace_all("_", " ") %>% 
                    str_to_title() %>% 
                    str_replace_all("Ci$", "CI"))
    
    datatable(
      df, 
      extensions = 'Buttons', 
      options = list(
        scrollX = TRUE,         
        scrollY = "500px",      
        paging = FALSE,         
        dom = 'Bfrtip',         
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  custom_filtered <- reactive({
    df <- recidiviz_df
    if(!is.null(input$custom_state) && length(input$custom_state) > 0)
      df <- df %>% filter(state %in% input$custom_state)
    if(!is.null(input$custom_bill_name) && length(input$custom_bill_name) > 0)
      df <- df %>% filter(bill_name %in% input$custom_bill_name)
    if(!is.null(input$custom_impact_type) && length(input$custom_impact_type) > 0)
      df <- df %>% filter(impact_type %in% input$custom_impact_type)
    if(!is.null(input$custom_projection_time) && length(input$custom_projection_time) > 0)
      df <- df %>% filter(estimate_time_frame %in% input$custom_projection_time)
    df
  })
  
  custom_agg <- eventReactive(input$generate_plot, {
    df <- custom_filtered()
    df
  })
  
  output$custom_plot <- renderPlot({
    req(input$generate_plot)
    
    df <- custom_agg()
    
    if(nrow(df) == 0){
      plot.new()
      text(0.5, 0.5, "No data to display. Adjust filters and click 'Generate Plot'.", cex = 1.5)
      return()
    }
    
    df <- df %>% mutate(estimate = as.numeric(estimate))
    
    p <- ggplot(df, aes(x = .data[[input$custom_group]], y = estimate))
    
    if(input$custom_chart_type == "bar"){
      p <- p + geom_col(aes(fill = estimate_time_frame), position = position_dodge())
    }else if(input$custom_chart_type == "line"){
      p <- p + geom_line(aes(group = estimate_time_frame, color = estimate_time_frame), size = 1.2) +
        geom_point(aes(color = estimate_time_frame))
    }else if(input$custom_chart_type == "scatter"){
      p <- p + geom_point(aes(color = estimate_time_frame), size = 3)
    }else if(input$custom_chart_type == "boxplot"){
      p <- p + geom_boxplot(aes(fill = estimate_time_frame))
    }
    
    if(length(input$custom_facet) == 1){
      p <- p + facet_wrap(vars(.data[[input$custom_facet[1]]]))
    }else if(length(input$custom_facet) == 2){
      p <- p + facet_grid(
        rows = vars(.data[[input$custom_facet[1]]]),
        cols = vars(.data[[input$custom_facet[2]]])
      )
    }
    
    p + theme_minimal() +
      labs(fill = "Projection Time Frame",
           color = "Projection Time Frame",
           x = input$custom_group %>% str_replace_all("_", " ") %>% str_to_title(),
           y = "Estimate") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$custom_table <- renderDT({
    req(input$generate_plot)
    datatable(
      custom_agg() %>% 
        dplyr::select(-c(modelability_score)) %>% 
        mutate(
          estimate_type = estimate_type %>% 
            str_replace_all("people_impacted", "People Impacted") %>% 
            str_replace_all("life_years", "Life Years"), 
          across(c("estimate", ends_with("_ci")), ~comma(.x))
        ) %>% 
        rename_with(~.x %>% 
                      str_replace_all("_", " ") %>% 
                      str_to_title() %>% 
                      str_replace_all("Ci$", "CI")),
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,        
        scrollY = "400px",     
        paging = FALSE,        
        dom = 'Bfrtip',        
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })

  output$download_custom_plot <- downloadHandler(
    filename = function() paste0("custom_plot_", Sys.Date(), ".png"),
    content = function(file){
      ggsave(file, plot = custom_plot(), width = 10, height = 7)
    }
  )
  
  output$download_custom_data <- downloadHandler(
    filename = function() paste0("custom_data_", Sys.Date(), ".csv"),
    content = function(file){
      write_csv(custom_agg(), file)
    }
  )
}


####Run App####
shinyApp(ui, server)
