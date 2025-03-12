options(shiny.port = 8050, shiny.autoreload = TRUE)
library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)

# Read in data and fill in missing values
df <- read.csv("data/SAU_EEZ_826_v50-1.csv")
df <- df |> mutate(end_use_type = if_else(end_use_type== "", "Discard",
                                          end_use_type))

# Define list of choices
breakdown <- c("Fishing Entity" = "fishing_entity",
               "Fishing Sector"="fishing_sector",
               "Reporting Status"="reporting_status",
               "Catch Type" = "catch_type",
               "Commercial Group" = "commercial_group",
               "End Use Type" = "end_use_type")

ggplot2::theme_set(ggplot2::theme_minimal())

ui <- page_sidebar(
  
  # Set theme
  # theme = bs_theme(bootswatch = "darkly",
  #                  bg = "#222222",
  #                  fg = "#86C7ED",
  #                  success ="#86C7ED"),
  
  # Title
  title = "Fisheries Catch in the United Kingdom EEZ from 1950-2019",
  
  sidebar = sidebar(title = "Filters",
                    selectInput("breakdown", "Select Time Series Breakdown Type",
                                choices = breakdown,
                                selected='reporting_status',
                                multiple=FALSE),
                    sliderInput("year",
                                "Select Year Range",
                                min = min(df$year),
                                max = max(df$year),
                                value = c(min(df$year), max(df$year)),
                                step = 1,
                                ticks = TRUE,
                                sep = ""),
                    selectInput("sector", "Filter by Sector",
                                choices = sort(unique(df$fishing_sector)),
                                selected='',
                                multiple=TRUE),
                    selectInput("commercial", "Filter by Commercial Group",
                                choices = sort(unique(df$commercial_group)),
                                selected='',
                                multiple=TRUE),
                    selectInput("fishing_ent", "Filter by Fishing Entity",
                                choices = sort(unique(df$fishing_entity)),
                                selected='',
                                multiple=TRUE)),
  
  layout_columns(
    value_box(title ="Total Tonnage",
              value = textOutput("tonnage")),
    
    value_box(title ="Total Landed Value",
              value = textOutput("value")),
    value_box(title="Total Number of Species Caught",
              value = textOutput("species_count")),
    value_box(title="Total Number of Fishing Entities",
              value = textOutput("fe_count")),
    
    card(card_header("Fisheries catch in 1950-2019"),
         plotOutput("timeseries")),
    
    card(card_header("Species composition of catch"),
         tableOutput("species_table")),
    
    col_widths = c(3, 3, 3, 3,  7, 5),
    row_heights = c(2, 7))
)

server <- function(input, output, session){
  # setup default values for filter
  selected_sector <- reactive({
    if (is.null(input$sector)) df$fishing_sector else input$sector
  })
  
  selected_cg <- reactive({
    if (is.null(input$commercial)) df$commercial_group else input$commercial
  })
  
  selected_fe <- reactive({
    if (is.null(input$fishing_ent)) df$fishing_entity else input$fishing_ent
  })
  
  # Filter dataframe for year range
  filtered_df <- reactive({
    df |> filter(between(year, input$year[1],input$year[2]),
                 fishing_sector %in% selected_sector(),
                 commercial_group %in% selected_cg(),
                 fishing_entity %in% selected_fe())
  })
  
  
  # Time series graph
  output$timeseries <- renderPlot({
    grouped <- filtered_df() |> 
      group_by(year, !!sym(input$breakdown)) |> 
      summarise(total = sum(tonnes, na.rm =TRUE)/1000,
                .groups = "drop")
    return (
      grouped |> ggplot(aes(x=year, y = total, color=!!sym(input$breakdown))) +
        geom_line(linewidth=1) +
        xlab("Year") +
        ylab("Catch (Thousand Tonnes)") +
        theme_classic(base_size = 18) +
        theme(legend.title = element_blank())
    )
  })
  
  # Total tonnage calculation
  output$tonnage <- renderText({
    total_ton <- round(sum(filtered_df()$tonnes, na.rm = TRUE),0) |> 
      format(big.mark = ",")
    paste0(total_ton, " t")
  })
  
  # Total value calculation
  output$value <- renderText({
    total_val <- round(sum(filtered_df()$landed_value, 
                           na.rm = TRUE)/1000000000,2) |> 
      format(big.mark = ",")
    paste0(total_val, " Billion USD")
  })
  
  # Total species count
  output$species_count <- renderText({
    length(unique(filtered_df()$scientific_name))
  })
  
  # Total fishing entity count
  output$fe_count <- renderText({
    length(unique(filtered_df()$fishing_entity))
  })
  
  # Species composition table
  output$species_table <- renderTable({
    top20 <- filtered_df() |> group_by(scientific_name) |>
      summarise(total_ton = sum(tonnes)) |> 
      mutate(percentage = total_ton/sum(total_ton)*100) |> 
      select(-total_ton) |> 
      arrange(desc(percentage)) |> 
      slice(1:20)
    
    if (sum(top20$percentage)<100){
      top20 <- top20|> add_row(scientific_name= "Other species",
                               percentage = 100-sum(top20$percentage))
    }
    top20 |> rename("Scientific Name" = scientific_name,
                    "Catch Percentage (%)" = percentage)
  },
  spacing = 'xs',
  digits = 2,
  align = 'c')
  
}

shinyApp(ui, server)





