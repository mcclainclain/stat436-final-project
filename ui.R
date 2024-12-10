library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(shinydashboardPlus)

cpi_urban = read_csv("./data/CustomerPriceIndexUrban.csv")
median_income = read_csv("./data/MedianHouseHoldincome.csv")
mortgage_average_us = read_csv("./data/MortgageAverageUS.csv")


cpi_urban <- cpi_urban %>%
  mutate(Oct = as.double(Oct))

median_income <- median_income %>%
  mutate(year = year(ymd(DATE))) %>%   # Extract year from the date column
  rename(Median_income = MEHOINUSWIA646N) %>%  # Rename the income column
  select(year, Median_income)


mortgage_average_us <- mortgage_average_us %>%
  mutate(year = year(DATE)) %>%  # Extract the year from the DATE column
  group_by(year) %>%              # Group by year
  summarise(Mortgage_Average = mean(MORTGAGE30US, na.rm = TRUE)) %>%  # Sum the Mortgage3ous
  ungroup() 

combined_mortgageIncome = full_join(median_income, mortgage_average_us, by = "year")


create_card = function(title, plotName) {
  return (
    card(
      card_header(title),
      plotlyOutput(plotName)
    )
  )
}

ui = page_navbar(
  title = "WI Housing Dashboard",
  bg = "#346c9b",
  nav_panel(
    title = "Counties",
    layout_sidebar(
      sidebar = sidebar(
        title = "Counties",
        p("WI Housing Data from 2000-2024"),
        selectInput("fillType", "Color By", c("Median Price", "# of Homes Sold")),
        conditionalPanel(
          condition = "input.fillType == '# of Homes Sold'",
          p("Fill based on total number of homes sold in the past 5 years")
        ),
        conditionalPanel(
          condition = "input.fillType == 'Median Price'",
          p("Fill based on average median price in the past 5 years"),
          p("Click on a county to see further breakdowns")
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Map"),
          plotlyOutput("map")
        ),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          textOutput("county_name"),
          create_card("Homes Sold", "homes"),
          create_card("Median Price", "median_price")
        )
      )
    )
  ),
  nav_panel(
    title = "CPI Data",
    layout_sidebar(
      sidebar=sidebar(
        
          conditionalPanel("input.plotTabs == 'tab1' | input.plotTabs == 'tab2'",
            sliderInput("line_years", "Select Year Range:",
                        min = min(cpi_urban$Year, na.rm = TRUE), 
                        max = max(cpi_urban$Year, na.rm = TRUE),
                        value = c(min(cpi_urban$Year, na.rm = TRUE), max(cpi_urban$Year, na.rm = TRUE)), 
                        sep = "", step = 1)
            ),
          conditionalPanel("input.plotTabs == 'tab2'",
            selectInput("year_month", "Group By:",
                        choices = c("Month", "Year"), selected = "Month")),
          conditionalPanel("input.plotTabs == 'tab3'",
          checkboxInput("show_values", "Show Values on Bars", value = TRUE)
          ), 
          p("The CPI is the average price of an arbitrary basket of goods you can buy at a store. \nIt is a key indication of how much things cost in a given year.")
        ),
        
          tabsetPanel(
            id = "plotTabs",
            tabPanel("CPI YOY", value = "tab1", br(), plotlyOutput("line_plot", height="75vh", width = "100%")),
            tabPanel("CPI % Change", value = "tab2", br(), plotlyOutput("cpi_changes", height="75vh")),
            tabPanel("Annual Average Bar Plot", value = "tab3", br(), plotOutput("bar_plot", height="75vh"))
          )
        
    )
  ),
  nav_panel(
    title = "Home Affordability",
    layout_sidebar(
      sidebar = sidebar(
        title = "Year Select",
        sliderInput("year_range", "Select Year Range:", 
                    min = min(combined_mortgageIncome$year, na.rm = TRUE), 
                    max = max(combined_mortgageIncome$year, na.rm = TRUE),
                    value = c(min(combined_mortgageIncome$year, na.rm = TRUE), max(combined_mortgageIncome$year, na.rm = TRUE)),
                    sep = "", step = 1),
        p("Shows Median Income and Housing Prices in Wisconsin against Federal Mortgage Rates")
      ),
      tabsetPanel(
        id = "affordtabs",
        tabPanel("YOY Changes", value = "tab1", br(), plotlyOutput("combined_plot", height="75vh")),
        tabPanel("% Changes", value = "tab2", br(), plotlyOutput("pct_change_plot", height="75vh"))
      )
    )
  )
)