library(tidyverse)
library(leaflet)
library(sf)
library(plotly)
library(scales)
library(showtext)

font_add_google("Open Sans", "opensans")
showtext_auto()

homes_sold = read_csv("./data/wi_homes_sold.csv")
median_price = read_csv("./data/wi_median_prices.csv")

homes_sold = homes_sold %>% 
  select(!...1) %>% 
  pivot_longer(cols=!c(County, Year), names_to="Month", values_to="Homes_Sold") %>% 
  relocate(County, .before=Year)


median_price = median_price %>% 
  select(!...1) %>% 
  pivot_longer(cols=!c(County, Year), names_to="Month", values_to="Median_Price") %>% 
  relocate(County, .before=Year)

wi = st_read("./data/County_Boundaries_24K.geojson")

counties_data = homes_sold %>% 
  left_join(median_price) %>% 
  drop_na() %>% 
  mutate(price = as.integer(gsub(",", "", substr(Median_Price, 2, nchar(Median_Price)))))
  

counties_last_5 = counties_data %>% 
  filter(Year >= 2019) %>% 
  group_by(County) %>% 
  summarize(num_sold = sum(Homes_Sold), price = mean(price))

wi_w_last_5 = wi %>% 
  left_join(counties_last_5, by=join_by(COUNTY_NAME == County))

cpi_urban = read_csv("./data/CustomerPriceIndexUrban.csv")
median_income = read_csv("./data/MedianHouseHoldincome.csv")
mortgage_average_us = read_csv("./data/MortgageAverageUS.csv")


cpi_urban <- cpi_urban %>%
  mutate(Oct = as.double(Oct))

cpi_data_long <- cpi_urban %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "Month",
    values_to = "CPI"     
  )

cpi_data_rolling = cpi_data_long %>% 
  mutate(
    Month = recode(Month,
                   "June" = "Jun",  # Recode full month names to abbreviations
                   "July" = "Jul"),
    month = match(Month, month.abb), 
    cpi_date = as.Date(paste(Year, sprintf("%02d", month), "01", sep = "-"))) %>% 
  arrange(cpi_date) %>%
  mutate(rolling_avg = zoo::rollmean(CPI, k = 12, fill = NA, align = "right")) %>% 
  drop_na()

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

mortgage_income_cpi = right_join(combined_mortgageIncome %>% rename(Year = year), cpi_urban, by = "Year") %>% filter(Year >= 1990)


server <- function(input, output, session) {
  
  observeEvent(input$affordtabs, {
    if (input$affordtabs == "tab1") {
      updateSliderInput(session, "year_range", min = 1971)
    } else if (input$affordtabs == "tab2") {
      updateSliderInput(session, "year_range", min = 1990)
    } 
  })

  sf_plot = reactive({
    plot = NULL
    switch( input$fillType,
                   "# of Homes Sold" = {ggplot(wi_w_last_5) + geom_sf(aes(fill=num_sold, text = paste("County:", COUNTY_NAME, "\nHomes Sold:", round(num_sold, 2)))
                    )},
                   "Median Price" = {ggplot(wi_w_last_5) + geom_sf(aes(fill=price, text = paste("County: ", COUNTY_NAME, "\nMedian Price: ", dollar(price), sep="")
                   ))}
           )    })
  
  get_clicked_county = reactive({
    
    click <- event_data("plotly_click", source = "map")
    if (!is.null(click)) {
      # Extract click coordinates and set as WGS 84 (same as CRS for Wisconsin data)
      clicked_point <- st_point(c(click$x, click$y))
      clicked_sf <- st_sfc(clicked_point, crs = st_crs(wi)) # Set CRS here to match
      
      buffered_point = st_buffer(clicked_sf, dist = 0.001)
      
      # Find county containing the clicked point
      clicked_county <- wi %>%
        filter(st_intersects(geometry, buffered_point, sparse = FALSE)) %>%
        pull(COUNTY_NAME) 
      
      if (length(clicked_county) == 0){
        return (NULL)
      }
      
      return (clicked_county[1])
    } else {
      return (NULL)
    }
  })
  
  
  
  
  
  output$map = renderPlotly({
    ggplotly(sf_plot() +
      labs(title=paste("WI Counties: By", input$fillType, "(Last 5 Years)"), fill=input$fillType) +
      theme_void() +
      theme(plot.title = element_text(size=15)), tooltip="text", source = "map") %>% 
      config(displayModeBar=F) %>% 
      layout(
        xaxis = list(visible = FALSE, showticklabels = FALSE, showgrid = FALSE),
        yaxis = list(visible = FALSE, showticklabels = FALSE, showgrid = FALSE)
      )
      
  })
 
  
  output$county_name <- renderText({
    
    clicked_county = get_clicked_county()
    
    
    
    # Display result
    if (length(clicked_county) > 0) {
      paste("Selected County:", clicked_county)
    } else {
      "No county clicked"
    }
  })
  
  output$homes = renderPlotly({
  
    clicked_county = get_clicked_county()
    
    if (length(clicked_county) == 0){
      return (ggplotly(ggplot() + theme_void()) %>% layout(xaxis = list(visible=F), yaxis=list(visible=F)))
    }
    
    avg_wi_homes_sold = counties_data %>% 
      group_by(County, Year) %>% 
      summarize(tot_sold = sum(Homes_Sold)) %>% 
      group_by(Year) %>% 
      summarize(avg_sold = mean(tot_sold))
    
    
    county_data = counties_data %>% 
      filter(County == clicked_county) %>% 
      group_by(Year) %>% 
      summarize(tot_sold = sum(Homes_Sold))

  
    plot_ly() %>%
      add_lines(
        data = county_data, 
        x = ~Year, y = ~tot_sold, 
        name = "County", 
        hoverinfo = NULL,
        line = list(color = "#346c9b", width = 4)
      ) %>%
      add_markers(
        data = county_data, 
        x = ~Year, y = ~tot_sold, 
        name = paste(clicked_county, "County"),
        text = ~paste("Year: ", Year, "\nHomes Sold: ", round(tot_sold, 2), sep=""),
        hoverinfo = "text",
        showlegend = F,
        marker = list(size = 8, color = "black"),
      ) %>%
      add_lines(
        data = avg_wi_homes_sold, 
        x = ~Year, y = ~avg_sold, 
        text = ~paste("Year: ", Year, "\nHomes Sold: ", round(avg_sold, 2), sep=""),
        hoverinfo = "text",
        name = "WI Avg", 
        line = list(color = "black", width = 4)
      ) %>%
      layout(
        title = list(text = paste("Homes Sold: ", clicked_county, " County", sep="")),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Homes Sold")
      ) %>% 
      config(displayModeBar = FALSE)
    
      
  })
  
  output$median_price = renderPlotly({
    clicked_county = get_clicked_county()
    
    if (length(clicked_county) == 0){
      return (ggplotly(ggplot() + theme_void()) %>% layout(xaxis = list(visible=F), yaxis=list(visible=F)))
    }
    
    avg_wi_median_price = counties_data %>% 
      group_by(County, Year) %>% 
      summarize(price = mean(price)) %>% 
      group_by(Year) %>% 
      summarize(avg_price = mean(price))
    
    
    county_price = counties_data %>% 
      filter(County == clicked_county) %>% 
      group_by(Year) %>% 
      summarize(price = mean(price))
    
    
    plot_ly() %>%
      add_lines(
        data = county_price, 
        x = ~Year, y = ~price, 
        name = "County", 
        hoverinfo = NULL,
        line = list(color = "#346c9b", width = 4)
      ) %>%
      add_markers(
        data = county_price, 
        x = ~Year, y = ~price, 
        name = paste(clicked_county, "County"),
        text = ~paste("Year: ", Year, "\nPrice: ", dollar(price), sep=""),
        hoverinfo = "text",
        showlegend = F,
        marker = list(size = 8, color = "black"),
      ) %>%
      add_lines(
        data = avg_wi_median_price, 
        x = ~Year, y = ~avg_price, 
        text = ~paste("Year: ", Year, "\nPrice: $", dollar(avg_price), sep=""),
        hoverinfo = "text",
        name = "WI Avg", 
        line = list(color = "black", width = 4)
      ) %>%
      layout(
        title = list(text = paste("Median Price: ", clicked_county, " County", sep="")),
        # xaxis = list(title = "Date"),
        yaxis = list(title = "Median Price")
      ) %>% 
      config(displayModeBar = FALSE)
    
   
  })
  
  
  output$line_plot <- renderPlotly({
    selected_years <- seq(input$line_years[1], input$line_years[2])
    cpi_1982 <- cpi_urban %>%
      filter(Year == 1982) %>%
      pull(Avg)
    
    
    hline <- function(y = 0, color = "black") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color, dash="dash")
      )
    }
    
    annotation <- list(yref = 'y', xref = "x", y = 100, x = 1970, text = "1982 CPI Baseline")
    
    plot_ly() %>% 
      add_lines(
        data = filter(cpi_urban, Year %in% selected_years),
        x = ~as.Date(paste(Year, "-01-01", sep = "")), y = ~Avg,
        name = "Yearly Average",
        text = ~paste("Year:", Year, "\nAvg. CPI:", Avg),
        hoverinfo = "text",
        line = list(color = "red", width = 2.5)
      ) %>%
      add_lines(
        data = filter(cpi_data_rolling, Year %in% selected_years),
        name = "Rolling Average",
        x = ~cpi_date, y = ~rolling_avg,
        text = ~paste(Month, Year, "\nRolling Avg. CPI:", round(rolling_avg, 2)),
        hoverinfo = "text",
        line = list(color = "#346c9b", width = 2.5)
      ) %>% 
      layout(
        title = paste("CPI by Year: ", input$line_years[1], "-", input$line_years[2], sep=""),
        shapes = list(
          hline(100)
        ),
        yaxis = list(
          title = "CPI"
        ),
        xaxis = list(
          title = "Date"
        ),
        annotations = list(annotation)
      )
  })
  
  output$cpi_changes <- renderPlotly({
    selected_years <- seq(input$line_years[1], input$line_years[2])
    
    
    cpi_data_pct_change = cpi_data_long %>% 
      mutate(Month = recode(Month,
                           "June" = "Jun",  # Recode full month names to abbreviations
                           "July" = "Jul"),) %>% 
      mutate(pct_change = (CPI - lag(CPI)) / lag(CPI) * 100)
  
    
    
    
    switch(input$year_month,
           "Month" = {
             ggplotly(
               ggplot(cpi_data_pct_change %>% filter(Year %in% selected_years), aes(x=factor(Month, levels = month.abb), y=pct_change)) +
                 geom_boxplot() +
                 labs(
                   title = "Changes in CPI by Month",
                   x = "Month",
                   y = "% Change in CPI"
                 ) +
                 theme_minimal(), tooltip = NULL
             ) %>% 
               layout(
                 title=list(
                   x = 0.5, xanchor='center'
                 ))
           },
           "Year" = {
             ggplotly(
                ggplot(cpi_data_pct_change %>% filter(Year %in% selected_years) %>% group_by(Year) %>% summarize(avg_change = mean(pct_change)), aes(x = Year, y = avg_change)) +
                  geom_line(size=1, color='#346c9b', alpha=.8) +
                  geom_point(color = "#346c9b", aes(text = paste("Year: ", Year, "\nPct. Change: ", round(avg_change, 2), "%", sep=""))) +
                  labs(
                    title = "Changes in CPI by Year",
                    x = "Year",
                    y = "% Change in CPI"
                  ) +
                  theme_minimal() + theme(plot.title = element_text(hjust=0.5)), tooltip = "text"
             )
           }
           )
    
  
  })
  
  output$bar_plot <- renderPlot({
    p <- ggplot(cpi_urban %>% filter(Year > 1990), aes(x = factor(Year), y = Avg)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Annual Average CPI", x = "Year", y = "Average CPI") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12, family="opensans"), axis.text.y = element_text(size=12, family="opensans"),
            plot.title = element_text(size = 18, hjust=0.5, family="opensans"), axis.title = element_text(size=13, family="opensans"))
    
    if (input$show_values) {
      p <- p + geom_text(aes(label = round(Avg, 1)), 
                         vjust = 0.5, angle = 90, size=4, family="opensans")
    }
    p
  })
  
  output$combined_plot <- renderPlotly({
    filtered_data <- combined_mortgageIncome %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    housingData <- median_price %>%
      mutate(Median_Price = as.numeric(gsub("[$,]", "", Median_Price)))
    
    yearly_housing <- housingData %>%
      group_by(Year) %>%
      summarize(Average_Median_Price = mean(Median_Price, na.rm = TRUE))
    
    median_incomeChange <- median_income %>%
      rename(Year = year)
    
    combined_data <- merge(yearly_housing, median_incomeChange, by = "Year") %>% rename(year=Year)
    
    
    plot_ly() %>% 
      add_lines(
        data = combined_mortgageIncome %>%filter(year >= input$year_range[1] & year <= input$year_range[2]),
        x = ~year, y = ~Mortgage_Average,
        name = "Mortage Rate", line = list(color = "blue"),
      ) %>% 
      add_markers(
        data = combined_mortgageIncome %>%  filter(year >= input$year_range[1] & year <= input$year_range[2]),
        x = ~year, y = ~Mortgage_Average,
        name = "Federal Mortage Rate",
        showlegend=F,
        text = ~paste("Year: ", year, "\nFederal Mortgage Rate: ", round(Mortgage_Average, 2), "%", sep=""),
        hoverinfo = "text",
        marker = list(color = "blue")
      ) %>% 
      add_lines(
        data = combined_mortgageIncome %>%  filter(year >= input$year_range[1] & year <= input$year_range[2]),
        x = ~year, y = ~Median_income/10000,
        name = "WI Median Income (in $10,000s)", line = list(color = "red"),
      ) %>% 
      add_markers(
        data = combined_mortgageIncome %>%  filter(year >= input$year_range[1] & year <= input$year_range[2]),
        x = ~year, y = ~Median_income/10000,
        name = "WI Median Income (in $10,000s)",
        showlegend=F,
        text = ~paste("Year: ", year, "\nMedian Income: ", dollar(Median_income), sep=""),
        hoverinfo = "text",
        marker = list(color = "red")
      ) %>% 
      add_lines(
        data = combined_data %>%  filter(year >= input$year_range[1] & year <= input$year_range[2]),
        x = ~year, y = ~Average_Median_Price/10000,
        name = "WI Home Price (in $10,000s)", line = list(color = "purple"),
      ) %>% 
      add_markers(
        data = combined_data %>%  filter(year >= input$year_range[1] & year <= input$year_range[2]),
        x = ~year, y = ~Average_Median_Price/10000,
        name = "WI Home Price (in $10,000s)",
        showlegend=F,
        text = ~paste("Year: ", year, "\nHome Price: ", dollar(Average_Median_Price), sep=""),
        hoverinfo = "text",
        marker = list(color = "purple")
      )  %>% 
      layout(
        title = list(text = "Housing Affordability in Wisconsin"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Value")
      )
  })
 
  output$pct_change_plot = renderPlotly({
    
    mortgage_average_us_pct_change <- mortgage_average_us %>%
      mutate(mortgage_diff = (Mortgage_Average - lag(Mortgage_Average)) / lag(Mortgage_Average) * 100)
    
    cpi_urban_pct = cpi_urban %>% 
      mutate(percent_change_cpi = (Avg - lag(Avg)) / lag(Avg) * 1000)
    
    
    median_income_pct = median_income %>% 
      mutate(percent_change_incomes = (Median_income - lag(Median_income)) / lag(Median_income) * 100)
    
    
    combined_mortgageIncome_jack = full_join(median_income_pct, mortgage_average_us_pct_change, by = "year")
    
    combined_mortgageIncome_jack = combined_mortgageIncome_jack %>% rename(Year = year)
    
    mortgage_income_cpi_jack = right_join(combined_mortgageIncome_jack, cpi_urban_pct, by = 'Year') %>% filter(Year >= 1990)
    
    
    filtered_data_2 <- mortgage_income_cpi_jack %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    
    
    
    p <- ggplot(filtered_data_2, aes(x = Year)) +
      geom_line(aes(y = mortgage_diff, color = "Mortgage % Change"), size = .6) +
      
      geom_line(aes(y = percent_change_cpi, color = 'CPI % Change'), size = .6) +
      
      geom_line(aes(y = percent_change_incomes, color = 'Income % Change'), size = .6) +
      
      scale_color_manual(
        values = c(
          "Mortgage % Change" = "#edb560",
          "CPI % Change" = "#346c9b",
          "Income % Change" = "#a7d9e4"
        )
      ) +
      
      labs(
        title = "Yearly Percent Change in Mortgage Rates, Income, And The CPI Since 1990",
        x = "Year",
        y = "% Change",
        color = "Metric"
      ) +
      theme_minimal() +
      
      theme(
        legend.position = "bottom",            
        legend.box = "horizontal",          
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10)  
      )
    
    ggplotly(p)
  }) 
 
}
