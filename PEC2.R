# visualización de datos - PEC2

# libraries
library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(rjson)
library(tidyquant)
library(plotly)

# Part I: Line Chart

# The dataset of this part is freely available at
# https://datos.gob.es/es/catalogo/l01462508-temperatura-valencia-2018-2022


# dataset (temperatures in Valencia between 2018 and 2022)
temperatures <- read.csv2(
  "C:/Users/Felletti/Desktop/PEC2/temperatura-valencia-2018-2022.csv")


# LINE CHARTS

# line chart of years 2018 to 2022 (all months)
ggplot(temperatures, aes(x = as.Date(fecha), y = tmed)) +
  geom_line(color = "#0000FF", size = .7) +
  labs(title = "Temperature in Valencia between 2018 and 2022",
       x = "Year",
       y = "Average temperature (°C)",
       caption = "Source: https://datos.gob.es/es/catalogo/l01462508-temperatura-valencia-2018-2022") +
  xlim(as.Date("2018-01-01"), as.Date("2022-12-31")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


# Line chart of 2022 (all months)

# Convert fecha column to Date format
temperatures$fecha <- as.Date(temperatures$fecha)

# Filter data for the year 2022
temp_2022 <- temperatures %>%
  filter(format(fecha, "%Y") == "2022")

# line chart for the year 2022
ggplot(temp_2022, aes(x = fecha, y = tmed)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Temperature in Valencia (2022)",
       x = "Date",
       y = "Temperature (°C)",
       caption = "Source: https://datos.gob.es/es/catalogo/l01462508-temperatura-valencia-2018-2022") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


# PART II: Arc Diagram

# dataset: https://www.kaggle.com/datasets/deepcontractor/dark-netflix-character-relationship
# the dataset has been converted from JSON to .csv

characters <- read.csv("C:/Users/Felletti/Desktop/PEC2/characters.csv")

# Gets all parent-child relationships (excluding NA values)
parent_child_edges <- characters[!is.na(characters$characters.parentOf.0),
                                 c("characters.name", "characters.parentOf.0")]

# Create igraph for parent-child relationships. I use the directed graph for
# creating the arc diagram.
parent_child_graph <- graph_from_data_frame(parent_child_edges, directed = T)


# Plots the arc diagram for parent-child relationships using ggraph and igraph
ggraph(parent_child_graph, layout = "linear") +
  geom_edge_arc(aes(alpha = ..index..), color = "blue") +
  geom_node_text(aes(label = name), size = 3, angle = 90, hjust = 1, vjust = 0.5,
                 check_overlap = T) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Dark Netflix Parent-Child Relationships")



# Part III: 

# dataset: https://www.kaggle.com/datasets/meetnagadia/apple-stock-price-from-19802021

apple_stock <- read.csv("C:/Users/Felletti/Desktop/PEC2/AAPL.csv")

# Select only data for the year 2020
apple_stock$Date <- as.Date(apple_stock$Date)
apple_stock_2020 <- subset(apple_stock, Date >= "2020-01-01" & Date < "2021-01-01")

# Convert to a tq object to make the OHLC chart
apple_stock_2020_tq <- tq_get("AAPL", from = min(apple_stock_2020$Date), to = max(apple_stock_2020$Date))


# Plot OHLC chart for year 2020 using ggplot

apple_stock_2020_tq %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_candlestick(aes(x = date, open = open, high = high, low = low, close = close, group = 1)) +
  labs(title = "Apple Stock Prices - Year 2020", y = "Adjusted Price", x = "Date") +
  theme_minimal()


# Plot OHLC chart for year 2020 using plotly (interactive graphic)
plot_ly(
  type = "candlestick",
  x = ~apple_stock_2020_tq$date,
  open = ~apple_stock_2020_tq$open,
  high = ~apple_stock_2020_tq$high,
  low = ~apple_stock_2020_tq$low,
  close = ~apple_stock_2020_tq$close,
) %>%
  layout(title = "Apple Stock Prices - Year 2020",
         yaxis = list(title = "Adjusted Price"),
         xaxis = list(title = "Date"))