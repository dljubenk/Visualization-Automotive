library(shiny)
library(shinyjs)


autos <- read.csv("autos_clean_new.csv")

shinyUI(fluidPage(
  useShinyjs(),
  navbarPage(title = "Auto Analysis",
             tabPanel("Resale", fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   useShinyjs(),
                   selectInput("topN", label = p("Top N"), 
                               choices = list("All", "Top 5", "Top 10")),
                   radioButtons("measureType", label = p("Choose type of measurement"),
                                choices = list("Standard deviation", "Ratio (normalized for mean)")),
                   selectInput("selectBrand", label = p("Select brand"), 
                               choices = c(c("All"), levels(unique(autos$brand)))),
                   sliderInput("range", label = "Registration year range of interest",
                               min = 1950, max = 2017, value = c(1950,2016))
                 ),
                 mainPanel(
                   plotOutput("resale", height=550)
                 )
                 
               )
             )),
             tabPanel("Selling time", fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("selectX", label = p("Select attribute"), 
                               choices = list("Year of registration", "Power", "Kilometer", "Price")),
                   selectInput("selectBrand2", label = p("Select brand"), 
                               choices = c(c("All"), levels(unique(autos$brand)))),
                   sliderInput("range2", label = "Registration year range of interest",
                               min = 1950, max = 2016, value = c(1950,2016))
                 ),
                 mainPanel(
                   plotOutput("second")
                 )
                 
               )
             )),
             tabPanel("Map", fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("selectBrand3", label = p("Select brand"), 
                               choices = c(c("All"), levels(unique(autos$brand)))),
                   selectInput("selectMapGearbox", label = p("Select gearbox"),
                               choices = c(c("All"), levels(unique(autos$gearbox)))),
                   selectInput("selectMapVehicletype", label = p("Select vehicle type"),
                               choices = c(c("All"), levels(unique(autos$vehicleType)))),
                   selectInput("selectMapFueltype", label = p("Select fuel type"),
                               choices = c(c("All"), levels(unique(autos$fuelType)))),
                   sliderInput("rangeMapPrice", label = "Price range of interest",
                               min = 100, max = 99999, value = c(100,99999)),
                   sliderInput("rangeMapPower", label = "Power range of interest",
                               min = 1, max = 450, value = c(1,450)),
                   sliderInput("rangeMapKm", label = "Kilometers ridden range of interest",
                               min = 5000, max = 150000, value = c(5000,150000)),
                   sliderInput("rangeMapYear", label = "Registration year range of interest",
                               min = 1950, max = 2016, value = c(1950,2016))
                 ),
                 mainPanel(
                   plotOutput("geo", width=800, height=800)
                 )
                 
               )
             )),
             tabPanel("Distribution", fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   HTML(paste(p("Change any of the inputs to let window re-appear."))),
                   selectInput("selectFill", label = p("Select fill"),
                               choices = list("Gearbox","Vehicle type", "Damaged", "Fuel type")),
                   br(),
                   selectInput("selectDist", label = p("Select distribution variable"),
                               choices = list("Year of registration",
                                              "Kilometers", "Power", "Price")),
                   br(),
                   selectInput("selectBrand4", label = p("Select brand"), 
                               choices = c(c("All"), levels(unique(autos$brand)))),
                   br(),
                   sliderInput("rangeDistYear", label = "Registration year range of interest",
                               min = 1950, max = 2016, value = c(1950,2016)),
                   br(),
                   sliderInput("rangeDistPrice", label = "Price range of interest",
                               min = 100, max = 99999, value = c(100,99999)),
                   br(),
                   sliderInput("rangeDistPower", label = "Power range of interest",
                               min = 1, max = 450, value = c(1,450)),
                   br(),
                   sliderInput("rangeDistKm", label = "Kilometers ridden range of interest",
                               min = 5000, max = 150000, value = c(5000,150000))
                 ),
                 mainPanel(
                   plotOutput("distribution")
                 )
                 
               )
             )),
             tabPanel("HorsePower", fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("vehicleType", "Vehicle type:",
                               c("All" = "all",
                                 "Compact" = "kleinwagen",
                                 "Limousine" = "limousine",
                                 "Cabrio" = "cabrio",
                                 "Combi" = "kombi",
                                 "SUV" = "SUV",
                                 "Bus" = "bus",
                                 "Coupe" = "coupe",
                                 "Other" = "andere")),
                   selectInput("brand", "Brand:",
                               c(c("All"), levels(unique(autos$brand))))
                 ),
                 mainPanel(
                   plotOutput("power")
                 )
                 
               )
             ))
  
)))