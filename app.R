# February 2023
# Assessment Shiny
# -------------------------------------------------

# LOAD PACKAGES

library(shiny)
library(shinythemes)
library(nycflights13)
library(tidyverse)
# Kleurenpalet dat ik graag gebruik. Makkelijk van elkaar te onderscheiden en kleurenblindvriendelijk
# https://rdrr.io/github/dtm2451/dittoSeq/man/dittoColors.html 
dittocolors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#666666", #1-8
                 "#AD7700", "#1C91D4", "#007756", "#D5C711", "#005685", "#A04700", "#B14380", "#4D4D4D") #9-16

# -------------------------------------------------
# DATA PRE-PROCESSING

# Datum info is geformatteerd in 3 kolommen (year, month, day)
# Voeg toe 1) "date" kolom in shiny date format (yyy-mm-dd) en 2) "date.monthly" kolom met dag = 01 voor plot per maand
flights <- flights %>% 
  unite("date", c(year, month, day), remove = F, sep = "-") %>% mutate(date = as.character(as.Date(date))) %>%
  unite("date.monthly", c(year, month), remove = F, sep = "-") %>% mutate(date.monthly = as.character(as.Date(paste(date.monthly, "01", sep = "-"))))

# nycflights13 bestaat uit meerdere relationele tabellen (flights, planes, weather, airports, airlines)
# We gebruiken 'flights' als primaire tabel en voegen informatie uit de andere tabellen daaraan toe

# Voor het plotten van passagierscapaciteit, weersomstandigheden en bestemmingen, voegen we informatie toe uit 'planes', 
# 'weather' en 'airports' aan de tabel 'flights':
flights <- flights %>% 
  left_join(planes %>% select(tailnum, seats), by = "tailnum", copy = FALSE, keep = FALSE, na_matches = "na") %>%
  left_join(weather %>% select(origin, time_hour, temp, dewp, humid, wind_dir, wind_speed, wind_gust, precip, pressure, visib), by = c("origin", "time_hour"), copy = FALSE, keep = FALSE, na_matches = "na") %>%
  left_join(airports %>% select(faa, lat, lon), by = c("dest"="faa"), copy = FALSE, keep = FALSE, na_matches = "na")

# -------------------------------------------------
# SHINY APP

# Define UI ----
ui <- fluidPage(theme = shinytheme("cerulean"),
  # Organiseer verschillende onderdelen van het dashboard in aparte tabs
  navbarPage("Casus Flight Analysis",
             
             # TAB 1 - passagierscapaciteit
             tabPanel("Passagierscapaciteit",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Selecteer het vliegveld en periode."),
                          selectInput("airport_1", "Selecteer vliegveld:",
                                      choices = c("JFK", "EWR", "LGA", "alle vliegvelden"),
                                      selected = "JFK"
                                      ),
                          dateRangeInput("daterange_1","Selecteer data:",
                                         start = "2013-01-01",
                                         end = "2013-12-31",
                                         min = "2013-01-01",
                                         max = "2013-12-31",
                                         format = "dd-mm-yyyy"
                                         ),
                          selectInput("barchoice", "Histogram per dag of per maand:",
                                      choices = c("Dag" = "date", "Maand" = "date.monthly"),
                                      selected = "date.monthly"
                                      )
                          ),
                        mainPanel(h4("Passagierscapaciteit vertrekkende vluchten"),
                                  p("Opmerking: de aantallen zijn een onderschatting vanwege ontbrekende data van sommige vluchten"),
                                  plotOutput("histogram"),
                                  )
                        )
                      ),
             
             # TAB 2 - vertraging
             tabPanel("Vertraging", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Visualiseer verschillende omstandigheden die mogelijk vertraging beïnvloeden."),
                          selectInput("variable", "Selecteer variabele:",
                                      choices = c("Afstand" = "distance", 
                                                  "Vluchtduur" = "air_time",
                                                  "Temperatuur" = "temp",
                                                  "Dauwpunt" = "dewp",
                                                  "Luchtvochtigheid" = "humid",
                                                  "Windrichting" = "wind_dir",
                                                  "Windkracht" = "wind_gust",
                                                  "Neerslag" = "precip",
                                                  "Luchtdruk" = "pressure",
                                                  "Zicht" = "visib"),
                                      selected = "Afstand"
                                      ),
                          radioButtons("radio", "Laat verschil zien tussen:",
                                       choices = c("Vliegvelden" = "origin", "Maatschappijen" = "carrier",
                                                   "Geen selectie" = ""),
                                       selected = ""
                                       )
                        ),
                      mainPanel(h3("Omstandigheden die vertraging kunnen beïnvloeden"),
                                p("Opmerking: vluchten waarvan gegevens van vertraging of geselecteerde variabele ontbreken zijn niet opgenomen in analyse"),
                                p("Selecteer punten om meer vluchtgegevens te bekijken"),
                                h4("Aankomstvertraging"),
                                plotOutput("scatter1", brush = "plot_brush"),
                                h4("Vertrekvertraging"),
                                plotOutput("scatter2", brush = "plot_brush"),
                                tableOutput("data")
                                )
                      )),
             
             # TAB 3 - bestemmingen
             tabPanel("Bestemmingen", 
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("daterange_3","Selecteer data:",
                                         start = "2013-01-01",
                                         end = "2013-12-31",
                                         min = "2013-01-01",
                                         max = "2013-12-31",
                                         format = "dd-mm-yyyy"
                          )
                        ),
                        mainPanel(h4("Populairste bestemmingen vanaf New York"),
                                  plotOutput("bestemming")
                        )
                      )),
             
             # TAB 4 - zoeken
             tabPanel("Zoeken", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Selecteer een specifiek vliegveld of periode. Of gebruik de sorteer- of zoekfunctie in de tabel om te zoeken naar specifieke informatie."),
                          selectInput("airport_4", "Selecteer vliegveld:",
                                      choices = c("JFK", "EWR", "LGA", "alle vliegvelden"),
                                      selected = "alle vliegvelden"
                          ),
                          dateRangeInput("daterange_4","Selecteer data:",
                                         start = "2013-01-01",
                                         end = "2013-12-31",
                                         min = "2013-01-01",
                                         max = "2013-12-31",
                                         format = "dd-mm-yyyy"
                          ),
                        ),
                        mainPanel(h4("Vertrekkende vluchten vanaf gekozen vliegveld in gekozen periode"),
                                  dataTableOutput("mytable"),
                        )
                      )),
             ) #Navbarpage
) #Fluidpage

# Define server logic ----
server <- function(input, output) {
  
  # TAB 1 -----------------
  
  # Bar graph van passagierscapaciteit van vertrekkende vluchten voor geselecteerde data en vliegvelden
  output$histogram <- renderPlot({
    if (input$airport_1 == "alle vliegvelden") {
      flights %>% filter(date >= input$daterange_1[1] & date <= input$daterange_1[2]) %>%
        drop_na(seats) %>%
        ggplot(aes_string(input$barchoice, "seats")) + geom_bar(stat = "identity", fill = dittocolors[2]) + 
        theme_classic() + labs(y="Passagiercapaciteit", x = "Data", title = "") + scale_x_discrete(guide = guide_axis(angle = 45))
    } else {
    flights %>% filter(origin %in% input$airport_1 & date >= input$daterange_1[1] & date <= input$daterange_1[2]) %>%
        drop_na(seats) %>%
      ggplot(aes_string(input$barchoice, "seats")) + geom_bar(stat = "identity", fill = dittocolors[2]) + 
      theme_classic() + labs(y="Passagiercapaciteit", x = "Data", title = "") + scale_x_discrete(guide = guide_axis(angle = 45))
    }
  })
  
  
  # TAB 2 -----------------
  
  # Scatterplot 1: correlatie tussen arr_delay en verschillende factoren (input$variable)
  output$scatter1 <- renderPlot({
    if (input$radio == "") {
    flights %>% drop_na(input$variable, arr_delay) %>%
        ggplot(aes_string(input$variable, "arr_delay")) + geom_point(alpha = 0.5) +
        geom_smooth(method=lm, se=F, fullrange=T) + # regression line
        scale_color_manual(values = dittocolors) + theme_classic() +
        labs(y="Aankomstvertraging in minuten", x = input$variable, title = "")
    } else {
      flights %>% drop_na(input$variable, arr_delay) %>%
        ggplot(aes_string(input$variable, "arr_delay", color = input$radio)) + geom_point(alpha = 0.5) +
        geom_smooth(method=lm, se=F, fullrange=T) + # regression line
        scale_color_manual(values = dittocolors) + theme_classic() +
        labs(y="Aankomstvertraging in minuten", x = input$variable, title = "")
    }
  })
  
  # Scatterplot 2: correlatie tussen dep_delay en verschillende factoren (input$variable)
  output$scatter2 <- renderPlot({
    if (input$radio == "") {
      flights %>% drop_na(input$variable, dep_delay) %>%
        ggplot(aes_string(input$variable, "dep_delay")) + geom_point(alpha = 0.5) +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + # regression line
        scale_color_manual(values = dittocolors) + theme_classic() +
        labs(y="Vertrekvertraging in minuten", x = input$variable, title = "")
    } else {
      flights %>% drop_na(input$variable, dep_delay) %>%
        ggplot(aes_string(input$variable, "dep_delay", color = input$radio)) + geom_point(alpha = 0.5) +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + # regression line
        scale_color_manual(values = dittocolors) + theme_classic() +
        labs(y="Vertrekvertraging in minuten", x = input$variable, title = "")
    }
  })
  
  # Tabel met vluchtinformatie van geselecteerde punten
  output$data <- renderTable({
    brushedPoints(flights %>% select(date, flight, origin, dest, carrier, arr_delay, dep_delay, input$variable), input$plot_brush)
  })
  
  # TAB 3 -----------------
  
  # Bar graph met bestemmingen van vluchten in geselecteerde periode, gesorteerd hoog-laag
  output$bestemming <- renderPlot({
    flights %>% filter(date >= input$daterange_3[1] & date <= input$daterange_3[2]) %>%
      group_by(dest) %>% summarise(count = n()) %>%
      ggplot(aes(reorder(dest,(count)), y = count), ) + geom_bar(stat = "identity", fill = dittocolors[3]) + 
      theme_classic() + labs(y="Aantal vluchten", x = "Bestemming", title = "") + coord_flip()
  }, height = 800)
  
  # TAB 4 -----------------
  
  # Overzichtstabel met volledige data - voor sorteren en zoeken
  output$mytable <- renderDataTable({
    if (input$airport_4 == "alle vliegvelden") {
      flights %>% filter(date >= input$daterange_4[1] & date <= input$daterange_4[2]) %>% 
        select(date, flight, origin, dest, temp, dewp, humid, wind_dir, wind_speed, wind_gust, precip, pressure, visib)
    } else {
      flights %>% filter(origin %in% input$airport_4 & date >= input$daterange_4[1] & date <= input$daterange_4[2]) %>% 
        select(date, flight, origin, dest, temp, dewp, humid, wind_dir, wind_speed, wind_gust, precip, pressure, visib)
    }
  })

} #server

# Run the app ----
shinyApp(ui = ui, server = server)


