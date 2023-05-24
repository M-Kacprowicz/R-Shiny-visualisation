library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(forcats)
library(scatterplot3d)

load("przejazdy.RData")
options(shiny.autoreload = TRUE) #odświeżanie przy zapisie

# Definiowanie interfejsu użytkownika

ui <- fluidPage(
  # Tytuł
  h1("Projekt zaliczeniowy",
     align = "center"),
  
  tags$style(".my-fluid-row { height: 500px; }"),
  # Pierwszy wiersz z wykresem i tabelą
  fluidRow(
    class = "my-fluid-row",
    column(
    width = 10,
    offset = 1,
    div(style = "height: 500px;", 
        plotOutput(outputId = "wykres"))
  )),
  
  hr(),
  #linia oddzielająca wiersz
  
  # Drugi wiersz z elementami sterującymi
  
  fluidRow(
    column(
      width = 3,
      selectInput(
        inputId = "Zadanie",
        label = h3("Numer zadania"),
        choices = list(
          "Zadanie 1",
          "Zadanie 2",
          "Zadanie 3",
          "Zadanie 4",
          "Zadanie 5",
          "Zadanie 6",
          "Zadanie 7"
        )
      )
    ),
    
    conditionalPanel(condition = "input.Zadanie == 'Zadanie 2'",
                     column(
                       width = 3,
                       label = h3("Opcja dla Zadanie 2"),
                       selectInput(
                         inputId = "stacje_zad2",
                         label = "Wybierz stację:",
                         choices = list(
                           "Pas Nadmorski",
                           "al. Zwycięstwa",
                           "ul. 3 Maja",
                           "al. Grunwaldzka (Wrzeszcz)",
                           "Błędnik",
                           "al. Hallera",
                           "ul. Chłopska",
                           "al. Grunwaldzka (UG)",
                           "ul. Kartuska",
                           "Kanał Raduni",
                           "al. Rzeczpospolitej",
                           "ul. Kołobrzeska",
                           "al. Havla",
                           "ul. Łostowicka",
                           "ul. Jaśkowa Dolina",
                           "ul. Nowolipie",
                           "ul. Kliniczna",
                           "ul. Wyzwolenia",
                           "ul. Rybińskiego",
                           "al. Żołnierzy Wyklętych",
                           "ul. Stryjewskiego",
                           "ul. Wita Stwosza",
                           "al. Jana Pawła II",
                           "ul. Sucharskiego",
                           "ul. Elbląska",
                           "Karczemki",
                           "ul. Słowackiego (Matarnia)"
                         )
                       )
                     )),
    conditionalPanel(condition = "input.Zadanie == 'Zadanie 3'",
                     column(
                       width = 3,
                       label = h3("Opcja dla Zadanie 3"),
                       selectInput(
                         inputId = "rok_zad3",
                         label = "Wybierz rok:",
                         choices = list(
                           "2013",
                           "2014",
                           "2015",
                           "2016",
                           "2017",
                           "2018",
                           "2019",
                           "2020",
                           "2021"
                         )
                       )
                     )),
    conditionalPanel(
      condition = "input.Zadanie == 'Zadanie 4'",
      column(
        width = 3,
        label = h3("Okres"),
        radioButtons(
          inputId = "okres_zad4",
          label = "Wybierz okres analizy:",
          choices = c("Miesiące",
                      "Dni tygodnia",
                      "Dni powszednie",
                      "Weekendy")
        )
      ),
      column(
        width = 3,
        label = h3("Stacja dla zadanie 4"),
        selectInput(
          inputId = "stacja_zad4",
          label = "Wybierz stację:",
          choices = list(
            "Pas Nadmorski",
            "al. Zwycięstwa",
            "ul. 3 Maja",
            "al. Grunwaldzka (Wrzeszcz)",
            "Błędnik",
            "al. Hallera",
            "ul. Chłopska",
            "al. Grunwaldzka (UG)",
            "ul. Kartuska",
            "Kanał Raduni",
            "al. Rzeczpospolitej",
            "ul. Kołobrzeska",
            "al. Havla",
            "ul. Łostowicka",
            "ul. Jaśkowa Dolina",
            "ul. Nowolipie",
            "ul. Kliniczna",
            "ul. Wyzwolenia",
            "ul. Rybińskiego",
            "al. Żołnierzy Wyklętych",
            "ul. Stryjewskiego",
            "ul. Wita Stwosza",
            "al. Jana Pawła II",
            "ul. Sucharskiego",
            "ul. Elbląska",
            "Karczemki",
            "ul. Słowackiego (Matarnia)"
          )
        )
      )
    ),
    conditionalPanel(condition = "input.Zadanie == 'Zadanie 5'",
                     column(
                       width = 3,
                       label = h3("Okres"),
                       radioButtons(
                         inputId = "okres_zad5",
                         label = "Wybierz okres analizy:",
                         choices = c("Miesiące",
                                     "Dni tygodnia",
                                     "Dni powszednie",
                                     "Weekendy")
                       )
                     )),
    conditionalPanel(condition = "input.Zadanie == 'Zadanie 6'",
                     column(
                       width = 3,
                       label = h3("Warunek_1"),
                       radioButtons(
                         inputId = "warunek1_zad6",
                         label = "Wybierz pierwszy warunek pogodowy:",
                         choices = c("Zachmurzenie",
                                     "Wiatr",
                                     "Temperatura",
                                     "Ciśnienie_woda",
                                     "Wilgotność",
                                     "Ciśnienie_stacja",
                                     "Ciśnienie_morze",
                                     "Opady_dzień",
                                     "Opady_noc")
                       )
                     ),
                     column(
                       width = 3,
                       label = h3("Warunek_2"),
                       radioButtons(
                         inputId = "warunek2_zad6",
                         label = "Wybierz drugi warunek pogodowy:",
                         choices = c("Zachmurzenie",
                                     "Wiatr",
                                     "Temperatura",
                                     "Ciśnienie_woda",
                                     "Wilgotność",
                                     "Ciśnienie_stacja",
                                     "Ciśnienie_morze",
                                     "Opady_dzień",
                                     "Opady_noc")
                       )
                     )),
    conditionalPanel(condition = "input.Zadanie == 'Zadanie 7'",
                     column(
                       width = 3,
                       label = h3("Warunek_1"),
                       radioButtons(
                         inputId = "warunek1_zad7",
                         label = "Wybierz pierwszy warunek pogodowy:",
                         choices = c("Zachmurzenie",
                                     "Wiatr",
                                     "Temperatura",
                                     "Ciśnienie_woda",
                                     "Wilgotność",
                                     "Ciśnienie_stacja",
                                     "Ciśnienie_morze",
                                     "Opady_dzień",
                                     "Opady_noc")
                       )
                     ),
                     column(
                       width = 3,
                       label = h3("Warunek_2"),
                       radioButtons(
                         inputId = "warunek2_zad7",
                         label = "Wybierz drugi warunek pogodowy:",
                         choices = c("Zachmurzenie",
                                     "Wiatr",
                                     "Temperatura",
                                     "Ciśnienie_woda",
                                     "Wilgotność",
                                     "Ciśnienie_stacja",
                                     "Ciśnienie_morze",
                                     "Opady_dzień",
                                     "Opady_noc")
                       )
                     ))
  )
)

# Definiowanie logiki działania R

server <- function(input, output) {
  liczba_dni <- przejazdy %>%
    group_by(Stacja) %>%
    summarise(liczba_dni = n_distinct(Data))
  
  output$wykres <- renderPlot(expr = {
    if (input$Zadanie == "Zadanie 1") {
      wykresik <-
        ggplot(data = liczba_dni, aes(x = reorder(Stacja, liczba_dni), y = liczba_dni)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_text(
          aes(label = liczba_dni),
          vjust = 0.2,
          hjust = 2,
          color = "black",
          angle = 90,
          size = 5
        ) +
        labs(x = "Punkt pomiarowy", y = "Liczba dni pomiarowych") +
        ggtitle("Rozkład liczby dni pomiarowych w poszczególnych punktach") +
        theme_light() + theme(plot.title = element_text(face = "bold", size = 15)) +
        theme(
          plot.subtitle = element_text(size = 10),
          axis.text.x = element_text(
            angle = 60,
            hjust = 1,
            size = 12
          )
        )
    }
    
    if (input$Zadanie == "Zadanie 2") {
      dane_filtr_2 <- przejazdy %>% filter(Stacja == input$stacje_zad2)
      wykresik <-
        ggplot(data = dane_filtr_2, aes(x = Data, y = Licznik)) +
        geom_line(size = 1L, colour = "#cb181d") +
        theme_light() + theme(plot.title = element_text(face = "bold", size = 15)) +
        theme(plot.subtitle = element_text(size = 10)) +
        labs(x = "Data", y = "Liczba przejazdów") +
        ggtitle("Rozkład liczby przejazdów dla wybranego punktu") +
        theme(axis.text.x = element_text(
          angle = 90,
          hjust = 0,
          size = 12
        ))
    }
    
    if (input$Zadanie == "Zadanie 3") {
      dane_filtr_3 <- przejazdy %>% filter(year(Data) == input$rok_zad3)
      wykresik <-
        ggplot(data = dane_filtr_3, aes(x = Stacja, y = Data, fill = Licznik)) +
        theme_minimal() +
        scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
        geom_tile() +
        labs(x = "Stacja", y = "Data", fill = "Licznik") +
        ggtitle("Heatmapa natężenia przejazdów na poszczególnych stacjach w danym roku") +
        theme(
          axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5,
            size = 10
          ),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position = "right",
          panel.grid = element_blank()
        ) +
        guides(fill = guide_colourbar(barwidth = 0.5,
                                      barheight = 20))
    }
    
    if (input$Zadanie == "Zadanie 4") {
      if (input$okres_zad4 == "Miesiące") {
        dane_filtr_4 <- przejazdy %>% filter(Stacja == input$stacja_zad4)
        wykresik <-
          ggplot(data = dane_filtr_4, aes(x = month(Data), y = Licznik)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(x = "Miesiąc", y = "Liczba przejazdów") +
          ggtitle("Rozkład przejazdów dla wybranej stacji i okresu") +
          theme_light() + theme(plot.title = element_text(face = "bold", size = 15)) +
          theme(
            plot.subtitle = element_text(size = 10),
            axis.text.x = element_text(
              angle = 60,
              hjust = 1,
              size = 12
            )
          ) +
          scale_x_continuous(breaks = 1:12, labels = month.name[1:12])
      }
      if (input$okres_zad4 == "Dni tygodnia") {
        dane_filtr_4 <- przejazdy %>% filter(Stacja == input$stacja_zad4)
        wykresik <-
          ggplot(data = dane_filtr_4, aes(x = wday(Data), y = Licznik)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(x = "Dzień tygodnia", y = "Liczba przejazdów") +
          ggtitle("Rozkład przejazdów dla wybranej stacji i okresu") +
          theme_light() + theme(plot.title = element_text(face = "bold", size = 15)) +
          theme(
            plot.subtitle = element_text(size = 10),
            axis.text.x = element_text(
              angle = 60,
              hjust = 1,
              size = 12
            )
          ) +
          scale_x_continuous(
            breaks = 1:7,
            labels = c(
              "Niedziela",
              "Poniedziałek",
              "Wtorek",
              "Środa",
              "Czwartek",
              "Piątek",
              "Sobota"
            )
          )
      }
      if (input$okres_zad4 == "Dni powszednie") {
        dane_filtr_4 <-
          przejazdy %>% filter(Stacja == input$stacja_zad4) %>% filter(wday(Data) %in% c(2:6))
        wykresik <-
          ggplot(data = dane_filtr_4, aes(x = wday(Data), y = Licznik)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(x = "Dzień tygodnia", y = "Liczba przejazdów") +
          ggtitle("Rozkład przejazdów dla wybranej stacji i okresu") +
          theme_light() + theme(plot.title = element_text(face = "bold", size = 15)) +
          theme(
            plot.subtitle = element_text(size = 10),
            axis.text.x = element_text(
              angle = 60,
              hjust = 1,
              size = 12
            )
          ) +
          scale_x_continuous(
            breaks = c(2:6),
            labels = c(
              "Poniedziałek",
              "Wtorek",
              "Środa",
              "Czwartek",
              "Piątek"
            )
          )
      }
      if (input$okres_zad4 == "Weekendy") {
        dane_filtr_4 <-
          przejazdy %>% filter(Stacja == input$stacja_zad4) %>% filter(wday(Data) %in% c(1, 7))
        wykresik <-
          ggplot(data = dane_filtr_4, aes(x = wday(Data), y = Licznik)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(x = "Dzień tygodnia", y = "Liczba przejazdów") +
          ggtitle("Rozkład przejazdów dla wybranej stacji i okresu") +
          theme_light() + theme(plot.title = element_text(face = "bold", size = 15)) +
          theme(
            plot.subtitle = element_text(size = 10),
            axis.text.x = element_text(
              angle = 60,
              hjust = 1,
              size = 12
            )
          ) +
          scale_x_continuous(breaks = c(7, 1),
                             labels = c("Sobota", "Niedziela"))
      }
      
      
      
    }
    
    if (input$Zadanie == "Zadanie 5") {
      if (input$okres_zad5 == "Miesiące") {
        wykresik <-
          ggplot(data = przejazdy, aes(
            x = month(Data),
            y = Licznik,
            fill = Stacja
          )) + geom_area(color = "black", alpha = 0.7) +
          labs(x = "Data",
               y = "Licznik",
               fill = "Stacja") +
          ggtitle("Zależność między stacjami") +
          theme_minimal() +
          scale_x_continuous(breaks = 1:12, labels = month.name[1:12]) +
          theme(
            plot.title = element_text(
              size = 16,
              face = "bold",
              margin = margin(b = 10)
            ),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = "gray90"),
            panel.border = element_blank()
          )
      }
      if (input$okres_zad5 == "Dni tygodnia") {
        wykresik <-
          ggplot(data = przejazdy, aes(x = wday(Data), y = Licznik, fill = Stacja)) +
          geom_area(color = "black", alpha = 0.7) +
          labs(x = "Data",
               y = "Licznik",
               fill = "Stacja") +
          ggtitle("Zależność między stacjami") +
          theme_minimal() +
          theme(
            plot.title = element_text(
              size = 16,
              face = "bold",
              margin = margin(b = 10)
            ),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = "gray90"),
            panel.border = element_blank()
          ) +
          scale_x_continuous(
            breaks = 1:7,
            labels = c(
              "Niedziela",
              "Poniedziałek",
              "Wtorek",
              "Środa",
              "Czwartek",
              "Piątek",
              "Sobota"
            )
          )
      }
      if (input$okres_zad5 == "Dni powszednie") {
        dane_filtr_5 <- przejazdy %>% filter(wday(Data) %in% c(2:6))
        wykresik <-
          ggplot(data = dane_filtr_5, aes(x = wday(Data), y = Licznik, fill = Stacja)) +
          geom_area(color = "black", alpha = 0.7) +
          labs(x = "Data",
               y = "Licznik",
               fill = "Stacja") +
          ggtitle("Zależność między stacjami") +
          theme_minimal() +
          theme(
            plot.title = element_text(
              size = 16,
              face = "bold",
              margin = margin(b = 10)
            ),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = "gray90"),
            panel.border = element_blank()
          ) +
          scale_x_continuous(
            breaks = c(2:6),
            labels = c(
              "Poniedziałek",
              "Wtorek",
              "Środa",
              "Czwartek",
              "Piątek"
            )
          )
      }
      if (input$okres_zad5 == "Weekendy") {
        dane_filtr_5 <- przejazdy %>% filter(wday(Data) %in% c(1, 7))
        wykresik <-
          ggplot(data = dane_filtr_5, aes(x = wday(Data), y = Licznik, fill = Stacja)) +
          geom_area(color = "black", alpha = 0.7) +
          labs(x = "Data",
               y = "Licznik",
               fill = "Stacja") +
          ggtitle("Zależność między stacjami") +
          theme_minimal() +
          theme(
            plot.title = element_text(
              size = 16,
              face = "bold",
              margin = margin(b = 10)
            ),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = "gray90"),
            panel.border = element_blank()
          ) +
          scale_x_continuous(breaks = c(7, 1),
                             labels = c("Sobota", "Niedziela"))
      }
      
    }
    
    if (input$Zadanie == "Zadanie 6") {
      wykresik <- 
        ggplot(data = przejazdy, aes_string(x = input$warunek1_zad6, y = input$warunek2_zad6, size = "Licznik")) +
        geom_point(color = "blue", shape = 16) +
        labs(x = input$warunek1_zad6, y = input$warunek2_zad6, size = "Ilość przejazdów") +
        ggtitle("Natężenie przejazdów w zależności od wybranych warunków pogodowych") +
        theme_light() + 
        theme(plot.title = element_text(face = "bold", size = 15)) +
        theme(
          plot.subtitle = element_text(size = 10),
          axis.text.x = element_text(
            angle = 0,
            hjust = 1,
            size = 12
          )) +
        scale_size(range = c(1,15))
      
    }
    
    if (input$Zadanie == "Zadanie 7") {
      wykresik <- 
        ggplot(data = przejazdy, aes_string(x = input$warunek1_zad7, y = input$warunek2_zad7, size = "Licznik")) +
        geom_point(aes(color = Stacja), shape = 16) +
        labs(x = input$warunek1_zad7, y = input$warunek2_zad7, size = "Ilość przejazdów") +
        ggtitle("Natężenie przejazdów na stacjach w zależności od wybranych warunków pogodowych") +
        theme_light() + 
        theme(plot.title = element_text(face = "bold", size = 15)) +
        theme(
          plot.subtitle = element_text(size = 10),
          legend.position = "right",
          legend.box.margin = margin(t = 50, b = 10),
          axis.text.x = element_text(
            angle = 0,
            hjust = 1,
            size = 12
          )) +
        scale_size(range = c(1,15)) +
        scale_color_discrete()
      
    }
    
    wykresik
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
