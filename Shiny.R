#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("ggpubr")
#install.packages("shiny")

data("swiss")
data("mtcars")
data("co2")
data("PlantGrowth")
data("Orange")
data("quakes")


#KLASYCZNE OBLICZENIE MDS
licz_klasycznie<-function(dane,kl){
  library(magrittr)
  library(dplyr)
  library(ggpubr)
  #Obliczenie MDS przy pomocy cmdsl
  mds <- dane %>%
    dist() %>%          
    cmdscale() %>%
    as_tibble()
  colnames(mds) <- c("Wymiar 1", "Wymiar 2")
  #PODZIAŁ NA KLASTRY METODA K-SREDNICH
  klastry <- kmeans(mds, kl)$cluster %>%
    as.factor()
  mds <- mds %>%
    mutate(groups = klastry)
  #WYKRES Z PODZIALEM NA GRUPY
  wykres<-ggscatter(mds, x = "Wymiar 1", y = "Wymiar 2", 
                    label = rownames(dane),
                    color = "groups",
                    palette = "jco",
                    size = 1, 
                    ellipse = TRUE,
                    ellipse.type = "convex",
                    repel = TRUE,title = "Wyniki analizy MDS z podzialem na grupy")+theme(plot.title = element_text(hjust = 0.5))
  return(wykres)
}
# METODA KRUSKALA
licz_nieparametrycznie<-function(dane, kl){
  library(magrittr)
  library(dplyr)
  library(ggpubr)
  # OBLICZENIE MDS PRZY POMOCY isoMDS()
  library(MASS)
  mds <- dane %>%
    dist() %>%          
    isoMDS() %>%
    .$points %>%
    as_tibble()
  colnames(mds) <- c("Wymiar 1", "Wymiar 2")
  #PODZIAŁ NA KLASTRY METODA K-SREDNICH
  klastry <- kmeans(mds, kl)$cluster %>%
    as.factor()
  mds <- mds %>%
    mutate(groups = klastry)
  #WYKRES Z PODZIALEM NA GRUPY
  wykres<-ggscatter(mds, x = "Wymiar 1", y = "Wymiar 2", 
                    label = rownames(dane),
                    color = "groups",
                    palette = "jco",
                    size = 1, 
                    ellipse = TRUE,
                    ellipse.type = "convex",
                    repel = TRUE,
                    title = "Wyniki analizy MDS z podzialem na grupy")+theme(plot.title = element_text(hjust = 0.5))
  return(wykres)
}

# WIZUALIZACJA MACIERZY KORELACJI
macierz_korelacji<-function(dane){
  res.cor <- cor(dane, method = "spearman")
  mds.cor <- (1 - res.cor) %>%
    cmdscale() %>%
    as_tibble()
  colnames(mds.cor) <- c("Wymiar 1", "Wymiar 2")
  wykres<-ggscatter(mds.cor, x = "Wymiar 1", y = "Wymiar 2", 
                    size = 1,
                    label = colnames(res.cor),
                    repel = TRUE,
                    title = "Macierz korelacji zmiennych"
  )+theme(plot.title = element_text(hjust = 0.5))## Wycentrowanie tytulu
  return(wykres)
}




#SHINY 
library(shiny)
library(leaflet)
library(RColorBrewer)
  ui <- fluidPage(
    includeCSS("www/app_ad.css"),
    titlePanel("ANALIZA WIELOWYMIAROWA"),
    

    sidebarLayout(
      

      sidebarPanel(

        sliderInput("klastry", "Ilosc grup na ktore dzielimy podobne dane:",
                    min = 1, max = 10,
                    value = 1, step = 1,
                     sep = ",",
                    animate = FALSE),
        
      ),
      

      mainPanel(
        selectInput("select", label = h3("Wybor zestawu danych z wbudowanych w R"), 
                    choices = list("Zestaw 1" = 1, "Zestaw 2" =2, "Zestaw 3" = 3), 
                    selected = 1),
        
        hr(),
        fluidRow(column(3, verbatimTextOutput("value"))),

        plotOutput("plot1"),
        plotOutput("plot2")
      ),
      fluid=T, position="right"
    )
  )

server <- function(input, output) {
  
  dane<-reactive({
    wybor_danych(input$select)
  })
  output$plot1 <- renderPlot({
    if(input$select==1){
      licz_klasycznie(mtcars,input$klastry)
    }
    else if(input$select==2){
      licz_klasycznie(swiss,input$klastry)
    }
    else if(input$select==3){
      licz_klasycznie(quakes,input$klastry)
    }
  })
  output$plot2 <- renderPlot({
    if(input$select==1){
      macierz_korelacji(mtcars)
    }
    else if(input$select==2){
      macierz_korelacji(swiss)
    }
    else if(input$select==3){
      macierz_korelacji(quakes)
    }
    
  })
  
}
shinyApp(ui, server)
