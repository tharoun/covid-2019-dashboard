##########################################################################
#                  An R/shiny app to monitoring the evolution            #
#            of covid 19 in burkina faso using R/Rstudio                 #                     
#                        by: Harouna TRAORE                              #
##########################################################################



library(shinydashboard)
library(shiny)
library(ggplot2)
library(leaflet)
library(plotly)
library(leaflet)
library(readxl)

################## Importation de la base ###########"""
dataBase<- read_excel("Covid19-bf.xlsx",col_types = c("date", "numeric", "numeric", 
                                                            "numeric", "numeric","numeric","numeric","numeric"))

dataBase$date <- format(dataBase$date,"%m/%d")

#base des villes
villes <- read_excel("base_ville.xlsx")
#######
couleurs <- colorNumeric("Spectral", villes$cas ,n=3)



####### Calculs statistiques #########################

# Populaion of Burkina Faso
pop <- 18000000 
# number of confirmed cases
conf <-dataBase$Total[length(dataBase$Total)] 
conf1 <- dataBase$Total[length(dataBase$Total)-1]
# number of active cases
act <- dataBase$Actifs[length(dataBase$Actifs)]
# =number of healings
guer <- dataBase$Guerison[length(dataBase$Guerison)]
#number of deaths
dec <- dataBase$Deces[length(dataBase$Deces)]
# healings rate
taux_gue <- round((guer/conf)*100,digits = 2)
#deaths rate
taux_let <- round((dec/conf)*100,digits = 2)
#penetration rate
taux_pen <- round((conf/pop)*100,digits = 8)
# number of men
nombre_homme <- round((dataBase$homme[length(dataBase$homme)])*100/conf,digits = 2)
#Number of women 
nombre_femme <- round((dataBase$femme[length(dataBase$femme)])*100/conf,digits = 2)



## Retrieving the current date
auj<- Sys.Date()
auj <- toString(auj)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    title = "Covid19-BF",
    skin = "blue",
    ## header settings
    dashboardHeader(
        title="Covid19-BF",
        titleWidth = 250,
        ## MENU creation in the header
        dropdownMenu(type = "messages",
                     #Adding a tab for my contacts
                     messageItem(
                         from = "Contacts",
                         message="tharouna90@gmail.com"
                     )
        )
    ),
    ####### End of header #########
    
    ### Creation of the sidebar ######
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Recap", tabName = "recap", icon = icon("dashboard"),badgeLabel =auj,badgeColor ="green"),
            menuItem("Health statistics", tabName = "stats", icon = icon("chart-pie"))

            
            
        )
        
    ),
    
    ##### Creation of the body of the application ###
    
    dashboardBody(
        ## Adding the contents of each menu
        tabItems(
            
            #### Contents of the recap menu#####
            tabItem(tabName="recap",
                    #Title of the Menu
                    headerPanel(tags$b("Situation of Covid 19 in Burkina Faso")),
                    # Disease status
                    fluidRow(
                      #Grouping of objects
                      box(
                        
                        infoBox("CONFIRMED", conf,icon=icon("user-md"),width = 5,color = "blue",subtitle="Cumulative confirmed cases"),
                        
                        infoBox("ACTIVES",act,icon=icon("hospital"),width = 5,color = "orange",subtitle = "Cases in progress"),
                        
                        infoBox("HEALED",guer,icon=icon("child"),width = 5,color = "green",subtitle = "Cases declared cured"),
                        
                        infoBox("DEATH",dec,icon=icon("sad-cry"),width = 5,color = "red",subtitle = "Cases of death"),
                        #Propiétés du box
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "primary",
                        width = 12)
                    ),
                    ###########
                    
                    fluidRow( 
                      box(
                        title = "Daily evolution of confirmed cases and deaths", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("plot", height = 250)
                      ),
                      box(
                        title = "Evolution of new cases and recoveries", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("plot1", height = 250)
                        
                      )
                      
                    ),
                    ############# Cartographie##############"
                    fluidRow(
                      
                      box(
                        title = "Mapping of confirmed cases",
                        leafletOutput('carte', width = "100%", height = 400),
                        status = "primary",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        width = 12
                      )
                    )
                    
                
            ),
            ## Statistiques sanitaire
            tabItem(tabName = "stats",
                    headerPanel(tags$b("Health statistics on Covid 19 in Burkina Faso")),
                    
                    fluidRow(
              
                      
                      box(
                        infoBox("HEALING RATE",paste0(taux_gue, "%"),icon=icon("child"),width = 5,color = "green"),
                        infoBox("LETHALITY RATE",paste0(taux_let, "%"),icon=icon("sad-cry"),width = 5,color = "red"),
                        infoBox("ATTACK RATE",paste0(taux_pen, "%"),icon=icon("exchange-alt"),width = 5,color = "purple"),
                        infoBox("CONTAMINATED PART OF WOMEN INFECTED",paste0(nombre_femme,"%"),icon=icon("venus"),width = 5,color = "fuchsia"),
                        infoBox("CONTAMINATED PART OF MAN",paste0(nombre_homme,"%"),icon=icon("mars"),width = 5,color = "olive"),

                        collapsible = TRUE,
                        solidHeader = TRUE,
                        status = "primary",
                        width = 12)
                    )
                    
                    
            )
            
            
            
            
            
            
        )
        
        ##############
        
        
    )
)
  ### End of creation ##########  
    


#### Conception du server
server <- function(input, output) {

  ## Curve of evolution of the number of deaths
  output$plot<-renderPlotly(
    { 
      courbeTotal <- plot_ly(dataBase, x = ~date,y = ~Total, name = "Confirmed", type = 'scatter', mode = 'lines+markers',color = I("blue")) 
      courbeTotal <- courbeTotal %>% add_trace(y = ~Actifs, name = 'Actives', mode = 'lines+markers',color = I("orange")) 
      courbeTotal <- courbeTotal %>% add_trace(y = ~Deces, name = "Deaths", mode = 'lines+markers',color = I("red"))

    }
  ) 
  #### Evolution curve of new cases ####
  output$plot1 <- renderPlotly(
    {
      courbeNew <- plot_ly(dataBase, x = ~date, y = ~Guerison, name = "Daily healing", type = 'scatter', mode = 'lines+markers',color = I("green")) 
      courbeNew <- courbeNew %>% add_trace(y = ~nouveau, name = 'New cases', mode = 'lines+markers',color = I("maroon")) 
      
    }
    
  )
  
  ############### Presentation of the map ########
  output$carte <- renderLeaflet(
    
    # https://www.coordonnees-gps.fr/
    
    {
      leaflet(villes) %>% addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   radius =~sqrt(cas)*2000 , popup = ~paste(villes,":",cas),
                   color =~couleurs(cas), fillOpacity = 0.5,label = ~paste(ville,":",cas))%>%
        addLegend(pal = couleurs, values = ~cas, opacity = 0.5)
      
    }
    
  )
  
  
    
}

# Execution de l'application 
shinyApp(ui = ui, server = server)
