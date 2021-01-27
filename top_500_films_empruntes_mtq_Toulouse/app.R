library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

movies <- as_tibble(read.csv("../DATA/top-500-des-films-les-plus-empruntes-a-la-bibliotheque-de-toulouse.csv",
                             sep=";"))

total_prets <- movies %>%
  group_by(TITRE) %>%
  summarise(
    n=n(),
    sum_prets = sum(nb_prets, na.rm=T))
total_prets <- total_prets %>% slice_max(sum_prets,n=500)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "red",
    
    ###Header
    dashboardHeader(
      
    ),
    
    ##Sidebar
    sidebar <- dashboardSidebar(
      sidebarMenu(
        menuItem("Top prêts", tabName = "Charts2", icon = icon("bar-chart-o")),
        menuItem("Top prêt par année", tabName = "Charts", icon = icon("bar-chart-o")),
        menuItem("Pret vs variables", tabName="explo",icon = icon("widgets"))
       )
    ),
    
    ###Body
    
    Body <- dashboardBody(
              tabItems(
        
              ###Charts by years
                tabItem(tabName = "Charts",
                            h2("Film les plus empruntés par année"),
                            fluidRow(
                              column(12,
                                     box(selectInput("ANNEE", "Sélectionnez une année :",choices = seq(2011,2018), selected = 2018),width=2),
                                     infoBoxOutput("filmempruntesparannees",width=3),
                                     infoBoxOutput("prctfilmparannee", width=3)
                              )
                            ),  
                            fluidRow(
                              column(12,
                                box(plotOutput(outputId = "barplot_peryr"),width=7),
                                    box(plotOutput("totpretfilmparanees"),width=2)
                              
                              )
                            )  ,
                            fluidRow(
                              column(4,
                                sliderInput("nbfilms","Nombre de films à afficher :", min = 1, max = 500, value = 10)
                                )
                            )
                  ),
               ###Charts total      
                  tabItem(tabName = "Charts2",
                          h2("Films les plus empruntés à la médiathèque de Toulouse entre 2011 et 2018"),
                          fluidRow(
                            column(12,
                                   infoBox("Total des films empruntés",format(sum(movies$nb_prets),big.mark = " "), fill = TRUE,width=3),
                                   infoBoxOutput("prctfilmtot", width=3)),
                            column(12,
                                   box(plotOutput(outputId = "barplottot"))
                            )
                          ),
                          fluidRow(
                            column(4,
                                  sliderInput("nbfilmstot","Nombre de films à afficher :", min = 1, max = 500, value = 10)
                            )
                          )
                       
                  ),
               
               ##Table par réal/studio
               tabItem(tabName = "explo",
                   selectInput("var", "Choisisez une variable :", c("Réalisateur"="AUTEUR","Studio"="Editeur")),
                   DT::dataTableOutput(outputId = "moviestable")
               )
            )
    ))



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  movies_subset <- reactive({
      #req(input$ANNEE) # ensure availability of value before proceeding
      movies %>% filter(ANNEE == input$ANNEE) %>%
        arrange(desc(nb_prets))%>%
        slice_max(nb_prets,n=input$nbfilms) 
      
  })
  
  total_prets_subset <- reactive({
   total_prets %>% 
      slice_max(sum_prets,n=input$nbfilmstot) 
    
  })
  
##Onglet "Top prêts"
  
  output$barplottot <- renderPlot({
    ggplot(total_prets_subset(), aes(reorder(TITRE,sum_prets), sum_prets))+ 
      geom_bar(stat="identity",fill="#F39C15",width=0.5) + coord_flip() +
      theme_light() + xlab("") + ylab("Nombre de prêts")+
      theme(axis.text = element_text(face="bold", size=13),
            axis.title = element_text(face="bold",size=15)) 
  }) 
  
  output$prctfilmtot <- renderInfoBox({
    infoBox(
      paste("Ces ", input$nbfilmstot, " films représentent"), 
      paste(round(sum(total_prets_subset()$sum_prets)/sum(total_prets$sum_prets)*100,1), "% du top 500"), 
      icon = icon("list"),
      fill = TRUE
    )
  })  
  
##Onglet "Top pret par année"
  
  output$barplot_peryr <- renderPlot({
    
    ggplot(movies_subset(), aes(reorder(TITRE,nb_prets), nb_prets)) + 
      geom_bar(stat="identity",fill="#F39C15") + coord_flip() +
      theme_light() + xlab("") + ylab("Nombre de prêts") +
      theme(axis.text = element_text(face="bold", size=13),
            axis.title = element_text(face="bold",size=15))
  })

  
  output$filmempruntesparannees <- renderInfoBox({
    infoBox(
      paste("Film empruntés en ", input$ANNEE), format(sum(movies$nb_prets[which(movies$ANNEE == input$ANNEE)]), big.mark = " "), icon = icon("list"),
      fill = TRUE
    )
  })
  
  output$prctfilmparannee <- renderInfoBox({
    infoBox(
      paste("Ces ", input$nbfilms, " films représentent"), 
      paste(round(sum(movies_subset()$nb_prets)/sum(movies$nb_prets[which(movies$ANNEE == input$ANNEE)])*100,1), "% du top 500"), 
      icon = icon("list"),
      fill = TRUE
    )
  })
  
  output$totpretfilmparanees <- renderPlot({
    ggplot(movies, aes(ANNEE, nb_prets)) + geom_bar(stat="identity", fill="lightgrey") + theme_light() + 
      geom_bar(data=movies[which(movies$ANNEE == input$ANNEE),], aes(ANNEE, nb_prets), stat="identity", fill="#21BBEA") +
      geom_bar(data=movies_subset(), aes(ANNEE, nb_prets), stat="identity", fill="#0399BE") +
      xlab("") + ylab("nombre total de films empruntés") + scale_x_continuous(breaks = seq(2011,2018)) +
      theme(axis.text.x = element_text(face="bold", size=12, angle=45),panel.background = element_rect(fill = "#EBEFF4"))
  })
  
###Onglet
  
  table_par_var <- reactive({movies %>% 
      group_by(input$var) %>% 
      summarise(nb_pret=sum(nb_prets)) %>% 
      arrange(desc(nb_pret)) %>%
      mutate(prct=nb_pret/sum(nb_pret)*100)
  })
  
  output$moviestable <- DT::renderDataTable(
    {DT::datatable(data = table_par_var(), 
                    #options = list(pageLength = 10), 
                    rownames = FALSE)
    })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
