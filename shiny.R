### Chargement des librairies

library(visNetwork)
library(visTree)
library(shiny)
library(rpart)
library(readxl)
library(dplyr)
library(tidytext)
library(ggplot2)
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(forcats)
library(scales)
library(circlize)
library(reshape)
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "ggplot2", "colourpicker"))

### Chargement et manipulation des données
#data <- read.table("Jeu-1-projet.csv",sep=",",header = T)
#colnames(data) <- c("ipsrc","ipdst","portdst","proto","action","date","regle")

### Définition des éléments de l'interface
ui <- dashboardPage(
  dashboardHeader(title = "Sécurité et visualisation des données"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "PagePrinc", icon = icon("home")),
                menuItem("Analyse des Flux", tabName = "flux", icon = icon("poll")),
                menuItem("CAH", tabName = "clustering", icon = icon("project-diagram")),
                menuItem("Arbre de décision", tabName = "arbre", icon = icon("tree")))
  
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "PagePrinc",
              h1("Description du Projet"),
              h1("Lecture des données"),
              fileInput("FileInput",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              
              h1("Données"),
              dataTableOutput('table')
      ),
      
      # visualization
      tabItem(tabName = "flux",
              h1("Exploration des variables"),
              fluidRow(
                tabBox(
                  width = 12,
                  height = "550px",
                  tabPanel(
                    title = "Top adresses et ports",
                    box(
                      title = textOutput("titre5"),
                      width = 8,
                      plotlyOutput("graphique_top")
                    ),
                    box(
                      title = "Variables à explorer",
                      width = 4,
                      radioButtons("choix_top", label = h3("Choix de l'Ip"),
                                   choices = list("Ip source" = "ipsrc", "port" = "portdst")),
                      sliderTextInput("Top_n", "nombre de top:", c(3, 4, 5, 6, 7,8,9,10)),
                      radioButtons("choix_proto", label = h3("Choix du protocole"),
                                   choices = list("TCP et UDP"="both","TCP" = "TCP", "UDP" = "UDP" )),
                      radioButtons("choix_port", label = h3("Choix du port"),
                                   choices = list("Tous les ports"="both","Inférieur ou égal à 1024" = "inf1024", "Supérieur 1024" = "sup1024"))
                      )
                    ),
                  tabPanel(
                    title = "UDP vs TCP",
                    box(
                      title = textOutput("titre6"),
                      width = 8,
                      plotOutput("graphique_proto")
                    ),
                    box(
                      title = "Variables à explorer",
                      width = 4,
                      radioButtons("choix_action", label = h3("Choix de l'action"),
                                   choices = list("Les deux"="both","Accepté" = "Permit", "Refusé" = "Deny"))
                  )),
                  tabPanel(
                    title = "Règles",
                    box(
                      title = textOutput("titre"),
                      width = 8,
                      plotlyOutput("graphique_regles")
                    ),
                    box(
                      title = "Variables à explorer",
                      width = 4,
                      radioButtons("choix_action_regle", label = h3("Choix de l'action"),
                                   choices = list("Les deux"="both","Accepté" = "Permit", "Refusé" = "Deny"))
                    )
                  ),
                  tabPanel(
                    title = "Université",
                    box(
                           width = 8,
                           plotlyOutput("graphique_universite")
                    ),
                    box(
                      title = "Variables à explorer",
                      width = 4,
                      sliderTextInput("nb_univ", "nombre de top:", c(3, 4, 5, 6, 7,8,9,10))
                    )
                ),
                tabPanel(
                  title = "Flux",
                  box(
                    title = textOutput("titre16"),
                    width = 8,
                    plotOutput("graphique_flux")
                  ),
                  box(
                    title = "Variables à explorer",
                    width = 4,
                    sliderTextInput("nb_add", "nombre de top:", c(3, 4, 5, 6, 7,8,9,10)),
                    #radioButtons("choix_proto_flux", label = h3("Choix du protocole"),
                                # choices = list("TCP" = "TCP", "UDP" = "UDP", "TCP et UDP"="both")),
                    #radioButtons("choix_port_add", label = h3("Choix du port"),
                               #  choices = list("Inférieur ou égal à 1024" = "inf1024", "Supérieur 1024" = "sup1024", "Tous les ports"="both"))
                  ))
              ))),
      tabItem(tabName = "clustering",
              h1("CAH"),
              sliderInput("clusters", "Nombre de cluster:", 
                          min = 2,        # 1 cluster is pointless
                          max = 10,       # too many is too crowded
                          value = 4),    # sensible start
              # radioButtons("proto", "Protocole :",
              #              c("TCP" = "tcp",
              #                "UDP" = "udp",
              #                "TCP & UDP" = "tcpudp")),
              # radioButtons("ports", "Ports :",
              #              c("Inférieur 1024" = "inf",
              #                "Supérieur 1024" = "sup",
              #                "Tous les ports" = "touslespo")),
              plotlyOutput("distPlot"),
              textOutput("test")
      ),
      tabItem(tabName = "arbre",
              h1("Arbre de décision"),
              varSelectInput("cible", "Variables à expliquer:", data, multiple = FALSE),
              varSelectInput("expli", "Variables explicatives:", data, multiple = TRUE),
              actionButton("go", "Valider"),
              visTreeModuleUI(id = "id1", rpartParams = FALSE, visTreeParams = FALSE)
      )
    )
  )
)


### Back-end
server <- function(input, output, session){
  
  datasetInput <- reactive({
    infile <- input$FileInput
    if(is.null(infile))
      return(NULL)
    read.csv(infile$datapath, header = TRUE)
  })
  
  output$graphique_regles <-  renderPlotly({
    #datasetInput()$regle <- fct_infreq(factor(datasetInput()$regle))
    data_regle <- datasetInput()
    if (input$choix_action_regle =="both"){
      diff_action <-subset(data_regle,(data_regle$action == "Deny" |data_regle$action == "Permit" ))

    }else{
      diff_action <-subset(data_regle, data_regle$action == input$choix_action_regle)
    }
    
    p = ggplot(data = diff_action, aes(x = factor(regle), fill =factor(regle))) +
      geom_bar( show.legend = TRUE) +
      xlab("Classement des règles")
    ggplotly(p)
  })
  
  
  
  output$graphique_top <- renderPlotly({
    data_top <- datasetInput()
    if (input$choix_port == "both"){
      if (input$choix_proto =="both"){
        tab =head(n=input$Top_n,sort(table(subset(data_top,(data_top$proto == "UDP" |data_top$proto == "TCP"), select=c(input$choix_top))),decreasing = TRUE))
      }else{
        tab =head(n=input$Top_n,sort(table(subset(data_top,data_top$proto == input$choix_proto , select=c(input$choix_top))),decreasing = TRUE))
      }
    }else if(input$choix_port =="inf1024"){
      if (input$choix_proto =="both"){
        tab =head(n=input$Top_n,sort(table(subset(data_top,(data_top$proto == "UDP" |data_top$proto == "TCP" & data_top$portdst <=1024), select=c(input$choix_top))),decreasing = TRUE))
      }else{
        tab =head(n=input$Top_n,sort(table(subset(data_top,(data_top$proto == input$choix_proto & data_top$portdst <1024), select=c(input$choix_top))),decreasing = TRUE))
      }
      
    }else if(input$choix_port =="sup1024"){
    if (input$choix_proto =="both"){
      tab =head(n=input$Top_n,sort(table(subset(data_top,(data_top$proto == "UDP" |data_top$proto == "TCP" & data_top$portdst >1024), select=c(input$choix_top))),decreasing = TRUE))
    }else{
      tab =head(n=input$Top_n,sort(table(subset(data_top,(data_top$proto == input$choix_proto & data_top$portdst >1024), select=c(input$choix_top))),decreasing = TRUE))
    }
    }
    tab =as.data.frame(tab)
      p <-ggplot(data = tab, aes(x =factor(Var1), y=Freq)) +
        geom_bar(stat="identity", fill="steelblue")+
        geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
        theme_minimal()

      ggplotly(p)
    })
  output$graphique_proto <- renderPlot({
    data_proto <- datasetInput()
    if (input$choix_action =="both"){
      diff_proto <-table(subset(data_proto$proto, (data_proto$action == "Deny" | data_proto$action == "Permit")))
      df_diff_proto <-as.data.frame(diff_proto)
    }else{
      diff_proto <-table(subset(data_proto$proto, data_proto$action == input$choix_action))
      df_diff_proto <-as.data.frame(diff_proto)
    }
    df_diff_proto %>%
      arrange(desc(Freq)) %>%
      mutate(prop = percent(Freq / sum(Freq))) -> df_diff_proto

    pie_acc <- ggplot(df_diff_proto, aes(x = "", y = Freq, fill = fct_inorder(Var1))) +
      ggtitle("Diagramme circulaire des protocoles acceptés")+
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x = 1) +
      guides(fill = guide_legend(title = "Protocoles"))
    plot(pie_acc)
  })
  output$graphique_universite <- renderPlotly({
    port_uni = datasetInput() %>%
           filter(!grepl("159.84.89.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])", ipsrc))
    
    port_uni_count = port_uni%>%
      count(ipsrc, sort=TRUE)%>%
      arrange(desc(n)) %>%
      slice(seq_len(input$nb_univ))
    
    port_uni_count =as.data.frame(port_uni_count)
    p <-ggplot(data = port_uni_count, aes(x =factor(ipsrc), y=n)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=n), vjust=-0.3, size=3.5)+
      theme_minimal()
    
    ggplotly(p)
    

  })
  output$graphique_flux <- renderPlot({

    mat = datasetInput() %>%count(ipsrc, ipdst) %>%
      arrange(desc(n)) %>%
      slice(seq_len(input$nb_add))
    
    mat = mat %>%
      cast_dtm(ipsrc, ipdst, n)
    mat = as.matrix(mat)

    circos.par(gap.degree = 0.5)
    # The chordDiagram command draws the links between O/D pairs. For details on
    # what each parameter means see the tutorial document linked above.
    chordDiagram(mat, directional = TRUE, annotationTrack = "grid", 
                 preAllocateTracks = list(list(track.height = 0.05), list(track.height = 0.05)))
    
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.index = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), mean(ylim), sector.index, facing = "reverse.clockwise", niceFacing = TRUE)
    }, bg.border = NA)
    circos.clear()
  })
  
  observeEvent(
    input$FileInput,
    updateVarSelectInput(session, "cible", data = datasetInput()))
  
  observeEvent(
    input$FileInput,
    updateVarSelectInput(session, "expli", data = datasetInput()))  
  

  output$table = DT::renderDataTable(datasetInput())
  
  
  output$distPlot <- renderPlotly({
    data_clus <- datasetInput()
    
    
    data_clus[,c(1,2,4:6)] <- lapply(data_clus[,c(1,2,4:6)],as.factor)
    gower_dist <- daisy(data_clus, metric = "gower")
    gower_mat <- as.matrix(gower_dist)
    k <- input$clusters
    
    pam_fit <- pam(gower_dist, diss = TRUE, k)
    tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
    
    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(pam_fit$clustering)) %>% 
      mutate(ipsource = data_clus$ipsrc)
    
    ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster))
  })
  
  # output$test <- renderPrint({
  #   data_2 <- datasetInput()
  #   data_2[,c(1,2,4:6)] <- lapply(data_2[,c(1,2,4:6)],as.factor)
  #   gower_dist2 <- daisy(data_2, metric = "gower")
  #   gower_mat2 <- as.matrix(gower_dist2)
  #   k2 <- input$clusters
  #   
  #   pam_fit2 <- pam(gower_dist2, diss = TRUE, k2)
  #   pam_results2 <- data %>%
  #     mutate(cluster = pam_fit2$clustering) %>%
  #     group_by(cluster) %>%
  #     do(the_summary = summary(.))
  #   pam_results2$the_summary
  #   
    
  #})
  
  observeEvent(input$go,{
    data = datasetInput()
    #var = list()
    var = input$expli
    cib = input$cible
    #temp <- data[,input$expli[]]
    var = unlist(as.character(input$expli))
    temp <- data %>% select(var,cib)
    print(cib)
    #temp <- data[,input$expli,drop=FALSE]
    res <- rpart(proto ~.,data=temp,method = "class", control = rpart.control(cp = 0.5, minsplit = 2),model =T)
    shiny::callModule(visTreeModuleServer, "id1", data = shiny::reactive(res))
  })
  
  
}



### Lancement de l'application
shinyApp(ui, server)

