#if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
#if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
#if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
#install.packages("plotly")
library(leaflet)
library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(forcats)
library(sp)
library(plotly)

#Basic Data modification
GeoPKO <- Almostfinal
class(GeoPKO$No.troops)
GeoPKO$NoTroops<-as.numeric(GeoPKO$No.troops)
GeoPKO$RPF_No<-as.numeric(GeoPKO$RPF_No)
GeoPKO$UNPOL<-as.numeric(GeoPKO$UNPOL.dummy)
GeoPKO$UNMO<-as.numeric(GeoPKO$UNMO.dummy)
GeoPKO$No.TCC<-as.numeric(GeoPKO$No.TCC)
GeoPKO$Av<- (GeoPKO$Avia + GeoPKO$HeSup)

HQicon <- awesomeIcons(
  icon = 'fas fa-home',
  markerColor = "red",
  iconColor = "#f7fcff",
  library = 'fa'
)

Medicon <- awesomeIcons(
  icon = 'fas fa-plus',
  markerColor = "white",
  iconColor = "red",
  library = 'fa'
)

UNPOLicon <- awesomeIcons(
  icon = 'fab fa-product-hunt',
  markerColor = "blue",
  iconColor = "#f6f6f6",
  library = 'fa'
)
UNMOicon <- awesomeIcons(
  icon = 'fas fa-binoculars',
  markerColor = "darkblue",
  iconColor = "#f6f6f6",
  library = 'fa'
)


Avicon <- awesomeIcons(
  icon = 'fas fa-plane',
  markerColor = "green",
  iconColor = "#f6f6f6",
  library = 'fa'
)

#Lollipop Data
Years <- GeoPKO
Years <- Years %>% group_by(Mission, Location)%>% summarize(start_date=min(Year), end_date=max(Year))

#Shiny leaflet launch

gif_df <- GeoPKO %>% select(Mission, Year, Country, Location, Latitude, Longitude, NoTroops, HQ, UNPOL, Med,Av,UNMO, No.TCC, nameoftcc_1, nameoftcc_2, nameoftcc_3, nameoftcc_4, nameoftcc_5, nameoftcc_6, nameoftcc_7, nameoftcc_8, nameoftcc_9, nameoftcc_10, nameoftcc_11, nameoftcc_12, nameoftcc_13, nameoftcc_14,nameoftcc_15,nameoftcc_16,nameoftcc_17) %>%
  group_by(Mission, Year, Location, Country) %>%
  mutate(ave.no.troops = as.integer(mean(NoTroops, na.rm=TRUE))) %>%  select(-NoTroops) %>% distinct() %>% drop_na(ave.no.troops)

gif_df$nameoftcc_1 <- str_replace_all(gif_df$nameoftcc_1, "NA", "")
gif_df$nameoftcc_2 <- str_replace_all(gif_df$nameoftcc_2, "NA", "")
gif_df$nameoftcc_3 <- str_replace_all(gif_df$nameoftcc_3, "NA", "")
gif_df$nameoftcc_4 <- str_replace_all(gif_df$nameoftcc_4, "NA", "")
gif_df$nameoftcc_5 <- str_replace_all(gif_df$nameoftcc_5, "NA", "")
gif_df$nameoftcc_6 <- str_replace_all(gif_df$nameoftcc_6, "NA", "")
gif_df$nameoftcc_7 <- str_replace_all(gif_df$nameoftcc_7, "NA", "")
gif_df$nameoftcc_8 <- str_replace_all(gif_df$nameoftcc_8, "NA", "")
gif_df$nameoftcc_9 <- str_replace_all(gif_df$nameoftcc_9, "NA", "")
gif_df$nameoftcc_10 <- str_replace_all(gif_df$nameoftcc_10, "NA", "")
gif_df$nameoftcc_11 <- str_replace_all(gif_df$nameoftcc_11, "NA", "")
gif_df$nameoftcc_12 <- str_replace_all(gif_df$nameoftcc_12, "NA", "")
gif_df$nameoftcc_13 <- str_replace_all(gif_df$nameoftcc_13, "NA", "")
gif_df$nameoftcc_14 <- str_replace_all(gif_df$nameoftcc_14, "NA", "")
gif_df$nameoftcc_15 <- str_replace_all(gif_df$nameoftcc_15, "NA", "")
gif_df$nameoftcc_16 <- str_replace_all(gif_df$nameoftcc_16, "NA", "")
gif_df$nameoftcc_17 <- str_replace_all(gif_df$nameoftcc_17, "NA", "")

qpal <- colorNumeric(c("#ffc100", "#ff9a00", "#ff7400", "#ff4d00","#dc6900","#e0301e","#a32020", "#602320", "#451d1b", "#060606"), gif_df$ave.no.troops)
qpal2 <- colorNumeric(c("#ffc100", "#ff9a00", "#ff7400", "#ff4d00","#dc6900","#e0301e","#a32020", "#602320", "#451d1b", "#060606"), gif_df$No.TCC)

####UI###
ui <- fluidPage(
  navbarPage ("Exploring GeoPKO",
              tabPanel ("Mapper", leafletOutput("basemap", height=850),
                        absolutePanel(top = 70, left = 80,width = 300, style = "background:rgba(255, 224, 189, 0.5)",
                                      span(tags$i(h6("The Geo-PKO dataset provides data on UN peacekeeping deployments. It offers information on key attributes of peacekeeping deployments at the local level, including location, size, troop type, headquarters, troop-contributing countries and other variables. This visualization is based on GeoPKO 2.0.")), style="color:#15110d"),
                                      span(tags$i(h6("Users can select different variables included in the GeoPKO 2.0 dataset in the right corner. Troop deployment is averaged per year. When selecting Troop contributing countries (TCC) the TCCs are shown in the label. All other variables are dichotomous and when selected only show indicate the presence of such units. ´Aviation´ includes both the variable of Helicopter Support(HeSup) and Aviation")), style="color:#15110d"),
                                      span(h5(tags$b(textOutput("reactive_year"), align = "Left"), style="color:#15110d")),
                                      span(h4(textOutput("reactive_troopcount"), align = "center"), style="color:#15110d"),
                                      span(h6(textOutput("reactive_UNPOLcount"), align = "right"), style="color:#527bd2"),
                                      span(h6(textOutput("reactive_UNMOcount"), align = "right"), style="color:#363b74"),
                                      pickerInput("missions","Select Mission(s)", choices=as.character(unique(gif_df$Mission)),selected =as.character(unique(gif_df$Mission)) , options = list(`actions-box` = TRUE),multiple = T),
                                      chooseSliderSkin("Shiny", color = "transparent"),
                                      setSliderColor("transparent", 1),
                                      sliderInput(inputId = "plot_date", 
                                                  label = "Select deployment year (1994-2020)",
                                                  min = 1994,
                                                  max = 2020,
                                                  value =1994,
                                                  step = 1,
                                                  sep= "",
                                                  animate = animationOptions(interval = 2000, loop = FALSE)),
                                      span(tags$i(h6("Data used by the GeoPKO 2.0 is derived from United Nations (UN) mission deployment maps, UN Secretary-General mission progress reports, and the Dag Hammarskjold Library Cartographic Section peacekeeping mission deployment maps. Data inconsistencies maybe due to lack of availability or changed methods of reporting.")), style="color:#15110d")
                        )
              ),tabPanel ("Time Maps",
                          sidebarLayout(
                            sidebarPanel(
                              p("What locations had Peacekeepers when? Select the options below to visualize."),
                              selectInput(inputId="Lollipop_map", label="Select a mission",
                                          choices=factor(Years$Mission), width=150), width= 2,
                              p("The lollipop graphs show per mission the years in which a location had active deployment of peacekeepers.")
                            ),
                            mainPanel(fluid=TRUE,
                                      plotOutput("lollipop"))
                          )
                          ),
              tabPanel ("About",tags$div(
                tags$h3("Geocoded UN Peacekeeping Operations Dataset"),tags$br(),
                "The Geo-PKO dataset provides data on UN peacekeeping deployments.", tags$br(),
                "It offers information on key attributes of peacekeeping deployments at the local level, including location, size, troop type, headquarters, troop-contributing countries and other variables.",tags$br(),tags$br(),
                tags$b("When using the data, please cite:"), tags$br(),"Cil, D., Fjelde, H., Hultman, L., & Nilsson, D. (2020). Mapping blue helmets: Introducing the Geocoded Peacekeeping Operations (Geo-PKO) dataset. Journal of Peace Research, 57(2), 360–370. ", tags$br(),
                tags$br(),tags$br(),tags$h4("Download the Data"),
                tags$b("Previous version: "),tags$br(),
                tags$a(href="https://www.pcr.uu.se/digitalAssets/818/c_818704-l_1-k_geo-pko-codebook_v1.2.pdf", "GeoPKO 1.2 codebook"),tags$br(),
                tags$a(href="https://www.pcr.uu.se/digitalAssets/818/c_818704-l_1-k_geo_pko_v.1.2.csv", "GeoPKO 1.2 csv"),tags$br(),
                tags$a(href="https://www.pcr.uu.se/digitalAssets/818/c_818704-l_1-k_geo_pko_v.1.2.rds", "GeoPKO 1.2 rds"),tags$br(),
                tags$br(),tags$br(),tags$h4("Further information"),
                tags$b("Dataset Homepage: "), tags$a(href="https://www.pcr.uu.se/data/geo-pko/", "The Geocoded Peacekeeping Operations Dataset"),tags$br(),
                tags$b("An R guide to using the GeoPKO dataset"), tags$a(href="https://github.com/nytimes/covid-19-data", "GitHub"),tags$br(),
                tags$b("UN deployment Maps"), tags$a(href="https://digitallibrary.un.org/", "UN Digital Library"),tags$br(),
                tags$b("Introductionary article: "),  tags$a(href="https://journals.sagepub.com/doi/10.1177/0022343319871978", "Mapping Blue Helmets"),"By Cil, D., Fjelde, H., Hultman, L., & Nilsson, D. (2020)",tags$br(),
                tags$b("Studies using this data: "),tags$br(),
                tags$a(href="https://www.cambridge.org/core/journals/international-organization/article/protection-through-presence-un-peacekeeping-and-the-costs-of-targeting-civilians/050CE5EC7C4D8049FD3973241EC0F97D", "Protection through Presence: UN Peacekeeping and the Costs of Targeting Civilians"),"By Fjelde, H., Hultman, L. & Nilsson, D. (2019)",tags$br(),
                tags$a(href="https://www.tandfonline.com/doi/full/10.1080/13533312.2019.1676642?scroll=top&needAccess=true", "UN Peacekeeping and Forced Displacement in South Sudan"),"By Sundberg, R. (2020)",
                tags$br(),tags$br(),tags$h4("Contributers Shiny App"),
                "Nguyen Ha, Research Assistant at the department of Peace and Conflict Research, Uppsala Univeristy",tags$br(),
                "Tanushree Rao, Intern at the department of Peace and Conflict Research, Uppsala Univeristy",tags$br(),
                "Lou van Roozendaal, Intern at the department of Peace and Conflict Research, Uppsala Univeristy",
                tags$br(),tags$br(),tags$h4("Code"),
                "Code is available on ",tags$a(href="https://github.com/hatnguyen267/GeoPKO-Shiny", "Github.")
              ))
  ))


#################Server#####################
server <- function(input, output, session){
  
  
  
####LeafletCode  
  
  filteredData <- reactive({
    gif_df %>% filter(Mission %in% input$missions & Year %in% input$plot_date)
  })
  
  
  output$basemap <- renderLeaflet({
    leaflet(GeoPKO, options = leafletOptions(minZoom = 2)) %>% 
      addTiles() %>% 
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Troop deployment", "TCC", "None"),
        overlayGroups = c("Medical units","Aviation","UNPOL", "UNMO", "Mission HQ"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("Medical units","Aviation", "UNPOL", "UNMO", "Mission HQ"))  %>%
      fitBounds(~-70,-50,~60,60) %>%
      setMaxBounds(~-70,-50,~60,60)%>%
      addLegend(pal = qpal, values = ~gif_df$ave.no.troops, group = "Troop deployment", title= "Legend Troop deployment") %>%
      addLegend(pal = qpal2, values = ~gif_df$No.TCC, group = "TCC", title= "Legend TCC")
  })
  
  output$reactive_year <- renderText({
    paste0("In ",unique(filteredData()$Year), " there were:")
  }) 
  
  output$reactive_troopcount <- renderText({
    paste0(prettyNum(sum(filteredData()$ave.no.troops), big.mark=","), " deployed peacekeepers")
  }) 
  
  output$reactive_UNPOLcount <- renderText({
    paste0(prettyNum(sum(filteredData()$UNPOL, na.rm=TRUE), big.mark=","), " UNPOL deployments")
  }) 
  
  output$reactive_UNMOcount <- renderText({
    paste0(prettyNum(sum(filteredData()$UNMO, na.rm=TRUE), big.mark=","), " UNMO deployments")
  })
 
  observe({
    leafletProxy(mapId = "basemap", data = filteredData()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = (filteredData1<-filteredData()%>%filter(ave.no.troops>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1/3.5), 
                       fillOpacity = 0.5, color = ~qpal(ave.no.troops), group = "Troop deployment", 
                       label=paste("<strong>Troop number:</strong>", filteredData1$ave.no.troops,"<br/><strong>Mission:</strong>", filteredData1$Mission,"<br/><strong>Location:</strong>",filteredData1$Location)%>% lapply(htmltools::HTML)) %>%
      addAwesomeMarkers(data = (filteredData2<-filteredData()%>%filter(UNPOL>0)), lat = ~Latitude, lng = ~Longitude,icon=UNPOLicon, group = "UNPOL", 
                       label=paste("<strong>UNPOL</strong> (",filteredData2$Mission,")<br/><strong>Location:</strong>",filteredData2$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData3<-filteredData()%>%filter(No.TCC==1)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1.5), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData3$Location, "<br/><strong>",filteredData3$No.TCC, " TCC:</strong><br/>", filteredData3$nameoftcc_1)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData4<-filteredData()%>%filter(UNMO>0)), lat = ~Latitude, lng = ~Longitude, icon=UNMOicon, group = "UNMO", 
                       label=paste("<strong>UNMO <br/>Mission:</strong>", filteredData4$Mission,"<br/><strong>Location:</strong>",filteredData4$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData5<-filteredData()%>%filter(HQ==3)), lat = ~Latitude, lng = ~Longitude, icon = HQicon, group = "Mission HQ", 
                       label=paste("<strong>Mission HQ:</strong>", filteredData5$Mission,"<br/><strong>Location:</strong>",filteredData5$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData22<-filteredData()%>%filter(Med==1)), lat = ~Latitude, lng = ~Longitude, icon = Medicon, group = "Medical units",
                        label=paste("<strong>Mission:</strong>", filteredData22$Mission,"<br/><strong>Location:</strong>",filteredData22$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData23<-filteredData()%>%filter(Av>0)), lat = ~Latitude, lng = ~Longitude, icon = Avicon, group = "Aviation", 
                        label=paste("<strong>Mission:</strong>", filteredData23$Mission,"<br/><strong>Location:</strong>",filteredData23$Location)%>% lapply(htmltools::HTML))%>%
        addCircleMarkers(data = (filteredData6<-filteredData()%>%filter(No.TCC==2)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                     fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                     label=paste("<strong>Location:</strong>",filteredData6$Location, "<br/><strong>",filteredData6$No.TCC, " TCCs:</strong><br/>", filteredData6$nameoftcc_1,"<br/>",filteredData6$nameoftcc_2)%>% lapply(htmltools::HTML))%>%
        addCircleMarkers(data = (filteredData7<-filteredData()%>%filter(No.TCC==3)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData7$Location, "<br/><strong>",filteredData7$No.TCC, " TCCs:</strong><br/>", filteredData7$nameoftcc_1,"<br/>",filteredData7$nameoftcc_2,"<br/>",filteredData7$nameoftcc_3)%>% lapply(htmltools::HTML))%>%
        addCircleMarkers(data = (filteredData8<-filteredData()%>%filter(No.TCC==4)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData8$Location, "<br/><strong>",filteredData8$No.TCC, " TCCs:</strong><br/>", filteredData8$nameoftcc_1,"<br/>",filteredData8$nameoftcc_2,"<br/>",filteredData8$nameoftcc_3,"<br/>",filteredData8$nameoftcc_4)%>% lapply(htmltools::HTML))%>%
        addCircleMarkers(data = (filteredData9<-filteredData()%>%filter(No.TCC==5)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                     fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                     label=paste("<strong>Location:</strong>",filteredData9$Location, "<br/><strong>",filteredData9$No.TCC, " TCCs:</strong><br/>", filteredData9$nameoftcc_1,"<br/>",filteredData9$nameoftcc_2,"<br/>",filteredData9$nameoftcc_3,"<br/>",filteredData9$nameoftcc_4,"<br/>",filteredData9$nameoftcc_5)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData10<-filteredData()%>%filter(No.TCC==6)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData10$Location, "<br/><strong>",filteredData10$No.TCC, " TCCs:</strong><br/>", filteredData10$nameoftcc_1,"<br/>",filteredData10$nameoftcc_2,"<br/>",filteredData10$nameoftcc_3,"<br/>",filteredData10$nameoftcc_4,"<br/>",filteredData10$nameoftcc_5,"<br/>",filteredData10$nameoftcc_6)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData11<-filteredData()%>%filter(No.TCC==7)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData11$Location, "<br/><strong>",filteredData11$No.TCC, " TCCs:</strong><br/>", filteredData11$nameoftcc_1,"<br/>",filteredData11$nameoftcc_2,"<br/>",filteredData11$nameoftcc_3,"<br/>",filteredData11$nameoftcc_4,"<br/>",filteredData11$nameoftcc_5,"<br/>",filteredData11$nameoftcc_6,"<br/>",filteredData11$nameoftcc_7)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData12<-filteredData()%>%filter(No.TCC==8)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData12$Location, "<br/><strong>",filteredData12$No.TCC, " TCCs:</strong><br/>", filteredData12$nameoftcc_1,"<br/>",filteredData12$nameoftcc_2,"<br/>",filteredData12$nameoftcc_3,"<br/>",filteredData12$nameoftcc_4,"<br/>",filteredData12$nameoftcc_5,"<br/>",filteredData12$nameoftcc_6,"<br/>",filteredData12$nameoftcc_7,"<br/>",filteredData12$nameoftcc_8)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData13<-filteredData()%>%filter(No.TCC==9)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData13$Location, "<br/><strong>",filteredData13$No.TCC, " TCCs:</strong><br/>", filteredData13$nameoftcc_1,"<br/>",filteredData13$nameoftcc_2,"<br/>",filteredData13$nameoftcc_3,"<br/>",filteredData13$nameoftcc_4,"<br/>",filteredData13$nameoftcc_5,"<br/>",filteredData13$nameoftcc_6,"<br/>",filteredData13$nameoftcc_7,"<br/>",filteredData13$nameoftcc_8,"<br/>",filteredData13$nameoftcc_9)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData14<-filteredData()%>%filter(No.TCC==10)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData14$Location, "<br/><strong>",filteredData14$No.TCC, " TCCs:</strong><br/>", filteredData14$nameoftcc_1,"<br/>",filteredData14$nameoftcc_2,"<br/>",filteredData14$nameoftcc_3,"<br/>",filteredData14$nameoftcc_4,"<br/>",filteredData14$nameoftcc_5,"<br/>",filteredData14$nameoftcc_6,"<br/>",filteredData14$nameoftcc_7,"<br/>",filteredData14$nameoftcc_8,"<br/>",filteredData14$nameoftcc_9,"<br/>",filteredData14$nameoftcc_10)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData15<-filteredData()%>%filter(No.TCC==11)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData15$Location, "<br/><strong>",filteredData15$No.TCC, " TCCs:</strong><br/>", filteredData15$nameoftcc_1,"<br/>",filteredData15$nameoftcc_2,"<br/>",filteredData15$nameoftcc_3,"<br/>",filteredData15$nameoftcc_4,"<br/>",filteredData15$nameoftcc_5,"<br/>",filteredData15$nameoftcc_6,"<br/>",filteredData15$nameoftcc_7,"<br/>",filteredData15$nameoftcc_8,"<br/>",filteredData15$nameoftcc_9,"<br/>",filteredData15$nameoftcc_10,"<br/>",filteredData15$nameoftcc_11)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData16<-filteredData()%>%filter(No.TCC==12)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData16$Location, "<br/><strong>",filteredData16$No.TCC, " TCCs:</strong><br/>", filteredData16$nameoftcc_1,"<br/>",filteredData16$nameoftcc_2,"<br/>",filteredData16$nameoftcc_3,"<br/>",filteredData16$nameoftcc_4,"<br/>",filteredData16$nameoftcc_5,"<br/>",filteredData16$nameoftcc_6,"<br/>",filteredData16$nameoftcc_7,"<br/>",filteredData16$nameoftcc_8,"<br/>",filteredData16$nameoftcc_9,"<br/>",filteredData16$nameoftcc_10,"<br/>",filteredData16$nameoftcc_11,"<br/>",filteredData16$nameoftcc_12)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData17<-filteredData()%>%filter(No.TCC==13)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData17$Location, "<br/><strong>",filteredData17$No.TCC, " TCCs:</strong><br/>", filteredData17$nameoftcc_1,"<br/>",filteredData17$nameoftcc_2,"<br/>",filteredData17$nameoftcc_3,"<br/>",filteredData17$nameoftcc_4,"<br/>",filteredData17$nameoftcc_5,"<br/>",filteredData17$nameoftcc_6,"<br/>",filteredData17$nameoftcc_7,"<br/>",filteredData17$nameoftcc_8,"<br/>",filteredData17$nameoftcc_9,"<br/>",filteredData17$nameoftcc_10,"<br/>",filteredData17$nameoftcc_11,"<br/>",filteredData17$nameoftcc_12,"<br/>",filteredData17$nameoftcc_13)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData18<-filteredData()%>%filter(No.TCC==14)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData18$Location, "<br/><strong>",filteredData18$No.TCC, " TCCs:</strong><br/>", filteredData18$nameoftcc_1,"<br/>",filteredData18$nameoftcc_2,"<br/>",filteredData18$nameoftcc_3,"<br/>",filteredData18$nameoftcc_4,"<br/>",filteredData18$nameoftcc_5,"<br/>",filteredData18$nameoftcc_6,"<br/>",filteredData18$nameoftcc_7,"<br/>",filteredData18$nameoftcc_8,"<br/>",filteredData18$nameoftcc_9,"<br/>",filteredData18$nameoftcc_10,"<br/>",filteredData18$nameoftcc_11,"<br/>",filteredData18$nameoftcc_12,"<br/>",filteredData18$nameoftcc_13,"<br/>",filteredData18$nameoftcc_14)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData19<-filteredData()%>%filter(No.TCC==15)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData19$Location, "<br/><strong>",filteredData19$No.TCC, " TCCs:</strong><br/>", filteredData19$nameoftcc_1,"<br/>",filteredData19$nameoftcc_2,"<br/>",filteredData19$nameoftcc_3, "<br/>",filteredData19$nameoftcc_4,"<br/>",filteredData19$nameoftcc_5,"<br/>",filteredData19$nameoftcc_6,"<br/>",filteredData19$nameoftcc_7,"<br/>",filteredData19$nameoftcc_8,"<br/>",filteredData19$nameoftcc_9,"<br/>",filteredData19$nameoftcc_10,"<br/>",filteredData19$nameoftcc_11,"<br/>",filteredData19$nameoftcc_12,"<br/>",filteredData19$nameoftcc_13,"<br/>",filteredData19$nameoftcc_14, "<br/>",filteredData19$nameoftcc_15)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData20<-filteredData()%>%filter(No.TCC==16)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData20$Location, "<br/><strong>",filteredData20$No.TCC, " TCCs:</strong><br/>", filteredData20$nameoftcc_1,"<br/>",filteredData20$nameoftcc_2,"<br/>",filteredData20$nameoftcc_3, "<br/>",filteredData20$nameoftcc_4,"<br/>",filteredData20$nameoftcc_5,"<br/>",filteredData20$nameoftcc_6,"<br/>",filteredData20$nameoftcc_7,"<br/>",filteredData20$nameoftcc_8,"<br/>",filteredData20$nameoftcc_9,"<br/>",filteredData20$nameoftcc_10,"<br/>",filteredData20$nameoftcc_11,"<br/>",filteredData20$nameoftcc_12,"<br/>",filteredData20$nameoftcc_13,"<br/>",filteredData20$nameoftcc_14, "<br/>",filteredData20$nameoftcc_15,"<br/>",filteredData20$nameoftcc_16)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData21<-filteredData()%>%filter(No.TCC==17)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData21$Location, "<br/><strong>",filteredData21$No.TCC, " TCCs:</strong><br/>", filteredData21$nameoftcc_1,"<br/>",filteredData21$nameoftcc_2,"<br/>",filteredData21$nameoftcc_3, "<br/>",filteredData21$nameoftcc_4,"<br/>",filteredData21$nameoftcc_5,"<br/>",filteredData21$nameoftcc_6,"<br/>",filteredData21$nameoftcc_7,"<br/>",filteredData21$nameoftcc_8,"<br/>",filteredData21$nameoftcc_9,"<br/>",filteredData21$nameoftcc_10,"<br/>",filteredData21$nameoftcc_11,"<br/>",filteredData21$nameoftcc_12,"<br/>",filteredData21$nameoftcc_13,"<br/>",filteredData21$nameoftcc_14, "<br/>",filteredData21$nameoftcc_15,"<br/>",filteredData21$nameoftcc_16,"<br/>",filteredData21$nameoftcc_17)%>% lapply(htmltools::HTML))
    
#####Lollipop Code
    sfdf2 <- reactive({
      req(input$Lollipop_map)
      Years %>% filter(Mission %in% input$Lollipop_map)
    })
    
    output$lollipop <- renderPlot({
      lolli <-   ggplot(sfdf2()) +
        geom_segment( aes(x=start_date, xend=end_date, y=fct_reorder(Location, start_date), yend=fct_reorder(Location, start_date)), color="grey") +
        geom_point( aes(x=end_date, y=Location), colour=rgb(0.9,0.3,0.1,0.9), size=3.5 ) +
        geom_point( aes(x=start_date, y=Location), colour=rgb(1.0,0.6,0.1,0.7), size=3) +
        scale_x_continuous(breaks = c(1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))+
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none",
              axis.text.x = element_text(angle=90, hjust=1),
              axis.ticks.length.x= unit(0.1, "cm"),
              panel.grid.minor.x = element_blank(),
              panel.spacing.x = unit(1,"lines")
        ) +
        xlab("Years") +
        ylab("Location")+
        labs(title=paste("Ocupation of Locations in", sfdf2()$Mission), 
             caption="Data from GeoPKO 2.0")
      lolli
  }, height = 1000)
     })
  
}

shinyApp(ui, server)
