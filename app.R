#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(scales)
library(ggplot2)
library(plotly)
library(ggthemes)
library(dplyr)

HR <- read.csv('https://raw.github.com/nmejia10/Prueba-Tecnica/master/HR_Mod.csv',stringsAsFactors = F,header=T,sep=";")

Dep_list <- as.list(unique(HR$Department))

Header<-dashboardHeader(title = "HR Dashboard",titleWidth = 200)
SideBar<-dashboardSidebar(selectInput(inputId = "department",label = "Department:", choices = Dep_list, selected="Sales",selectize = FALSE))

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
  ,valueBoxOutput("value4")
)
frow2 <- fluidRow( 
  box(
    title = "Satisfaction Level Per Job Role"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("SatJR", height = "300px")
  )
  ,box(
    title = "Attrition per Satisfaction Level"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Attr", height = "300px")
  ) 
  ,box(
    title = "Daily Rate per Satisfaction Level"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("DailyRS", height = "300px")
  )
  ,box(
    title = "Work Life Balance per Satisfaction Level"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("WLB", height = "300px")
  )
)
# combine the two fluid rows to make the body
Body <- dashboardBody(frow1, frow2)




ui <- dashboardPage(title = 'This is my Page title', Header, SideBar, Body, skin='red')



server <- function(input, output) {
  
  result <- HR 
  
  output$value1 <- renderValueBox({
    
    result <- result %>% filter(Department==input$department)
    
    Tot<- result %>%
      tally() %>%
      pull() %>% 
      as.integer()
    
    Att<-result %>% 
      filter(Attrition=="Yes")  %>%
      tally() %>%
      pull() %>% 
      as.integer()
    
    Att<-Att/Tot
    
    valueBox(paste0(round(Att * 100), "%"),
             subtitle = "Attrition",
             icon = icon("percent"),
             color = "teal")
  })
  
  output$value2 <- renderValueBox({
    
    result <- result %>% filter(Department==input$department)
    
    result <-mean(result$EnvironmentSatisfaction)
    
    valueBox(prettyNum(result, big.mark = ","),
             subtitle = "Average Eviroment Satisfaction",
             icon = icon("list-alt"),
             color = "blue")
  })
  
  output$value3 <- renderValueBox({
    
    result <- result %>% filter(Department==input$department)
    
    result <-mean(result$DailyRate)
    
    valueBox(prettyNum(result, big.mark = ","),
             subtitle = "Average DailyRate",
             icon = icon("dollar")
             ,color = "green")
  })
  
  
  output$value4 <- renderValueBox({
    
    result <- result %>% filter(Department==input$department)
    
    result <-mean(result$TotalWorkingYears)
    
    valueBox(prettyNum(result, big.mark = ","),
             subtitle = "Average Years in the Company",
             icon = icon("calendar")
             ,color = "purple")
  })
  
  output$SatJR <- renderPlotly({

    
    resultF <- result %>% filter(Department==input$department)
    
    Job_list<-as.list(unique(resultF$JobRole))
    M<-length(unique(resultF$JobRole))
    
    Grph<-data.frame()
    
    cont<-1
    
    for(i in 1:4){
      for(j in 1:M){
        result <- resultF %>%
          filter(JobSatisfaction == i) %>% filter(JobRole==paste(Job_list[j]))
        
        result <- result %>%
          tally() %>%
          pull() %>% 
          as.integer()
        
        Grph[cont,1]<-paste(Job_list[j])
        Grph[cont,2]<-as.numeric(i)
        Grph[cont,3]<-as.numeric(result)
        
        cont<-cont+1
      }
      
    }
    colnames(Grph)<-c("Role","Satisfaction","Count")
    
    Grph$Satisfaction<-factor(Grph$Satisfaction)
    G<-ggplot(Grph, aes(fill=Satisfaction, y=Count, x=Role)) + 
      geom_bar( stat="identity") +labs(x="Role",y="Count", title="")
    ggplotly(G)
    
    
  })
  
  
  
  output$Attr <- renderPlot({
    
    
    resultF <- result %>% filter(Department==input$department)
    
    
    Att_list <- as.list(unique(HR$Attrition))
    Grph<-data.frame()
    
    cont<-1
    
    for(i in 1:4){
      for(j in 1:2){
        result <-resultF %>%
          filter(JobSatisfaction == i) %>% filter(Attrition==paste(Att_list[j]))
        
        result <- result %>%
          tally() %>%
          pull() %>% 
          as.integer()
        
        Grph[cont,1]<-paste(Att_list[j])
        Grph[cont,2]<-as.numeric(i)
        Grph[cont,3]<-as.numeric(result)
        
        cont<-cont+1
      }
      
    }
    colnames(Grph)<-c("Attrition","Satisfaction","Count")
    Grph$Satisfaction<-factor(Grph$Satisfaction)
    
    ggplot(data = Grph, aes(x = "", y = Count, fill = Satisfaction )) + 
      geom_bar(stat = "identity", position = position_fill()) +
      geom_text(aes(label = Count), position = position_fill(vjust = 0.5)) +
      coord_polar(theta = "y") +
      facet_wrap(~ Attrition)+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) + theme(legend.position='bottom')
    
    
    
    
  })
  
  
  output$DailyRS <- renderPlot({
    
    resultF <- result %>% filter(Department==input$department)
    
    ggplot(resultF,aes(x=DailyRate))+geom_histogram()+facet_grid(~JobSatisfaction)+ labs(y="Count")
    
    
    
    
  })
  
  output$WLB <- renderPlotly({
    
    
    resultF <- result %>% filter(Department==input$department)
    
    Job_list<-as.list(unique(resultF$WorkLifeBalance))
    M<-length(unique(resultF$WorkLifeBalance))
    
    Grph<-data.frame()
    
    cont<-1
    
    for(i in 1:4){
      for(j in 1:M){
        result <- resultF %>%
          filter(JobSatisfaction == i) %>% filter(WorkLifeBalance==j)
        
        result <- result %>%
          tally() %>%
          pull() %>% 
          as.integer()
        
        Grph[cont,1]<-paste(Job_list[j])
        Grph[cont,2]<-as.numeric(i)
        Grph[cont,3]<-as.numeric(result)
        
        cont<-cont+1
      }
      
    }
    colnames(Grph)<-c("Work_Life_Balance","Satisfaction","Count")
    
    Grph$Satisfaction<-factor(Grph$Satisfaction)
    G<-ggplot(Grph, aes(fill=Satisfaction, y=Count, x=Work_Life_Balance)) + 
      geom_bar( stat="identity") +labs(x="Work/Life Balance",y="Count", title="")
    ggplotly(G)
    
    
  })
  
  
  
}

shinyApp(ui, server)





