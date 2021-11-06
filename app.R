library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")
#The user interface
header <- dashboardHeader(title = "Irreducible Polynomial",titleWidth = 500)
sidebar <- dashboardSidebar(
  sidebarMenu(id="menu",
    menuItem(jaxI("x^2 + 2"), tabName = "poly1"),
    menuItem(jaxI("x^2 + 3"), tabName = "poly2"),
    menuItem(jaxI("x^2 + x + 1"), tabName = "poly3"),
    menuItem(jaxI("x^2 + x + 2"), tabName = "poly4"),
    menuItem(jaxI("x^2 + 2x + 3"), tabName = "poly5"),
    menuItem(jaxI("x^2 + 2x + 4"), tabName = "poly6"),
    menuItem(jaxI("x^2 + 3x + 3"), tabName = "poly7"),
    menuItem(jaxI("x^2 + 3x + 4"), tabName = "poly8"),
    menuItem(jaxI("x^2 + 4x + 1"), tabName = "poly9"),
    menuItem(jaxI("x^2 + 4x + 2"), tabName = "poly10")
  )    
)
body <- dashboardBody(
    tags$style(type='text/css', "#select {font-size: 32px !important} "),
    tabItems(
      tabItem(tabName = "poly1",
        h2("Replace", jaxI("x^2"), "with 3"),
      ),
      tabItem(tabName = "poly2",
        h2("Replace", jaxI("x^2"), "with 2"),
      ),
      tabItem(tabName = "poly3",
        h2("Replace", jaxI("x^2"), "with 4x + 4")
      ),
      tabItem(tabName = "poly4",
        h2("Replace", jaxI("x^2"), "with 4x + 3")
      ),
      tabItem(tabName = "poly5",
              h2("Replace", jaxI("x^2"), "with 3x + 2")
      ),
      tabItem(tabName = "poly6",
              h2("Replace", jaxI("x^2"), "with 3x + 1")
      ),
      tabItem(tabName = "poly7",
              h2("Replace", jaxI("x^2"), "with 2x + 2")
      ),
      tabItem(tabName = "poly8",
              h2("Replace", jaxI("x^2"), "with 2x + 1")
      ),
      tabItem(tabName = "poly9",
              h2("Replace", jaxI("x^2"), "with x + 4")
      ),
      tabItem(tabName = "poly10",
              h2("Replace", jaxI("x^2"), "with x + 3")
      )
    ),
    fluidRow(
        # column(3,selectInput("left",NULL,c("[0]","[1]","[2]"),selected = "[1]"),selectize = FALSE),
        # column(2,awesomeRadio("op",NULL,c("+","-","*","/"), width = "100px")),
        # column(3,selectInput("right",NULL,c("[0]","[1]","[2]"),selected = "[1]"),selectize = FALSE),
        # column(2,actionBttn(inputId = "btncomp",label = "=")),
        column(2,uiOutput(outputId = "result")),
        column(2,uiOutput(outputId = "generator"))  
    ),
    fluidRow(align = "center",uiOutput("mod")),
    fluidRow(align = "center",uiOutput("replace")),
    fluidRow(align = "center",uiOutput("show"))
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("fieldcalc.R")

quadPoly <- function(x, a, b, p) {
  (x^2 + a*x + b)%%p
}


#Functions that read the input and modify the output and input
server <- function(session, input, output) {

  #Variables that are shared among server functions
  p <<- 5
  degree <<- 2
  gen <<- c(0,1)
  # coeffs <- vector("list",49)
  # dropdowns <- vector("list",49)
  # displays <- vector("list",49)
  
    #Initialization
  # updateSelectInput(session,"left",choices = c("[0]","[1]","[2]")) 
  # updateSelectInput(session,"right",choices = c("[0]","[1]","[2]"))            
    #Functions that respond to events in the input
  observeEvent(input$menu, {
    if(input$menu == "poly1") {
      variableReplace <<- c(3,0)
      generatorText <- renderText("'X' is NOT a generator")
    }
    if(input$menu == "poly2") {
      variableReplace <<- c(2,0)
      generatorText <- renderText("'X' is NOT a generator")
    }
    if(input$menu == "poly3"){
      variableReplace <<- c(4,4)
      generatorText <- renderText("'X' is NOT a generator")
    }
    if(input$menu == "poly4") {
      variableReplace <<- c(3,4)
      generatorText <- renderText("'X' is a generator")
    }
    if(input$menu == "poly5") {
      variableReplace <<- c(2,3)
      generatorText <- renderText("'X' is a generator")
    }
    if(input$menu == "poly6") {
      variableReplace <<- c(1,3)
      generatorText <- renderText("'X' is NOT a generator")
    }
    if(input$menu == "poly7") {
      variableReplace <<- c(2,2)
      generatorText <- renderText("'X' is a generator")
      
    }
    if(input$menu == "poly8") {
      variableReplace <<- c(1,2)
      generatorText <- renderText("'X' is NOT a generator")
    }
    if(input$menu == "poly9") {
      variableReplace <<- c(4,1)
      generatorText <- renderText("'X' is NOT a generator")
    }
    if(input$menu == "poly10") {
      variableReplace <<- c(3,1)
      generatorText <- renderText("'X' is a generator")
    }
    
    result <- character(0)
    # for (i in 24) {
    #   
    #   coeffs <- powerTable(p,degree,gen,variableReplace)
    #   result <- paste(exp, result, lapply(coeffs, convertPoly), sep="<br/>")
    # }
    # coeffs <- powerTable(p,degree,gen,variableReplace)
    # newResult <- lapply(coeffs, convertPoly)
    # output$result <- renderUI((newResult))
    
    coeffs <- powerTable(p,degree,gen,variableReplace)
    result <- paste(result, lapply(coeffs, convertPoly), sep="<br/>")

    output$result <- renderUI(HTML(result))
    
    output$generator <- (generatorText)
    
    # output$result <- renderUI(lapply(powerTable(p,degree,gen,variableReplace), convertPoly))
    
  #   dropdowns <<- lapply(coeffs,convertPoly)
  #   displays <<- lapply(coeffs,convertPoly,carats = FALSE)
  #   updateSelectInput(session,"left",choices = dropdowns)
  #   updateSelectInput(session,"right",choices = dropdowns) 
  #   output$mod <- renderUI(h3(paste("All coefficients are modulo ",p)))
  #   output$replace <- renderUI("")
  #   if (n > 1)
  #     output$replace <- renderUI(
  #             h3(HTML(paste("Replace x<sup>",n,"</sup>   by ",displays[n+2]))))
  # })
  # observeEvent(input$btncomp, {
  #   index.left <- which(dropdowns == input$left)[1]
  #   index.right <- which(dropdowns == input$right)[1]
  #   if (input$op == "+"){
  #     sum <- (coeffs[[index.left]]+coeffs[[index.right]])%%p
  #     index.sum <- which(sapply(coeffs,function(x)all(x==sum)))
  #     output$result <- renderUI(dropdowns[index.sum])
  #     s <- paste("[",displays[index.left],"]+","[",displays[index.right],
  #                "]=[",displays[index.sum],"]")
  #     output$show <- renderUI(h3(HTML(s)))
  #   }
  #   if (input$op == "-"){
  #     diff <- (coeffs[[index.left]]-coeffs[[index.right]]+p)%%p
  #     index.diff <- which(sapply(coeffs,function(x)all(x==diff)))
  #     output$result <- renderUI(dropdowns[index.diff])
  #     s <- paste("[",displays[index.left],"]-","[",displays[index.right],
  #                "]=[",displays[index.diff],"]")
  #     output$show <- renderUI(h3(HTML(s)))
  #   }
  #   if (input$op == "*"){
  #     if ((index.left==1)||(index.right==1)) {
  #       output$result <- renderUI(dropdowns[1])
  #       } 
  #     #Now the index is two greater than the logarithm
  #     if ((index.left>1)&&(index.right>1)) {
  #       log <- index.left+index.right-4
  #       index.prod <- log%%(p^n-1)+2
  #       output$result <- renderUI(h4(dropdowns[index.prod]))
  #       s <- paste("[",displays[index.left],"]*","[",displays[index.right],
  #                  "]=[",displays[index.prod],"]")
  #       output$show <- renderUI(h3(HTML(s)))
  #     }
  #   }
  #   if (input$op == "/"){
  #     if (index.right == 1)
  #       output$result <- renderUI("Cannot divide by zero!")
  #     if (index.right>1 && index.left == 1) output$result <- renderUI(dropdowns[1]) 
  #     if (index.left > 1 && index.right >1){
  #       #Take the difference of logarithms - number of nonzero field elements
  #       #Add 2 to this logarithm to get the table index
  #       index.div <- (index.left-index.right+p^n-1)%%(p^n-1)+2
  #       output$result <- renderUI(dropdowns[index.div])
  #       s <- paste("[",displays[index.left],"]/","[",displays[index.right],
  #                  "]=[",displays[index.div],"]")
  #       output$show <- renderUI(h3(HTML(s)))
  #     }
  #   }
  #   
  })

}

#Run the app
shinyApp(ui = ui, server = server)


