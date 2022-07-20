library(shiny)
library(shinythemes)
library(DT)
library(rhandsontable)
library(markdown)
library(knitr)


ui = navbarPage("appRiori",
    theme = shinythemes::shinytheme("yeti"),
    tabPanel("Introduction", 
             navlistPanel("Intro, basics and tutorials",
                          tabPanel("Welcome",
                                   withMathJax(includeMarkdown("welcome.md"))),
                          tabPanel("Some theorical aspects",
                                   withMathJax(includeMarkdown("basics.md"))),
                          tabPanel("How appRiori works",
                                   withMathJax(includeMarkdown("tutorial0.md"))),
                          tabPanel("Type of contrasts",
                                   uiOutput('markdown')),
                          tabPanel("Example 1: Single variable",
                                   uiOutput('example1')),
                          tabPanel("Example 2: Interactions",
                                   uiOutput('example2'))
             )
             ),
    tabPanel("Data", 
             sidebarLayout(
               sidebarPanel(
                 
                 
                 
                fileInput("file1", "Choose CSV File",
                                multiple = FALSE,
                                accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                                                                ".csv")),
                 
                 tags$hr(),
                 
                 
                 checkboxInput("header", "Header", TRUE),
                 
                 
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ";"),
                 
                 
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = NULL),
                 
                 
                 radioButtons("deci", "Decimal",
                              choices = c("Comma" = ',',
                                          "Period" = '.'),
                              selected = '.'),
                 
                 
                 tags$hr(),
                 
                 checkboxGroupInput("show_vars", "Columns to show:",
                                    choices = NULL, selected = NULL)
                 
               ),
               
               
               
               mainPanel(
                 h2("Your data"),
                 
                 DT::dataTableOutput("contents"),
                 
                 
                 tags$hr(),
                 
                 h2("Data structure"),
                 verbatimTextOutput("structure")
               )
             )
    ),
    tabPanel("Single variable",
             sidebarLayout(
               sidebarPanel(
                 selectInput('in1', 'Select the variable', c(), selectize=TRUE),
                 
                 
                 tags$hr(),
                 selectInput('cont', 'Select contrast type', c("Dummy","Deviation","Scaled",
                                                               "Sliding difference", "Helmert",
                                                               "Reverse Helmert","Polynomial",
                                                               "Customized"), selectize=TRUE),
                 
                 conditionalPanel("input.cont=='Customized'",
                                  
                                  selectInput('hm1', 'How many comparisons do you want to set?', c(),selectize = T)
                                  ),
                 hr(),
                 tags$h3("Correlation matrix"),
                 verbatimTextOutput("cormat"),
                 tags$h3("Check for linear dependence"),
                 verbatimTextOutput(outputId='contrasts_warnings')
               ),

               
               mainPanel(h2("Contrasts on single variables"),
                 fluidRow(
                   column(3,
                          h3("Levels"),
                          verbatimTextOutput("lev")
                   ),
                   column(3,
                          h3("Original contrast matrix"),
                          verbatimTextOutput("original")
                   ),
                   column(3,
                          h3("New contrast matrix"),
                          verbatimTextOutput("new")
                   ),
                   column(3,
                          h3("Hypothesis matrix"),
                          verbatimTextOutput("hypmat")
                   )
                 ),
                 
                 fluidRow(
                   conditionalPanel("input.cont== 'Customized'",
                                    uiOutput("inputGroup")),
                   
                   
                   h3("Get your code!!"),
                   actionButton("sub", "Submit"),
                   conditionalPanel("input.sub>0",
                                    verbatimTextOutput("res")
                 )
                 
               )
             )
             )
  ),
  
  tabPanel("Interactions",
           sidebarLayout(
             sidebarPanel(
               selectInput("radio", label = h3("Type of interaction"),
                            choices = c("Two way", "Three way"), 
                           selectize=TRUE),
               
               checkboxInput("onlyI", label = "Only Interaction", value = FALSE),

               
               checkboxInput("fc2", label = "Fully customized", value = FALSE),
               
               conditionalPanel("input.fc2==true && input.onlyI==true",
                                
                                span(tags$h4("Please, DEFLAG one option."),style="color:red")
               ),
               
               conditionalPanel("input.fc2==true ",
                                
                                selectInput('hm2', 'How many comparisons do you want to set?', c(),selectize = T)
               ),
               hr(),
               tags$h3("Correlation matrix"),
               verbatimTextOutput("cormat_int"),
               tags$h3("Check for linear dependence"),
               verbatimTextOutput(outputId='contrasts_warnings2')
               
             ),
             
             mainPanel(
               h2("Select you variables"),
                       
                         column(4,
                                selectInput('v1', 'Select the first variable', c(), selectize=TRUE),
                                
                                selectInput('cont1', 'Select the first contrast type', c("Dummy","Deviation","Scaled",
                                                                              "Sliding difference", "Helmert",
                                                                              "Reverse Helmert","Polynomial"), selectize=TRUE)
                         ),
                         column(4,
                                selectInput('v2', 'Select the second variable', c(), selectize=TRUE),
                                
                                selectInput('cont2', 'Select the second contrast type', c("Dummy","Deviation","Scaled",
                                                                               "Sliding difference", "Helmert",
                                                                               "Reverse Helmert","Polynomial"), selectize=TRUE)
                         )
                         ,
                         column(4,
                                conditionalPanel(condition="input.radio == 'Three way' ",

                                  selectInput('v3', 'Select the third variable', c(), selectize=TRUE),

                                  selectInput('cont3', 'Select the third contrast type', c("Dummy","Deviation","Scaled",
                                                                                 "Sliding difference", "Helmert",
                                                                                 "Reverse Helmert","Polynomial"), selectize=TRUE)
                                )
                         ),
               
                       
                         fluidRow(
                                h3("Levels"),
                                verbatimTextOutput("lev_int")
                         ),
                         fluidRow(
                                h3("Original contrast matrix"),
                                verbatimTextOutput("original_int")
                         ),
                         fluidRow(
                                h3("New contrast matrix"),
                                verbatimTextOutput("new_int")
                         ),
                         fluidRow(
                                h3("Hypothesis matrix"),
                                verbatimTextOutput("hypmat_int")
                         )
                         
                       ,

                       fluidRow(
                         conditionalPanel("input.fc2==true",
                                          uiOutput("inputGroup2")),
                         hr()
                         ),
                       fluidRow(h3("Get your code!!"),
                                  actionButton("sub2", "Submit"),
                                  conditionalPanel("input.sub2>0",
                                              verbatimTextOutput("res_int"))

                         
                       
             )
           )
  )

)

)
