library(shiny, warn.conflicts = FALSE)
library(shinythemes, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(rhandsontable, warn.conflicts = FALSE)
library(markdown, warn.conflicts = FALSE)

ui = navbarPage("appRiori",
    theme = shinythemes::shinytheme("yeti"),
    tabPanel("Introduction",
             navlistPanel("Intro, basics and tutorials",
                          tabPanel("Welcome",
                                   withMathJax(includeMarkdown(system.file("tutorial","welcome.md",package="appRiori")))),
                          tabPanel("Some theorical aspects",
                                   withMathJax(includeMarkdown(system.file("tutorial","basics.md",package="appRiori")))),
                          tabPanel("How appRiori works",
                                   withMathJax(includeMarkdown(system.file("tutorial","tutorial0.md",package="appRiori")))),
                          tabPanel("Type of contrasts",
                                   withMathJax(includeMarkdown(system.file("tutorial","cont.md",package="appRiori")))),
                          tabPanel("Example 1: Single variable",
                                   withMathJax(includeMarkdown(system.file("tutorial","ex1.md",package="appRiori")))),
                          tabPanel("Example 2: Interactions",
                                   withMathJax(includeMarkdown(system.file("tutorial","ex2.md",package="appRiori"))))
             )
             ),
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("data_type", "Which dataset to use?",
                              choices = c("Preinstalled/default dataset" = "preinstalled",
                                          "Upload own data (CSV file)" = "upload"),
                              selected = NULL),




                 tags$hr(),

                 conditionalPanel("input.data_type == 'preinstalled'",

                                  selectInput("default_data", "Default dataset", NULL),

                  ),


                 conditionalPanel("input.data_type == 'upload'",

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
                                               selected = '.')


                  ),

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
                 selectInput('in1', h3(strong('Step 1: Select the variable'), style = "font-size:26px;"), c(), selectize=TRUE),


                 tags$hr(),
                 selectInput('cont', h3(strong('Step 2: Select the contrast type'), style = "font-size:26px;"), c("Treatment","Simple","Sum","Scaled",
                                                               "Sliding difference", "Helmert",
                                                               "Reverse Helmert","Polynomial",
                                                               "Customized"), selectize=TRUE),

                 conditionalPanel("input.cont=='Customized'",

                                  selectInput('hm1', 'How many comparisons do you want to set?', c(),selectize = T)
                                  ),
                 hr(),
                 tags$h3(strong("Step 3.1: Examine the contrasts correlation matrix"), style = "font-size:26px;"),
                 verbatimTextOutput("cormat"),
                 conditionalPanel("input.cont=='Customized'",
                   tags$h3(strong("Step 3.1.1: Check for linear dependence"), style = "font-size:26px;"),
                   verbatimTextOutput(outputId='contrasts_warnings')),
                 tags$h3(strong("Step 4: Information on your selection"), style = "font-size:26px;"),
                 verbatimTextOutput("selection")

               ),


               mainPanel(h3(strong("Step 3.0: Watch the results"), style = "font-size:26px;"),
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


                   h3(strong("Step 5: Get your code!!"), style = "font-size:26px;"),
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
               selectInput("radio", label = h3(strong("Step 1: Select the type of design"), style = "font-size:26px;"),
                            choices = c("Two way", "Three way"),
                           selectize=TRUE),
               #
               # checkboxInput("onlyI", label = "Only Interaction", value = FALSE),
               #
               #
               checkboxInput("fc2", label = h4("Fully customized design"), value = FALSE),

               # conditionalPanel("input.fc2==true && input.onlyI==true",
               #
               #                  span(tags$h4("Please, DEFLAG one option."),style="color:red")
               # ),

               hr(),
               tags$h3(strong("Step 4.1: Examine the contrasts correlation matrix"), style = "font-size:26px;"),
               verbatimTextOutput("cormat_int"),
               conditionalPanel("input.fc2==true ",
                 tags$h3(strong("Step 4.1.1: Check for linear dependence"), style = "font-size:26px;"),
                 verbatimTextOutput(outputId='contrasts_warnings2')),
               tags$h3(strong("Step 5: Information on your selection"), style = "font-size:26px;"),
               verbatimTextOutput("selection2")

             ),

             mainPanel(
               h3(strong("Step 2: Select your variables"), style = "font-size:26px;"),

                         fluidRow(
                           column(4,
                                  selectInput('v1', h4(em('Step 2.1: Select the first variable')), c(), selectize=TRUE)
                           ),
                           column(4,
                                  selectInput('v2', h4(em('Step 2.2: Select the second variable')), c(), selectize=TRUE)
                           ),
                           column(4,
                                  conditionalPanel(condition="input.radio == 'Three way' ",
                                    selectInput('v3', h4(em('Step 2.3: Select the third variable')), c(), selectize=TRUE)
                                  )
                           ),
                         ),

                         conditionalPanel("input.fc2==false",
                                          fluidRow(
                                            column(4,
                                                   selectInput('cont1', h4(strong('Step 3.1: Select the first contrast type')), c("Treatment","Simple","Sum","Scaled",
                                                                                                                                  "Sliding difference", "Helmert",
                                                                                                                                  "Reverse Helmert","Polynomial","Customized"), selectize=TRUE)
                                            ),
                                            column(4,
                                                   selectInput('cont2', h4(strong('Step 3.2: Select the second contrast type')), c("Treatment","Simple","Sum","Scaled",
                                                                                                                                   "Sliding difference", "Helmert",
                                                                                                                                   "Reverse Helmert","Polynomial","Customized"), selectize=TRUE)
                                            )
                                            ,
                                            column(4,
                                                   conditionalPanel(condition="input.radio == 'Three way' ",
                                                                    selectInput('cont3', h4(strong('Step 3.3: Select the third contrast type')), c("Treatment","Simple","Sum","Scaled",
                                                                                                                                                   "Sliding difference", "Helmert",
                                                                                                                                                   "Reverse Helmert","Polynomial","Customized"), selectize=TRUE)
                                                   )
                                            ),
                                          ),


                                          fluidRow(
                                            conditionalPanel("input.cont1=='Customized'",
                                                             h4("Customized Contrasts, first variable"),
                                                             selectInput('ihm1', 'How many comparisons do you want to set?', c(),selectize = T),
                                                             uiOutput("inputGroup.i1")
                                            ),

                                          ),

                                          fluidRow(
                                            conditionalPanel("input.cont2=='Customized'",
                                                             h4("Customized Contrasts, second variable"),
                                                             selectInput('ihm2', 'How many comparisons do you want to set?', c(),selectize = T),
                                                             uiOutput("inputGroup.i2")
                                            ),

                                          ),
                                          fluidRow(
                                            conditionalPanel("input.cont3=='Customized'",
                                                             h4("Customized Contrasts, third variable"),
                                                             selectInput('ihm3', 'How many comparisons do you want to set?', c(),selectize = T),
                                                             uiOutput("inputGroup.i3")
                                            ),
                                          ),
                         ),

                         conditionalPanel("input.fc2==true",
                                          h4(strong('Step 3: Fully customized contrasts')),
                                          selectInput('hm2', 'How many comparisons do you want to set?', c(),selectize = T),
                                          uiOutput("inputGroup2")
                         ),

               h3(strong("Step 4: Watch the results"), style = "font-size:26px;"),
                         fluidRow(
                           column(6,
                                h3("Levels"),
                                verbatimTextOutput("lev_int")
                         ),
                         column(6,
                                h3("Original contrast matrix"),
                                verbatimTextOutput("original_int")
                         )),
                         fluidRow(
                           column(6,
                                h3("New contrast matrix"),
                                verbatimTextOutput("new_int")
                         ),
                         column(6,
                                h3("Hypothesis matrix"),
                                verbatimTextOutput("hypmat_int")
                         ))

                       ,

                       fluidRow(
                         hr()
                         ),
                       fluidRow(h3(strong("Step 6: Get your code!!"), style = "font-size:26px;"),
                                  actionButton("sub2", "Submit"),
                                  conditionalPanel("input.sub2>0",
                                              verbatimTextOutput("res_int"))



             )
           )
  )

)

)
