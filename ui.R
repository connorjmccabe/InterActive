#OLS VERSION
# ui.R
require(foreign)
# require(shinyjs)
require(pander)


shinyUI(fluidPage(
  titlePanel("InterActive: A tool for the quantitative display of interactions"),

  sidebarLayout(
    # column(8,
    sidebarPanel(
      # helpText("Plot Interactions Dynamically using Johnson-Neyman (J-N) Technique."),
      # helpText("Connor McCabe, University of Washington"),

      fileInput('file1', 'Choose File (max size = 30 MB)',
               accept=c('text/csv',
                        'text/comma-separated-values,text/plain',
                        '.csv',
                        '.sav')),

      # if (!is.null(input$file1)){
      tags$hr(),
      uiOutput("ui.categorical"),
      uiOutput("ui.scale"),
      uiOutput("ui.manual"),
      uiOutput("ui.formula"),
      uiOutput("ui.covars"),
      uiOutput("ui.foc"),
      uiOutput("ui.mod"),
      uiOutput("ui.dv"),
      uiOutput("ui.poly"),
      uiOutput("ui.go"),
    width = 3),

    mainPanel(em(h2(textOutput("msg"))),
      tabsetPanel(
      tabPanel("Plot",
               
               column(7,
                      fluidRow(
                        column(2, align="center",
                              ""),
                        column(2, align="center",
                              uiOutput("ui.sm1")),
                        column(2, align="center",
                              uiOutput("ui.sm2")),
                        column(2, align="center",
                              uiOutput("ui.sm3")),
                        column(2, align="center",
                              uiOutput("ui.sm4")),
                        column(2, align="center",
                              uiOutput("ui.sm5"))),
                      plotOutput("modplot"),
                      uiOutput("ui.static"),
                      uiOutput("ui.modlevel"),
                      uiOutput("ui.downloadplot")),
               column(4,
                      textOutput("regFormula"),br(),
                      textOutput("resultstitle"),
                      tableOutput("results"),
                      textOutput("resultstitlecount"),
                      tableOutput("resultscount"),
                      # textOutput("test.tab"),
                      textOutput("co")
               )
               ),
      tabPanel("Customize Plot",
               plotOutput("plot.final"),
               fluidRow(column(4,
                               uiOutput("ui.xlab"),
                               uiOutput("ui.ylab")),
                        column(5,
                               uiOutput("ui.title")),
                        column(6,
                               uiOutput("ui.greyscale"))
               ),
               downloadButton('downloadPlot.final', 'Download Plot')
               # column(4,
               #       tableOutput("rostest")
               #         )
      ),
      tabPanel("Marginal Effects",
               # verbatimTextOutput("focal.hyp"),
               column(8,
                      plotOutput("rosplot"),
                      downloadButton('rosplot.final',"Download Regions of Significance Plot")),
               column(4,
                      tableOutput("rostest")
               )),
      tabPanel("Plot Data",
               column(8,
                      tableOutput("plotdata")),
               column(4,
                      downloadButton('downloadEst', 'Download Plot Data'))
      ),
      tabPanel("Data",tableOutput("datashow"))

      )#tabsetPanel
      ) #mainpanel
    # )#column
  ) #sidebarlayout
))
