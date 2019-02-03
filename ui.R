shinyUI(pageWithSidebar(
  headerPanel("Calculation of CI using PL method"),
  
  sidebarPanel(
    h5("Before uploading:"),
    helpText("1. Fit the model; 
             2. Exclude the parameter of interest from fitting; 
             3. Scan parameter of interest (Exact+Fitting); 
             4. Upload the file."),
    
    tags$hr(),
    fileInput('file1', 'Upload DBSolve output file:',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    tags$hr(),
    
    selectInput("confLevel", "Confidence level:",
      choices=c("0.5","0.75","0.9","0.95","0.99"), selected="0.95"),
    
    checkboxInput('logF.0.', 'Logarythmic F.0.', F),
    
    checkboxInput("auto", "Manual optimum"),
    conditionalPanel(
      condition = "input.auto == true",
      numericInput("parbest", "Optimal value of parameter:", 1),
      numericInput("F.0.best", "Optimal value of F.0.", 0.01)
    ),
    
    numericInput("Nexp", "Number of experimental points:", 30),
    
    numericInput("Npar", "Number of free parameters:", 4),
    
    checkboxInput('useFirstCol', 'Use first column', F),
    
    tags$hr(),
    h5("Troubles:"),
    helpText("If left or right confidense level is not reached, try an another interval for scan.")
  ),
  
  mainPanel(
    h3("Calculated statistics:"),
    tableOutput("stat"),
    downloadButton('downloadPDF', 'Save results as .PDF'),
    tags$hr(),
    
    tabsetPanel(
      tabPanel("Objective function profile", plotOutput("plot1"), plotOutput("plot1limited")),
      tabPanel("Confidence profile", plotOutput("plot2"), plotOutput("plot2limited")),
      tabPanel("Input", tableOutput('contents'))
      )
  )
))
