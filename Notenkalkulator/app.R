#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Notenkalkulator Biologie BSc, Eintritt ab HS2020"),

    # Sidebar with a slider input for number of bins 
    mainPanel(
      helpText("Notenkalkulator für Bachelorstudierende im Studienreglement 2020."),
      helpText("Für die Berechnung der Durchschnittsnote werden nur die Noten für unten aufgelistete Lerneinheiten betrachtet."),
      helpText("Noten für Kompensationsfächer (0-2, beim Nichtbestehen von LE im 2. Jahr) kann man bei Kursen des 3. Jahres eintragen."),
      helpText("Noteneingaben < 1 werden als 1 betrachtet, > 6 als 6."),
    
    # Basisjahr  
    
    h3("Basisjahr"),
      
    fluidRow(
        column(2, helpText("Basisjahr")),
        column(3, 
            numericInput("block_1",
                        "Block 1:",
                        min = 1,
                        max = 6, 
                        step = 0.01,
                        value = 4)
        ),
        column(3,
               numericInput("block_2",
                           "Block 2:",
                           min = 1,
                           max = 6, 
                           step = 0.01,
                           value = 4)
        ),
        column(3,
               fluidRow(
                 helpText("Durchschnitt Basisjahr")
               ),
               fluidRow(textOutput('basisjahr_avg'))
              )

        ),
    
    # Zweites Jahr
    
    h3("Fächer des 2. Jahres"),
    
    fluidRow(
        
        # 3. Semester
        column(4,
          fluidRow(
            numericInput("pchem","Physikalische Chemie",
                          min = 1,  max = 6, 
                          step = 0.01,  value = 4)),
          fluidRow(
            numericInput("gdb3multi","GdB III: Multizellularität",
                         min = 1,  max = 6, 
                         step = 0.01,  value = 4)),
          fluidRow(
            numericInput("gdinfo","Grundlagen der Informatik",
                         min = 1,  max = 6, 
                         step = 0.01,  value = 4)),
          fluidRow(
            numericInput("bioanalytik","Bioanalytik",
                         min = 1,  max = 6, 
                         step = 0.01,  value = 4)),
          fluidRow(
            numericInput("statistik2","Statistik II",
                         min = 1,  max = 6, 
                         step = 0.01,  value = 4))
        )
    ),
    
    # Drittes Jahr
    
    fluidRow()
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    validate_grade <- function(grade) {
      if (grade > 6) {
        grade = 6
      }
      if (grade < 1) {
        grade = 1
      }
      return(grade)
    }
      
  
    # Calculate Basisjahr average
      
    basis_avg <- reactive({
      
      b1 <- validate_grade(input$block_1)
      b2 <- validate_grade(input$block_2)
      
      paste(round((b1*16+b2*22) / (16+22), 2))
    
    })    
    
    # Calculate 2nd year average
      
    second_avg <- reactive ({
      # 3rd sem
      
      pc <- validate_grade(input$pchem) #input$physchem
      gdb3 <- validate_grade(input$gdb3multi)
      gdi <- validate_grade(input$gdinfo)
      analytics <- validate_grade(input$bioanalytic)
      stat2 <- validate_grade(input$statistik2)
      
      # 4th sem
        
      ges_kra <- validate_grade()
      gg <- validate_grade()
      biochem <- validate_grade()
      sb <- validate_grade()
      
    })
    
    third_avg <- reactive({
      # compensation
      
      compensate1 <- validate_grade()
      compendate2 <- validate_grade()
      
      # bk, kk
      
      k1 <- validate_grade()
      k2 <- validate_grade()
      k3 <- validate_grade()
      k4 <- validate_grade()
      k5 <- validate_grade()
      k6 <- validate_grade()
      k7 <- validate_grade()
      k8 <- validate_grade()
      k9 <- validate_grade()
      k10 <- validate_grade()
      
    })
    
  
    output$basisjahr_avg <- renderText(basis_avg())
    
    output$secondyear_avg <- renderText(second_avg())
    
    output$thirdyear_avg <- renderText(third_avg())

}

# Run the application 
shinyApp(ui = ui, server = server)
