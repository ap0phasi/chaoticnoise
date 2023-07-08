library(shiny)
library(tuneR)

addResourcePath("Music", "Music")
#Global Variables
# Read the stereo audio file
audio <- readWave("../inputs/8_Channel_ID.wav")

# Extract left and right channels
newaudio <- audio

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("oscillatorType", "Select Oscillator Type",
                  choices = c("Chaotic", "Cosine")),
      conditionalPanel(
        condition = "input.oscillatorType == 'Chaotic'",
        numericInput("r", "Parameter r", value = 3.9),
        numericInput("x0", "Initial value x0", value = 0.2)
      ),
      conditionalPanel(
        condition = "input.oscillatorType == 'Cosine'",
        numericInput("amplitude", "Amplitude", value = 1),
        numericInput("frequency", "Frequency", value = 1)
      ),
        actionButton("play","Play")
    ),
    mainPanel(
      plotOutput("oscillatorPlot")
    )
  )
)

server <- function(input, output, session) {
  
  oscillatorValues <- reactiveValues(values=NULL)
  
  output$oscillatorPlot <- renderPlot({
    req(input$oscillatorType)
    print(oscillatorValues$values)
    plot(oscillatorValues$values[1:100],type="l")
  })
  
  observe({
    if (input$oscillatorType=="Chaotic"){
      Tlen <- dim(newaudio@.Data)[1] 
      r <- input$r
      x0 <- input$x0
      
      oscillatorValues$values <- chaoticOscillator(Tlen, r, x0)
    }else if (input$oscillatorType == "Cosine"){
      amp <- input$amplitude
      freq <- input$frequency
      
      Tlen <- dim(newaudio@.Data)[1] 
      oscillatorValues$values <- cos((1:Tlen)*freq)*amp
    }
  })
  
  observeEvent(input$play, {
    newaudio@.Data <- apply(audio@.Data,2,function(x)x*oscillatorValues$values)
    
    # Create unique channel names
    channel_names <- c("FL", "FR", "FC", "LF", "BL", "BR", "SL", "SR")
    
    # Assign the channel names to the WaveMC object
    colnames(newaudio@.Data) <- channel_names
    writeWave(newaudio, "Music/newfile.wav")
    
 
  insertUI(selector = "#play",
             where = "afterEnd",
             ui = tags$audio(src = "Music/newfile.wav", type = "../inputs/newfile.wav", autoplay = TRUE, controls = NA, style="display:none;"), immediate = TRUE 
  )
  })
  
}

shinyApp(ui, server)
