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
      numericInput("r", "Parameter r", value = 3.9),
      numericInput("x0", "Initial value x0", value = 0.2),
      div(h4("Play"),style = "color: black;",
          actionButton("play","Play"))
    ),
    mainPanel(
      plotOutput("oscillatorPlot")
    )
  )
)

server <- function(input, output, session) {
  output$oscillatorPlot <- renderPlot({
    Tlen <- 1000  # Length of the chaotic oscillator array
    r <- input$r
    x0 <- input$x0
    
    oscillator <- chaoticOscillator(Tlen, r, x0)
    plot(oscillator, type = "l", xlab = "Time", ylab = "Amplitude")
  })
  
  observeEvent(input$play, {
    Tlen <- 1000  # Length of the chaotic oscillator array
    r <- input$r
    x0 <- input$x0
    
    newaudio@.Data <- apply(audio@.Data,2,function(x)x*chaoticOscillator(Tlen, r, x0))
    
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
