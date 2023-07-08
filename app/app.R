#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tuneR)
library(neuralcoil)
library(plotly)

addResourcePath("Music", "Music")

#Functions
adjust_speed <- function(signal, original_interval, new_interval) {
    # Calculate the original and new lengths of the signal
    original_length <- length(signal)
    new_length <- round(original_length * new_interval / original_interval)
    
    # Generate the indices for resampling
    original_indices <- seq_along(signal)
    new_indices <- seq(1, original_length, length.out = new_length)
    
    # Perform linear interpolation to adjust the speed
    adjusted_signal <- approx(original_indices, signal, new_indices)$y
    
    return(adjusted_signal)
}

modify_tracks <- function(tracklist,pan_coil_pmat,vol_coil_pmat,dt,pan_factor,vol_factor,speed_factor){
    slices = unique(seq(1,length(tracklist[[1]]),dt),length(tracklist[[1]]))
    newtracklist=list()
    for (itrack in 1:ntracks){
        temptrack=data.frame(left=NULL,right=NULL)
        
        for (iSS in 1:(length(slices)-1)){
            selvec = slices[iSS]:(slices[iSS+1])
            modsec = tracklist[[itrack]][selvec,]
            if (length(modsec@right)==0){
                modsec@right = as.vector(array(0,length(modsec@left)))
            }
            pans = c(pan_coil_pmat[iSS,seq(1,dim(pan_coil_pmat)[2],2)[itrack]],
                     pan_coil_pmat[iSS,seq(2,dim(pan_coil_pmat)[2],2)[itrack]])
            pans = pans/sum(pans)*length(pans)
            pans = (1-pan_factor)+pans*pan_factor
            
            modsec@left = modsec@left*pans[1]
            modsec@right = modsec@right*pans[2]
            
            speeds = vol_coil_pmat[iSS,(ntracks+1):(ntracks*2)]
            speeds = speeds/sum(speeds)*length(speeds)
            speeds = (1-speed_factor)+speeds*speed_factor
            if (!length(speeds)==ntracks){
                sdadasdad
            }

            vols = vol_coil_pmat[iSS,1:ntracks]
            vols = vols/sum(vols)*length(vols)
            vols = (1-vol_factor)+vols*vol_factor
            if (!length(vols)==ntracks){
                sdadasdad
            }
            
            modsec@left = adjust_speed(modsec@left,1,speeds[itrack])*vols[itrack]
            modsec@right = adjust_speed(modsec@right,1,speeds[itrack])*vols[itrack]
            temptrack = rbind(temptrack,data.frame(left = modsec@left,right=modsec@right))
        }
        newtracklist[[itrack]]=temptrack
    }
    
    #combine tracks
    combined_left <- numeric(0)
    combined_right <- numeric(0)
    for (i in 1:length(newtracklist)) {
        track <- newtracklist[[i]]
        if (length(combined_left)==0) {
            combined_left <- track$left
            combined_right <- track$right
        } else {
            combined_left <- combined_left + track$left
            combined_right <- combined_right + track$right
        }
    }
    combined_wave <- tuneR::normalize(Wave(cbind(combined_left, combined_right), 
                                           samp.rate = tracklist[[1]]@samp.rate,
                                           bit=tracklist[[1]]@bit),unit=as.character(tracklist[[1]]@bit))
    
    #writeWave(combined_wave, "combined_audio.wav")
}

#Global Variables

#Read in track list. This multi-track provided by https://www.cambridge-mt.com/ms/mtk/#FriendZWorldMusic

inputdir = "inputs/multitrack/FunnyValentines_SleighRide"
trackfiles = list.files(inputdir,"[.]wav")
tracklist = list()
for (itr in trackfiles){
    tracklist[[paste(itr)]]=tuneR::readWave(file.path(inputdir,itr))
}

#Determine Required Coil Size
Tlen <<- 30 #time length of generated coil
ntracks = length(tracklist)
dt=round(length(tracklist[[1]]@left)/Tlen)
n.s <<- ntracks*2

#For pan oscillation, we need 2 x <ntracks>, with <ntracks> subgroups.
#For volume and speed oscillation we need 2 subgroups of size <ntracks>
#These will be treated as separate coils

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(width=4,
               numericInput("pan_factor","Pan Effect",value=0,min=0,max=1)
               ),
        column(width=4,
               numericInput("vol_factor","Volume Effect",value=0,min=0,max=1)
               ),
        column(width=4,
               numericInput("speed_factor","Speed Effect",value=0,min=0,max=1)
               )
    ),
    fluidRow(
        actionButton("generate","Generate Coils"),
        actionButton("play","Play")
    ),
    fluidRow(
        column(width = 6,
            plotlyOutput("volbar"),
            plotOutput("volPlot")
        ),
        column(width=6,
            plotlyOutput("speedbar"),
            plotOutput("panPlot")
        )
    )
            
)

server <- function(input, output, session) {
    
    timer = reactiveVal(0)
    uservals = reactiveValues(pan_coil_pmat=readRDS("saves/pan_coil.RdA"),
                              vol_coil_pmat=readRDS("saves/vol_coil.RdA"),
                              dt = 0)

    observe({
        if (uservals$dt>0){
            invalidateLater(uservals$dt*1000,session)
            isolate({
                if (timer()<Tlen){
                    timer(timer()+1)
                    print(timer())
                }else{
                    timer(0)
                    uservals$dt = 0
                }

            })
        }
    })

    output$volbar <- renderPlotly({
        if (timer()>0){
            dataplot = data.frame(Category = gsub("[.]wav","",trackfiles),
                                  Value = uservals$vol_coil_pmat[timer(),1:ntracks])
            plot_ly(dataplot, x = ~Category, y = ~Value, type = "bar",
                    marker = list(colorscale = list(c(0,1), c("lawngreen", "red")),color = ~Value)) %>%
                layout(title = "Volume Graph", xaxis = list(title = "Category"), yaxis = list(title = "Value"),
                       bargap = 0)
        }
    })
    
    output$speedbar <- renderPlotly({
        if (timer()>0){
            dataplot = data.frame(Category = gsub("[.]wav","",trackfiles),
                                  Value = uservals$vol_coil_pmat[timer(),(ntracks+1):(ntracks*2)])
            plot_ly(dataplot, x = ~Category, y = ~Value, type = "bar",
                    marker = list(colorscale = list(c(0,1), c("lawngreen", "red")),color = ~Value)) %>%
                layout(title = "Speed Graph", xaxis = list(title = "Category"), yaxis = list(title = "Value"),
                       bargap = 0)
        }
    })
    
    output$volPlot <- renderPlot({
        if (length(uservals$vol_coil_pmat)>0){
            matplot(uservals$vol_coil_pmat,type="l")
        }
        
    })
    
    output$panPlot <- renderPlot({
        if (length(uservals$pan_coil_pmat)>0){
            matplot(uservals$pan_coil_pmat,type="l")
        }
    })
    
    observeEvent(input$generate,{
        uservals$pan_coil_pmat = NULL
        uservals$vol_coil_pmat = NULL
        
        print("generating coils")
        #pan_coil
        buildcoil(n.s,sym=F)
        rotvals=runif(3,0,1000)
        startvals=complex(length(n.s),runif(n.s,0,1),runif(n.s,0,1))
        RandVec=complex(length(group.index),runif(length(group.index),0,1),runif(length(group.index),0,1))
        cont<<-T
        loc<<-T
        sub.num<<-ntracks #Number of conserved subgroups
        vfara_inert<<-10 #inertia
        vfara_init<<-1 #initial inertia
        
        coil_out=runcoil(RandVec = RandVec,rotvals = rotvals,startvals=startvals)
        Pmat=coil_out[[1]]
        complex_states=coil_out[[2]]
        uservals$pan_coil_pmat = Pmat
        
        #Volume and speed coil
        buildcoil(n.s,sym=F)
        rotvals=runif(3,0,1000)
        startvals=complex(length(n.s),runif(n.s,0,1),runif(n.s,0,1))
        RandVec=complex(length(group.index),runif(length(group.index),0,1),runif(length(group.index),0,1))
        cont<<-T
        loc<<-T
        sub.num<<-2 #Number of conserved subgroups
        vfara_inert<<-10 #inertia
        vfara_init<<-1 #initial inertia
        
        coil_out=runcoil(RandVec = RandVec,rotvals = rotvals,startvals=startvals)
        Pmat=coil_out[[1]]
        complex_states=coil_out[[2]]
        uservals$vol_coil_pmat = Pmat
        
        print("Coils Ready")
    })
    
    
    observeEvent(input$play, {
        newaudio <- modify_tracks(tracklist,uservals$pan_coil_pmat,uservals$vol_coil_pmat,dt,input$pan_factor,input$vol_factor,input$speed_factor)
        writeWave(newaudio, "Music/newfile.wav")
        
        uservals$dt = length(newaudio@left)/newaudio@samp.rate/Tlen
        insertUI(selector = "#play",
                 where = "afterEnd",
                 ui = tags$audio(src = "Music/newfile.wav", type = "../inputs/newfile.wav", autoplay = TRUE, controls = NA, style="display:none;"), immediate = TRUE 
        )
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
