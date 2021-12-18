library(shiny)
library(shinydashboard)
library(bioacoustics)
library(warbleR)
library(monitoR)
library(seewave)
library(lubridate)
library(keras)
library(imager)
library(jpeg)
library(plotly)
library(ggplot2)
library(listarrays)

modelo<-load_model_hdf5("C:/Users/Usuario/Desktop/NUEVA APP/modelo.h5")

ui <- navbarPage("Detección e identificación de llamadas de quirópteros",
                 tabPanel("Wav",
                          sidebarLayout(
                              sidebarPanel(
                                  fileInput(inputId = "audio", 
                                            label = "Seleccione archivos wav",
                                            accept = c(".wav"),
                                            multiple = TRUE)),
                              mainPanel(
                                  textOutput("call"))
                          )
                 ),
                 tabPanel("JPEG",
                          sidebarLayout(
                              sidebarPanel(
                                  fileInput(inputId = "espectro", 
                                            label = "Seleccione archivos jpeg",
                                            accept = c(".jpeg"),
                                            multiple = TRUE)),
                              mainPanel(
                                  textOutput("predic"))
                          )
                 )
)








server <- function(input, output) {
    
    llamada <- reactive({        
        audio <- input$audio
        if(is.null(audio))
            return(NULL) 
        
        b<-read_audio(audio$datapath)
        b@samp.rate<-250000
        longitud<- length(b@left) / b@samp.rate
        
        
        TD <- threshold_detection(
            b,
            time_exp = 1,
            min_dur = 1, 
            max_dur = 500,
            min_TBE = 10,
            max_TBE = 5000,
            EDG = 0.996,
            LPF = 160000,
            HPF = 6000,
            FFT_size = 256,
            FFT_overlap = 0.875,
            start_thr = 25,
            end_thr = 30, 
            SNR_thr = 10, 
            angle_thr = 45,
            duration_thr = 660,
            NWS = 1000,
            acoustic_feat = TRUE,
            ticks = TRUE)
        
        b<-addsilw(b, at = "start", d = 0.5, f = 250000)
        b<-addsilw(b, at = "end", d = 0.5, f = 250000)
        
        p <-length(TD$data)
        
        f<-nrow(TD$data$event_data)
        
        lala<-print(paste("Nº de llamadas encontradas en", audio[1] , f, sep = ' '))
        
        if(p >= 1){
            
            for (j in (1:f)) {
                
                evento<-TD$data$event_data$starting_time[j]
                
                inicio<-period_to_seconds(hms(evento))-0.2
                final<-inicio+0.4
                
                jpeg(file = paste(audio,j, '.jpeg', sep = ''))
                
                spectro(b, 
                        tlim = c(inicio, final),
                        f = 250000,
                        main = paste(audio,j, sep = '_'))
                dev.off()
                
            }
        }
        return(lala)
        
    })
    
    output$call <- renderText({ 
        return(llamada())
    })
    
    
    
    prediccion <- reactive({
        
        espectro <- input$espectro
        if(is.null(espectro))
            return(NULL) 
        
        
        imagen <- image_load(espectro$datapath)
        imagen <- image_to_array(imagen)
        imagen <- image_array_resize(imagen, 
                                     height = 100, width = 100)
        imagen<-expand_dims(imagen,1)
        imagen <- 1-(imagen/255)
        
        pred<-modelo %>%
            predict(imagen)
        
        prob<-max(pred)
        prob<-round(as.numeric(prob),3)
        
        n<-which.max(pred)
        
        bats<-c("Eptesicus serotinus", "Hypsugo savii",
                "Miniopterus schreibersii", "Nyctalus leisleri",        
                "Nyctalus noctula", "Pipistrellus kuhlii",      
                "Pipistrellus nathusii", "Pipistrellus pipistrellus",
                "Pipistrellus pygmaeus", "Plecotus austriacus",      
                "Vespertilio murinus") 
        
        especie<-bats[n]
        
        result<-paste(especie,"con una probabilidad de", prob,sep = ' ')
        return(result)
    })
    
    output$predic <- renderText({ 
        return(prediccion())
    })
    
    
}

shinyApp(ui, server)

