library('bioacoustics')
library('warbleR')
library('monitoR')
library('seewave')
library('lubridate')     

## Ponemos la carpeta donde están los audios

setwd('C:/Users/Álvaro/Desktop/Master/TFM/Base de datos/AUDIOS')

setwd('C:/Users/Álvaro/Desktop/Master/TFM/Datos/train_ML/00_todos')
setwd('C:/Users/Álvaro/Desktop/Master/TFM/Datos//Bat Calls of Britain and Europe Sound Library')
setwd('C:/Users/Álvaro/Desktop/Master/TFM/Datos/Training-files')
setwd('C:/Users/Álvaro/Desktop/Master/TFM/Pruebas')

## Cogemos los nombres

a<-list.files()

## Creamos la función para extraer audios, plots y excel de las llamadas

extra<-function(a){
  for (i in (1:(length(a)))) {
    
    b<-read_audio(a[i])
    b@samp.rate<-250000
    
    longitud<- length(b@left) / b@samp.rate
    
    
    TD <- threshold_detection(
      b, # Either a path to an audio file (see ?read_audio), or a Wave object
      time_exp = 1, # Time expansion factor of 1. Only needed for bat recordings.
      min_dur = 1, # Minimum duration threshold of 1 milliseconds (ms)
      max_dur = 500, # Maximum duration threshold of 0.5 s 
      min_TBE = 10, # Minimum time window between two audio events of 10 milliseconds
      max_TBE = 5000, # Maximum time window between two audio events, here 5 seconds
      EDG = 0.996, # Temporal masking with Exponential Decay Gain from 0 to 1
      LPF = 160000, # Low-Pass Filter of 150 kHz
      HPF = 6000, # High-Pass Filter of 6 kHz
      FFT_size = 256, # Size of the Fast Fourrier Transform (FFT) window
      FFT_overlap = 0.875, # Percentage of overlap between two FFT windows
      start_thr = 25, # 25 dB threshold at the start of the audio event
      end_thr = 30, # 30 dB threshold at the end of the audio event
      SNR_thr = 10, # 10 dB SNR threshold at which the extraction of the audio event stops
      angle_thr = 45, # 45° of angle at which the extraction of the audio event stops
      duration_thr = 660, # Noise estimation is resumed after 440 ms
      NWS = 1000,
      acoustic_feat = TRUE,
      ticks = TRUE)
    
    b<-addsilw(b, at = "start", d = 0.5, f = 250000)
    b<-addsilw(b, at = "end", d = 0.5, f = 250000)
    
    
    p <-length(TD$data)
    
    if(p >= 1){
      
      dat<-rbind(dat, TD$data$event_data)
      
      f<-nrow(TD$data$event_data)
      print(paste("Nº de llamadas encontradas en",a[i] , f, sep = ' '))
      
      for (j in (1:f)) {
        
        evento<-TD$data$event_data$starting_time[j]
        
        print(paste("Inicio de", j, evento, sep = ' '))
        
        inicio<-period_to_seconds(hms(evento))-0.2
        final<-inicio+0.4
        
        jpeg(file = paste(a[i],j, '.jpeg', sep = ''))
        
        spectro(b, 
                tlim = c(inicio, final),
                f = 250000,
                main = paste(j, a[i], sep = '_'))
        dev.off()
        
      } 
    }else {
      print(paste("No se han encontrado llamadas en", a[i], sep = ' '))
    }
  }
  
  write.csv(dat, "datos_extra.csv")
}

dat<-data.frame(filename=character(), 
                starting_time=character(), 
                duration = numeric(), 
                freq_max_amp= numeric(),  
                freq_max= numeric(), 
                freq_min= numeric(), 
                bandwidth= numeric(), 
                freq_start= numeric(),     
                freq_center= numeric(), 
                freq_end= numeric(), 
                freq_knee= numeric(), 
                fc= numeric(),             
                freq_bw_knee_fc= numeric(), 
                bin_max_amp= numeric(), 
                pc_freq_max_amp= numeric(), 
                pc_freq_max= numeric(),   
                pc_freq_min= numeric(), 
                pc_knee= numeric(),
                temp_bw_knee_fc= numeric(), 
                slope= numeric(),          
                kalman_slope= numeric(), 
                curve_neg= numeric(), 
                curve_pos_start= numeric(), 
                curve_pos_end= numeric(),
                mid_offset= numeric(), 
                snr= numeric(), 
                hd= numeric(), 
                smoothness= numeric())

extra(a)
