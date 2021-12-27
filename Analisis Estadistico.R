################ Analisis Estádistico #################

library(openxlsx)
library(corrplot)
library(nortest)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(plotrix)


setwd('C:/Users/Álvaro/Desktop/Master/TFM/Base de datos/EXCEL')


data<-read.xlsx('Final_Data.xlsx')
data<-data[,-1]
data$gender<-as.factor(data$gender)
data$specie<-as.factor(data$specie)

table(is.na(data))


summary(data)
str(data)

table(data$gender)
table(data$specie)

## Correlacion

mat_cor<-data[,-c(27,28)] %>% cor(method="pearson") %>% round(digits=2)
corrplot(mat_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)


## Boxplot

boxplot(data[,-c(27,28)],  las = 2, col = c(1:26))

### Por género

plots <- list() 


for (i in 1:26) {
  p<-ggplot(data, aes(x= gender, y= data[,i], color=gender)) +
    geom_boxplot() +
    ggtitle(print(colnames(data[i]))) + 
    xlab("Gender")
  plots[[i]] = p
  print(p)
}

### Por especie

for (i in 1:26) {
  p<-ggplot(data, aes(x=specie, y=data[,i], color=specie)) +
    geom_boxplot() +
    ggtitle(print(colnames(data[i]))) + 
    xlab("Specie") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8), 
          panel.grid.minor = element_blank())
  plots[[i]] = p
  print(p)
}


## Normalidad

for (i in 1:27) {
  qqnorm(data[,i], main = colnames(data[i]))
  qqline(data[,i])
}


## Género, media y error

for (i in 1:26) {
  
  datos<-data.frame(data[,27],data[,i])
  colnames(datos)<-c('gender', 'variable')
  
  a<-datos%>%
    group_by(gender) %>%
    summarize(media=mean(variable,na.rm=TRUE),
              error=std.error(variable,na.rm=TRUE))
  
  p<-ggplot(a, aes(x=gender, y=media, color=gender)) +
    
    geom_point(position=position_dodge(.9), stat="identity") +
    geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=media-error, 
                                                              ymax=media+error)) +
    ggtitle(print(colnames(data[i]))) + 
    xlab("Gender")
  
  plots[[i]] = p
  print(p)
}


## Especie, media y error

for (i in 1:26) {
  
  datos<-data.frame(data[,28],data[,i])
  colnames(datos)<-c('specie', 'variable')
  
  plots <- list() 
  
  a<-datos%>%
    group_by(specie) %>%
    summarize(media=mean(variable,na.rm=TRUE),
              error=std.error(variable,na.rm=TRUE))
  
  p<-ggplot(a, aes(x=specie, y=media, color=specie)) +
    
    geom_point(position=position_dodge(.9), stat="identity") +
    geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=media-error, 
                                                              ymax=media+error)) +
    ggtitle(print(colnames(data[i]))) + 
    xlab("Specie") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8), 
          panel.grid.minor = element_blank())
  
  plots[[i]] = p
  print(p)
}

