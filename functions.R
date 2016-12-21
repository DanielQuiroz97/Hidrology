library(zoo)
library(data.table)
#library(DT)
library(dplyr)
library(tidyr)
#library(ggplot2)
#library(NISTunits)
source('windRose.R')
#----- Indentifiers  for every station
sttn <- c('*A.csv','*B.csv','*C.csv','*D.csv','*E.csv')
patt <-  '*[A,B,C,D,E].csv' 

rdscv <- function(f) {
  #Read all .csv file of one station
  dt <- read.csv(file = eval(f), header = T, 
                 sep = ';', stringsAsFactors =F, 
                 col.names =  c( 'Fecha', 'hrai', 
                                 'hrama', 'hrami', 
                                 'pre', 'preacum', 'atmpr',
                                 'tmpai', 'tmpama', 
                                 'tmpami', 'windd', 
                                 'winds'), dec = '.'  )
  
  del <- lapply(dt[,c(2:12)], function(f) as.numeric(f)) %>% as.data.frame()
  del$Fecha <- as.POSIXct(dt[,1], format = '%d/%m/%Y  %H:%M')
  del <- del %>% mutate(Estaciones = substring(f, 
                                             length(strsplit(f,split = '')[[1]]) - 10,
                                             length(strsplit(f,split = '')[[1]])-6 ) )
  if (length(del) == 12) {print(paste(f,'Length different'))}
  if(is.character(del[,6])) {print(paste(f, 'character'))}
  return(del)
}


#---- Reado all stations
read.sttn <- function(stations) {
  na <- list.files( pattern = patt)
  
}

#----- Mean table
meandata <- function(dt) {
  dt %>% mutate(Fecha = as.Date(Fecha)) %>%
    group_by(Fecha,Variable, Estaciones) %>% 
    summarise(Valor = mean(Valor, na.rm = T)) 
}

#------ Control the number of data frames by current date
data.control <- function(n) {
  patt <-  '*[A,B,C,D,E].csv'
  na <- list.files(pattern = '*[A,B,C,D,E].csv')
  date <- seq(as.Date("2016/09/25"), as.Date("2016/10/25"), by = 'days')
  currend <- Sys.Date()
  nofdt <- match(currend,date)*n
  ifelse(length(na) == nofdt, print('go on'), 
         print(paste('There must be', nofdt,
                     'data frames, there are',length(na)) ) )
  
}

#### Plots and Theme for graphics

glab <- function(variab) {
  if (variab == 'hrai') { lab <- 'Humedad Relativa \ndel Aire [%]'}
  else if (variab == 'atmpr') { lab <- 'Presión atmosférica \n[hPa]'}
  else if (variab == 'tmpai') { lab <- 'Temperatura del Aire \n[°C]'}
  else if (variab == 'pre') { lab <- 'Precipitación [mm]'}
  else{title <- ''}
  return(lab)
}




# Plot Relative Humidity
multiple.plot <- function(variable, data ) {
  data %>% dplyr::filter(Variable %in% c(variable))  %>% #, 'hrama', 'hrami'
    ggplot( aes(x=Fecha,y=Valor, color =Estaciones, 
                fill = Variable )) +#, shape = Estaciones) ) + 
    geom_line() + ggtitle('') + 
    xlab('') +  ylab(glab(variab = variable)) +guides( fill = F) + 
    #+ scale_x_date(breaks = date_breaks('month')) +
     scale_colour_brewer(palette = "Dark2") + mythem()
  
}


# ____ Plot Wind Rose
mythem <- function() {
  theme(
    legend.background = element_rect(fill = 'white', colour = 'white'), 
    legend.title=element_text(face="italic", family="Times", colour="gray8",size=14),
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.minor = element_line(colour = "gray80", size = .5),
    axis.text.x=element_blank())
}

# To plot wiht other variables, inserto o coment the var argument
# To plot bars for rain, inster stat = 'identity' in geom_bar
facecplot <- function(data, var = c('hrai','atmpr','tmpai','pre'), ncol = 1, facet_by= c('Variable')) {
  data%>% filter(Variable %in% c(var)) %>%
    ggplot( aes(x=Fecha,y=Valor, color =Estaciones,
                shape = Estaciones) ) + 
    geom_line()  +  #aes(linetype = Estaciones)
    labs(x ='', y = '')+
    scale_colour_brewer(palette = "Dark2") + 
    scale_x_datetime(date_breaks = '1 day', date_labels = '%b-%d %H:%M') +
    facet_wrap(c(facet_by),scales = 'free_y',
               ncol = ncol,
               labeller =as_labeller(c(hrai = 'Humedad Relativa del Aire [%]',
                                       tmpai = 'Temperatura del Aire [°C]',
                                       atmpre = 'Presión atmosférica [hPa]',
                                       ESPOCH= 'ESPOCH',
                                       Laguacoto = 'Laguacoto',
                                       Tola = 'La Tola',
                                       M0031 = 'M0031',
                                       M0037 = 'M0037',
                                       M5089 = 'M5089',
                                       M5090 = 'M5090',
                                       M5092 = 'M5092'
               ))) + geom_smooth(span = 0.3)+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}

windplot <- function(data,nbind = 5,ncol = 2, nrow = 2) {
  data <- filled
  del <- data%>% spread(Variable, Valor) 
  grupos <- c(0,seq(45/2,360, by = 45), 360 ) 
  labdir <-  c(360 ,seq(45,360, by = 45) ) 
  grupos2 <- seq(0,ceiling(max(del$winds, na.rm = T)), length.out = nbind + 1)
  labdir2 <- paste( round(seq(0,ceiling(max(del$winds, na.rm = T))-2,
                              length.out = nbind),0),
                    round(seq(2,ceiling(max(del$winds, na.rm = T)),
                              length.out = nbind),0),
                    sep = ' - ') 
  
  #BIN CONTROL IN FLOOR INSTEAD CEILING
  del <- del %>% select(Estaciones,windd, winds)%>% 
    group_by(Estaciones) %>% mutate(bindirs = cut(windd,grupos,labels = labdir),
                                    bindspd = cut(winds,grupos2,labels = labdir2)) 
  
  del %>% filter( !is.na(bindirs) ) %>%ggplot(aes(x = bindirs, fill = bindspd)) +
    geom_bar(position = 'stack') + coord_polar(start = -pi/8) + 
    facet_wrap(~Estaciones,ncol = ncol, nrow = nrow) +
    labs(fill = 'Velocidad m/s')+ xlab('Frecuencia e Intensidad del Viento') +
    theme_bw()
  
}

plotrain <- function(data) {
  data %>% filter(Variable %in%  c('pre')) %>% 
   plot_ly(x = ~Fecha, y = ~Valor, color = ~Estaciones) %>%
    add_bars()
}

multiplotly <- function(data, variable){
  data %>% filter(Variable %in% c(as.character(variable)) ) %>% na.omit %>%
    plot_ly(x = ~Fecha, y = ~Valor, color = ~Estaciones) %>% add_lines()
  
}

windplotly <-  function(data, nbind = 5 ) {
  del <- data%>% spread(Variable, Valor) 
  grupos <- c(0,seq(45/2,360, by = 45), 360 ) 
  labdir <-  c(360 ,seq(45,360, by = 45) ) 
  grupos2 <- seq(0,ceiling(max(del$winds, na.rm = T)), length.out = nbind + 1)
  labdir2 <- paste( round(seq(0,ceiling(max(del$winds, na.rm = T))-2,
                              length.out = nbind),0),
                    round(seq(2,ceiling(max(del$winds, na.rm = T)),
                              length.out = nbind),0),
                    sep = ' - ') 
  
  #BIN CONTROL IN FLOOR INSTEAD CEILING
  del <- del %>% select(Estaciones,windd, winds)%>% 
    group_by(Estaciones) %>% mutate(bindirs = cut(windd,grupos,labels = labdir),
                                    bindspd = cut(winds,grupos2,labels = labdir2)) 
  del <- del %>% select(Estaciones,windd, winds)%>% 
    plotly()
}

movingAverage <- function(x, n=1, centered=FALSE) {
  
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x))
  count <- rep(0, length(x))
  
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), x[1:(length(x)-i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # return sum divided by count
  s/count 
}

reapMA <- function(values) {
  i <- 0
  print(sum(is.na(values)))
  while(sum(is.na(values) > 1)){ 
    values <- rollapply(values,
                        width =6,
                        FUN = mean,
                        align = 'center',
                        fill = NA,
                        partial = T,
                        na.rm = T)
    i <- i+1
    print(paste(i,sum(is.na(values))))
  }
  return(values)
}

