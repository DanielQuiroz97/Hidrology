
source('functions.R')
#Mornign Tables
patm <- './Tablas/CSV/Manana/'  
names_mana <- list.files(path = patm, pattern = '*[A,B,C,D,E].csv')
morning <- paste0(patm, names_mana)
#Reading all data frames
dtmornign <- lapply(morning, rdscv) %>% bind_rows()
# Gathering data
gatherfm <- dtmornign %>% gather(Fecha, Estaciones)
names(gatherfm) <- c('Fecha', 'Estaciones','Variable', 'Valor')
gatherfm$Variable <- as.factor(gatherfm$Variable)
gatherfm <-gatherfm %>% distinct(Fecha, Estaciones, Variable,.keep_all = T)%>%
  arrange(Fecha)

#Night Tables
patn <- './Tablas/CSV/Noche/'
names_noch <-list.files(path = patn, pattern = '*[A,B,C,D,E].csv')
night <- paste0(patn, names_noch)
dtnight <- lapply(night, rdscv) %>% bind_rows()
gatherfn <- dtnight %>% gather(Fecha, Estaciones)
names(gatherfn) <- c('Fecha', 'Estaciones','Variable', 'Valor')
gatherfn$Variable <- as.factor(gatherfn$Variable)
gatherfn <- gatherfn %>% distinct(Fecha, Estaciones, Variable,.keep_all = T) %>%
  arrange(Fecha)

#Gather Both Tables
alldata <- bind_rows(gatherfn,gatherfm) %>% distinct(Fecha, Estaciones,
                                                     Variable, .keep_all = T) %>%
  arrange(Estaciones,Variable) %>% arrange(Fecha)

# Filled data
filled <- alldata %>% spread(Variable,Valor)
filled <- filled[c(1:3245) , c(1:13)] %>% gather(Fecha,Estaciones)
names(filled) <- c('Fecha', 'Estaciones','Variable', 'Valor')

filled <- filled %>% group_by(Estaciones,Variable) %>%  #mutate(tmp_lag = lag(Valor, n=3)) %>%
  mutate(Valor = ifelse(is.na(Valor), round(reapMA(Valor),1), Valor)) 


outtables <- function(data, metvar) {
  outdata <-  data %>% filter(Variable %in% c(as.character(metvar))) %>%
    spread(Estaciones,Valor)  %>% arrange(Fecha)
  outdata <- outdata[,-2]
  #outdata[,1] <- as.POSIXct(outdata[,1], format = '%d/%m/%Y  %H:%M')
  return(outdata)
}

col3 <- colorRampPalette(c("red", "lightskyblue", "blue")) 

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
      #Set the path of data frame
  output$graph <- renderPlotly({
    switch (input$data,
            morning =  {data <- gatherfm},
            night   =  {data <- gatherfn},
            all     =  {data  <- alldata},
            filled  =  {data <- filled}
    )
    switch (input$variable,
            hrai = multiplotly(variable = 'hrai', data = data),
            atmpr = multiplotly(variable = 'atmpr', data = data),
            tmpai  = multiplotly(variable = 'tmpai', data = data),
            pre    = plotrain(data = data)
            #wind = windplot(data = data)
    )
    
    })
  histogram <- renderPlot({
    hist(rnorm(100))
  })

  output$downloadData <- downloadHandler(
    filename = function(){paste(input$variabletabla, 
                                Sys.Date(), '.csv', sep = '_')},
    content = function(file){
      switch (input$datatabla,
             morning = {out <- gatherfm},
             night   = {out <- gatherfn},
             all     = {out <- alldata},
             filled  =  {data <- filled}
      )
      download <- outtables(data = out,metvar = input$variabletabla)
      write.csv(download, file,row.names = F, sep = ";")
    }
  )
  
  output$table <- renderTable({
    out <- switch (input$datatabla,
      morning = gatherfm,
      night   = gatherfn,
      all     = alldata,
      filled  =  filled
    )
    tableout <- outtables(data = out,metvar = input$variabletabla)
    tableout$Fecha <- format(tableout$Fecha, '%d/%m/%Y  %H:%M')
   head(tableout ,10 ) 
  })
  
  output$sumariodt <- renderTable({
    switch (input$datatabla,
            morning = {out <- gatherfm},
            night   = {out <- gatherfn},
            all     = {out <- alldata},
            filled  =  {out <- filled}
    )
    sta <-  outtables(data = out,metvar = input$variabletabla) %>% summary()
    sta <- sta[,-1]
    rownames(sta) <- c('Min', '1st Quan', 'Median', 'Mean',
                       '3r Quan', 'Max', 'NAs ')
    return(sta)
  })
  

    
  output$dtcorr <- renderTable({
    switch (input$datacorr,
            morning = {data <- gatherfm},
            night   = {data <- gatherfn},
            all     = {data <- alldata},
            filled  =  {data <- filled}
    )
    dt <- data %>% filter(Variable %in% c(input$variablecorr)) %>%
      spread(Estaciones, Valor)
    if(input$perweek != 'nw') {
      cortar <- c(1, 168, 336, 504, 673)
      dt <- dt[cortar[as.numeric(input$perweek)]:cortar[as.numeric(input$perweek)+1],]
    }
    dt <- dt[,3:7] %>% na.omit %>% cor(method = input$corrMethod)
    rown <- names(dt)
    rownames(dt) <- rown
    return(dt)
  })
  
  output$graphcorr <- renderPlot({
    switch (input$datacorr,
            morning = {data <- gatherfm},
            night   = {data <- gatherfn},
            all     = {data <- alldata},
            filled  =  {data <- filled}
    )
    dt <- data %>% filter(Variable %in% c(input$variablecorr)) %>%
      spread(Estaciones, Valor)
    if(input$perweek != 'nw') {
      cortar <- c(1, 168, 336, 504, 673)
      dt <- dt[cortar[as.numeric(input$perweek)]:cortar[as.numeric(input$perweek)+1],]
    }
   p <- dt[,3:7] %>% na.omit %>% cor(method = input$corrMethod) %>% 
     corrplot.mixed(upper = 'circle', lower = 'square',addCoef.col = 'white', 
              col = col3(20))
   return(p)
   #print(p)
  })
  
  output$downloadcorr <- downloadHandler(
    filename = function(){paste(input$corrMethod, 
                                'corr', '.csv', sep = '_')},
    content = function(file){
      switch (input$datatabla,
              morning = {data <- gatherfm},
              night   = {data <- gatherfn},
              all     = {data <- alldata},
              filled  =  {data <- filled}
      )
      dt <- data %>% filter(Variable %in% c(input$variablecorr)) %>%
        spread(Estaciones, Valor)
      dt <- dt[,3:7] %>% na.omit %>% cor(method = input$corrMethod)
      rown <- names(dt)
      rownames(dt) <- c('M0031', 'M0037', 'M5080', 'M5090', 'M5092')
      write.csv(dt, file,row.names = T)
    } 
  )
  
})
