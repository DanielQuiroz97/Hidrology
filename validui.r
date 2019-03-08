
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    fluidRow(
      column(4,
             img(src = "LOGO.png", height = 90, width = 250),
             align = "left"
      ),
      column(7,
             h2('Data Viewer'),
             h4('Hidrology and Climatology'),
             h5("Daniel Quiroz"),
             align = "center"
      )
    ),
    hr(), 
    fluidRow(
      column(2,
             radioButtons('data','Date Section:',
                          c('Morning'     =  'morning',
                            'Night'   = 'night')),
             radioButtons('variable','Meteorological Variables',
                          c('Air Relative Humidity'   =   'hrai',
                            'Atmospheric Preassure'   =   'atmpr',
                            'Aire Temperature'        =   'tmpai',
                            'Precipitation'           =   'pre',
                            'Wind'                    =   'wind'))
             
      ),
      column(10,
             plotOutput('graph',width = '1100px',height = '550px') )
    )
    
    
    
  )
)
