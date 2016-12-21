library(dplyr)
library(tidyr)
library(plotly)
library(shiny)
library(corrplot)
navbarPage('Meteorological Data Viewer',
           tabPanel('Plot',
                    fluidPage(
                      fluidRow(
                        column(4,
                               img(src = "logo.png", height = 90, width = 250),
                               align = "left"
                        ),
                        column(7,
                               h2('Data Viewer'),
                               h4('Hidrology and Climatology'),
                               h5("Daniel Quiroz y Gabriel Gaona"),
                               align = "center"
                        )
                        ),
                      hr(),
                    fluidRow(
                      column(3, 
                             sidebarPanel( 
                               selectInput('data','Date Section:',
                                           c('Data'    = 'morning',
                                             #'Night'      = 'night',
                                             #'All Data'   = 'all',
                                             'Filled Data'= 'filled'),'filled'),
                               selectInput('variable','Meteorological Variables',
                                           c('Air Relative Humidity'   =   'hrai',
                                             'Atmospheric Preassure'   =   'atmpr',
                                             'Aire Temperature'        =   'tmpai',
                                             'Precipitation'           =   'pre',
                                             'Wind'                    =   'wind')),
                               width = 25
                             )),
                      
                      column(9,
                             plotlyOutput('graph',width = '1000px',height = '490px'))
                      ) 
                    )
           ),
           tabPanel('Tables',
                    fluidRow(
                      column(3, 
                             sidebarPanel( 
                               selectInput('datatabla','Date Section:',
                                           c(#'Morning'     =  'morning',
                                             #'Night'       = 'night',
                                             'All Data'    = 'all',
                                             'Filled Data'= 'filled'),'filled'),
                               selectInput('variabletabla','Meteorological Variables',
                                           c('Air Relative Humidity'   =   'hrai',
                                             'Atmospheric Preassure'   =   'atmpr',
                                             'Aire Temperature'        =   'tmpai',
                                             'Precipitation'           =   'pre',
                                             'Wind Speed'              =   'winds',
                                             'Wind Direction'          =   'windd')),
                               downloadButton('downloadData', 'Download'),
                               width = 25
                             )
                             ),
                      column(5,
                             h3('Data'),
                             h5('Just will be displayed 10 observations of the data'),
                             tableOutput('table')),
                      column(4,
                             h3('Some Statistics'),
                             tableOutput('sumariodt'))
                      )
                    ),
           tabPanel('Correlation',
                    fluidRow(
                      column(3, 
                             sidebarPanel( 
                               selectInput('datacorr','Date Section:',
                                           c(#'Morning'     =  'morning',
                                             #'Night'       = 'night',
                                             'All Data'    = 'all',
                                             'Filled Data'= 'filled'),'filled'),
                               selectInput('perweek', 'Data Per Week',
                                           c('No Week' = 'nw',
                                             '1' = '1',
                                             '2' = '2',
                                             '3' = '3',
                                             '4' = '4'), 'No Week'),
                               selectInput('variablecorr','Meteorological Variables',
                                           c('Air Relative Humidity'   =   'hrai',
                                             'Atmospheric Preassure'   =   'atmpr',
                                             'Aire Temperature'        =   'tmpai',
                                             'Wind Speed'              =   'winds',
                                             'Wind Direction'          =   'windd')),
                               selectInput('corrMethod','Correlation Method',
                                           c('Pearson'  = 'pearson',
                                             'Kendall'  = 'kendall',
                                             'Spearman' = 'spearman'),'pearson'),
                               downloadButton('downloadcorr','Download Correlation'),
                               width = 25
                             )
                      ),
                      column(4,
                             h3('Correlation Matrix', align = 'center'),
                             tableOutput('dtcorr'), align = 'center'),
                      column(5,
                             h3('Correlation Plot', align = 'center'),
                             plotOutput('graphcorr'))
                    ))
)


