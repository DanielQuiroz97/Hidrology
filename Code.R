source('functions.R')
setwd('./Tablas/CSV/Manana')
setwd('./Tablas/CSV/Noche')
#----- READ DATA with specific pattern
na <- list.files(path = './Tablas/CSV/Manana',pattern= '*[A,B,C,D,E].csv') 
na <- paste0('./Tablas/CSV/Manana')

#data.control(na) #Number of data frames
data.control()
data <- lapply(na, rdscv) %>% bind_rows() #Convine data frames

##### Gathering Data
tmp <- data %>% gather(Fecha, Estaciones)
names(tmp) <- c('Fecha', 'Estaciones','Variable', 'Valor')
tmp <- tmp %>% distinct(Fecha, Estaciones, Variable,.keep_all = T)

#---- Build Mean table for each date for each variable 
tmp.mean <- meandata(tmp)

#Humedad Relativa de Aire
metvar <- c('hrai', 'atmpr', 'tmpai', 'pre')
allplot <- lapply(metvar, multiple.plot, data = tmp)
allplot[[1]]
allplot[[2]]
allplot[[3]]
allplot[[4]]

allplotmena <- lapply(metvar, multiple.plot, data = tmp)
grid_arrange_shared_legend(allplot[[1]],allplot[[3]],
                           allplot[[2]],nrow = 1,ncol = 3)

#Wind Rose
wind <- windplot(tmp) + theme_bw()
wind

tmp <- gatherfm %>% filter(Variable %in% 'hrai') %>%
  spread(Estaciones, Valor) 

cortar <- seq(0,length(tmp[,1]), by = length(tmp[,1])/4)
cortar[1] <- 1

alldata <- alldata %>%  group_by(Estaciones,Variable) %>%  #mutate(tmp_lag = lag(Valor, n=3)) %>%
  mutate(Valor = ifelse(is.na(Valor), round(reapMA(Valor),1), Valor)) 
 
 

 
 
 
 