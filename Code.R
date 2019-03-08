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

alldata <- alldata %>%  group_by(Estaciones,Variable) %>%
  #mutate(tmp_lag = lag(Valor, n=3)) %>%
  mutate(Valor = ifelse(is.na(Valor), round(reapMA(Valor),1), Valor)) 
 

tmp <- read.xlsx(file = './Tablas/Excel/2Project/M1246_Las_Lajas_abril.xlsx',
                 startRow = 2, header = T, sheetIndex = 1)

tmp <- parLapply(cl, c('./Tablas/Excel/2Project/M1246_Las_Lajas_abril.xlsx'),
                fun = xlsxread)

lajasnm <- list.files('./Tablas/Excel/2Project/', 
                      full.names = T, pattern = '*M1246')

tmp <- parLapply(cl, lajasnm, fun = xlsxread)  %>% bind_rows()


tmp1 <- tmp %>% gather(Fecha, Estaciones)
names(tmp1) <-c('Fecha', 'Estaciones','Variable', 'Valor')

tmp1 %>% filter(Variable %in% c('hrai', 'tmpaip')) %>%
  ggplot(aes(Fecha, Valor, color = Estaciones)) + geom_line() + 
  facet_wrap('Variable', scales = 'free')


## ########### For the Second Project


nivelnm <- list.files('./Tablas/Excel/2Project/', 
                      pattern = '*H', full.names = T)
nmP2M <- list.files('./Tablas/CSV/2PROJECT/', pattern = '*M') %>% 
  paste0('./Tablas/CSV/2PROJECT/', .)

namescol <- c('Fecha', 'humreMa', 'humreIn', 'humreMi', 'humrePr', 
              'precSu', 'presAt', 'rsgs', 'rsgm', 'rsgp', 'rsrp', 'tempp',
              'tempm', 'tempm','tempai', 'windd', 'winds')

dt <-  lapply(X = nmP2M, FUN = function(f) read.csv(eval(f), 
                                             sep = ';',
                                             encoding = 'latin1',
                                             stringsAsFactors = F,
                                             header = T,
                                             col.names = namescol,
                                             dec = ',') ) %>%
  bind_rows() %>% mutate(Estacion = 'M5091')

dt[,1] <-  as.POSIXct(dt[,1], format = '%d/%m/%Y  %H:%M') 
dt <-  dt %>% gather(key = Fecha, value = Estacion  )
names(dt) <- c('Fecha', 'Estaciones', 'Variable', 'Valor')
dt <- dt%>% mutate(fecha = Fecha) %>% 
  separate(Fecha, c('Date', 'Hour'),sep = ' ') %>% 
  separate(Date, c('Year', 'Mounth', 'Day')) %>% 
  mutate(Mounth = factor(Mounth,labels  = month.name)) 

hdt   %>% na.omit %>%
  ggplot(aes(Fecha,Caudalm3) ) + geom_line() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab('Caudal [m3/s]') + xlab('Fecha')
  
getResum <- function(dt, nm, fn) {
  resum <-  lapply(unique(dt$Variable), 
         FUN = function(f) filter(dt, Variable %in% f) ) %>% 
    lapply(., FUN = function(dt) with(dt, tapply(Valor, 
                                                 Mounth, 
                                                 FUN = eval(fn), na.rm =T))  ) %>%
    as.data.frame()%>% mutate(Mes = month.name)
  names(resum) <- c(nm[-1],'Mes')
  return(resum)
} 

getResum(dt = dt, nm = namescol, fn = 'min') %>% 
  write.xlsx('mes_min.xlsx', row.names = F, showNA = F)
#ggsave('Caudal.png',width = 7,height = 6, units = 'in')


### For H station
dischCur <- function(x) 53.83*x

hdt <- read.csv('Tablas/CSV/2PROJECT/H0472_CAÑAR EN PTO.INCA.csv') %>%
  select(-DIA) %>% t %>% as.data.frame %>% mutate(DIA = 1:31) %>%
  gather(key = DIA,value = 'Value') 
 
data <- data.frame(Caudal = c(0, 69.982),  Nivel = c(0, 1.3))
lm(formula = Caudal~Nivel, data = data)

names(hdt) <- c('DIA', 'Mes', 'Valor')
hdt <- hdt%>%  mutate(Mes = as.numeric(gsub('V*','',Mes)) ) %>%
  mutate(Fecha = as.POSIXct(paste(DIA, Mes,'2015', sep = '-'), 
                            format = '%d-%m-%Y' ), 
         Caudalm3s = dischCur(Valor)) %>% 
  mutate(VolumenM3 = Caudalm3s*86400)


with(hdt, tapply(VolumenM3, Mes, sum, na.rm = T)) %>% 
  as.data.frame() %>% mutate(Mes = month.name) %>% 
  write.xlsx('Suma_Volumn_Mes.xlsx', row.names =F)

