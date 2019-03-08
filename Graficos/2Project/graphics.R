#############Graphics

# Radiacion Solar Global
dt  %>% na.omit %>% filter(Variable %in% c('rsgs')) %>% 
  filter(Mounth != 'January')%>%
  ggplot(aes(fecha,Valor) ) + geom_bar( stat = 'identity') + 
  geom_smooth( )+
  facet_wrap('Mounth',scales = 'free_x')+ theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab('Radiación Solar \nGlobal [W/m2]') + xlab('Fecha')

# Precipitacion
dt  %>% na.omit %>% filter(Variable %in% c('precSu')) %>% filter(Valor < 100)%>%
  filter(Mounth != 'January' & Mounth %in% c('March','April','May')  )%>%
  ggplot(aes(fecha,Valor) ) + geom_bar( stat = 'identity') + 
  #geom_smooth( )+
  facet_wrap('Mounth',scales = 'free_x')+ theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab('Precipitación [mm]') + xlab('Fecha')


# Temperatura 
dt   %>% filter(Variable %in% c('tempai')) %>% filter(Valor > 20)%>%
  filter(Mounth != 'January' )%>%
  ggplot(aes(fecha,Valor) ) + geom_line() + 
  geom_smooth( )+
  facet_wrap('Mounth',scales = 'free_x', ncol = 2)+ theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab('Temperatura del aire [ºC]') + xlab('Fecha')


curve(dischCur, 0, 3, col = 'blue',
      xlab = 'Nivel [m]', ylab = paste('Cauadal',expression(m3/s)),
      main = 'Función de Calibración')
points(x = c(0, 1.3) ,  y = c(0,69.982), col = 'red')
text(x=0.25, y= 150, 'f(x) = 53x')

# Caudal
hdt   %>% na.omit %>%
  ggplot(aes(Fecha,Caudalm3) ) + geom_line() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab('Caudal [m3/s]') + xlab('Fecha')
