library(data.table)
na <- list.files()
reandt <- function(f) {
  all <- read.csv(file =  eval(f), header = F, sep = '|' )
  mp <- all[(seq(1,200,2)),]
  #mn <- colMeans(all)
  #plot(mn)
  return(mp)
}