# cargar sus libs
library("dplyr")
library("ggplot2")
library("scales")
# library("png")                                                        # usaremos el paquete png
# library("ggpubr")                                                  # y el paquete ggpubr
# bg_image <- readPNG( source = "logo.png", native = TRUE )         # leemos el archivo de imagen que usaremos

graficadora_barras <- function( inicio, fin) {
  # cargar data
  comienzos <- read.csv( file = "inicios_SNI.csv" )
  
  # data handling
  filtrado <- comienzos %>%
    filter( anio >= inicio,
            anio <= fin ) %>%
    group_by( nivel ) %>%
    summarize( miembros = n() ) %>%
    mutate( proporcion = miembros / sum(miembros) ) %>%
    mutate( etiqueta = percent( proporcion ) )
  
  # Generamos un subtitulo
  sub <- paste( "Ingreso entre", inicio, " y", fin )
  
  # Listo ! Graficamos barras base
  ggplot( data = filtrado,
          mapping = aes( x = nivel,
                         y = proporcion ) ) +
    # background_image( bg_image) +  
    geom_col( color = "black",
              fill = "skyblue" ) +
    geom_label( mapping = aes( label = etiqueta ),
                nudge_y = 0.05 ) +
    scale_y_continuous( limits = c(0,1),
                        labels = percent,
                        expand = c(0,0) ) +
    scale_x_continuous( breaks = 0:3,
                        labels = c("C", "I", "II", "III") ) +
    labs( title = "Nivel de Ingreso al SNI",
          subtitle = sub,
          y = "Miembros",
          caption = "Fuente: BioFreelancer 2021" ) +
    theme_classic( base_size = 15 )
}
