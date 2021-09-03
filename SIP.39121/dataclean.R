#
library("openxlsx")
library("dplyr")
library("stringr")
library("ggplot2")
library("ggsci")
library("scales")

# leer data
padron <- read.xlsx( xlsxFile = "Anexo.39121_Membres”a Nacional_1984-2021.xlsx" )

# homogeneizar nombres
caracteres <- "[ ,-]"

padron2 <- padron %>% 
  mutate( homonombre = str_replace_all( string = NOMBRE.DEL.INVESTIGADOR,
                                        pattern = caracteres,
                                        replacement = "" ) %>% str_to_lower( ),
          .before = 1 ) %>% 
  filter( EXP != "No Disponible" ) %>% 
  mutate( EXP = as.numeric(EXP) )

# cuantos expedientes unicos hay
padron2$EXP %>% unique() %>% length()

# Lista completa de nombres en el padron
nombres <- padron2 %>%
  select( EXP, NOBILIS) %>% 
  unique() %>%
  arrange( EXP ) %>% 
  filter( NOBILIS == "DRA." |
            NOBILIS == "DR." ) %>% 
  rename( nobilis_genero = NOBILIS )

# retaggear el padron con el nobilis_gender
# esto nos permite inferir el genero de los que ingresaron con nivel menor a phd
padron3 <- left_join( x = padron2,
                      y = nombres,
                      by = "EXP" ) %>% 
  filter( !is.na(nobilis_genero) )

# Acumular la cantidad de años de SNI para cada investigador ====
duracion <- padron3 %>% 
  select( AÑO, EXP, nobilis_genero ) %>% 
  unique( ) %>% 
  group_by( EXP, nobilis_genero ) %>% 
  summarize( años_en_el_padron = n( ) ) %>% 
  ungroup()

# Histograma
# promedio de años en el historico
promedio_historico <- mean( duracion$años_en_el_padron ) %>% round( )

año_minimo <- min( padron3$AÑO )

año_maximo <- max( padron3$AÑO )

#
titulo_histo <- paste( "Permanencia acumulada en SNI" )
subtitulo_histo <- paste( "Con", prettyNum( nrow( duracion ), big.mark = "," ),
                          "miembros en su historia,",
                          "el investigador promedio acumula ~",
                          promedio_historico, "años en el SNI" )

caption_generico <- paste( "Fuente: BioFreelancer con datos de CONACyT",
                           año_minimo, "-", año_maximo)

# preparamos ejes
minx <- 1
maxx <- max( duracion$años_en_el_padron )

marcas_x <- minx:maxx

ggplot( data = duracion, mapping = aes( x = años_en_el_padron ) ) +
  geom_vline( xintercept = promedio_historico,
              lty = "dashed", color = "red" ) +
  geom_histogram( binwidth = 1, color = "black", fill = "cornflowerblue" ) +
  scale_y_continuous( expand = c(0,0),
                      labels = comma ) +
  scale_x_continuous( breaks = marcas_x ) +
  labs( title = titulo_histo,
        subtitle = subtitulo_histo,
        caption = caption_generico,
        x = "años en el padron",
        y = "miembros" ) +
  theme_classic( base_size = 14 )

#
ggsave( filename = "histo1.png", width = 10, height = 5, dpi = 300 )

# Vemos la duracion en el padron, por genero...
# calcula los promedios de permanencia acumuladad por genero
permanencia_by_nobilis <- duracion %>% 
  group_by( nobilis_genero ) %>% 
  summarize( promedio_duracion = mean( años_en_el_padron ) )

# subtitulo violines
subtitulo_violines <- paste0( "Dr. en promedio acumula ~ ",
                              permanencia_by_nobilis %>%
                                filter( nobilis_genero == "DR.") %>%
                                pull(promedio_duracion) %>% round(),
                              " años \n",
                              "Dra. ~ ",
                              permanencia_by_nobilis %>%
                                filter( nobilis_genero == "DRA.") %>%
                                pull(promedio_duracion) %>% round(),
                              " años" )

ggplot( data = duracion,
        mapping = aes( x = nobilis_genero,
                       y = años_en_el_padron,
                       fill = nobilis_genero ) ) +
  geom_violin( alpha = 0.5 ) +
  geom_boxplot( width = 0.02,
                fill = "white",
                outlier.size = 0.1 ) +
  geom_point( data = permanencia_by_nobilis,
              mapping = aes( y = promedio_duracion ),
              shape = 23, size = 3,
              color = "black" ) +
  scale_fill_d3( ) +
  labs( title = titulo_histo,
        subtitle = subtitulo_violines,
        x = "",
        y = "años acumulados en el padron",
        fill = "nobilis",
        caption = caption_generico) +
  theme_bw( base_size = 15 ) +
  theme( legend.position = "none" )

# save violin
ggsave( filename = "violin1.png", width = 6, height = 6, dpi = 300 )

# Cuanto tiempo tardan los investigadores en dar el salto SNI
# niveles posibles
padron2$CATEGORIA %>% unique()

# vamos a convertir el nivel C en 0
candidatos0 <- padron2 %>% 
  mutate( CATEGORIA = ifelse( CATEGORIA == "C",
                              yes = "0",
                              no =  CATEGORIA),
          CATEGORIA = as.numeric(CATEGORIA) )


# re checamos niveles posibles
candidatos0$CATEGORIA %>% unique()

# identificar cuando cambia de nivel un investigador
saltos_1 <- candidatos0 %>% 
  arrange( EXP, AÑO ) %>% 
  group_by( EXP ) %>% 
  mutate( nivel_previo = lag( CATEGORIA ), .before = 1 ) %>%
  mutate( años_acumulados = 1:n( ) -1, .before = 1  ) %>% 
  mutate( cambio_de_nivel = case_when( is.na(nivel_previo) ~ "COMIENZA",
                                       CATEGORIA == nivel_previo ~ "SE_MANTIENE",
                                       CATEGORIA > nivel_previo ~ "SUBE",
                                       CATEGORIA < nivel_previo ~ "BAJA",
                                       TRUE ~ "INDETERMINADO"),
          .before = 1  ) %>% 
  ungroup()

# prefiltramos solo para cambios de nivel hacia arribas
subidas_de_nivel <- saltos_1 %>% 
  filter( cambio_de_nivel == "SUBE" | cambio_de_nivel == "COMIENZA")

# probar con un exp
# testexp <- saltos_1 %>%
#   filter( EXP == 3697 )

# cual es el numero minimo de años acumulados para alcanzar el sni2
alcanzar_sni1 <- subidas_de_nivel %>%
  filter( CATEGORIA == 1 ) %>% 
  group_by( EXP ) %>% 
  summarize( nivel_alcanzado_en = min( años_acumulados ) ) %>% 
  mutate( nivel = "1" ) %>% 
  ungroup()

alcanzar_sni2 <- subidas_de_nivel %>%
  filter( CATEGORIA == 2 ) %>% 
  group_by( EXP ) %>% 
  summarize( nivel_alcanzado_en = min( años_acumulados ) ) %>% 
  mutate( nivel = "2" ) %>% 
  ungroup()

alcanzar_sni3 <- subidas_de_nivel %>%
  filter( CATEGORIA == 3 ) %>% 
  group_by( EXP ) %>% 
  summarize( nivel_alcanzado_en = min( años_acumulados ) ) %>% 
  mutate( nivel = "3" ) %>% 
  ungroup()

# juntamos todos los niveles
todos_juntos <- bind_rows( alcanzar_sni1,
                           alcanzar_sni2,
                           alcanzar_sni3 )

# promedio de años para alcanzar nivel
promedio_alcanzar_nivel <- todos_juntos %>% 
  group_by( nivel ) %>% 
  summarize( promedio_para_nivel = mean(nivel_alcanzado_en) ) %>% 
  ungroup()

# chan chan chaaaan...  
ggplot( data = todos_juntos,
        mapping = aes( x = nivel,
                       y = nivel_alcanzado_en,
                       fill = nivel ) ) +
  geom_violin( alpha = 0.5 ) +
  geom_boxplot( width = 0.015,
                fill = "white",
                outlier.size = 2,
                outlier.shape = 21 ) +
  geom_point( data = promedio_alcanzar_nivel,
              mapping = aes( y = promedio_para_nivel ),
              shape = 23, size = 3,
              color = "black" ) +
  scale_fill_futurama( ) +
  labs( title = "Tiempo que toma subir de nivel SNI",
        subtitle = "El rombo marca el promedio",
        x = "Cambio a Nivel",
        y = "años para alcanzar el Nivel",
        # fill = "nobilis",
        caption = caption_generico) +
  theme_bw( base_size = 15 ) +
  theme( legend.position = "none" )

# guardamos
ggsave( filename = "violin2.png",
        width = 7, height = 5, dpi = 300 )

## preparemos una data para otro scipt y otro ejercicio ====
# solo data de el inicio de los SNI
export_comienzos <- saltos_1 %>% 
  filter( cambio_de_nivel == "COMIENZA" ) %>% 
  select( cambio_de_nivel, AÑO, EXP, CATEGORIA) %>% 
  unique( )

# guardar
write.csv( x = export_comienzos, file = "inicios_SNI.csv" )

## ahm... cuantos entraron siendo cada nivel de SNI
de_entrada <- saltos_1 %>% 
  filter( cambio_de_nivel == "COMIENZA" ) %>% 
  group_by( CATEGORIA ) %>% 
  summarize( miembros = n() ) %>% 
  ungroup( ) %>% 
  mutate( proporcion = miembros / sum(miembros) )

# ver en barras
ggplot( data = de_entrada,
        mapping = aes( x = CATEGORIA, y = miembros) ) +
  geom_col( color = "black", fill = "cornflowerblue" ) +
  geom_text( mapping = aes( label = paste0(miembros %>%
                              prettyNum( big.mark = ","),
                              " (",
                              proporcion %>% percent( accuracy = 0.1),
                              ")")) ,
             nudge_y = 1.5e3, size = 7, color = "gray30" ) +
  scale_y_continuous( expand = c(0,0,0.1,0),
                      labels = comma ) +
  scale_x_continuous( breaks = 0:3,
                      labels = c("C", "1", "2" , "3") ) +
  labs( title = "Menos del 1% del SNI ingresa en nivel 3",
        # subtitle = "El rombo marca el promedio",
        x = "Nivel",
        y = "Miembros que ingresaron con el Nivel",
        # fill = "nobilis",
        caption = caption_generico) +
  theme_classic( base_size = 25 ) +
  theme( axis.title.y = element_text( size = 20),
         axis.text = element_text( size = 15))

# guardamos
ggsave( filename = "barras_ingreso.png",
        width = 10, height = 7, dpi = 300 )
