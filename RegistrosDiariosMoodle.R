#### Código para procesar los datos de los reigistros de la plataforma Moodle ####
###
#----- Cargo Funciones ---- #
library(tidyverse) # para manipulación de la base
library(lubridate) # complemneto para fechad "tidy"
library(patchwork) # pegar graficos
theme_set(theme_minimal())

################# Datos en csv ################
                    ## Importante !!
# Verificar si las columnas de las planillas son las mismas
####

path<-""  # Ruta del archivo 

logs <- read_csv(paste(path,".csv",sep=""),
         col_types=cols(
            Hora = col_character(),
            `Nombre completo del usuario` = col_factor(),
            `Usuario afectado` = col_factor(),
            `Contexto del evento` = col_character(), #lo dejo asi porque le voy a extraer algunos caracteres
            Componente = col_character(),
            `Nombre evento` = col_factor(),
            Descripción = col_factor(),
            Origen = col_factor(),
            `Dirección IP` = col_factor()
         ))


##### Sample Data #####
load("Data/SampleData.RData")

#### Edicion de Fechas y cambio nombres de Variables ####

logs2 <- logs %>% mutate(Fecha_Hora =dmy_hm(Hora)) %>%
                mutate(Fecha = date(Fecha_Hora)) %>% # Solo fecha
                mutate(Hora = hour(Fecha_Hora)) %>% # Solo hora
               rename( Nombre = "Nombre completo del usuario",
               U_afec = "Usuario afectado",
               Contexto_evento = "Contexto del evento",
               Evento = "Nombre evento")

# Voy a remover primer palabra de Contexto Evento para que referencie igual que Componente

string_remove <- c(unique(logs2$Componente),"Archivo","Curso")# Hago un vector de caracteres a remover

string_remove <- str_c(string_remove,collapse="|") # lo separo con condicional para evaluar todos juntos

logs2<- logs2 %>% mutate(Contexto_evento = str_remove(Contexto_evento,string_remove)) %>% # Remuevo patrones
         mutate(Contexto_evento = str_replace(Contexto_evento,": ", ""))  # Reemplazo : por nada

#### Para trabajar los datos proporcionales ####
# El total de estudiantes matriculados es:
cursando<-length(unique(logs2$Nombre))

#####  Graficos  #####
# ---- Actividad Semana ----  

# Graficos de porciones (Dona) para ver Prop de activos por semana
# Si entra una o varias veces no cambia, basta con que entre una vez para ser registrado en esa semana

### Función que formatea los datos para Dona 
## https://www.r-graph-gallery.com/128-ring-or-donut-plot.html 
dona_data<-function(x){
   dat<-data.frame(fraction=c(x,1-x),category=c("Exito","Fracaso"))
   dat = dat[order(dat$fraction), ]
   dat$ymax = cumsum(dat$fraction)
   dat$ymin = c(0, head(dat$ymax, n=-1))
   dat
}
##

act<- logs2 %>% 
   mutate(Semana= isoweek(Fecha)) %>% # Convierto la semana a numero 
   group_by(Semana) %>% # Agrupo por semanas
   distinct(Nombre) %>% # Me quedo con un registro único de estudiante por semana
   summarize(Act= length(Nombre)/cursando) # Proporción de activos

prop_act<- act %>%
   split(.$Semana) %>% # una lista con el valor de prop de activos semanales
map(~ dona_data(.$Act)) %>% # Le aplico la funcion a cada semana
   map_dfr(~ as.data.frame(.),.id="Semana") %>% # Convierte en DF con la varible id que identofica la semana
   mutate(SemanaCurso = as.numeric(Semana) - min(as.numeric(Semana)-1)) # Convierto semana a numerica y el resto el valor anteriori para hacerlas empezar en Primera del curso

activos_semana<-ggplot(prop_act, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
   geom_rect(color="black") +
   coord_polar(theta="y") +
   xlim(c(0, 4)) +
   facet_wrap(~SemanaCurso) +
   geom_label(data=prop_act[prop_act$category=="Exito",], aes(x = 0, y = 0,
                                                                label=paste(round(fraction*100,1),"%")),
              size=rel(2)) +
   labs(title="Activos por semana",x=NULL,y=NULL) +
   theme_minimal() +
   theme(axis.text=element_blank()) +
   theme(axis.ticks=element_blank()) +
   theme(legend.title = element_blank()) +
   theme(plot.title = element_text(hjust = 0.5))+
   scale_fill_manual(values=c("#c51b8a","#feb24c"),
                     guide=F) 

plot(activos_semana)

# Si quisiera solo una semana especifica
# Ej la ultima semana 

activos_semana_ultima <- prop_act %>%  filter( SemanaCurso == max(SemanaCurso)) %>% 
   ggplot(aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
   geom_rect(color="black") +
   coord_polar(theta="y") +
   xlim(c(0, 4)) +
   geom_label(data=prop_act[prop_act$category=="Exito",], 
              aes(x = 0, y = 0,label=paste(round(fraction*100,1),"%")),
              size=rel(10)) +
   labs(title=paste("Activos semana",max(prop_act$SemanaCurso)),x=NULL,y=NULL) +
   theme(axis.text=element_blank()) +
   theme(axis.ticks=element_blank()) +
   theme(legend.title = element_blank()) +
   theme(plot.title = element_text(hjust = 0.5))+
   scale_fill_manual(values=c("#c51b8a","#feb24c"),
                     guide=F) 

plot(activos_semana_ultima)


#---- Recursos y Componentes ----

componentes<-logs2 %>% 
   group_by(Componente) %>%
   distinct(Nombre) %>%
   count(Componente) %>%
   mutate(n2 = n/cursando) %>% 
   filter (Componente %in% c("Sistema","Recurso","Foro")) %>% # Solo voy a mirar estos tres
   ggplot(aes(x=fct_reorder(Componente,n2, .fun=max,.desc=TRUE),y=n2)) + 
   geom_bar(stat="identity",fill="#2ca25f",width=0.5) +
   labs(x=NULL,y="Proporción de estudiantes", 
        title="Registros de estudiantes por componente",
        subtitle=paste(min(logs2$Fecha),max(logs2$Fecha), sep="/")) 

plot(componentes)

### Todos los Recurso Juntos! 
# ---- MUCHA INFORMACION ###

Recursos<- logs2 %>%
   group_by(Componente,Contexto_evento) %>%
   distinct(Nombre) %>% 
   filter(Componente %in% c("Recurso","Página")) %>%
   count(Componente) %>% 
   filter(n>100) %>%
   mutate(Prop= n/Cursando) %>%
   filter(Prop > 0.2) %>%
   ggplot(aes(x=fct_reorder(Contexto_evento,Prop,.fun=max),y=Prop)) + 
   geom_segment(aes(x=fct_reorder(Contexto_evento,Prop,.fun=max), xend=Contexto_evento,
                    y=0,yend=Prop),
                stat = "identity",color="grey") + 
   geom_point(size=3,pch=21,fill="#2ca25f") +
   labs(x=NULL, y="Propoción de estudiantes",
        title="Recursos materiales (teoricos, tutoriales, etc)") +
      coord_flip() + 
   theme(axis.text.y = element_text(face="bold",size=rel(0.75))) 

plot(Recursos)

# # 
# Solo aquellos que tengan la palabra "Teórico"

recursos_teoricos<- logs2 %>%
   group_by(Contexto_evento,Componente) %>%
   distinct(Nombre) %>% 
   filter(Componente %in% c("Recurso") ) %>%
   filter(grepl("Teórico",Contexto_evento)) %>%
   group_by(Contexto_evento) %>%
   count() %>% 
   mutate(Prop= n/Cursando) %>% 
   ggplot(aes(y=fct_relevel(Contexto_evento,rev),x=Prop)) + 
   geom_segment(aes(y=fct_relevel(Contexto_evento,rev), yend=Contexto_evento,
                    x=min(Prop)-0.2,xend=Prop),
                stat = "identity",color="grey") + 
   geom_point(size=3,pch=21,fill="#2ca25f") +
   labs(x="Propoción de estudiantes", y=NULL,
        title="Teoricos") +
    theme(axis.text.y = element_text(face="bold",size=rel(0.75))) 

plot(recursos_teoricos)


#---- Actividades por dias, fechas horas ----
# Grillas de Hora y dias
 
ha <- logs2 %>% group_by(Hora) %>% 
    distinct(Nombre) %>%
    count() %>%
    ggplot(aes(x=Hora, y=n/Cursando)) + geom_line() +
    labs( subtitle="Visitas por hora",y=NULL,
          title="A")
 
 #  Remuevo la fecha de los parciales y acumulo por dia hora
data_hb<- logs2 %>% mutate(Day = wday(Fecha, label=TRUE,abbr=TRUE)) %>%
    filter (Fecha != ("2020-05-16")) %>% 
    group_by(Day,Hora) %>% distinct(Nombre) %>% count () 
 
hb_sp<-data_hb %>% 
    ggplot(aes(x=Hora,y=Day, fill=n)) + geom_tile() + 
    labs(fill="N° Estudiantes",y=NULL,
         subtitle="Acumulado por día y hora",
         title="B") +
     scale_fill_gradient2(low="#e0ecf4", high="#8856a7",mid = "#9ebcda",
                         midpoint= median(data_hb$n)) +
       theme(legend.position="bottom")
 
 patch<- ha + hb_sp
 
plot(patch)
 