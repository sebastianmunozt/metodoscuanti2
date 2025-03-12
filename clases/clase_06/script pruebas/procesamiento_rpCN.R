#. CERRONAVIA
# 0. Cargo librerías ####

options("install.lock"=FALSE)
library(pacman)
webshot::install_phantomjs()
pacman::p_load (tidyverse, haven, openxlsx, gtsummary, gt, openxlsx, 
                udunits2,sf,ggmap,cowplot, ggthemes, splitstackshape, 
                extrafont, webshot, scales)


#elegir fuentes
font_import()
y
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       #vector of font family names
Sys.setenv(LANGUAGE="es")


# 1. Género####
baseSBgenero <- read.xlsx (xlsxFile = "bases/CR/CR SM.xlsx", sheet = "genero", startRow = 2)

colnames(baseSBgenero)

#1.1 gráfico #### 

baseSBgeneroG <- ggplot (baseSBgenero, aes (x=Categoría, y=Porcentaje.Area, fill=Categoría)) +
  geom_col() +
  theme_minimal_hgrid()+   
  labs (title = "Género Área de Influencia", 
        x = "Género", y = "%", 
        caption="*Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)")+
  scale_fill_economist()+
  guides (fill="none") + 
  theme (plot.title = element_text(hjust = 0.5), 
         plot.caption = element_text(hjust=0),
         text=element_text(family="Calibri"))

#guardo gráfico Género
ggsave("Outpus/CN/01.1.baseSBgeneroG.png", baseSBgeneroG,scale = 1.2)


colnames (baseSBgenero)

#1.2.tabla ####
baseSBgeneroT <- baseSBgenero %>% 
  gt () %>%  tab_header("Género en Área de Influencia y Total Comunas") %>% 
  cols_label(Porcentaje.Area ="% Área", 
             Area ="Área", 
             CN = "Cerronavia", 
             Porcentaje.CN = "% Cerronavia", 
             PH= "Pudahuel", 
             Porcentaje.PH = "% Pudahuel") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  fmt_number(
    columns = c(Porcentaje.Area, Porcentaje.CN, Porcentaje.PH),
    decimals = 1,
    use_seps = FALSE,
    dec_mark = ",") %>% 
  fmt_number(
    columns = c(Area, CN, PH),
    decimals = 0,
    dec_mark = ",", 
    sep_mark = ".") %>%
  grand_summary_rows (fns = list (TOTAL = "sum"), 
                      columns = c(Area, Porcentaje.Area, CN, Porcentaje.CN, PH, Porcentaje.PH), 
                      formatter = fmt_number, decimals = 0, missing_text = "", sep_mark = ".") %>%  
  tab_footnote(
    footnote = "Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)",
    locations = cells_title(groups = "title"))

#guardo género
baseSBgeneroT %>%
gtsave (
"01.02.baseSBgeneroT.png", expand = 10,
path = "Outpus/CN/")



#2. Edad ####
baseSBedad <- read.xlsx (xlsxFile = "bases/CR/CR SM.xlsx", sheet = "edad", startRow = 2, rows =2:22)

#2.1. tabla####
baseSBedadt <- baseSBedad %>% 
  gt () %>%  tab_header("Población por Edad en Área de Influencia y Total Comuna") %>% 
  cols_label(Porcentaje.Area ="% Área", 
             Area ="Área", 
             CN = "Cerronavia", 
             Porcentaje.CN = "% Cerronavia", 
             PH= "Pudahuel", 
             Porcentaje.PH = "% Pudahuel") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  fmt_number(
    columns = c(Porcentaje.Area, Porcentaje.CN, Porcentaje.PH),
    decimals = 1,
    use_seps = FALSE,
    dec_mark = ",") %>% 
  fmt_number(
    columns = c(Area, CN, PH),
    decimals = 0,
    dec_mark = ",", 
    sep_mark = ".") %>% 
grand_summary_rows (fns = list (TOTAL = "sum"), 
                    columns = c(Area, Porcentaje.Area, CN, Porcentaje.CN, PH, Porcentaje.PH), 
                    formatter = fmt_number, decimals = 0, missing_text = "", sep_mark = ".") %>%  
  tab_footnote(
    footnote = "Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)",
    locations = cells_title(groups = "title"))


#guardo tabla edad 
baseSBedadt %>%
  gtsave(
    "02.01 baseSBedadt.png", expand = 10,
    path = "Outpus/CN/")


#2.2. Gráfico de torta reuniendo edades####
baseSBedad2 <- read.xlsx (xlsxFile = "bases/CR/CR SM.xlsx", sheet = "edad", startRow = 2, rows =27:31)
baseSBedad2  <- baseSBedad2  %>% 
  filter(Categoría !="total")

ggplot(baseSBedad2 ,mapping = aes(x="",y=Porcentaje.Area, fill=Categoría))+
  geom_bar(stat = "identity",color="white")+
  geom_text(aes(label=Porcentaje.Area),
            position=position_stack(vjust=0.5))

baseSBedad2g <- ggplot(baseSBedad2,aes(x="",y=Porcentaje.Area, fill=Categoría))+
  geom_bar(stat = "identity",color="white")+
  scale_fill_economist()+
  geom_text(aes(label=percent(Porcentaje.Area/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  theme_void()+
  labs(title="Grupos de Edad Área de Influencia", caption="*Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)")+
  theme (plot.title = element_text(hjust = 0.5, size=18),
         plot.caption = element_text(hjust=0),
         text=element_text(family="Calibri")) 

ggsave("Outpus/CN/02.2 baseSBedad2g.png", baseSBedad2g,scale = 1.2)

#3. Tipo Población####
baseSBorig <- read.xlsx (xlsxFile = "bases/CR/CR SM.xlsx", sheet = "tipo1", startRow = 2)

baseSBorig <- baseSBorig %>% 
  mutate(Categoría = case_when (Categoría =="Indígena u originario_x000D_" ~ "Indígena u originario", 
                                Categoría =="Inmigrantes" ~ "Inmigrantes", 
                                Categoría =="Total" ~ "Total", 
                                Categoría =="Otros" ~ "Otros")) 

baseSBorig  <- baseSBorig  %>% 
  filter(Categoría !="Total")

#3.1. tabla####
baseSBorigT <- baseSBorig %>% 
  gt () %>%  tab_header("Tipo población en Área de Influencia y Total Comuna") %>% 
  cols_label(Porcentaje.Area ="% Área", 
             Area ="Área", 
             CN = "Cerronavia", 
             Porcentaje.CN = "% Cerronavia", 
             PH= "Pudahuel", 
             Porcentaje.PH = "% Pudahuel") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  fmt_number(
    columns = c(Porcentaje.Area, Porcentaje.CN, Porcentaje.PH),
    decimals = 1,
    use_seps = FALSE,
    dec_mark = ",") %>% 
  fmt_number(
    columns = c(Area, CN, PH),
    decimals = 0,
    dec_mark = ",", 
    sep_mark = ".") %>% 
  grand_summary_rows (fns = list (TOTAL = "sum"), 
                      columns = c(Area, Porcentaje.Area, CN, Porcentaje.CN, PH, Porcentaje.PH), 
                      formatter = fmt_number, decimals = 0, missing_text = "", sep_mark = ".") %>%  
  tab_footnote(
    footnote = "Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)",
    locations = cells_title(groups = "title"))

#guardo tabla edad 
baseSBorigT %>%
  gtsave(
    "06.02 baseSBorigT.png", expand = 10,
    path = "Outpus/CN/")

#3.2. gráfico####
baseSBorigG <-ggplot (baseSBorig, aes (x=reorder (Categoría, Porcentaje.Area), y=Porcentaje.Area, fill=Categoría)) +
  geom_col() +
  theme_minimal_hgrid()+   
  labs (title = "Tipo de Población Área de Influencia", 
        x = "Tipo", y = "%", caption="*Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)")+
  scale_fill_economist()+
  guides (fill="none") + 
  theme (plot.title = element_text(hjust = 0.5),
         plot.caption = element_text(hjust=0),
         text=element_text(family="Calibri"))
#guardo
ggsave("Outpus/CN/06.01 baseSBorigG.png", baseSBorigG ,scale = 1.2)


# 4. Escolaridad#### 
baseSBescolaridad <- read.xlsx (xlsxFile = "bases/CR/CR SM.xlsx", sheet = "escolaridad", startRow = 2, rows =2:12)

baseSBescolaridad  <- baseSBescolaridad  %>% 
  filter(Categoría !="Total")



#4.1. tabla####

baseSBescolaridadt <- baseSBescolaridad %>% 
  gt () %>%  tab_header("Población por Máximo Nivel Educativo Alcanzado") %>% 
  cols_label(Porcentaje.Area ="% Área", 
             Area ="Área", 
             CN = "Cerronavia", 
             Porcentaje.CN = "% Cerronavia", 
             PH= "Pudahuel", 
             Porcentaje.PH = "% Pudahuel") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  fmt_number(
    columns = c(Porcentaje.Area, Porcentaje.CN, Porcentaje.PH),
    decimals = 1,
    use_seps = FALSE,
    dec_mark = ",") %>% 
  fmt_number(
    columns = c(Area, CN, PH),
    decimals = 0,
    dec_mark = ",", 
    sep_mark = ".") %>% 
  grand_summary_rows (fns = list (TOTAL = "sum"), 
                      columns = c(Area, Porcentaje.Area, CN, Porcentaje.CN, PH, Porcentaje.PH), 
                      formatter = fmt_number, decimals = 0, missing_text = "", sep_mark = ".") %>%  
  tab_footnote(
    footnote = "Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)",
    locations = cells_title(groups = "title"))


baseSBescolaridadt %>%
  gtsave(
    "03.02 baseSBescolaridadt.png", expand = 10,
    path = "Outpus/CN/")



#4.2. gráfico####
baseSBescolaridad <- read.xlsx (xlsxFile = "bases/CR/CR SM.xlsx", sheet = "escolaridad", startRow = 2, rows =17:21)

baseSBescolaridad  <- baseSBescolaridad  %>% 
  filter(Categoría !="Total")

ggplot(baseSBescolaridad ,mapping = aes(x="",y=Porcentaje.Area, fill=Categoría))+
  geom_bar(stat = "identity",color="white")+
  geom_text(aes(label=Porcentaje.Area),
            position=position_stack(vjust=0.5))

baseSBescolaridadG <- ggplot(baseSBescolaridad,aes(x="",y=Porcentaje.Area, fill=Categoría))+
  geom_bar(stat = "identity",color="white")+
  scale_fill_economist()+
  geom_text(aes(label=percent(Porcentaje.Area/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  theme_void()+
  labs(title="Máximo Nivel Educativo Alcanzado", caption="*Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)" )+
  theme (plot.title = element_text(hjust = 0.5, size=15),
         plot.caption = element_text(hjust=0),
         text=element_text(family="Calibri")) 

#guardo
ggsave("Outpus/CN/03.01 baseSBescolaridadG.png", baseSBescolaridadG ,scale = 1.2)


#5. NES ####
baseSBnes <- read.xlsx (xlsxFile = "bases/CR/CR SM.xlsx", sheet = "nes", startRow = 2, rows =22:26)

baseSBnes  <- baseSBnes  %>% 
  filter(Categoría !="Total")


#5.1.Gráfico####
baseSBnesG <- ggplot (baseSBnes, aes (x=Categoría, y=Porcentaje.Area, fill=Categoría)) +
  geom_col() +
  theme_minimal_hgrid()+   
  labs (title = "% GSE Área de Influencia", 
        x = "GSE", y = "%", 
        caption="*Fuente: Mapcity Chile 2022")+
  scale_fill_economist()+
  guides (fill="none") + 
  theme (plot.title = element_text(hjust = 0.5), 
         plot.caption = element_text(hjust=0),
         text=element_text(family="Calibri"))

#guardo
ggsave("Outpus/CN/04.01 baseSBnesG.png", baseSBnesG ,scale = 1.2)


#5.2. Tabla####
baseSBnest <- baseSBnes %>% 
  gt () %>%  tab_header("Población por GSE Área, Comunas y Región Metropolitana") %>% 
  cols_label(Porcentaje.Area ="% Área", 
             Area ="Área", 
             CN = "Cerronavia", 
             Porcentaje.CN = "% Cerronavia", 
             PH= "Pudahuel", 
             Porcentaje.PH = "% Pudahuel",
             Porcentaje.RM = "% RM") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  fmt_number(
    columns = c(Porcentaje.Area, Porcentaje.CN, Porcentaje.PH, Porcentaje.RM),
    decimals = 1,
    use_seps = FALSE,
    dec_mark = ",") %>% 
  fmt_number(
    columns = c(Area, CN, PH, RM),
    decimals = 0,
    dec_mark = ",", 
    sep_mark = ".") %>%  
  grand_summary_rows (fns = list (TOTAL = "sum"), 
                    columns = c(Area, Porcentaje.Area, CN, Porcentaje.CN, PH, Porcentaje.PH, RM, Porcentaje.RM), 
                    formatter = fmt_number, decimals = 0, missing_text = "", sep_mark = ".") %>%  
  tab_footnote(
    footnote = "Fuente: Mapcity Chile 2022",
    locations = cells_title(groups = "title"))

#guardo
baseSBnest %>%
gtsave (
"04.02 baseSBnest.png", expand = 10,
path = "Outpus/CN/")


# 6. Tipo de Trabajo ####
baseSBtipotrabajo <- read.xlsx (xlsxFile = "bases/CR/CR SM.xlsx", sheet = "tipotrabajo", startRow = 2)

baseSBtipotrabajo  <- baseSBtipotrabajo  %>% 
  filter(Categoría !="Total")

#6.1. tabla####
baseSBtipotrabajoT <- baseSBtipotrabajo %>% 
  gt () %>%  tab_header("Población por Tipo de Trabajo en Área de Influencia y Total Comuna") %>% 
  cols_label(Porcentaje.Area ="% Área", 
             Area ="Área", 
             CN = "Cerronavia", 
             Porcentaje.CN = "% Cerronavia", 
             PH= "Pudahuel", 
             Porcentaje.PH = "% Pudahuel") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  fmt_number(
    columns = c(Porcentaje.Area, Porcentaje.CN, Porcentaje.PH),
    decimals = 1,
    use_seps = FALSE,
    dec_mark = ",") %>% 
  fmt_number(
    columns = c(Area, CN, PH),
    decimals = 0,
    dec_mark = ",", 
    sep_mark = ".") %>% 
grand_summary_rows (fns = list (TOTAL = "sum"), 
                    columns = c(Area, Porcentaje.Area, CN, Porcentaje.CN, PH, Porcentaje.PH), 
                    formatter = fmt_number, decimals = 0, missing_text = "", sep_mark = ".") %>%  
  tab_footnote(
    footnote = "Fuente: Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)",
    locations = cells_title(groups = "title"))


#guardo
baseSBtipotrabajoT %>%
  gtsave (
    "05.02 baseSBtipotrabajoT.png", expand = 10,
    path = "Outpus/CN/")


#6.2 gráfico####
# población por tipo de trabajo
baseSBtipotrabajoG <- ggplot (baseSBtipotrabajo, aes (x=reorder (Categoría, Porcentaje.Area), y=Porcentaje.Area, fill=Categoría)) +
  geom_col() +
  theme_minimal_hgrid()+   
  labs (title = "Población por Tipo de Trabajo en Área de Influencia", 
        x = "Tipo de trabajador/a", y = "%", caption="*Censo Áreas Urbanas INE 2017, actualizado 2020 (Equifax)")+
  scale_fill_economist()+
  guides (fill="none") + 
  theme (plot.title = element_text(hjust = 0.4, size=9.6), 
         text=element_text(family="Calibri"), 
         plot.caption = element_text(hjust=0),
         axis.text.x = element_text (size=8, angle=55, vjust = 0.6))

#guardo
ggsave("Outpus/CN/05.01 baseSBtipotrabajoG.png", baseSBtipotrabajoG ,scale = 1.2)





