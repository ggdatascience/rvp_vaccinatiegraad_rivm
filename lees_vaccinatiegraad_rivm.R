#Lees Vaccinatiegraad RIVM
#NB We kregen de data aangeleverd met in sheet 2023 geen in CELL A1 
#(itt de sheets van andere jaren waar TOTAAL NEDERLAND  staat) Deze inconsistentie 
#zorgt ervoor dat de rij die initieel voor kolomkoppen wordt gebruikt eentje opschuift
#t.a.v de andere sheets. Dat maakt het onmogelijk om alles tegelijk in te lezen. 
#Opgelost door zelf iets in A1 van sheet 2023 te zetten.
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#workbook laden
wb <- openxlsx::loadWorkbook(bestand)
#checken welke sheets er zijn
sheets <- wb %>% openxlsx::sheets()

#Aanname: Alle met een 4 cijfers is een jaar
jaren <- sheets[str_detect(sheets,"[:digit:]{4}")]

#als aantal_jaren meenemen is ingesteld: 
#vector jaren inkorten en laatste x jaren overhouden
if(!is.null(aantal_jaren_meenemen)){
  
  jaren <- tail(jaren, aantal_jaren_meenemen)
  
}

#DATA NL ophalen
#Voor iedere sheet met een jaartal: Data NL ophalen. 
cijfers_nl <- lapply(jaren, function(x){
  
  
  df <- openxlsx::read.xlsx(bestand,sheet = x) %>%
    #kolommen X47,X48 en X49 bestaan alleen in 2023. 
    #MenC/ACWY Adolescenten. Verwijderen. 
    select(-any_of(c("X47","X48","X49")))
  
  
  #Tabellen zijn onhandig geformatteerd voor verwerking in R. 
  #Informatie die over de inhoud kolommen gaat staat op meerdere rijen en niet boven elke kolom
  
  #Vector met bruikbare kolomnamen maken.
  #Kolominformatie ophalen uit rijen 1:6 (3,8 in excel)
  kolominfo <- df[1:6,-1] %>%
    #transponeren zodat we tidyr::fill() kunnen gebruiken
    t() %>%
    as.data.frame() %>% 
    #van boven naar beneden aanvullen waar NA (dus eigenlijk vlnr)
    fill(everything()) %>%
    #kolomnaam opbouwen uit alle rij-info excl geboortejaar
    mutate(kolomnamen = paste(`1`,`2`,`3`,`4`,`6`) %>%
             str_trim() %>%
             str_replace_all(" ","_")) %>%
    pull(kolomnamen) 
  
  #kolomnamen globaal toewijzen voor hergebruik in vaccinatiegraad_rivm_ggd
  kolomnamen <<- c("id",kolominfo)
  
  #namen aan df toewijzen
  names(df) <- kolomnamen
  
  df %>%
    select(#Alleen de variabelen ophalen die gekozen zijn 
      matches(cohorten) & 
        matches(vaccinsoort) &
        matches(vaccinatietoestand)) %>%
    #Rij 7 = de rij waar de cijfers NL staan (rij 9 in Excel)
    filter(row_number() %in% c(7)) %>% 
    #Alles lang maken
    pivot_longer(everything()) %>%
    mutate(
      #Type cijfer ophalen (aantal gevaccineerd,aantal clienten en %)
      type_cijfer = str_extract(name,"(?<=jaar_).*"),
      #Inhoudelijke naam ophalen
      name = str_remove(name,"(?<=jaar).*")) %>%
    #Weer breed maken; kolom per type cijfer
    pivot_wider(values_from = value,
                names_from = type_cijfer) %>%
    #Jaar; aanpassen v. rapportage jaar naar werkelijk jaar
    mutate(jaar = as.numeric(x) - 1)
  
}) %>% 
  #dfs per jaar samenvoegen
  do.call(rbind,.) %>%
  mutate(`%` = as.numeric(`%`)*100 %>% round(1),
         #Label v. NL cijfers maken: naam vaccin + cohortnaam + leeftijd/vaccinstatus
         name =  paste(
           #Naam vaccin
           str_extract(name, glue("(?<={cohorten}_)[[:alpha:]|\\(|\\)]*")),
           #Cohortnaam
           str_extract(name, glue("{cohorten}")) %>% str_remove("_"),
           #linebreak
           "\n",
           #Leeftijd off vaccinatietoestand
           case_when(vacctoestand_ipv_leeftijd ~ str_extract(name,vaccinatietoestand) %>%
                       str_replace_all("_"," "),
                     TRUE ~ paste(str_extract(name, "[:digit:]+"), "jaar")
                     ))
          )


#Voor iedere sheet met een jaartal: data per pc4 & gemeente ophalen
vaccinatiegraad_rivm_ggd <- lapply(jaren, function(x){

  df <- openxlsx::read.xlsx(bestand,sheet = x) %>%
    #kolommen X47,X48 en X49 bestaan alleen in 2023. 
    #MenC/ACWY Adolescenten. Verwijderen. 
    select(-any_of(c("X47","X48","X49")))
  
  #namen kolommen toewijzen (gedefinieerd in cijfers_nl)
  names(df) <- kolomnamen
  
  #Tabel bij begint en eind afknippen .
  start_tabel = which(df$id == "Rijlabels")
  eind_tabel = which(df$id == "Eindtotaal")
  
  df <- df %>%
    select(id,
           #Alleen ophalen wat boven is opgegeven
           matches(cohorten) &
           matches(vaccinsoort) &
           matches(vaccinatietoestand)) %>%
    #tabel filteren op rijnummers waar gemeente & pc4 data in zit 
    filter(row_number() > start_tabel & row_number() < eind_tabel) %>%
    #Gemeentenaam uit rij met postcodes halen voor apparte kolom
    mutate(gemeentenaam = case_when(
      #Als er een letter in de variabele zit is het een gemeente ipv postcode
      str_detect(id, "[:alpha:]") ~ id,
      TRUE ~ NA)) %>%
    #gemeentenaam aanvullen van boven naar beneden
    tidyr::fill(gemeentenaam, .direction = "down") 
  
  df %>%
    #Jaar; aanpassen v. rapportage jaar naar werkelijk jaar
    mutate(jaar = as.numeric(x) -1) %>%
    #alle aantallen naar numeric & percentage afronden
    mutate(across(contains("_"), as.numeric),
           across(contains("%"),~ round(.x * 100, 1))) %>%
    #Na's vervangen met 0 zodat NA als <5 clienten geteld kan worden
    mutate_if(is.numeric, replace_na,0) 
}) %>% 
  #dataframes koppelen
  do.call(rbind,.) 

#Op verzoek:  Kaart zonder exacte percentages maken.maar met categorieën
if(!percentages_in_kaart) {
  
  vaccinatiegraad_rivm_ggd <- vaccinatiegraad_rivm_ggd %>%
    mutate(across(contains("%"), ~
                    
                    case_when(.x < 90 ~ "< 90%",
                              .x < 95 ~ "90-94%",
                              .x >= 95 ~ ">= 95%",
                              TRUE ~ "?"
                    )))
}

#Sheetnamen in RIVM excel zijn rapportagejaren. gaat over voorgaande jaren. dus: -1
jaren <- as.numeric(jaren) - 1

#Gemeentedata uit df halen; als er tekst in het 'id' veld staat is het een gemeente
vaccinatiegraad_gemeente <- vaccinatiegraad_rivm_ggd %>%
  filter(id %>% str_detect("[:alpha:]"))

#Shapefile lezen & vaccinatie data aan koppelen
gemeente_sf <- st_read("../shapefiles/cbsgebiedsindelingen2022.gpkg",
                       layer = "gemeente_gegeneraliseerd",
                       quiet = T) %>%
  select(-id) %>%
  left_join(vaccinatiegraad_gemeente, by = c("statnaam" = "gemeentenaam")) %>%
  #Waar "jaar" missing is = is het geen HvB gemeente
  filter(!is.na(jaar)) %>%
  mutate(niveau = "gemeente",
         gemeentenaam = id) %>%
  st_transform(4326)


#Postcodedata uit df halen
vaccinatiegraad_pc4 <- vaccinatiegraad_rivm_ggd %>% 
  filter(id %>% str_detect("[:digit:]"))

#PC4 is niet alle jaren gelijk; missing pc4 als lege "grijze" polygonen weergeven in die jaren. 
#PC4 aanvullen in jaren waar er pc4 ontbreken

#koppeltabel maken van gemeentenamen en pc4
gemeentenamen_pc4 <- vaccinatiegraad_pc4 %>%
  group_by(id, gemeentenaam) %>%
  summarise() %>%
  ungroup()


#dataframe met alle mogelijke combinaties pc4/jaar maken
pc4_jaar_df <- expand.grid(c(gemeentenamen_pc4$id,
                             #Eventueel handmatig toegevoegde postcodes meenemen
                             toegevoegde_postcodes),jaren) %>%
  #gemeentenaam koppelen
  left_join(gemeentenamen_pc4, by = c("Var1" = "id")) %>%
  #PC4 5048 nog een gemeentenaam geven
  mutate(gemeentenaam = ifelse(is.na(gemeentenaam),
                               "Tilburg",gemeentenaam))

#namen gelijk trekken met vaccinatiegraad_pc4
names(pc4_jaar_df) <- c("id","jaar", "gemeentenaam_2")

#Lege pc4 aan vaccinatiegraaddata koppelen
vaccinatiegraad_pc4 <- vaccinatiegraad_pc4 %>%
  full_join(pc4_jaar_df, by = c("id","jaar")) %>%
  mutate(gemeentenaam = ifelse(is.na(gemeentenaam),
                               gemeentenaam_2,
                               gemeentenaam)) %>%
  select(-gemeentenaam_2) %>%
  mutate_if(is.numeric, replace_na,0)


#PC4 shapefile inlezen en vaccinatiedata data koppelen  
pc4_sf <- st_read("../shapefiles/PC4.shp",
                  quiet = T) %>%
  left_join(vaccinatiegraad_pc4, by = c("PC4" = "id")) %>%
  mutate(id = PC4,
         niveau = "PC4") %>%
  #Alle PC4 waar jaar missing is, zijn niet in HvB
  filter(!is.na(jaar)) %>%
  st_transform(4326)



#Dataframe met labels voor kaartlagen maken
categorieen <- names(vaccinatiegraad_rivm_ggd %>% select(-c(id,gemeentenaam,jaar))) %>%
  str_remove("_Aantal.*|_%") %>%
  unique()

niveaus <- c("PC4","gemeente")

#Alle combinaties van categorie & nivea, + een jaarvariabele
kaartlagen <- expand.grid(categorieen, max(jaren), niveaus) %>%
  data.frame()

names(kaartlagen) <- c("categorie","jaar","niveau") 

kaartlagen <- kaartlagen %>% 
  mutate(
    categorie = as.character(categorie), #van factor naar character
    cohort = str_extract(categorie,cohorten),
    vaccinsoort = str_extract(categorie,glue("(?<={cohorten}_)[[:alpha:]|\\(|\\)]*")),
    #o.b.v. de variabele vacctoestand_ipv_leeftijd
    #Wordt een leeftijd of de vaccinatietoestand uit de categorie gehaald
    leeftijd_of_vaccinatietoestand =  case_when(
      vacctoestand_ipv_leeftijd ~ str_extract(categorie, vaccinatietoestand) %>%
        str_replace_all("_"," "),
      TRUE ~ str_extract(categorie,"[:digit:]+") %>% paste("jaar")),
    #vaccinatietoestand ook altijd los meenemen
    vaccinatietoestand = str_extract(categorie, vaccinatietoestand) %>% 
      str_replace_all("_"," "),
    naam_noemer = glue("{cohort} ({leeftijd_of_vaccinatietoestand})"),
    cohort = str_remove(cohort,"_"),    #_ in 'adolescente_meisjes' verwijderen
    groepnaam = glue("{vaccinsoort} {cohort} ({leeftijd_of_vaccinatietoestand}) per {niveau} {max(jaren)}"),
    data = case_when(niveau == "PC4" ~ "pc4_sf",
                     niveau == "gemeente" ~ "gemeente_sf"))

if(!jaar_in_kaartlaag) {
  kaartlagen$groepnaam <- kaartlagen$groepnaam %>% str_remove(" [:digit:]{4}$")
}
