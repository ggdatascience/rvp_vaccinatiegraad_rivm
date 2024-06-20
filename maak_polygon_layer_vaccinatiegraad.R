#Functie om polygoonlaag te maken voor leaflet kaart RIVM vaccinatiegraden op PC4 en gemeenteniveau
maak_leaflet_polygon_layer <- function(kaart, data, huidig_jaar, categorie,groepnaam, 
                                       naam_noemer, percentages_in_kaart = T, 
                                       kleuren_graad = c("green" = 95,
                                                         "#FB6A4A" = 90,
                                                         "#EF3B2C" = 85,
                                                         "#A50F15" = 75,
                                                         "#67000D" = 0)
                                       ){
  
# print(glue("{groepnaam} verwerken"))

#referenties naar relevante variabelen maken
clienten = glue("{categorie}_Aantal_clienten")
percentage = glue("{categorie}_%")

#kleuren vacc. graad: waarden & kleurcodes uit elkaar halen
afkapwaarden <- kleuren_graad %>% unname()
kleuren <- kleuren_graad %>% names()


if(percentages_in_kaart){
#Kleuren aan dataframe toewijzen o.b.v vaccinatiegraad
data = data %>%
  mutate(
    
    kleuren = case_when(
      #Minder dan 5 clienten = niet laten zien
      !!sym(clienten) < minimum_verbergen ~ "grey",
      #95% of hoger = WHO norm
      !!sym(percentage) >= afkapwaarden[1] ~ kleuren[1],
      #Alles daaronder is verschillende graden van slecht
      !!sym(percentage) >= afkapwaarden[2] ~ kleuren[2],
      !!sym(percentage) >= afkapwaarden[3] ~  kleuren[3],
      !!sym(percentage) >= afkapwaarden[4] ~  kleuren[4],
      TRUE ~  kleuren[5])
    )

} else{

  #Kleuren anders toewijzen wanneer er voor !percentages_in_kaart is gekozen
  data = data %>%
    mutate(kleuren = case_when(
      !!sym(clienten) < minimum_verbergen ~ "grey",
      !!sym(percentage) == ">= 95%" ~  "#004529",
      !!sym(percentage) == "90-94%" ~  "#ADDD8E",
      !!sym(percentage) == "< 90%" ~  "#A50F15",
      
    ))
}

#Voor de polygoon willen we alleen het laatste jaar laten zien
sf <- data %>% filter(jaar == huidig_jaar)

#Popup Labels maken per polygoon
labels <- lapply(sf$id, function(x){
  
  titel_popup <- if(is.null(alt_popup_titel)){
    glue("<h5>Vaccinatiegraad {groepnaam}</h5>") 
  }else{
    alt_popup_titel
  }
  
  #Subset maken voor specifieke gemeente/pc4
  temp_df = sf %>% filter(id == x)
  
  #Naam gebied toewijzen
  naam = case_when(
    #Label gebied voor "gemeente"
    temp_df$niveau == "gemeente" ~ glue("<h6>{temp_df$id}</h6>"),
    #Label gebied voor PC4
    temp_df$niveau == "PC4" ~ glue("<h6> PC4: {temp_df$id} ({temp_df$gemeentenaam}) </h6>"),
    )
    

  #Als < 5 clienten niks weergeven & functie afbreken
  if(temp_df[[clienten]] < minimum_verbergen){
    
    return(glue("<h5>Vaccinatiegraad {groepnaam}</h5>
                {naam}
                <p>Aantal {naam_noemer} te laag om vaccinatiegraad te weergeven (< {minimum_verbergen})</p>"))
    
  }
  
  #lege disclaimervar maken, vullen als clienten < minimum_waarschuwing
  disclaimer = ""
  if(temp_df[[clienten]] < minimum_waarschuwing){

    disclaimer = glue("<p style='color:red;' > LET OP: Minder dan {minimum_waarschuwing} {naam_noemer} </p>")
  }
  
  #Bij percentages boven de 95% willen we niet het exacte percentage laten zien maar ">=95%"
  if(temp_df[[percentage]] >= 95 | temp_df[[percentage]] == ">= 95%" ){
    melding = glue("<strong><span style='color:{temp_df$kleuren}'> >95% </span></strong> uit totaal {temp_df[[clienten]]} {naam_noemer}")
  } else{
    melding = glue("<strong><span style='color:{temp_df$kleuren}'>{temp_df[[percentage]]}% </span></strong> (uit totaal {temp_df[[clienten]]} {naam_noemer})")
  }
  
  #Data uit meerjaren_df halen voor trendlijntje & kleurtjes
  trend_df <- data %>% filter(id == x)
  
  
  
  tabel_header = NULL
  
  #labels van jaren aanpassen als er alternatieve jaarnamen zijn toegewezen
  if(!is.null(names(jaren))){
    
    #Het woordgedeelte vd alteratieve namen wordt een header voor de tabel
    tabel_header <- c(length(jaren),1)
    
    names(tabel_header) <-  c(names(jaren) %>% str_remove("[:digit:]{4}") %>% str_trim() %>% unique(), " ")
    
    
    #De kolomkoppen worden het 'jaar' gedeelte vd alternatieve namen
    nieuwe_jaarlabels <- str_extract(names(jaren),"[:digit:]{4}")
    trend_df$jaar <- nieuwe_jaarlabels
  }
  
  
  #Trendwaarden als vector opslaan; alles op of hoger dan 95 naar 95.
  if(percentages_in_kaart){
    trendwaarden <- ifelse(trend_df[[percentage]] >= 95, 95,trend_df[[percentage]])
    
  } else{
  
    #als we categorieen willen laten zien kunnen we niet een trendlijn
    #maken op de exacte cijfers. Die zijn er immers niet.
    #Daarom 1,2,3. zodat we  kunnen laten zien wat hoger / lager is
    trendwaarden <- case_when(trend_df[[percentage]] == "< 90%" ~ 1,
                              trend_df[[percentage]] == "90-94%" ~ 2,
                              trend_df[[percentage]] == ">= 95%" ~ 3,
                              
                              )
    
  }

  trend <- trend_df %>%
    select(jaar,!!sym(percentage), kleuren) %>%
    #>= 95 niet exact laten zien
    mutate(percentage = case_when(
      !!sym(percentage) >= 95 ~ ">= 95",
      TRUE ~ as.character(!!sym(percentage))
      ),
      #Kleurcodering cellen:
      percentage = cell_spec(percentage,
                             background = kleuren,
                             color = "white")) %>%
    data.frame() %>%
    select(jaar,percentage) %>%
    #Breed maken
    pivot_wider(names_from = jaar, values_from = percentage) %>%
    #lege trendlijnvariabele maken
    mutate(trendlijn = "") %>%
    kableExtra::kable(escape = F,
                      format = "html") %>%
    kable_minimal() %>%
    #Plaatje van de trendlijn toevoegen in kolom na jaarkolommen
    column_spec(length(jaren) + 1, image = spec_plot(list(trendwaarden), same_lim = T)) %>% 
    add_header_above(tabel_header) %>% 
    row_spec(1, extra_css = "white-space: nowrap;") #forceren dat ">= 95" geen linebreak heeft
  #Werkelij de popup samenstellen
  return(
    glue(
      "
      {titel_popup}
      {naam}
      {disclaimer}
      {melding}
      {trend}
      "
      
    )
    
  )

  
})


  #Kaartlaag toevoegen
  kaart %>%
  addPolygons(data        = sf,
              color       = "black", 
              fillColor   = sf$kleuren,
              opacity     = 0.5,
              weight      = 1,
              fillOpacity = 0.5,
              group       = groepnaam,
              popup       = labels,
              labelOptions = labelOptions(
                style = list(direction = "auto")),
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE))
  


}


