#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#rsconnect::setAccountInfo(name='larsqviller',
#token='D1145CB7E92797F291018CEDC5CBFDD3',
#secret='H7d8s3dcSLDq649O0Znp2ZGAlaiDEqi4ykHVdjuV')

#rsconnect::deployApp('/home/lars/Dropbox/Vetinst_min_egen/shiny/smitteApp/kartapp/app.R')



## rm(list=ls(all=T))

## Setter stier
#sti <- '/home/lars/Dropbox/Vetinst_min_egen/shiny/smitteApp/kartapp/publish/'
#sti <- 'E:/Dropbox/Vetinst_min_egen/shiny/smitteApp/kartapp/data/'
#sti <- 'http://odin.vetinst.no/ta/pd/smittepress/publish/'

library('leaflet')
library('raster')
library('gdistance')
library('rgdal')
library('rgeos')
library('sp')
library('shiny')
#library('rsconnect')


# Funksjoner --------------------------------------------------------------
## Velger lokaliteter innenfor omkretsen som settes i kartet
velgelok <- function(omkr, lokvalg, kartvalg){
  omkrets   <- omkr* 1000
  aktiv.lok <- velgAktiv(kartvalg)[[1]]
  lok.valg  <- lok[lok$LOK_NR == lokvalg,]
  lok.valg.trans <- spTransform(lok.valg, CRS("+init=epsg:32633"))
  aktiv.lok.trans <- spTransform(aktiv.lok, CRS("+init=epsg:32633"))
  lok.omkrets <- gBuffer(lok.valg.trans,width=omkrets)
  lok.omkrets.trans <- spTransform(lok.omkrets, CRS("+init=epsg:4326"))
  over_lok <- as.logical(over(aktiv.lok.trans, lok.omkrets, returnList = T))
  over_lok[is.na(over_lok)] <- F
  lok.clip    <- aktiv.lok[over_lok,]
  data_ut <- list(lok.clip, lok.omkrets.trans,lok.omkrets)
  return(data_ut)
}

velgTilListe <- function(lokliste, valgtliste){
  redusertListe <- lokliste[lokliste$LOKALITETSNUMMER %in% valgtliste$LOK_NR, ]
  redusertListe.sort <- redusertListe[order(redusertListe$LOKALITETSNUMMER),] 
  return(redusertListe.sort)
}
## Velger ut rasterkartet som gjelder for uken
velgRaster <- function(valgtuke){
  smittekart <- subset(pressene, which(names(pressene) == valgtuke))
  return(smittekart)
}

velgAktiv <- function(valgtuke){
  ukenummer  <- substring(valgtuke, 7,nchar(valgtuke))
  aktiv_now <- aktive[which(substring(names(aktive),2,nchar(names(aktive))) == ukenummer)]
  aktiv.lok <-  lok[lok$LOK_NR %in% rownames(na.omit(aktiv_now)),]
  aktivdata <- list(aktiv.lok, aktiv_now)
  return(aktivdata)
}


redLeafIcon <- makeIcon(
  iconUrl = "map_marker_red.png",
  iconWidth = 26, iconHeight = 40,
  iconAnchorX = 13, iconAnchorY = 40
)

dataliste <- function(kartvalg){
  aktiv.lok <- velgAktiv(kartvalg)[[1]]
  aktiv.pos = which(names(aktive) %in% names(velgAktiv(kartvalg)[[2]]))
  
  Totale_sisteuke <-  data.frame("Totalt smittepress mobile" = log(Totalt[, aktiv.pos]+1), LOK_NR = as.numeric(substring(row.names(Totalt), 1,7)))
  #Totale_ad_sisteuke <-  data.frame("Totalt smittepress voksne" = log(Totalt.ad[, aktiv.pos]+1), LOK_NR = as.numeric(substring(row.names(Totalt.ad), 1,7)))
  
  tamed <- c("TILL_KOM", "FORMÅL", "PRODUKSJONSFORM", "LOK_NR", "LOK_NAVN", "N_GEOWGS84", "Ø_GEOWGS84")
  
  lokreg_redus <- lokreg[lokreg$LOK_NR %in% aktiv.lok$LOK_NR,]
  lokreg_redus <- lokreg_redus[!duplicated(lokreg_redus$LOK_NR),]
  lokreg_redus <- lokreg_redus[,names(lokreg_redus) %in% tamed]
  dim(lokreg_redus)
  
  lokreg_redus <- merge(lokreg_redus, Totale_sisteuke, by = "LOK_NR")
  #lokreg_redus <- merge(lokreg_redus, Totale_ad_sisteuke, by = "LOK_NR")
  #lokreg_redus <- merge(lokreg_redus, interne_sisteuke, by = "LOK_NR")
  dim(lokreg_redus)
  head(lokreg_redus)
  
  names(lokreg_redus)[c(1,2, 5)] <- c("LOKALITETSNUMMER", "KOMMUNE", "LOKALITETSNAVN")
  return(lokreg_redus)
}

lesekart <- function(mappe){
  filnavn <- dir(mappe)
  filnavn_uten_tiff <- substring(filnavn, 1, nchar(filnavn)-4)
  filer <- paste0(substring(filnavn_uten_tiff, 1,5), substring(filnavn_uten_tiff, 7,nchar(filnavn_uten_tiff)))
  pressListe <- lapply(filnavn, function(x) raster(paste0(mappe,x)))
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  unifyExtent <- function(x) {# x = pressliste
    extents <- lapply(x, extent)
    mode1   <- Mode(sapply(extents, function(x) x[1]))
    mode2   <- Mode(sapply(extents, function(x) x[2]))
    mode3   <- Mode(sapply(extents, function(x) x[3]))
    mode4   <- Mode(sapply(extents, function(x) x[4]))
    avvik   <- which(sapply(extents, function(x) x[1]) != mode1 | sapply(extents, function(x) x[2]) != mode2 |sapply(extents, function(x) x[3]) != mode3 |sapply(extents, function(x) x[3]) != mode3)
    avvik
    sekvens   <- c(1:length(x))
    utenAvvik <- sekvens[-avvik][1]
    if(length(avvik)>0) for(i in 1:length(avvik)) extent(x[[avvik[i]]]) <- extent(x[[utenAvvik]])
    return(x)
  }
  pressListe <- unifyExtent(pressListe)
  #names(pressListe) <- filer
  pressStack <- stack(pressListe)
  return(pressStack)
}

# Laster data -------------------------------------------------------------

## Laster Lokreg-shape
lok            <- readOGR('./lokreg_geo.shp', layer = "lokreg_geo")
aktive_alle    <- read.table('./Aktiv.txt', header=T, sep = '\t', dec = ',')
aktive_alle[row.names(aktive_alle) == '10173',] <- 1
aktive0        <- aktive_alle[,c(521: ncol(aktive_alle))]
aktive0[,ncol(aktive0)] <- aktive0[,ncol(aktive0)-1]
aktive         <- cbind(aktive0, XNextWeek1 = aktive0[,ncol(aktive0)-1], XNextWeek2 = aktive0[,ncol(aktive0)-1], XNextWeek3 = aktive0[,ncol(aktive0)-1])
#ncol(aktive)
#aktiv_now      <- row.names(aktive)[!is.na(aktive[,ncol(aktive)-3])]
week_now <- tail(names(aktive_alle),1)
#aktiv.lok      <-  lok[lok$LOK_NR %in% aktiv_now,]

#lokreg_redus <- read.table('./lokreg_redus.csv', sep = ';', dec =",")

mappe <- './rasters/'
pressene <- lesekart(mappe)

#names(pressene)[9] <- 'pressXNextWeek1'
#names(pressene)[10] <- 'pressXNextWeek2'
names(pressene)
#fargegrunnlag <- raster('./fargegrunnlag100.tif')
r1 <- raster(nrows=10, ncols=20)
r1 <- setValues(r1, c(0:199)/10)
fargegrunnlag <- r1
range(r1)


## Leser inn lusedata
#mobile <- read.table('./BevegligeLus.txt', header=T, sep = '\t', dec = ',')
lokreg <- read.table('./Locreg2016-10-17x.txt', header=T, sep = '\t', dec = ',', fileEncoding = "cp1252")
Totalt <- read.table('./MobileTotaltFra2012.txt', header=T, sep = '\t', dec = ',')
#Totalt.ad <- read.table('./HunnLusTotaltFra2012.txt', header=T, sep = '\t', dec = ',')
#internt <- read.table(paste0(sti, 'prodMobileDagFra2012.txt'), header=T, sep = '\t', dec = ',')


#dataliste(input$kartvalg)

## Henter ut siste smittepress og lusetall:


pal <- colorBin(c("#ffffff","#ffffff","#ffffcc","#ffffcc","#fed976","#fed976","#feb24c","#e31a1c", "#bd0026", "#800026"), values(fargegrunnlag), na.color = "transparent", bins=10)

ui <- fluidPage(
  #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
  
  tags$head(tags$script(src="script.js")),
  tags$head(
    tags$link(rel = "stylesheet", href = "typekit.css")),
  tags$head(tags$link(rel="stylesheet", 
                      type="text/css",
                      href="style.css")),  
  #headerPanel("Hello Shiny!"),
  htmlTemplate("./header.html",
               doc = includeMarkdown("Dokumentasjon.Rmd")
  ),
  # navbarPage("Veterinærinstituttets smittepress -Betaversjon", id="nav",
  #            tabPanel("Interaktivt kart",
  div(class="outer",
      tags$head(
        # Include Shiny's our custom CSS
        includeCSS("./styles.css"),
        includeScript("./gomap.js")
      ),
      leafletOutput("mymap", width = "100%", height="100%"),
      absolutePanel(id = "controls",
                    class = "panel panel-default",
                    fixed = TRUE,
                    draggable = TRUE,
                    #top = 135,
                    top = 175,
                    left = 10,
                    right = "auto",
                    bottom = "auto",
                    width = 330,
                    height = "auto",
                    
                    htmlTemplate("./formkart.html")
                    
      )
  )
)





server <- function(input, output, session) {
  
  mylok <- reactive({
    
    l <- input$lok
    if (input$sokeradio == "goto") {
      l <- input$lokS
    }
    return(l)
  })
  
  mykartvalg <- reactive({
    k <- paste0("press", week_now)
    if (input$velgradio == "1uke") {
      k <- "pressXNextWeek1"
    }
    if (input$velgradio == "2uker") {
      k <- "pressXNextWeek2"
    }
    if (input$velgradio == "hist") {
      week <- input$week
      year <- input$year
      k <- "0"
      if (!(input$week == "0")) {
        if(as.numeric(week) > 9)
          k <- paste("pressX", year, week, sep="")
        else {
          k <- paste("pressX", year, "0", week, sep="")
        }
      }
    }
    return(k)
  })
  
  
  output$mymap <- renderLeaflet({
		lineWeight <- 0
	  if (input$sokeradio == "find") {
	  	lineWeight <- 6
	  }

      
      leaflet({if(!("begr" %in% input$visvar)) data = velgelok(input$km_om, mylok(), mykartvalg())[[1]] else data = velgAktiv(mykartvalg())[[1]]}) %>% 
        addTiles()%>% 
        addMarkers(popup = ~as.character(LOK_NR)) %>%
        addMarkers(data = lok[lok$LOK_NR == mylok(),],popup = ~as.character(LOK_NR), icon = redLeafIcon) %>%
        addRasterImage({if(!(input$sokeradio == "smi")) crop(velgRaster(mykartvalg()), 
                                                             velgelok(input$km_om, 
                                                                      mylok(), mykartvalg())[[3]]) else velgRaster(mykartvalg())}, 
                       maxBytes=Inf, 
                       colors = pal, 
                       opacity = 0.65)  %>% 
        addLegend(pal = pal, values = values(fargegrunnlag), title = "Smittepress") %>% 
        addPolygons(data=velgelok(input$km_om, mylok(), mykartvalg())[[2]], weight = lineWeight, fillColor = "transparent") 
  })
  
  output$mytable <- renderTable({if(input$listevalg == "valgt") 
    velgTilListe(dataliste(mykartvalg()),velgelok(input$km_om, mylok(), mykartvalg())[[1]])
    else
      dataliste(mykartvalg())[order(dataliste(mykartvalg())$LOKALITETSNUMMER),]
    
  })
  
}

shinyApp(ui, server)