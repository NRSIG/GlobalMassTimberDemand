library("shiny")
library('shinyjs')
library("shinyhelper")
library("ggplot2")
library("scales")
library("tidyverse")
library("leaflet")
library("sf")
library("ggpattern")

# App startup steps ----

## csv data ----

# testing git...

# set up a database you will work with
dataa <- read.csv("atable.csv")
dataa <- dataa %>% filter(Year>=2025)
dataa <- dataa %>% 
  mutate(Country = case_when(
    Country == 'U.S.' ~ 'United States', 
    Country == 'UK' ~ 'United Kingdom',
    TRUE ~ Country
  )
)

cn <- levels(as.factor(dataa$Country))

yy <- seq(2025, 2060, 1)

dataa$Height <- factor(dataa$Height, levels = c( "13 & higher", "7 to 12",  "6 & lower"))
dataa$lev1 <- factor(dataa$lev1, levels = c("Conservative Estimate", "Optimistic Estimate", "Extreme Estimate")) 
sc <- levels(as.factor(dataa$lev1))

dat <- dataa %>% filter(!is.na(Total))
rm(dataa)

## mapping data ------
sfMap <- read_sf('countries_project_simplify.shp')
names(sfMap) <- sub("COUNTRY","Country", names(sfMap))
sfMap$Country <- as.factor(sfMap$Country)
sfMap <- sfMap %>% filter(Country %in% (dat %>% pull(Country)))

# UI definition ----
ui <- fluidPage(
  # include shiny js
  useShinyjs(),
  
  title = 'Mass Timber Demand Projections',
  
  ## style ----
  tags$head(tags$style(HTML(
    '
    #taoGlobalMassTimber thead{
      background-color: #ece9e3;
    }
    
    #taoGlobalMassTimber th{
      border-bottom: thin solid grey;
    }
    
    .modal-content{
      border-radius: 0;
    }
    
    .modal-header{
      background-color: #ece9e3;
      border-bottom: thin solid grey;
    }
    
    .modal-footer{
      background-color: #fff;
    }
    
    .trSelected {
      background-color: #ffebeb !important;
      border-top: 1.1px solid red;
      border-bottom: 1.1px solid red;
    }
    '
  ))),
  
  ## header ----
  tags$div(
    style = 'display: inline-block; padding: 5px; margin-left: -15px; width: calc(100% + 30px); background-color: #ece9e3; border-bottom: thin solid grey;',
    tagAppendAttributes(
      tags$h2('Mass Timber Demand Projections'),
      style = 'float: left; margin: 0; line-height: 75px;'
    ),
    tags$div(
      style = 'float: right; height: 75px;',
      tags$a(
        href="https://www.cintrafor.org/", target="_blank",
        tags$img(src='c4_logo.png',height='75',width='250')
      )
    )
  ),
  
  ## sidebar ----
  sidebarLayout(
    sidebarPanel(
      style = 'background: #f8f8f8; border: thin solid grey; border-radius: 0;',
      width = 3,
      
      ### sei geographic scale ----
      helper(
        selectInput('seiGeographicScale', 'Geographic Scale', choices = c('Global', 'National')),
        type = 'markdown', title = 'Geographic Scale', icon = 'circle-question',
        content = 'GeographicScale'
      ),
      
      ### sei projection scenario ----
      helper(
        selectInput('seiScenario', 'Projection Scenario', choices = sc),
        type = 'markdown', title = 'Projection Scenario', icon = 'circle-question',
        content = 'Scenario'
      ),
      
      ### sli map year ----
      helper(
        sliderInput('sliMapYear', 'Year', value = 2025, min = 2025, max = 2060, step = 1, ticks = FALSE, sep = ''),
        type = 'markdown', title = 'Year', icon = 'circle-question',
        content = 'MapYear'
      ),
      
      conditionalPanel(
        'input.seiGeographicScale == "Global"',
        
        ### cbi by region ----
        helper(
          checkboxInput('cbiByRegion', 'Subtotal by Region', value = FALSE),
          type = 'markdown', title = 'Subtotal By Region', icon = 'circle-question',
          content = 'SubtotalByRegion'
        )
      ),
      
      conditionalPanel(
        'input.seiGeographicScale == "National"',
        
        ### sei country ----
        helper(
          selectInput('seiCountry', 'Country', choices = cn),
          type = 'markdown', title = 'Country', icon = 'circle-question',
          content = 'Country'
        ),
        
        ### sli chart years ----
        #sliderInput('sliProjectionYears', 'Projection Years', min = min(as.numeric(yy)), max = max(as.numeric(yy)), value = c(2040,2060), step= 1),
      ),
      
      ### sei building type ----
      helper(
        selectInput('seiVolumeCategories', 'Subtotal by Volume Category', choices = c('None', 'Building Type', 'Building Height', 'Product Type')),
        type = 'markdown', title = 'Subtotal By Volume Category', icon = 'circle-question',
        content = 'SubtotalByVolumeCategory'
      ),
      
      ### abi download data ----
      actionButton('abiDownloadData', 'Download Data', icon = icon('download'))
    ),
    mainPanel(
      #style = 'border-left: thin solid grey;',
      width = 9,
      
      ## main panel global ----
      #conditionalPanel(
      #  'input.seiGeographicScale == "Global"',
        
        fluidRow(
          style = 'margin-bottom: 15px;',
          ### mapo global map ----
          column(
            width = 7,
            tags$div(
              style = 'border: thin solid grey; overflow: hidden;',
              leafletOutput('lfoMap', height = 400)
            ),
          ),
          column(
            width = 5,
            tags$div(
              id = 'teoMassTimberParentDiv',
              style = 'height: 402px; width: 100%; border: 5px solid grey; padding: 5px;',
              tags$div(
                style = 'height: 33%; width: 100%; font-weight: bold; font-size: 24px;',
                ### teo total adoption beginning ----
                textOutput('teoTotalAdoptionBeginning', inline = TRUE),
                ### teo total adoption type ----
                textOutput('teoTotalAdoptionType', inline = TRUE),
                ### teo total adoption region ----
                textOutput('teoTotalAdoptionRegion', inline = TRUE),
                ### teo total adoption year ----
                textOutput('teoTotalAdoptionYear', inline = TRUE)
              ),
              tagAppendAttributes(
                ### teo total adoption value ----
                textOutput('teoTotalAdoptionValue', inline = FALSE),
                style = 'height: 34%; width: 100%; font-weight: bold; font-size: 100px; text-align: center; vertical-align: middle;'
              ),
              tagAppendAttributes(
                ### teo total adoption units ----
                textOutput('teoTotalAdoptionUnits', inline = FALSE),
                style = 'height: 33%; width: 100%; font-weight: bold; font-size: 24px; display: flex; justify-content: flex-end; align-items: flex-end;'
              )
            )
          )
        ),
        
        fluidRow(
          style = 'margin-bottom: 15px;',
          column(
            width = 12,
            tags$div(
              style = 'border: thin solid grey; overflow: hidden;',
              ### plo global adoption over time ----
              plotOutput('ploGlobalAdoptionOverTime', hover = 'ploGlobalAdoptionOverTime_hover', height = 400)
            ),
          ),
        ),
        
        fluidRow(
          style = 'margin-bottom: 15px;',
          column(
            width = 6,
            tags$div(
              style = 'border: thin solid grey; overflow: hidden;',
              ### plo global adoption one year ----
              plotOutput('ploGlobalAdoptionOneYear', hover = 'ploGlobalAdoptionOneYear_hover', height = 300)
            )
          ),
          column(
            width = 6,
            tags$div(
              style = 'border: thin solid grey; margin-bottom: 15px; overflow: hidden;',
              ### plo global diffusion rate ----
              plotOutput('ploGlobalDiffusionRateByRegion', height = 300)
            ),
          )
        ),
        
        fluidRow(
          ### tao global mass timber table ----
          column(
            width = 12,
            tags$div(
              style = 'height: 500px; border: thin solid grey; overflow: scroll; margin-bottom: 15px;',
              tableOutput('taoGlobalMassTimber')
            )
          )
        )
      #)
    )
  )
)

# Server  ------
server <- function(input, output) {
  # shinyhelper function 
  observe_helpers()
  
  # variables ----
  
  # reactive values ----
  rvData <- reactiveValues(
    chartFontSize = 16,
    dfCountryDiffusionRates = data.frame(),
    dfGlobalDiffusionRates = data.frame(),
    dfGlobalDiffusionRatesByVolCategory = data.frame(),
    dfOneCountryOneYear = data.frame(),
    dfOneCountryAllYears = data.frame(),
    dfCountrySummaryOneYear = data.frame(),
    dfGlobalSummaryAllYears = data.frame(),
    dfGlobalSummaryOneYear = data.frame(),
    cGlobalRange = c(),
    interactiveRegionName = 'Global',
    interactiveCountryName = 'None',
    interactiveYear = 2025,
    interactiveVolumeCategory = 'None'
  )
  
  # events ----
  
  ## sli map year ----
  observeEvent(input$sliMapYear, {
    removeClass(class = 'trSelected', selector = 'tr')
    addClass(class = 'trSelected', selector = paste0('tr:contains(', input$sliMapYear, ')'))
  }, ignoreInit = FALSE)
  
  ## all controls ----
  observeEvent(c(input$seiGeographicScale, input$seiScenario, input$sliMapYear, input$seiCountry, input$cbiByRegion, input$seiVolumeCategories), {
    d <- dat
    
    # add region variable
    if(input$cbiByRegion){
      d <- d %>% mutate(RegionCategory = Region)
    } else {
      d <- d %>% mutate(RegionCategory = 'Global')
    }
    
    # add volume variable
    if(input$seiVolumeCategories == 'Building Type'){
      d <- d %>% rename(VolumeCategory = Type) %>% select(RegionCategory, Country, Year, lev1, VolumeCategory, Total, Rate)
      t <- 'Building Type'
    } else if(input$seiVolumeCategories == 'Building Height'){
      d <- d %>% rename(VolumeCategory = Height) %>% select(RegionCategory, Country, Year, lev1, VolumeCategory, Total, Rate)
      t <- 'Building Height'
    } else if(input$seiVolumeCategories == 'Product Type') {
      d <- d %>% select(RegionCategory, Country, Year, lev1, glt, clt, Rate) %>%
        pivot_longer(cols = c(glt, clt), names_to = 'VolumeCategory', values_to = 'Total')
      t <- 'Product Type'
    } else {
      d <- d %>% mutate(VolumeCategory = 'None') %>% select(RegionCategory, Country, Year, lev1, VolumeCategory, Total, Rate)
    }
    
    # filter
    d <- d %>% filter(lev1 %in% input$seiScenario)
    
    # summarize global adoption rate
    rtsG1 <- d %>%
      group_by(RegionCategory, VolumeCategory, Year) %>%
      summarize(Rate = mean(Rate, na.rm = TRUE, weights = Total) * 100, .groups = 'drop')
    
    # summarize global adoption rate
    rtsG2 <- d %>%
      group_by(RegionCategory, Year) %>%
      summarize(Rate = mean(Rate, na.rm = TRUE, weights = Total) * 100, .groups = 'drop')
    
    # summarize country adoption rate
    rtsC <- d %>%
      filter(Country == input$seiCountry) %>%
      group_by(Country, VolumeCategory, Year) %>%
      summarize(Rate = mean(Rate, na.rm = TRUE) * 100, .groups = 'drop')
    
    # summarize by nation
    n <- d %>%
      group_by(RegionCategory, VolumeCategory, Country, Year) %>%
      summarize(TotalVolCategoryYear = sum(Total, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(VolumeCategory)) %>%
      group_by(RegionCategory, Country, Year) %>%
      mutate(TotalCountry = sum(TotalVolCategoryYear)) %>%
      mutate(CumTotalYearCountry = cumsum(TotalVolCategoryYear)) %>%
      ungroup() %>%
      group_by(RegionCategory, Year) %>%
      mutate(TotalRegion = sum(TotalVolCategoryYear)) %>%
      group_by(Country) %>%
      mutate(TotalCountryMax = max(TotalCountry)) %>%
      ungroup() %>%
      mutate(Prop = TotalCountry / TotalRegion) %>%
      mutate(Prop = .1 + .8 * (Prop - min(Prop)) / (max(Prop) - min(Prop))) %>%
      ungroup() %>%
      mutate(PropCountry = TotalCountry / max(TotalCountry)) %>%
      mutate(PropCountry = .1 + .8 * (PropCountry - min(PropCountry)) / (max(PropCountry) - min(PropCountry)))
    
    # range for national scale
    if(input$seiGeographicScale == 'National'){
      r <- n %>% pull(TotalCountry) %>% range()
    }
    
    # get one country for all years
    nay <- n %>% filter(Country == input$seiCountry)
    
    # filter national dataset for one year
    n <- n %>% filter(Year == input$sliMapYear)
    
    # summarize by region and volume category
    d <- d %>% group_by(RegionCategory, VolumeCategory, Year) %>% 
      summarize(TotalYearRegionVolCategory = sum(Total, na.rm = TRUE), .groups = 'drop') %>%
      group_by(RegionCategory, Year) %>%
      mutate(TotalYearRegion = sum(TotalYearRegionVolCategory)) %>%
      arrange(desc(VolumeCategory)) %>%
      mutate(CumTotalYearRegion = cumsum(TotalYearRegionVolCategory)) %>%
      ungroup()
    
    # get range for scales
    if(input$seiGeographicScale == 'Global'){
      r <- d %>% pull(TotalYearRegion) %>% range()
    }
    
    # expand range to avoid map and chart errors
    r[1] <- r[1] - 1
    r[2] <- r[2] + 1
    
    # update reactive values
    rvData$legendState <- TRUE # TRUE/FALSE
    rvData$dfCountryDiffusionRates <- rtsC
    rvData$dfGlobalDiffusionRates <- rtsG2
    rvData$dfGlobalDiffusionRatesByVolCategory <- rtsG1
    rvData$dfOneCountryOneYear <- nay %>% filter(Year == input$sliMapYear)
    rvData$dfOneCountryAllYears <- nay
    rvData$dfCountrySummaryOneYear <- n
    rvData$dfGlobalSummaryAllYears <- d
    rvData$dfGlobalSummaryOneYear <- d %>% filter(Year == input$sliMapYear)
    rvData$cGlobalRange <- r
    rvData$volumeCategoryLabel <- t
    rvData$interactiveYear <- input$sliMapYear
  })
  
  ## leaflet show/hide legend easy button event ----
  observeEvent(input$easyButtonShowHideLegend, {
    rvData$legendState <- as.logical(input$easyButtonShowHideLegend)
  })
  
  ## leaflet mouse events ----
  observeEvent(input$lfoMap_shape_mouseover, {
    rvData$interactiveCountryName <- input$lfoMap_shape_mouseover$id
  })
  observeEvent(input$lfoMap_shape_mouseout, {
    rvData$interactiveCountryName <- 'None'
  })
  
  ## leaflet country ----
  observeEvent(c(input$seiGeographicScale, input$seiCountry), {
    if(input$seiGeographicScale == 'Global'){
      cBBox <- sfMap %>% st_bbox()
    } else {
      cBBox <- sfMap %>%
        filter(Country == input$seiCountry) %>%
        st_bbox()
    }
    
    leafletProxy('lfoMap') %>%
      flyToBounds(
        lng1 = cBBox$xmin[[1]], lat1 = cBBox$ymin[[1]], 
        lng2 = cBBox$xmax[[1]], lat2 = cBBox$ymax[[1]]
      )
  })
  
  ## plo global adoption over time mouse over event ----
  observeEvent(input$ploGlobalAdoptionOverTime_hover, {
    if(is.null(input$ploGlobalAdoptionOverTime_hover)){
      rvData$interactiveRegionName <- 'Global'
      rvData$interactiveYear <- input$sliMapYear
      rvData$interactiveVolumeCategory <- 'None'
    } else {
      rvData$interactiveRegionName <- ifelse(
        is.null(input$ploGlobalAdoptionOverTime_hover$panelvar1), 
        'Global', 
        input$ploGlobalAdoptionOverTime_hover$panelvar1
      )
      
      if(input$ploGlobalAdoptionOverTime_hover$x < 2025){
        rvData$interactiveYear <- 2025
      } else if(input$ploGlobalAdoptionOverTime_hover$x > 2060){
        rvData$interactiveYear <- 2060
      } else {
        rvData$interactiveYear <- input$ploGlobalAdoptionOverTime_hover$x %>% round()
      }
      
      if(input$seiVolumeCategories == 'None' | input$ploGlobalAdoptionOverTime_hover$x < 2025 | input$ploGlobalAdoptionOverTime_hover$x %>% floor() > 2060){
        rvData$interactiveVolumeCategory <- 'None'
      } else {
        if(input$seiGeographicScale == 'Global'){
          d <- rvData$dfGlobalSummaryAllYears %>% 
            filter(Year == rvData$interactiveYear, RegionCategory == rvData$interactiveRegionName, input$ploGlobalAdoptionOverTime_hover$y < CumTotalYearRegion)
          if(nrow(d == 1)) {
            rvData$interactiveVolumeCategory <- d %>% slice(1) %>% pull(VolumeCategory)
          } else {
            rvData$interactiveVolumeCategory <- 'None'
          }
        } else {
          d <- rvData$dfOneCountryAllYears %>% 
            filter(Year == rvData$interactiveYear, Country == input$seiCountry, input$ploGlobalAdoptionOverTime_hover$y < CumTotalYearCountry)
          if(nrow(d == 1)) {
            rvData$interactiveVolumeCategory <- d %>% slice(1) %>% pull(VolumeCategory)
          } else {
            rvData$interactiveVolumeCategory <- 'None'
          }
        }
        
      }
    }
  }, ignoreNULL = FALSE)
  
  ## plo global adoption one year mouse over event ----
  observeEvent(input$ploGlobalAdoptionOneYear_hover, {
    if(is.null(input$ploGlobalAdoptionOneYear_hover)){
      rvData$interactiveRegionName <- 'Global'
      rvData$interactiveVolumeCategory <- 'None'
    } else if(input$seiGeographicScale == 'Global') {
      cRegionNames <- input$ploGlobalAdoptionOneYear_hover$domain$discrete_limits$y
      y <- input$ploGlobalAdoptionOneYear_hover$y %>% round()
      
      if(y > 0 & y <= length(cRegionNames)){
        rvData$interactiveRegionName <- cRegionNames[[y]]
      } else {
        rvData$interactiveRegionName <- 'Global'
      }
      
      if(input$seiVolumeCategories != 'None'){
        d <- rvData$dfGlobalSummaryOneYear %>%
          filter(RegionCategory == rvData$interactiveRegionName, input$ploGlobalAdoptionOneYear_hover$x < CumTotalYearRegion)
        if(nrow(d == 1)) {
          rvData$interactiveVolumeCategory <- d %>% slice(1) %>% pull(VolumeCategory)
        } else {
          rvData$interactiveVolumeCategory <- 'None'
        }
      }
    } else {
      rvData$interactiveRegionName <- 'Global'
      
      if(input$seiVolumeCategories != 'None'){
        d <- rvData$dfOneCountryOneYear %>%
          filter(input$ploGlobalAdoptionOneYear_hover$x < CumTotalYearCountry)
        if(nrow(d == 1)) {
          rvData$interactiveVolumeCategory <- d %>% slice(1) %>% pull(VolumeCategory)
        } else {
          rvData$interactiveVolumeCategory <- 'None'
        }
      }
    }
  }, ignoreNULL = FALSE)
  
  # outputs ----
  
  ## map output ----
  output$lfoMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$Esri.WorldGrayCanvas,
        options = providerTileOptions(noWrap = FALSE)
      ) %>%
      addEasyButton(easyButton(
        id = 'ebiShowHideLegend',
        states = list(
          easyButtonState(
            stateName = 'show-legend',
            icon = icon('eye'),
            title = 'Show Legend',
            onClick = JS(
            "
            function(btn, map) {
                Shiny.onInputChange('easyButtonShowHideLegend', 'TRUE');
                btn.state('hide-legend');
            }
            ")
          ),
          easyButtonState(
            stateName = 'hide-legend',
            icon = icon('eye-slash'),
            title = 'Hide Legend',
            onClick = JS(
              "
            function(btn, map) {
                Shiny.onInputChange('easyButtonShowHideLegend', 'FALSE');
                btn.state('show-legend');
            }
            ")
          )
        )
      )
    )
  })
  
  ## update map
  observe({
    palFill <- colorNumeric('viridis', domain = rvData$cGlobalRange)
    
    sfMap <- sfMap %>% inner_join(rvData$dfCountrySummaryOneYear, by = 'Country')
    
    if(input$seiGeographicScale == 'Global'){
      m <- leafletProxy('lfoMap', data = sfMap) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          stroke = TRUE, color = ~palFill(TotalRegion), weight = 1, opacity = 1,
          fill = TRUE, fillColor = ~palFill(TotalRegion), fillOpacity = ~Prop,
          layerId = ~Country
        ) %>%
        addControl(
          tags$div(
            input$sliMapYear,
            style = 'font-weight: bold;'
          ), 
          position = 'topright'
        )
      
      if(rvData$legendState){
        m <- m %>%
          addLegend(
            position = 'bottomright', 
            pal = palFill, opacity = 1,
            values = seq(rvData$cGlobalRange[1], rvData$cGlobalRange[2]),
            title = 'Cubic Meters<br>(x 1000)'
          )
      }
    } else {
      m <- leafletProxy('lfoMap', data = sfMap) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          stroke = TRUE, color = ~palFill(TotalCountry), weight = 1, opacity = 1,
          fill = TRUE, fillColor = ~palFill(TotalCountry), fillOpacity = .5,
          layerId = ~Country
        ) %>%
        addControl(
          tags$div(
            input$sliMapYear,
            style = 'font-weight: bold;'
          ), 
          position = 'topright'
        )
      
      if(rvData$legendState){
        m <- m %>%
          addLegend(
            position = 'bottomright', 
            pal = palFill, opacity = 1,
            values = seq(rvData$cGlobalRange[1], rvData$cGlobalRange[2]),
            title = 'Cubic Meters<br>(x 1000)'
          )
      }
    }
  })
  
  ## teo total adoption beginning ----
  output$teoTotalAdoptionBeginning <- renderText({
    if(rvData$interactiveCountryName != 'None' | input$seiGeographicScale == 'National'){
      'Mass timber adoption'
    } else {
      'Global mass timber adoption'
    }
  })
  
  ## teo total adoption type ----
  output$teoTotalAdoptionType <- renderText({
    if(rvData$interactiveVolumeCategory == 'None'){
      ''
    } else {
      paste0('(', rvData$interactiveVolumeCategory, ')')
    }
  })
  
  ## teo total adoption region ----
  output$teoTotalAdoptionRegion <- renderText({
    if(rvData$interactiveCountryName == 'United States'){
      'in the United States'
    } else if(rvData$interactiveCountryName != 'None'){
      paste('in', rvData$interactiveCountryName)
    } else if(rvData$interactiveRegionName == 'United States'){
      'in the United States'
    } else if(rvData$interactiveRegionName %in% c('China', 'Latin America', 'West Europe')){
      paste('in', rvData$interactiveRegionName)
    } else if(input$seiGeographicScale == 'National' & input$seiCountry == 'United States'){
      'in the United States'
    } else if(input$seiGeographicScale == 'National'){
      paste('in', input$seiCountry)
    }
    else {
      ''
    }
  })
  
  ## teo total adoption year  ----
  output$teoTotalAdoptionYear <- renderText({paste0(' in ', rvData$interactiveYear, ':')})
  
  ## teo total adoption value ----
  output$teoTotalAdoptionValue <- renderText({
    if(rvData$interactiveCountryName != 'None' & input$seiGeographicScale =='Global'){
      d <- rvData$dfCountrySummaryOneYear %>% 
        filter(Country == rvData$interactiveCountryName) %>%
        rename(TotalYearRegionVolCategory = TotalVolCategoryYear)
      h <- colorNumeric('viridis', domain = rvData$cGlobalRange)(d$TotalRegion[1])
    } else if(rvData$interactiveCountryName != 'None' & input$seiGeographicScale =='National'){
      d <- dat %>% 
        filter(lev1 == input$seiScenario, Year == rvData$interactiveYear, Country == rvData$interactiveCountryName) %>%
        rename(TotalYearRegionVolCategory = Total)
      h <- colorNumeric('viridis', domain = rvData$cGlobalRange)(d %>% pull(TotalYearRegionVolCategory) %>% sum())
    } else if(input$seiGeographicScale == 'National') {
      d <- rvData$dfOneCountryAllYears %>% 
        filter(Year == rvData$interactiveYear, Country ==input$seiCountry) %>%
        rename(TotalYearRegionVolCategory = TotalVolCategoryYear)
      h <- colorNumeric('viridis', domain = rvData$cGlobalRange)(d$TotalCountry[1])
    } else if(rvData$interactiveRegionName %in% c('China', 'United States', 'Latin America', 'West Europe')) {
      d <- rvData$dfGlobalSummaryAllYears %>% 
        filter(Year == rvData$interactiveYear, RegionCategory == rvData$interactiveRegionName)
      h <- colorNumeric('viridis', domain = rvData$cGlobalRange)(d$TotalYearRegion[1])
    } else {
      d <- rvData$dfGlobalSummaryAllYears %>% 
        filter(Year == rvData$interactiveYear)
      r <- rvData$dfGlobalSummaryAllYears %>% group_by(Year) %>% summarize(Total = sum(TotalYearRegion), .groups = 'drop') %>% pull(Total) %>% range()
      h <- colorNumeric('viridis', domain = r)(d %>% pull(TotalYearRegion) %>% sum())
    }
    
    if(rvData$interactiveVolumeCategory != 'None'){
      d <- d %>% filter(VolumeCategory == rvData$interactiveVolumeCategory)
    }
    
    s <- d %>% pull(TotalYearRegionVolCategory) %>% sum()
    
    runjs(paste0('$("#teoMassTimberParentDiv").css("border-color", "', h, '");'))
    runjs(paste0('$("#teoMassTimberParentDiv").css("background-color", "', h, '33");'))
    
    s <- s %>% round() %>% format(big.mark = ',')
    if(nchar(s) <= 4){
      runjs("$('#teoTotalAdoptionValue').css('font-size', '100px');")
    } else if(nchar(s) == 5) {
      runjs("$('#teoTotalAdoptionValue').css('font-size', '80px');")
    } else {
      runjs("$('#teoTotalAdoptionValue').css('font-size', '70px');")
    }
    
    s
  })
  
  ## teo total adoption units ----
  output$teoTotalAdoptionUnits <- renderText({
    'thousand cubic meters'
  })
  
  ## plo global adoption total over time ----
  output$ploGlobalAdoptionOverTime <- renderPlot({
    if(input$seiGeographicScale == 'Global'){
      if(input$seiVolumeCategories == 'None'){
        g <- rvData$dfGlobalSummaryAllYears %>%
          ggplot(aes(x = Year, y = TotalYearRegion, fill = TotalYearRegion)) +
          geom_col(width = 1) +
          geom_vline(xintercept = input$sliMapYear, col = 'red') +
          scale_fill_viridis_c() +
          ggtitle('Mass Timber Adoption Over Time') +
          ylab('cubic meters (x 1000)') +
          scale_y_continuous(label = comma) +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot', legend.position = 'none')
      } else {
        g <- rvData$dfGlobalSummaryAllYears %>%
          ggplot(aes(x = Year, y = TotalYearRegionVolCategory, fill = TotalYearRegion, alpha = VolumeCategory)) +
          geom_col(fill = 'white', alpha = 1, width = 1) +
          geom_col(width = 1) +
          geom_vline(xintercept = input$sliMapYear, col = 'red') +
          scale_fill_viridis_c() +
          scale_alpha_discrete(range = c(.5, 1)) +
          scale_y_continuous(label = comma, limits = c(0, rvData$cGlobalRange %>% max())) +
          ggtitle('Mass Timber Adoption Over Time') +
          ylab('cubic meters (x 1000)') +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot', legend.position = 'top') +
          guides(fill = 'none', alpha = guide_legend(title=rvData$volumeCategoryLabel))
      }
      
      if(input$cbiByRegion){
        g <- g + facet_grid(cols = vars(RegionCategory))
      }
    } else {
      if(input$seiVolumeCategories == 'None'){
        g <- rvData$dfOneCountryAllYears %>%
          ggplot(aes(x = Year, y = TotalCountry, fill = PropCountry)) +
          geom_col(width = 1) +
          geom_vline(xintercept = input$sliMapYear, col = 'red') +
          scale_fill_viridis_c(limits = c(.1, .9)) +
          #ylim(0, rvData$cGlobalRange %>% max()) +
          ggtitle('Mass Timber Adoption Over Time') +
          ylab('cubic meters (x 1000)') +
          scale_y_continuous(label = comma) +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot', legend.position = 'none')
      } else {
        g <- rvData$dfOneCountryAllYears %>%
          ggplot(aes(x = Year, y = TotalVolCategoryYear, fill = PropCountry, alpha = VolumeCategory)) +
          geom_col(fill = 'white', alpha = 1, width = 1) +
          geom_col(width = 1) +
          geom_vline(xintercept = input$sliMapYear, col = 'red') +
          scale_fill_viridis_c(limits = c(.1, .9)) +
          scale_alpha_discrete(range = c(.5, 1)) +
          #ylim(0, rvData$cGlobalRange %>% max()) +
          ggtitle('Mass Timber Adoption Over Time') +
          ylab('cubic meters (x 1000)') +
          scale_y_continuous(label = comma) +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot', legend.position = 'top') +
          guides(fill = 'none', alpha = guide_legend(title=rvData$volumeCategoryLabel))
      }
    }
    
    g
  })
  
  ## plo global adoption total one year ----
  output$ploGlobalAdoptionOneYear <- renderPlot({
    if(input$seiGeographicScale == 'Global'){
      if(input$seiVolumeCategories == 'None'){
        rvData$dfGlobalSummaryOneYear %>%
          ggplot(aes(x = TotalYearRegion, y = RegionCategory, fill = TotalYearRegion)) +
          geom_col() +
          scale_fill_viridis_c(limits = rvData$cGlobalRange) +
          scale_x_continuous(label = comma, limits = c(0, rvData$cGlobalRange %>% max())) +
          ggtitle(paste('Adoption in', input$sliMapYear)) +
          xlab('cubic meters (x 1000)') +
          ylab(NULL) +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot', legend.position = 'none')
      } else {
        rvData$dfGlobalSummaryOneYear %>%
          ggplot(aes(x = TotalYearRegionVolCategory, y = RegionCategory, fill = TotalYearRegion, alpha = VolumeCategory)) +
          geom_col(fill = 'white', alpha = 1) +
          geom_col(col = NA) +
          scale_fill_viridis_c(limits = rvData$cGlobalRange) +
          scale_alpha_discrete(range = c(.5, 1)) +
          theme_bw(base_size = rvData$chartFontSize) +
          scale_x_continuous(label = comma, limits = c(0, rvData$cGlobalRange %>% max())) +
          ggtitle(paste('Adoption in', input$sliMapYear)) +
          xlab('cubic meters (x 1000)') +
          ylab(NULL) +
          theme(plot.title.position = 'plot', legend.position = 'top') +
          guides(fill = 'none', alpha = guide_legend(title=NULL))
      }
    } else {
      if(input$seiVolumeCategories == 'None'){
        rvData$dfOneCountryOneYear %>%
          filter(Country == input$seiCountry) %>%
          ggplot(aes(x = TotalVolCategoryYear, y = RegionCategory, fill = PropCountry)) +
          geom_col() +
          scale_fill_viridis_c(limits = c(.1, .9)) +
          scale_x_continuous(label = comma, limits = c(0, rvData$dfOneCountryOneYear %>% pull(TotalCountryMax) %>% unique())) +
          ggtitle(paste('Adoption in', input$sliMapYear)) +
          xlab('cubic meters (x 1000)') +
          ylab(NULL) +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot', legend.position = 'none')
      } else {
        rvData$dfOneCountryOneYear %>%
          filter(Country == input$seiCountry) %>%
          ggplot(aes(x = TotalVolCategoryYear, y = RegionCategory, fill = PropCountry, alpha = VolumeCategory)) +
          geom_col(fill = 'white', alpha = 1) +
          geom_col(col = NA) +
          scale_fill_viridis_c(limits = c(.1, .9)) +
          scale_alpha_discrete(range = c(.5, 1)) +
          theme_bw(base_size = rvData$chartFontSize) +
          scale_x_continuous(label = comma, limits = c(0, rvData$dfOneCountryOneYear %>% pull(TotalCountryMax) %>% unique())) +
          ggtitle(paste('Adoption in', input$sliMapYear)) +
          xlab('cubic meters (x 1000)') +
          ylab(NULL) +
          theme(plot.title.position = 'plot', legend.position = 'top') +
          guides(fill = 'none', alpha = guide_legend(title=NULL))
      }
    }
  })
  
  ## plo annual mass timber volume by region ----
  output$ploGlobalVolumeByRegion <- renderPlot({
    if(input$seiVolumeCategories == 'Building Type'){
      d <- dat %>% rename(Category = Type) %>% select(Region, Year, lev1, Category, Total)
    } else if(input$seiVolumeCategories == 'Building Height'){
      d <- dat %>% rename(Category = Height) %>% select(Region, Year, lev1, Category, Total)
    } else {
      d <- dat %>% select(Region, Year, lev1, glt, clt) %>%
        pivot_longer(cols = c(glt, clt), names_to = 'Category', values_to = 'Total')
    }
    d %>%
      dplyr::filter((Year %in% input$sliMapYear) & (lev1 %in% input$seiScenario) ) %>%
      group_by(Region, Category) %>% 
      summarize(Total = sum(Total, na.rm = TRUE), .groups = 'drop') %>%
      ggplot(aes(x = Region, y = Total, fill = Category)) +
      geom_col() +
      theme(plot.title.position = 'plot') +
      theme_bw(base_size = rvData$chartFontSize)
  })
  
  ## plo diffusion rate over time ----
  output$ploGlobalDiffusionRateByRegion <- renderPlot({
    if(input$seiGeographicScale == 'Global'){
      if(input$cbiByRegion){
        rvData$dfGlobalDiffusionRates %>%
          ggplot(aes(x = Year, y = Rate, color = RegionCategory)) +
          geom_line(size = 2) +
          geom_vline(xintercept = input$sliMapYear, col = 'red') +
          scale_color_viridis_d() +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot', legend.position = 'top') +
          ggtitle('Adoption Rate') +
          ylab('Rate (%)') +
          guides(color = guide_legend(title=NULL, nrow = 1))
      } else {
        rvData$dfGlobalDiffusionRates %>%
          ggplot(aes(x = Year, y = Rate, color = RegionCategory)) +
          geom_line(size = 2) +
          geom_vline(xintercept = input$sliMapYear, col = 'red') +
          scale_color_viridis_d() +
          ggtitle('Adoption Rate') +
          ylab('Rate (%)') +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot') +
          guides(color = 'none')
      }
    } else {
      if(input$seiVolumeCategories == 'None'){
        rvData$dfCountryDiffusionRates %>%
          ggplot(aes(x = Year, y = Rate, color = VolumeCategory)) +
          geom_line(size = 2) +
          geom_vline(xintercept = input$sliMapYear, col = 'red') +
          scale_color_viridis_d() +
          ggtitle('Adoption Rate') +
          ylab('Rate (%)') +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot') +
          guides(color = 'none')
      } else {
        rvData$dfCountryDiffusionRates %>%
          ggplot(aes(x = Year, y = Rate, color = VolumeCategory)) +
          geom_line(size = 2) +
          geom_vline(xintercept = input$sliMapYear, col = 'red') +
          scale_color_viridis_d() +
          ggtitle('Adoption Rate') +
          ylab('Rate (%)') +
          theme_bw(base_size = rvData$chartFontSize) +
          theme(plot.title.position = 'plot', legend.position = 'top') +
          guides(color = guide_legend(title = NULL, nrow = 1))
      }
    }
  })
  
  ## tao global mass timber ----
  output$taoGlobalMassTimber <- renderTable({
    if(input$seiGeographicScale == 'Global'){
      rvData$dfGlobalSummaryAllYears %>%
        mutate(VolumeCategory = as.character(VolumeCategory)) %>%
        inner_join(rvData$dfGlobalDiffusionRatesByVolCategory, by = c('RegionCategory', 'VolumeCategory', 'Year')) %>%
        rename(
          Region = RegionCategory,
          Category = VolumeCategory,
          Volume = TotalYearRegionVolCategory
        ) %>%
        mutate(Category = case_when(Category == 'None' ~ 'All', TRUE ~ Category)) %>%
        select(Region, Category, Year, Volume, Rate)
    } else {
      rvData$dfOneCountryAllYears %>%
        mutate(VolumeCategory = as.character(VolumeCategory)) %>%
        inner_join(rvData$dfCountryDiffusionRates, by = c('Country', 'VolumeCategory', 'Year')) %>%
        rename(
          Category = VolumeCategory,
          Volume = TotalVolCategoryYear
        ) %>%
        mutate(Category = case_when(Category == 'None' ~ 'All', TRUE ~ Category)) %>%
        select(Country, Category, Year, Volume, Rate)
    }
  }, width = '100%', striped = TRUE)
  
  ## abi download data modal ----
  observeEvent(input$abiDownloadData, {
    showModal(modalDialog(
      title = 'Download Data',
      size = 's',
      easyClose = TRUE,
      
      ### sei download data ----
      helper(
        selectInput('seiDownloadData', 'Dataset', choices = c('All', 'Filtered')),
        type = 'markdown', title = 'Download Data', icon = 'circle-question',
        content = 'DownloadData'
      ),
      
      ### dbi download data ui ----
      downloadButton('dbiDownloadData', 'Download Data')
    ))
  }, 
  ignoreInit = TRUE)
  
  ## dbi download data ----
  output$dbiDownloadData <- downloadHandler(
    filename = function(){
      if(input$seiDownloadData == 'Filtered' & input$seiGeographicScale == 'Global'){
        fileName <- 'mass_timber_adoption_global.csv'
      } else if(input$seiDownloadData == 'Filtered' & input$seiGeographicScale == 'National') {
        countryName <- str_replace_all(input$seiCountry, ' ', '_')
        fileName <- paste0('mass_timber_adoption_', countryName,'.csv')
      } else {
        fileName <- 'mass_timber_adoption_all.csv'
      }
    },
    content = function(file) {
      if(input$seiDownloadData == 'Filtered' & input$seiGeographicScale == 'Global'){
        d <- rvData$dfGlobalSummaryAllYears %>%
          mutate(VolumeCategory = as.character(VolumeCategory)) %>%
          inner_join(rvData$dfGlobalDiffusionRatesByVolCategory, by = c('RegionCategory', 'VolumeCategory', 'Year')) %>%
          rename(
            Region = RegionCategory,
            Category = VolumeCategory,
            Volume = TotalYearRegionVolCategory
          ) %>%
          mutate(Category = case_when(Category == 'None' ~ 'All', TRUE ~ Category)) %>%
          select(Region, Category, Year, Volume, Rate)
      } else if(input$seiDownloadData == 'Filtered' & input$seiGeographicScale == 'National') {
        d <- rvData$dfOneCountryAllYears %>%
          mutate(VolumeCategory = as.character(VolumeCategory)) %>%
          inner_join(rvData$dfCountryDiffusionRates, by = c('Country', 'VolumeCategory', 'Year')) %>%
          rename(
            Category = VolumeCategory,
            Volume = TotalVolCategoryYear
          ) %>%
          mutate(Category = case_when(Category == 'None' ~ 'All', TRUE ~ Category)) %>%
          select(Country, Category, Year, Volume, Rate)
      } else if(input$seiDownloadData == 'All'){
        d <- dat %>%
          select(Country, Region, Year, Type, Height, lev1, glt, clt, Total) %>%
          rename(
            Category = Type,
            Scenario = lev1,
            GlueLamTimber = glt,
            CrossLamTimber = clt
          )
      }
      write.csv(d, file, row.names = FALSE)
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
