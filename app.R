library(bs4Dash)
library(ggplot2)
library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud2)
library(lubridate)
library(plotly)
library(shinycssloaders)
library(reactable)
library(htmltools)
library(waiter)
library(shiny)



appname <- Sys.getenv('APPNAME_TW')
key     <- Sys.getenv('KEY_TW')
secret  <- Sys.getenv('SECRET_TW')
token      = Sys.getenv('TOKEN_TW')
token_secret = Sys.getenv('TOKEN_SECRET_TW')

twitter_token <- create_token(
  app = appname,
  consumer_key= key,
  consumer_secret = secret,
  access_token= token,
  access_secret= token_secret,
  set_renv = TRUE)



## StopWords en Español --> Palabras vacias

sw = read.csv(
  "https://raw.githubusercontent.com/stopwords-iso/stopwords-es/master/stopwords-es.txt",
  stringsAsFactors = F, 
  header = TRUE, 
  col.names = c("word"),
  encoding = 'UTF-8') %>% 
  bind_rows(tibble(
    word = c(
      "https",
      "t.co",
      "rt",
      "amp",
      "rstats",
      "gt"
    )
  )
  )


## Datos de sentimientos en español

ss = read.csv(
  "https://raw.githubusercontent.com/JoseCardonaFigueroa/sentiment-analysis-spanish/master/data/emotions.csv",
  header = F,
  col.names = c("word", "sentiment"),
  stringsAsFactors = F, 
  encoding = 'UTF-8')




ss$sentiment[ss$sentiment == 'anger' ] = 'Enojo'
ss$sentiment[ss$sentiment == 'disgust' ] = 'Asco'
ss$sentiment[ss$sentiment == 'joy' ] = 'Alegria'
ss$sentiment[ss$sentiment == 'surprise' ] = 'Sorprendido'
ss$sentiment[ss$sentiment == 'fear' ] = 'Miedo'
ss$sentiment[ss$sentiment == 'sadness' ] = 'Tristeza'



new_words = function(input){
  words_final = c()
  vocales_acento = c("á", "é", "í", "ó", "ú")
  vocales_sin_acento = c("a", "e", "i", "o", "u")
  count = 0
  for (vocal in vocales_acento) {
    count = count + 1
    words_final[count] = gsub(vocales_sin_acento[count], vocal, input)
  }
  return(words_final)
}


# Crear la UI del dashboard

# UI ----------------------------------------------------------------------



ui <- dashboardPage(
  options = F,
  preloader = list(
    html = tagList(spin_6(), "Loading Twitter Dashboard..."), color = "#384bb5"),
  header = dashboardHeader(
    skin = "dark",
    fixed = T,
    title = dashboardBrand(
      title ="Twitter Words Dash",
      image = "https://assets.stickpng.com/thumbs/580b57fcd9996e24bc43c53e.png")
  ),
  # dashboardSidebar ----------------------------------------------------------------------
  sidebar = dashboardSidebar(
    background = "lightgray",
    background_dark = "darkgray",
    skin = "light",
    width = 500,
    sidebarMenu(
      menuItem(
        text = "Principal",
        tabName = "dashboard",
        startExpanded = T,
        icon = shiny::icon("dinosaur")
      ),
      selectInput(
        inputId = "input1",
        label = "Twitters",
        choices = c(100,500,1000,1500,2000,2500,3000,4000), 
        selected = 100),
      textInput(
        inputId = "input2",
        label = "Word"
      ),
      actionButton(
        inputId = "submit", 
        "Query", 
        status = "primary"),
      menuItem(
        text = "Word Cloud",
        tabName = "wordcloud",
        icon = shiny::icon("info-circle")),
      menuItem(
        text = "Acerca de",
        tabName = "about",
        icon = shiny::icon("info-circle"))
      )
  ),
  body = dashboardBody(
    tags$style(
      type = 'text/css', 
      '.rt-align-left {color: black!important; }',
      '.rt-align-center {color: black!important; }',
      '.ReactTable {color: black!important; }',
      '.sidebar .shiny-input-container {padding: 0px 21px 0px 16px!important; }',
      '.btn-primary {color: #fff!important; background-color: #5d6b7a!important; border-color: #007bff!important; box-shadow: none!important;}'
    ),
    tabItems(
      tabItem(
        # Sección Principal ----------------------------------------------------------------------
        tabName = "dashboard",
        h3("Twitter Words Dash"),
        p("Dashboard sobre frecuencia de palabras claves en ", 
          span("Twitter", style = "color: blue;"), " para Buenos Aires, Argentina."),
        fluidRow(
          bs4ValueBoxOutput("infobox_word"),
          bs4ValueBoxOutput("infobox_user"),
          bs4ValueBoxOutput("infobox_trend")
        ),
        
        fluidRow(
          column(
            width = 12,
            tabBox(
              width = 12,
              
              tabPanel(
                # UI freqplot ----------------------------------------------------------------------
                title = "Freq Plot",
                withSpinner(plotlyOutput("freqplot"),
                            type = 1)
              ),
              tabPanel(
                # UI table_output ----------------------------------------------------------------------
                title = "Tweets Table",
                withSpinner(
                  reactable::reactableOutput("table_output"),
                  type = 1
                )
              )
              
            )
          )
        )),
      # UI wordcloud ----------------------------------------------------------------------
      tabItem(
        tabName = "wordcloud",
        h1("WordCloud"),
        p("Word Cloud sobre frecuencia de palabras claves en ", 
          span("Twitter", style = "color: blue;"), " para Buenos Aires, Argentina."),
        fluidRow(
          column(
            width = 8,
            tabBox(
              width = 12,
              tabPanel(
                title = "Word Cloud",
                withSpinner(wordcloud2Output("wordcloud"),
                            type = 1)
              )
            )
          ),
          column(
            width = 4,
            tabBox(
              width = 12,
              tabPanel(
                title = "inputs",
                sliderInput(inputId = "sizewordcloud",
                            label = "Size",
                            min = 0.1, 
                            max = 0.5,
                            value = 0.5, 
                            step = 0.1,
                            width = '200px'),
                sliderInput(inputId = "rotatewordcloud",
                            label = "Rotation",
                            min = 0.1, 
                            max = 2,
                            value = 0.5, 
                            step = 0.1,
                            width = '200px'),
                sliderInput(inputId = "gridwordcloud",
                            label = "Grid",
                            min = 10, 
                            max = 30,
                            value = 20, 
                            step = 2,
                            width = '200px'),
                selectInput(inputId = "themewordcloud",
                            label = "Theme",
                            width = '200px',
                            choices = c("Random dark" = "random-dark", 
                                        "Random light" = "random-light"), 
                            selected = "Random dark")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "about",
        h1("Acerca de:"),
        fluidRow(
          box(
            title = strong("Dashboard sobre analisis de palabras en Twitter Argntina."), solidHeader = TRUE,
            p("El Dashboard contiene la siguientes funcionalidades para interactuar con la API de Twitter y obtener Insights sobre alguna/s palabra/s en los ultimos Twitters para ", strong("Argentina"), "."),
            p("Podemos ver las siguientes métricas:"),
            tags$ol(
              tags$li("Usuario que con mas frecuencia utilizo la palabra buscada en los ultimos Twitters."),
              tags$li("Que palabras acompañan la palabra buscada (Analisis de sentimiento)."),
              tags$li(" Grafico de barras para observar la frecuencia de las distintas palabras que acompañan a la consultada.")
            ),
            p("Los datos utilizados provienen de la API de Twitter:"),
            tags$ul(
              tags$li(tags$a(href="https://developer.twitter.com/es/developer-terms/policy", "Twitter Developers"))
            ),
            p("Packages:"),
            tags$ul(
              tags$li(tags$a(href="https://ggplot2.tidyverse.org/", "ggplot2")),
              tags$li(tags$a(href="https://rinterface.github.io/bs4Dash/index.html", "bs4Dash")),
              tags$li(tags$a(href="https://dplyr.tidyverse.org/", "dplyr")),
              tags$li(tags$a(href="https://cran.r-project.org/web/packages/htmltools/index.html", "htmltools")),
              tags$li(tags$a(href="https://plotly.com/r/", "plotly")),
              tags$li(tags$a(href="https://glin.github.io/reactable/", "reactable")),
              tags$li(tags$a(href="https://cran.r-project.org/web/packages/rtweet/index.html", "rtweet")),
              tags$li(tags$a(href="https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html", "tidytext")),
              tags$li(tags$a(href="https://cran.r-project.org/web/packages/textdata/index.html", "textdata"))
            )
            
          ),
          box(
            title = strong("Trabajo hecho por:"), solidHeader = TRUE,
            h5("Galoto Maximiliano"),
            tags$ul(
              tags$li(tags$a(href="https://www.linkedin.com/in/maximilianogaloto", "Linkedin")),
              tags$li(tags$a(href="https://github.com/MGaloto", "GitHub")))
            
          )
        )
        

      )
    )
  )
)

# SV ----------------------------------------------------------------------
server <- function(input, output,session) {

  
# reactiveValues----------------------------------------------------------------------
  
  rv <- reactiveValues()
  

  
  
  observeEvent(input$submit, {
    req(input$input1)
    req(input$input2)
    

    on.exit(
      removeNotification(
        showNotification(
          paste0("Waiting.."), 
          duration = as.numeric(input$input1) / 100, 
          closeButton = TRUE,
          type = "warning")
        )
      , add = TRUE)
    
# SV search_tweets ----------------------------------------------------------------------
    
    rv$tweet_words = search_tweets(
      input$input2, 
      n =  as.numeric(input$input1), 
      include_rts = T, 
      lang = "es",
      geocode = "-34.603722,-58.381592,2000km") %>% 
      mutate(
        created_at = ymd_hms(
          format(
            created_at, tz = "America/Argentina/Buenos_Aires")
        )
      ) 
    
    rv$ranking = rv$tweet_words %>%
      select(screen_name,
             text) %>%
      unnest_tokens(word, text) %>%
      filter(!grepl("^\\d+", word)) %>%
      mutate(word = chartr("áéíóúñÁÉÍÓÚÑ", "aeiounAEIOUN", word)) %>% 
      anti_join(
        sw %>% 
          bind_rows(tibble(word = input$input2)) %>% 
          bind_rows(tibble(word = new_words(input$input2))), by = "word") %>%
      group_by(word) %>%
      tally %>%
      arrange(desc(n)) %>% 
      head(50)
    
    rv$name_most_freq = rv$ranking[1,"word"]
    rv$number_most_freq = rv$ranking[1,"n"]
    
    rv$tw_table = rv$tweet_words %>% select(created_at,
                                            user_id,
                                            media_url,
                                            text,
                                            screen_name,
                                            status_id) %>% 
      mutate(Date = as.Date(rtweet:::format_date(created_at)),
             Time = paste0(format(created_at, "%H"),
                           ":",
                           format(created_at, "%M")),
             Url = paste0("https://twitter.com/", 
                          as.character(screen_name),
                          "/status/",
                          as.character(status_id)
             ),
             User = paste0("https://twitter.com/", 
                           as.character(screen_name)
             )
      ) %>% select(Date, Time,Url, User, text, screen_name)
    
    
    rv$ranking_user = rv$tw_table %>% 
      group_by(User, screen_name) %>% 
      count() %>% 
      arrange(desc(n)) %>% 
      select(User, screen_name) %>% 
      head(1)
    
    rv$trend = get_trends("buenos aires") %>% 
      arrange(desc(tweet_volume)) %>% 
      select(trend, tweet_volume) %>% 
      head(1)
    
    
  }, ignoreNULL = FALSE)
  
  
# SV wordcloud ----------------------------------------------------------------------
  output$wordcloud <- renderWordcloud2({
    req(rv$tweet_words)
    
    wordcloud2(rv$ranking,
               minSize = 10, 
               gridSize = as.numeric(input$gridwordcloud), 
               size = as.numeric(input$sizewordcloud), 
               rotateRatio =as.numeric(input$rotatewordcloud),
               fontFamily = "Arial",
               color=input$themewordcloud)
  })
# SV plotly ----------------------------------------------------------------------
  
  output$freqplot <- renderPlotly({
    req(rv$tweet_words)
    
    ggplotly(ggplot(rv$ranking[order(-rv$ranking$n),] %>%
                      head(15),
                    aes(x = reorder(word, n),
                        y = n,
                        text = map(paste('<b>Metricas:</b>','<br>',
                                         '<b>Word:</b>',word,'<br>',
                                         '<b>Freq:</b>',n),HTML))) +
               guides(fill = "none") +
               geom_bar(stat = "identity",
                        color = "black",
                        alpha = 0.9,
                        aes(fill = cut(n, breaks = 5)),
                        size = 0.5) +
               scale_fill_manual(
                                 values = RColorBrewer::brewer.pal(9, "Blues")[5:9]) +
               coord_flip() +
               theme_dark() +
               theme(panel.background = element_rect(fill = "white"),
                     axis.text.y = element_text(angle = 0, hjust = 0.5)) +
               labs(title = "Twitter Frequency Words",
                    x = "",
                    y = "Frequency"), tooltip = "text") %>%
      config(displayModeBar = F)
    
    
    
  })
  
# SV table_output ----------------------------------------------------------------------
  
  output$table_output = reactable::renderReactable({
    req(rv$tw_table)
    
    
    reactable::reactable(
      rv$tw_table,
      columns = list(
        Date = colDef(
          name = "Date",
          align = "center",
          minWidth = 60
        ),
        Time = colDef(
          name = "Time",
          align = "center",
          minWidth = 60
        ),
        screen_name = colDef(
          name = "User",
          cell = function(screen_name) {
            htmltools::tags$a(
              href = paste0(
                "https://twitter.com/", as.character(screen_name)
              ),
              target = "_blank", paste0("@",screen_name))
          },
          minWidth = 60
        ),
        text = colDef(
          name = "Tweet",
          align = "left",
          minWidth = 120
        ),
        Url = colDef(
          name = "Url Tweet",
          cell = function(Url) {
            htmltools::tags$a(
              href = as.character(Url),
              target = "_blank", as.character(Url))
          }
        ),
        User = colDef(
          name = "User Link",
          cell = function(User) {
            htmltools::tags$a(
              href = as.character(User),
              target = "_blank", as.character(User))
          }
        )
      ),
      searchable = TRUE,
      striped = TRUE,
      defaultPageSize = 8)
    
    
  })
  
  # value box ----------------------------------------------------------------------
  
  output$infobox_word <- renderbs4ValueBox({

    bs4ValueBox(
      value = paste0(if (is.null(rv$name_most_freq)) {"Insertar Word y Ejecutar Query"} else {paste0("Palabra: ",as.character(rv$name_most_freq))}),
      subtitle = paste0(if (is.null(rv$number_most_freq)) {""} else {paste0("Se encuentra ",as.character(rv$number_most_freq), " veces dentro de los ",as.character(input$input1)," Twitters.")}),
      icon = shiny::icon("table"),
      color = "primary",
      width = NULL,
      footer = div("Mas frecuente")
    )
  })
  
  
  output$infobox_user <- renderbs4ValueBox({

    bs4ValueBox(
      value = paste0(if (is.null(rv$ranking_user)) {"Insertar Word y Ejecutar Query"} else {paste0("User: ",as.character(rv$ranking_user[1,"screen_name"]))}),
      subtitle = paste0(if (is.null(rv$ranking_user)) {""} else {paste0("Usuario con mas Twitts que contienen la palabra ",as.character(rv$name_most_freq))}),
      icon = shiny::icon("user-circle"),
      color = "indigo",
      width = 3,
      gradient = T,
      footer = div("User")
    )
    
  })
    
    output$infobox_trend <- renderbs4ValueBox({

      bs4ValueBox(
        value = paste0(if (is.null(rv$trend)) {"Insertar Word y Ejecutar Query"} else {paste0("Trend en Argentina: ",as.character(rv$trend %>% pull(trend)))}),
        subtitle = paste0(if (is.null(rv$trend)) {""} else {paste0("Total de Twitts que contienen el Trend: ",as.character(rv$trend %>% pull(tweet_volume)))}),
        icon = shiny::icon("hourglass-half"),
        color = "fuchsia",
        width = NULL,
        footer = div("Trend Argentina")
      )
      
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)