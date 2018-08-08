
library(DT)
library(BBmisc)
library(plyr)
library(dplyr)
library(magrittr)
library(stringi)
library(rsconnect)
library(httr)
httr::config(ssl_verifypeer = FALSE)
library(shiny)
library(shinyjs)
library(shinythemes)
library(rstan)
library(parallel)
library(shinythemes)
en_US <- suppressAll(readRDS('data/en_US.rds'))

options(mc.cores = parallel::detectCores())
options(rpubs.upload.method = "internal")
options(download.file.method = "wininet")
options(mc.cores = parallel::detectCores())
Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")
rstan_options(auto_write = TRUE)
require(httr)
httr::config(ssl_verifypeer = FALSE)
options(download.file.method = "wininet")
utils::packageDescription("rsconnect")
gc()
utils::packageDescription("rsconnect")
# memory.limit()
# memory.limit(size=10000000)
Sys.setenv(rsconnect.max.bundle.size=100000000000000000)

httr::config(ssl_verifypeer = FALSE)
options(curl_options=list(dns_cache_timeout=10000000000, timeout_ms=10000000000000000, connecttimeout_ms=1000000000000,  accepttimeout_ms=10000000000000, ftp_response_timeout=100000000000000, ssl_verifypeer=FALSE, ssl_verifyhost=FALSE))


################################################
options(timeout = 100000000)


####################################RPUBS 7/19/18


options(rsconnect.max.bundle.size=1000000000)
options(rsconnect.max.bundle.files=100000000)
Sys.setlocale("LC_ALL", "English")

library(shiny)

# Define UI
ui = fluidPage(
    theme = shinytheme("yeti"),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(list(.big = "font-size: 2em")),
    div(id = "myapp",
        h2("Coursera Data Science Capstone Final Project: Kathy Targowski Ashenfelter"),
        checkboxInput("big", "Increase Font", FALSE),
        textInput("name", "Text Input :", ""),
        p('Please enter up to 2 words, separated by a space. I will take the liberty of predicting the next the word(s) based on my predictive algorithm.'),
        a(id = "toggleAdvanced", "Display More Information", href = "#"),
        shinyjs::hidden(
            div(id = "advanced",
                p('- Presentation:', HTML("<a href='http://www.rpubs.com/kashenfelter/410499'> Final Project Capstone Pitch Slides</a>")),
                p('- GitHub:', HTML("<a href='https://github.com/kashenfelter/Data_Science_Capstone_Final_Project'>Source Code</a>"))
            )
        ),
        p("Date and Time of Prediction: ",
          span(id = "time", date()),
          a(id = "update", "Update", href = "#")
        ),
        actionButton("reset", "Clear Input")
    ),
    mainPanel(
        verbatimTextOutput('text1'),
        verbatimTextOutput('text2'),
        br(),
        div(id = "bestMatch", p('I predict your next word will be :', htmlOutput('text3'))),
        hr(),
        DT::dataTableOutput('table')
    )
)

# Define server logic required for app

server = function(input, output) {

    shinyjs::onclick("toggleAdvanced",
                     shinyjs::toggle(id = "advanced", anim = TRUE))

    shinyjs::onclick("update", shinyjs::html("time", date()))

    observe({
        shinyjs::toggleClass("bestMatch", "big", input$big)
    })

    output$text1 <- renderText({
        if(!is.null(input$name) & input$name != "")
            mydfm = en_US$mydfm
        else
            mydfm = 0

        paste0('Document-feature matrix of: ',
               as.character(dim(mydfm))[1],
               ' documents, ',as.character(dim(mydfm))[2],' features.')
    })

    output$text2 <- renderText({
        if(!is.null(input$name) & input$name != "")
            mydfm = en_US$mydfm
        else
            mydfm = 0
        paste0('Document-feature matrix of: ',
               as.character(dim(mydfm))[1],
               ' documents, ',as.character(dim(mydfm))[2],' features.')
    })

    output$table <- DT::renderDataTable({
        corpUS = en_US$corpUS

        #'@ if(!is.null(input$name) | input$name != "") {
        criteria = strsplit(input$name, ' ')[[1]]

        len = length(criteria)
        if(len == 1) {
            corpUS %<>% filter(word1 == criteria[1]) %>% tbl_df
        } else if(len == 2) {
            corpUS %<>% filter(word1 == criteria[1] &
                                   word2 == criteria[2]) %>% tbl_df
            #'@ } else if(len == 3) {
            #'@   corpUS %<>% filter(word1 == criteria[1] &
            #'@                    word2 == criteria[2] &
            #'@                    word3 == criteria[3]) %>% tbl_df
            } else {
                corpUS = data.frame() %>% tbl_df
            }
        DT::datatable(corpUS)
        })

    output$text3 <- renderText({
        corpUS = en_US$corpUS

        ## http://rpubs.com/Hsing-Yi/176027
        #'@ if(!is.null(input$name) | input$name != "") {
        criteria = strsplit(input$name, ' ')[[1]]

        len = length(criteria)
        if(len == 1) {
            corpUS %<>% filter(word1 == criteria[1]) %>% tbl_df %>% .[1, 2] %>% unlist
        } else if(len == 2) {
            corpUS %<>% filter(word1 == criteria[1] &
                                   word2 == criteria[2]) %>% tbl_df %>% .[1, 3] %>% unlist
            #'@ } else if(len == 3) {
            #'@   corpUS %<>% filter(word1 == criteria[1] &
            #'@                        word2 == criteria[2] &
            #'@                        word3 == criteria[3]) %>% tbl_df %>% .[1, ]
            } else {
                corpUS = 'Unknown predictive next word.'
            }
        return(corpUS)
        })

    observeEvent(input$reset, {
        shinyjs::reset("myapp")
    })
    }


# Run the application
shinyApp(ui = ui, server = server)

