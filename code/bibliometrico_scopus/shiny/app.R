library(shiny)
library(tidyverse)
library(tidytext)
library(scales)
library(ggthemes)
library(ggwordcloud)


td_gamma <- readr::read_csv("E:/r/bigdata-socio-humanidades/data/tm_gamma_25.csv")
td_beta <- readr::read_csv("E:/r/bigdata-socio-humanidades/data/tm_beta_25.csv") 
metadata <- readr::read_csv(file = "E:/r/bigdata-socio-humanidades/data/metadata.csv")

keywords <- metadata %>% filter(tag=="keyword")
articles <- metadata %>%
        select(-source) %>% 
        filter(!tag == "keyword") %>%
        pivot_wider(id_cols = id, names_from = tag, values_from = value) %>%
        left_join(keywords %>% count(id) %>% rename(keywords=n)) %>%
        mutate(
                year=as.integer(year),
                cited=as.integer(cited)
        ) %>%
        filter(
                year>=2010,
                !is.na(abstract)
        )


ui <- fluidPage(
        fluidRow( 
                column(6, plotOutput("tufte", height = "500px")),
                column(6,
                        inputPanel(
                                selectInput("topic_selected", "Topic",
                                            choices = unique(td_beta$topic))
                        ), plotOutput("betaPlot") )
        ),
        fluidRow(
                column(6, tableOutput("gammaTable") ) ,
                column(3, plotOutput("keywordPlot") ) ,
                column(1, tableOutput("yeartopics") ) , 
                column(2, 
                       textOutput("topkeywordsText"),
                       textOutput("topwordsText"),
                       )
        ),
        fluidRow(
                column(12, tableOutput("abstractTable") ) 
        )
)

server <- function(input, output, session) {

        output$tufte <- renderPlot(({
                top_terms <- td_beta %>%
                        arrange(beta) %>%
                        group_by(topic) %>%
                        top_n(20, beta) %>%
                        arrange(-beta) %>%
                        select(topic, term) %>%
                        summarise(terms = list(term)) %>%
                        mutate(terms = map(terms, paste, collapse = ", ")) %>% 
                        unnest(cols=c(terms))
                
                gamma_terms <- td_gamma %>%
                        group_by(topic) %>%
                        summarise(gamma = mean(gamma)) %>%
                        arrange(desc(gamma)) %>%
                        left_join(top_terms, by = "topic") %>%
                        mutate(topic = paste0("Topic ", topic),
                               topic = reorder(topic, gamma))
                
                gamma_terms %>%
                        top_n(50, gamma) %>%
                        ggplot(aes(topic, gamma, label = terms, fill = topic)) +
                        geom_col(show.legend = FALSE) +
                        geom_text(hjust = 0, nudge_y = 0.0005) +
                        coord_flip() +
                        scale_y_continuous(expand = c(0,0),
                                           limits = c(0, 0.15)) +
                        theme_tufte(ticks = FALSE) +
                        theme(plot.title = element_text(size = 16 ),
                              plot.subtitle = element_text(size = 13)) +
                        labs(x = NULL, y = expression(gamma))
                
        }))
        
        output$betaPlot <- renderPlot({
                td_beta %>%
                        filter(topic == as.integer(input$topic_selected)) %>%
                        top_n(15, beta) %>%
                        mutate(term = reorder(term, beta)) %>%
                        ggplot(aes(term, beta, fill = beta)) +
                        geom_col(show.legend = FALSE) +
                        scale_fill_gradient(low = "#5FBA7D", high = "#108AEC") +
                        scale_y_continuous(expand = c(0,0),
                                           labels = percent_format()) +
                        coord_flip() +
                        theme_minimal(base_size = 18) +
                        labs(x = NULL, y = expression(beta))
        })
        
        output$gammaTable <- renderTable({
                td_gamma %>%
                        filter(topic == as.integer(input$topic_selected)) %>%
                        left_join(articles %>% select(document=id, titulo, journal, year)) %>%
                        top_n(10, wt = gamma) %>%
                        arrange(desc(gamma)) %>%
                        select(-topic) 
        })
        
        output$keywordTable <- renderTable({
                td_gamma %>%
                        filter(topic == as.integer(input$topic_selected)) %>%
                        left_join(metadata %>% 
                                          filter(tag=="keyword") %>%
                                          select(document=id, value)) %>%
                        top_n(100, wt = gamma) %>%
                        select(-topic) %>%
                        count(value, sort = TRUE) %>%
                        head(10)
        })
        
        output$keywordPlot <- renderPlot({
                set.seed(42)
                td_gamma %>%
                        filter(topic == as.integer(input$topic_selected)) %>%
                        left_join(metadata %>% 
                                          filter(tag=="keyword") %>%
                                          select(document=id, value)) %>%
                        top_n(100, wt = gamma) %>%
                        select(-topic) %>%
                        count(value, sort = TRUE) %>%
                        filter(n>1)%>%
                        ggplot(aes( label = value, size = n, color = n ) ) +
                                geom_text_wordcloud_area() +
                                scale_size_area(max_size = 24) +
                                theme_minimal()
        })
        
        output$yeartopics <- renderTable({
                td_gamma %>%
                        filter(topic == as.integer(input$topic_selected)) %>%
                        left_join(articles %>% select(document=id, journal, year)) %>%
                        top_n(100, wt = gamma) %>%
                        count(year, sort = TRUE) %>%
                        arrange(year)
        })
        
        output$abstractTable <- renderTable({
                td_gamma %>%
                        filter(topic == as.integer(input$topic_selected)) %>%
                        left_join( articles %>% select(document=id, titulo, abstract, doi)) %>%
                        top_n(10, wt = gamma) %>%
                        arrange(desc(gamma)) %>%
                        select(doi,titulo,abstract) 
        })
        
        output$topkeywordsText <- renderText({
                td_gamma %>%
                        filter(as.integer(topic) == as.integer(input$topic_selected)) %>%
                        left_join(metadata %>% 
                                          filter(tag=="keyword") %>%
                                          select(document=id, value)) %>%
                        top_n(100, wt = gamma) %>%
                        select(-topic) %>%
                        count(value, sort = TRUE) %>%
                        head(5) %>% pull(value) %>% paste(collapse = ", ")
        })

        output$topwordsText <- renderText({
                td_beta %>%
                        filter(topic == as.integer(input$topic_selected)) %>%
                        top_n(5, beta) %>%
                        pull(term) %>% paste(collapse = ", ")
        })
        

}

shinyApp(ui, server)