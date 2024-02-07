#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud2)
library(htm2txt)

# load the default text from the file
default_text <- paste0(readLines("data/viral_phylodynamics.txt"), collapse = " ")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Client-Side Wordcloud!"),

    # add options to the sidebar
    sidebarLayout(
        sidebarPanel(
            textAreaInput(
                "text",
                "Use this text to draw the word cloud.",
                height = "200px",
                value = default_text,
            ),
            sliderInput(
                "size",
                "Font size. The larger size means the bigger word.",
                min = 0.1,
                max = 5,
                value = 0.65
            ),
            sliderInput(
                "minSize",
                "Min font size, default is 0.",
                min = 0,
                max = 50,
                value = 0
            ),
            sliderInput(
                "gridSize",
                "Size of the grid in pixels for marking the availability of the canvas the larger the grid size, the bigger the gap between words.",
                min = 0,
                max = 20,
                value = 1
            ),
            selectInput(
                "fontFamily",
                "Font to use.",
                choices = c("Arial", "Arial Black", "Comic Sans MS", "Courier New", "Georgia", "Impact", "Lucida Console", "Lucida Sans Unicode", "Palatino Linotype", "Tahoma", "Times New Roman", "Trebuchet MS", "Verdana"),
                selected = "Impact"
            ),
            textInput(
                "fontWeight",
                "Font weight to use, e.g. normal, bold or 600",
                value = "normal"
            ),
            selectInput(
                "color",
                "color of the text, keyword 'random-dark' and 'random-light' can be used. color vector is also supported in this param",
                choices = c("random-dark", "random-light"),
                selected = "random-light"
            ),
            textInput(
                "backgroundColor",
                "Color of the background.",
                value = "black"
            ),
            sliderInput(
                "minRotation",
                "If the word should rotate, the minimum rotation (in rad) the text should rotate.",
                min = 0,
                max = 2 * pi,
                value = 0.5
            ),
            sliderInput(
                "maxRotation",
                "If the word should rotate, the maximum rotation (in rad) the text should rotate. Set the two value equal to keep all text in one angle.",
                min = 0,
                max = 2 * pi,
                value = 1
            ),
            checkboxInput(
                "shuffle",
                "Shuffle the points to draw so the result will be different each time for the same list and settings.",
                value = FALSE
            ),
            sliderInput(
                "rotateRatio",
                "Probability for the word to rotate. Set the number to 1 to always rotate.",
                min = 0,
                max = 1,
                value = 0.5
            ),
            selectInput(
                "shape",
                "The shape of the 'cloud' to draw.",
                choices = c("circle", "cardioid", "diamond", "triangle-forward", "triangle", "pentagon", "star"),
                selected = "circle"
            ),
            sliderInput(
                "ellipticity",
                "Degree of 'flatness' of the shape wordcloud2.js should draw.",
                min = 0,
                max = 1,
                value = 1
            ),
            sliderInput(
                "min_len",
                "Minimum length of the word to be included in the word cloud.",
                min = 1,
                max = 20,
                value = 3
            ),
            sliderInput(
                "min_freq",
                "Minimum frequency of the word to be included in the word cloud.",
                min = 1,
                max = 100,
                value = 5
            ),
            sliderInput(
                "max_words",
                "Maximum number of words to be included in the word cloud.",
                min = 1,
                max = 1000,
                value = 100
            ),
            textInput(
                "removeWords",
                "Words to remove from the word cloud, separated by comma.",
                value = "these,pmid,displaystyle,doi,pmc,et,al,fig,figure,table,tables,also,however,although,thus,therefore,despite,moreover,additionally,consequently,subsequently,nevertheless,nonetheless,regardless,whereas,meanwhile"
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # make sure the plot fills the whole panel!
            wordcloud2Output("wordcloud", width = "100%", height = "90vh")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    Data <- reactive({
        # load data
        text_corpus <- tolower(input$text)
        # Remove punctuation
        text_corpus <- gsub("[[:punct:]]", "", text_corpus)
        # Split text into words
        words <- strsplit(text_corpus, "\\s+")[[1]]
        word_frequencies <- table(words)
        # Convert table to data frame
        df_frequencies <- as.data.frame(word_frequencies, stringsAsFactors = FALSE)

        # Rename columns
        names(df_frequencies) <- c("name", "freq")

        # Sort by frequency in descending order
        df_frequencies <- df_frequencies[order(-df_frequencies$freq), ]

        # remove words in the stop words list
        stop_words <- c("the", "of", "and", "in", "to", "a", "is", "that", "for", "on", "with", "as", "by", "this", "it", "from", "at", "an", "or", "be", "not", "are", "was", "have", "has", "but", "were", "so", "if", "what", "which", "when", "where", "who", "how", "why", "there", "then", "than", "them", "their", "they", "we", "us", "our", "your", "you", "he", "she", "his", "her", "him", "hers", "its", "it's", "my", "me", "i", "am", "is", "are", "was", "were", "be", "been", "being", "do", "does", "did", "done", "doing", "will", "would", "shall", "should", "can", "could", "may", "might", "must", "ought", "need", "dare", "used", "had", "has", "have", "having", "having", "let", "lets", "let's", "make", "makes", "made", "making", "see", "sees", "saw", "seeing", "seem", "seems", "seemed", "seeming", "say", "says", "said", "saying", "go", "goes", "went", "gone", "going", "get", "gets", "got", "getting", "take", "takes")
        df_frequencies <- df_frequencies[!(df_frequencies$name %in% stop_words), ]
        # remove words with length less than 3
        df_frequencies <- df_frequencies[nchar(df_frequencies$name) > input$min_len, ]
        # remove words with frequency less than min frequency
        df_frequencies <- df_frequencies[df_frequencies$freq > input$min_freq, ]
        # remove additional words
        remove_words <- unlist(strsplit(input$removeWords, ","))
        df_frequencies <- df_frequencies[!(df_frequencies$name %in% remove_words), ]
        # remove words up to max words
        df_frequencies <- df_frequencies[seq_len(min(nrow(df_frequencies), input$max_words)), ]

        return(df_frequencies)
    })

    output$wordcloud <- renderWordcloud2({
        wordcloud2(
            data = Data(),
            size = input$size,
            minSize = input$minSize,
            gridSize = input$gridSize,
            fontFamily = input$fontFamily,
            fontWeight = input$fontWeight,
            color = input$color,
            backgroundColor = input$backgroundColor,
            minRotation = input$minRotation,
            maxRotation = input$maxRotation,
            shuffle = input$shuffle,
            rotateRatio = input$rotateRatio,
            shape = input$shape,
            ellipticity = input$ellipticity
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
