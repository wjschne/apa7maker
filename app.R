library(shiny)
library(DT)
library(dplyr)
library(crudtable)

d_author <- readr::read_csv("author.csv", col_types = "icccccccl")
d_affiliation <- readxl::read_excel("dbdata.xlsx", sheet = "affiliation")
d_authoraffliation <- readxl::read_excel("dbdata.xlsx", sheet = "authoraffiliation")
dao <- dataFrameDao(d_author)

ui <- fluidPage(title = "Authors", 
                h1("Authors"),
                actionButton(inputId = "addAuthor", label = "Add"),
                actionButton(inputId = "editAuthor", label = "Edit"),
                actionButton(inputId = "deleteAuthor", label = "Remove"),
                DTOutput("tblAuthor"),
                shiny::textOutput("rowindex"),
                h1("Affiliations"),
                DTOutput("tblAffiliation"),
                crudTableUI("tblAuthor1")
)

make_form <- function(x, name, table = "") {
    type <- class(x)
    name <- stringr::str_remove(name, paste0(table, "_"))
    
    if (name == "id") return(NULL)
    
    print(x)
    myinput <- switch(type, logical = shiny::checkboxInput, integer = shiny::numericInput, numeric = shiny::numericInput, character = shiny::textInput, factor = shiny::selectInput)
    myinput(inputId = paste0(table, "_", name), label = stringr::str_to_title(name) , value = ifelse(length(x) == 0 && is.logical(x), FALSE, x))
}



server <- function(input, output) {
    
    df <- reactiveValues(df_author = d_author)
    
    output$tblAuthor <- renderDT(DT::datatable(df$df_author , selection = list(mode = "single", selected = 1), editable = F, style = "bootstrap4",colnames = colnames(d_author) %>% stringr::str_remove("author_") %>% stringr::str_to_title(), options = list(dom = "t", columnDefs = list(list(visible = F, targets = "author_id")), selected = 1), rownames = F))
    author_proxy <- dataTableProxy("tblAuthor")
    
    
    d_author_s <- reactive(d_author %>% slice(input$tblAuthor_rows_selected))
    
    output$tblAffiliation <- renderDT(DT::datatable(d_affiliation, selection = "single", editable = "all", style = "bootstrap4", options = list(dom = "t"), rownames = F, colnames = ))
    
    
    
    observeEvent(input$editAuthor, {
        showModal(modalDialog(fluidPage(h3("Edit Row"), 
                                        purrr::map2(d_author_s(), colnames(d_author), make_form, table = 'author')), easyClose = TRUE, footer = tagList(
                                            modalButton("Cancel"),
                                            actionButton("ok_edit_author", "OK"))))
    })
    
    observeEvent(input$addAuthor, {
        showModal(modalDialog(fluidPage(h3("Add Row"), 
                                        purrr::map2(d_author_s() %>% filter(author_id == 0), colnames(d_author), make_form, table = 'author')), easyClose = TRUE, footer = tagList(
                                            modalButton("Cancel"),
                                            actionButton("ok_add_author", "OK"))))
    })
    
    
    observeEvent(input$ok_edit_author,{
        my_inputs <- names(input)[stringr::str_starts(names(input), "author_") ]
        for (i in my_inputs) {
            d_author[input$tblAuthor_rows_selected, i] <<- input[[i]]
        }
        current_row <- input$tblAuthor_rows_selected
        df$df_author <<- d_author
        
        selectRows(author_proxy, selected = current_row)
        
        
        shiny::removeModal()
    }
    )
    
    observeEvent(input$ok_add_author,{
        my_inputs <- names(input)[stringr::str_starts(names(input), "author_") ]
        new_author <- data.frame()
        for (i in my_inputs) {
            new_author[1, i] <- input[[i]]
        }
        new_author$author_id <- nrow(d_author) + 1
        d_author <<- d_author %>% dplyr::add_row(new_author)
        df$df_author <<- d_author
        selectRows(author_proxy, selected = nrow(d_author))
        shiny::removeModal()
    }
    )
    
    observeEvent(input$deleteAuthor, {
        if (input$tblAuthor_rows_selected > 0) {
            d_author <<- d_author %>% slice(-input$tblAuthor_rows_selected)   
            df$df_author <<- d_author
        }
    })
    
    output$rowindex <- renderText({
        s <- input$tblAuthor_rows_selected
        s
        
    })
    
    crudTableServer('tblAuthor1', dao)
}

shinyApp(ui, server)
