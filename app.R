webr::install("dplyr")
library(toastui)
library(shiny)
library(dplyr)
library(tippy)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(tidyr)
library(stringr)
library(purrr)
library(yaml)
library(rclipboard)
library(readr)
library(snakecase)
library(tibble)
# set_grid_theme(
#   row.even.background = "#ddebf7",
#   cell.normal.border = "#9bc2e6",
#   cell.normal.showVerticalBorder = TRUE,
#   cell.normal.showHorizontalBorder = TRUE,
#   cell.header.background = "#2679ab",
#   cell.header.text = "#FFF",
#   cell.selectedHeader.background = "#2679ab",
#   cell.focused.border = "#2679ab",
# )
ifempty <- function(x) {
  if (is.null(x))
    return(NULL)
  if (x == is.na(x))
    return(NULL)
  if (length(x) == 0)
    return(NULL)
  if (x == "")
    return(NULL)
  x
}


# ui ----
ui <- page_fluid(
  rclipboardSetup(),
  theme = bs_theme(),
  tags$style(".btn-left {margin-left: 15px}"),
  title = "Writing in APA Style, 7th Edition with Quarto",
  h1("APAQUARTO: A Quarto Extension for Writing in APA Style"),
  navset_pill_list(
    widths = c(3, 9),
    ## title/general ----
    nav_panel(
      title = "General Options",
      panel(
        heading = "Title Information",
        status = "primary",
        textInput("title", label = "Title", width = "100%") ,
        textInput(
          "shorttitle",
          tooltip(
            trigger = list("Short Title", bs_icon("info-circle")),
            "Running text in header. If blank, the running header is the title in upper case."
          ),
          width = "100%"
        )
      ),
      panel(
        heading = "Document Options",
        status = "primary",
        checkboxInput(
          "floatsintext",
          label = "Plots and figures appear in text instead at the end.",
          value = TRUE,
          width = "100%"
        ),
        checkboxInput(
          "numbered-lines",
          label = "Numbered lines (not available in .html format)",
          value = FALSE,
          width = "100%"
        ),
        checkboxInput(
          "no-ampersand-parenthetical",
          label = tooltip(
            trigger = list(
              "Use \"and\" (or its replacement word) in parenthetical citations (e.g., Schneider and McGrew, 2018).",
              bs_icon("info-circle")
            ),
            "The default is leave this box unchecked. Checking this box makes non-standard APA citations."
          ),
          value = FALSE,
          width = "100%"
        ),
        checkboxInput(
          "mask",
          label = "Mask authors",
          value = FALSE,
          width = "100%"
        ),
        selectizeInput(
          inputId = "masked-citations",
          label = "A list of citation keys that should be masked if `mask` is `true`. Enter each key.",
          width = "100%",
          multiple = TRUE,
          choices = character(0),
          selected = NULL,
          options = list(
            'plugins' = list('remove_button'),
            'create' = TRUE,
            'persist' = TRUE
          )
        ),
        selectizeInput(
          inputId = "bibiography",
          label = tooltip(
            trigger = list("Bibliography file(s)", bs_icon("info-circle")),
            "Files must exist in same folder as the Quarto document."
          ),
          width = "100%",
          multiple = TRUE,
          choices = character(0),
          selected = NULL,
          options = list(
            'plugins' = list('remove_button'),
            'create' = TRUE,
            'persist' = TRUE
          )
        ),
        selectizeInput(
          inputId = "nocite",
          label = tooltip(
            trigger = list("List of uncited references", bs_icon("info-circle")),
            "If meta-analysis is checked, these references will be treated as meta-analytic citations."
          ),
          width = "100%",
          multiple = TRUE,
          choices = character(0),
          selected = NULL,
          options = list(
            'plugins' = list('remove_button'),
            'create' = TRUE,
            'persist' = TRUE
          )
        ),
        checkboxInput(
          "meta-analysis",
          label = span(
            "All uncited references in will be treated as meta-analytic citations."
          ),
          value = TRUE,
          width = "100%"
        ),
      ),
      panel(
        heading = "Suppress Document Elements",
        status = "primary",
        checkboxGroupInput(
          inputId = "suppress",
          label = NULL,
          choices = c(
            `Title Page` = "suppress-title-page",
            `Title Page Number` = "suppress-title-page-number",
            Title = "suppress-title",
            `Short Title` = "suppress-short-title",
            `Title in Introduction`  = "suppress-title-introduction",
            Author = "suppress-author",
            Affiliation = "suppress-affiliation",
            `Author Note` = "suppress-author-note",
            `ORCID` = "suppress-orcid",
            `Status Change Paragraph` = "suppress-status-change-paragraph",
            `Disclosures Paragraph` = "suppress-disclosures-paragraph",
            `CRediT Statement` = "suppress-credit-statement",
            `Corresponding Paragraph` = "suppress-corresponding-paragraph",
            `Corresponding Group` = "suppress-corresponding-group",
            `Corresponding Department` = "suppress-corresponding-department",
            `Corresponding Affiliation` = "suppress-corresponding-affiliation-name",
            `Corresponding Address` = "suppress-corresponding-address",
            `Corresponding City` = "suppress-corresponding-city",
            `Corresponding Region` = "suppress-corresponding-region",
            `Corresponding Postal Code` = "suppress-corresponding-postal-code",
            `Corresponding Email` = "suppress-corresponding-email",
            `Abstract` = "suppress-abstract",
            `Impact Statement`  = "suppress-impact-statement",
            `Keywords` = "suppress-keywords"
          )
        )
      )
    ),
    ## Formats ----
    nav_panel(
      title = "Format Options",
      checkboxGroupInput(
        "formattype",
        label = "Formats",
        inline = TRUE,
        width = "100%",
        choices = c(
          `Word (.docx)` = "apaquarto-docx",
          `Web (.html)` = "apaquarto-html",
          `Typst (.pdf)` = "apaquarto-typst",
          `LaTeX (.pdf)` = "apaquarto-pdf"
        ),
        selected = c(
          "apaquarto-docx",
          "apaquarto-html",
          "apaquarto-typst",
          "apaquarto-pdf"
        )
      ),
      panel(
        heading = "Word (.docx) and Typst (.pdf)",
        status = "primary",
        numericInput(
          "blank-lines-above-title",
          label = "Number of blank lines above title",
          value = 2,
          width = "100%",
          min = 0
        ),
        numericInput(
          "blank-lines-above-author-note",
          label = "Lines between Author Names and Author Notes",
          value = 2,
          min = 0,
          width = "100%"
        )
      ),
      panel(heading = "Typst (.pdf)",
            status = "primary",
            checkboxInput("list-of-figures", label = "List of Figures", width = "100%"),
            checkboxInput("list-of-tables", label = "List of Table", width = "100%")),
      panel(
        heading = "LaTeX (.pdf)",
        status = "primary",
        radioButtons(
          inline = TRUE,
          "documentmode",
          label = "Document Mode",
          choices = c(
            Manuscript = "man",
            Journal = "jou",
            `LaTeX Style Document` = "doc",
            Student = "stu"
          ),
          selected = "man"
        ),
        radioButtons(
          inline = TRUE,
          "fontsize",
          label = "Font size",
          choices = c(`10` = "10pt", `11` = "11pt", `12` = "12pt"),
          selected = "12pt"
        ),
        radioButtons(
          inline = TRUE,
          "a4paper",
          label = "Paper size",
          choices = c(
            `U.S. Letter (8.5 inchy by 11 inch)` = FALSE,
            `A4 (210mm by 297mm` = TRUE
          ),
          selected = FALSE
        ),
        panel(
          heading = "Journal Mode Options",
          textInput(
            "journal",
            "Journal Title",
            width = "100%",
            placeholder = "Psychological Review"
          ),
          textInput(
            "volume",
            "Journal Volume (Number and Pages)",
            width = "100%",
            placeholder = "2024, Vol. 131, No. 2, 10--60"
          ),
          textInput(
            "copyrightnotice",
            "Copyright Year",
            width = "100%",
            placeholder = "2025"
          ),
          textInput(
            "copyrighttext",
            "Copyright Text",
            width = "100%",
            placeholder = "All rights reserved"
          )
        ),
        panel(
          heading = "Student Paper Options",
          textInput("course", "Course", width = "100%", placeholder = "Introduction to Statistics (EDUC 5101)"),
          textInput(
            "professor",
            "Professor",
            width = "100%",
            placeholder = "W. Joel Schneider"
          ),
          checkboxInput("includeduedate", 
                        "Include due date"),
          dateInput("duedate", "Due Date", width = "100%", format = "yyyy-mm-dd"),
          textInput(
            "student-note",
            "Student Paper Note",
            width = "100%",
            placeholder = "Student ID: 12345"
          )
        )
      )
      
    ),
    ## authors ----
    nav_panel(
      title = "Authors",
      panel(
        heading = "Authors and Affiliations",
        status = "primary",
        tags$h3("Affiliation(s)"),
        fluidRow(column(width = 12, datagridOutput2("gd_author"))),
        actionButton(inputId = "addAuthor", label = "Add Author"),
        
        tags$h3("Affiliation(s)", style = "margin-top: 12px"),
        
        
        fluidRow(column(
          width = 12, datagridOutput2("gd_affiliation")
        )),
        actionButton(inputId = "addAffiliation", label = "Add Affiliation"),
        p(
          "Before clicking outside the table, be sure to finish editing a cell by clicking Enter on the keyboard or by clicking another cell within the table."
        )
      )
    ),
    ## author note ----
    nav_panel(
      title = "Author Note",
      panel(
        status = "primary",
        heading = "Author Note",
        tags$h3("Status Changes"),
        textInput(
          "affiliation-change",
          label = "Affiliation Change",
          width = "100%",
          placeholder = "Fred Jones is now at Generic State University."
        ),
        textInput(
          "deceased-note",
          label = "Author Deceased",
          width = "100%",
          placeholder = "Fred Jones is deceased."
        ),
        tags$h3("Disclosures"),
        textInput(
          "study-registration",
          label = "Study Registration",
          width = "100%",
          placeholder = "This study was registered at ClinicalTrials.gov (Identifier NTC998877)."
        ),
        textInput(
          "data-sharing",
          label = "Data Sharing",
          width = "100%",
          placeholder = "Data from this study can be accessed at https://academicdata.org/jones2024."
        ),
        textInput(
          "related-report",
          label = "Related Report",
          width = "100%",
          placeholder = "This article is based on the dissertation completed by Jones (2018)"
        ),
        textInput(
          "conflict-of-interest",
          label = "Conflict of Interest",
          width = "100%",
          placeholder = "Fred Jones has been a paid consultant for Corporation X, which funded this study."
        ),
        textInput(
          "financial-support",
          label = "Financial Support",
          width = "100%",
          placeholder = "This study was supported by Grant 123 from Academic Funders United."
        ),
        textInput(
          "gratitude",
          label = "Gratitude/Acknowledgements",
          width = "100%",
          placeholder = "The authors are grateful to Sidney Fiero for thoughtful comments on an early draft of this paper."
        ),
        textInput(
          "author-agreements",
          label = "Authorships Agreements",
          width = "100%",
          placeholder = "Because the authors are equal contributors, order of authorship was determined by a fair coin toss."
        ),
        textInput(
          "correspondence-note",
          label = "Custom Correspondence Note",
          width = "100%",
          placeholder = "Any text here will override the correspondence note that would otherwise be generated automatically."
        )
        
      )
    ),
    ## abstract ----
    nav_panel(
      title = "Abstract",
      panel(
        status = "primary",
        heading = "Abstract Page",
        textAreaInput(
          "abstract",
          label = "Abstract",
          width = "100%",
          rows = 6,
          resize = "vertical"
        ),
        textAreaInput(
          "impact-statment",
          label = "Impact Statement",
          width = "100%",
          rows = 6,
          resize = "vertical"
        ),
        selectizeInput(
          inputId = "keywords",
          label = "Keywords. Enter each word or phrase.",
          width = "100%",
          multiple = TRUE,
          choices = character(0),
          selected = NULL,
          options = list(
            'plugins' = list('remove_button'),
            'create' = TRUE,
            'persist' = TRUE
          )
        ),
        checkboxInput("word-count", label = "Word Count")
        
      )
    ),
    ## language ----
    nav_panel(
      title = "Language",
      panel(
        status = "primary",
        heading = "Language Options",
        selectInput(
          inputId = "lang",
          label = span(
            "Primary Language ",
            tags$a("(More information)", href = "https://quarto.org/docs/authoring/language.html")
          ),
          choices = c(
            Chinese = 'zh',
            Czech = 'cs',
            Dutch = 'nl',
            English = 'en',
            Finnish = 'fi',
            French = 'fr',
            German = 'de',
            Italian = 'it',
            Japanese = 'ja',
            Korean = 'ko',
            Polish = 'pl',
            Portuguese = 'pt',
            Russian = 'ru',
            Spanish = 'es'
          ),
          selected = "en"
        ),
        h3("Options Specific to apaquarto"),
        p(
          a("More information", href = "https://wjschne.github.io/apaquarto/options.html#language-options")
        ),
        textInput(
          "citation-last-author-separator",
          "Separator for the last author in narrative citations. For example, Smith, Davis, and Jones (2025)",
          placeholder = "and",
          width = "100%"
        ),
        textInput(
          "citation-masked-author",
          "Replacement phrase for masked citations",
          placeholder = "Masked Citation",
          width = "100%"
        ),
        textInput(
          "citation-masked-date",
          "Replacement phrase for date in masked citations",
          placeholder = "n.d.",
          width = "100%"
        ),
        textInput(
          "title-block-author-note",
          "The heading of the author note",
          placeholder = "Author Note",
          width = "100%"
        ),
        textInput(
          "title-block-correspondence-note",
          "Correspondence introduction",
          placeholder = "Correspondence concerning this article should be addressed to",
          width = "100%"
        ),
        textInput(
          "title-block-role-introduction",
          "The phrase introducing the author roles",
          placeholder = "Author roles were classified using the Contributor Role Taxonomy (CRediT; https://credit.niso.org/) as follows:",
          width = "100%"
        ),
        textInput(
          "title-impact-statement",
          "Impact statement heading",
          placeholder = "Impact Statement",
          width = "100%"
        ),
        textInput(
          "title-word-count",
          span(
            "The phrase before the word count when",
            tags$code("word-count: true")
          ),
          placeholder = "Word Count",
          width = "100%"
        ),
        textInput(
          "references-meta-analysis",
          "Explanation for meta-analytic explanations",
          placeholder = "References marked with an asterisk indicate studies included in the meta-analysis.",
          width = "100%"
        )
      )
    ),
    ## make yaml----
    nav_panel(
      title = "Make Document",
      panel(
        heading = "Make Document",
        status = "primary",
        actionButton("btnmakedocument", label = "Update", class = "mb-2"),
        br(),
        uiOutput("makedocument")
      )
    )
  )
  
  
  
)
# server ----
server <- function(input, output, session) {
  d_author <- read_csv("author.csv", col_types = "icccllcccccccccccccc")
  
  d_affiliation <- read_csv("affiliation.csv", col_types = "iiccccccccc")
  
  cnames <- colnames(d_author) |>
    str_remove_all("^author_") |>
    str_remove_all("^role_") |>
    to_title_case()
  cnames[cnames == "Orcid"] <- "ORCID"
  cnames[cnames == "Id"] <- "Delete"
  
  anames <- colnames(d_affiliation) |>
    str_remove_all("^affiliation_") |>
    str_remove_all("^role_") |>
    to_title_case()
  
  anames[anames == "Url"] <- "URL"
  anames[anames == "Id"] <- "Delete"
  
  author_select <- reactiveVal(1)
  r_affiliation_current <- reactiveVal(d_affiliation)
  r_affiliation <- reactiveVal(d_affiliation)
  
  
  # gd_author ----
  output$gd_author <- renderDatagrid2({
    e_author <- datagrid(
      d_author,
      colnames = cnames,
      data_as_input = TRUE,
      sortable = FALSE,
      colwidths = "auto",
      bodyHeight = "auto",
      editingEvent = "click"
    ) %>%
      grid_columns(
        column = c("author_name", "author_orcid", "author_email"),
        width = c(300, 200, 200)
      ) |>
      grid_columns(
        column = c("author_corresponding", "author_deceased"),
        width = 120
      ) |>
      grid_columns(column = c("author_id"), width = 80) |>
      grid_columns(
        column = c(
          "role_conceptualization",
          "role_data_curation",
          "role_formal_analysis",
          "role_funding_acquisition",
          "role_investigation",
          "role_methodology",
          "role_project_administration",
          "role_resources",
          "role_software",
          "role_supervision",
          "role_validation",
          "role_visualization",
          "role_writing",
          "role_editing"
        ),
        width = c(140, 100, 140, 160, 100, 100, 160, rep(100, 7)),
        align = "center"
      ) |>
      grid_col_button(
        "author_id",
        inputId = "author_delete",
        label = "Delete",
        icon = icon("trash")
      ) |>
      grid_editor(column = "author_name", type = "text") %>%
      grid_col_checkbox(column = "author_corresponding") %>%
      grid_editor(column = "author_email", type = "text") %>%
      grid_editor(column = "author_orcid", type = "text") %>%
      grid_col_checkbox(column = "author_deceased") %>%
      grid_editor(
        column = c(
          "role_conceptualization",
          "role_data_curation",
          "role_formal_analysis",
          "role_funding_acquisition",
          "role_investigation",
          "role_methodology",
          "role_project_administration",
          "role_resources",
          "role_software",
          "role_supervision",
          "role_validation",
          "role_visualization",
          "role_writing",
          "role_editing"
        ),
        type = "radio",
        choices = c("No", "Yes", "Lead", "Supporting", "Equal")
      ) |>
      # grid_editor_opts(editingEvent = "click") |>
      grid_click(inputId = "author_click") |>
      grid_complex_header(
        "Roles" = c(
          "role_conceptualization",
          "role_data_curation",
          "role_formal_analysis",
          "role_funding_acquisition",
          "role_investigation",
          "role_methodology",
          "role_project_administration",
          "role_resources",
          "role_software",
          "role_supervision",
          "role_validation",
          "role_visualization",
          "role_writing",
          "role_editing"
        )
      )
  })
  
  # gd_affiliation ----
  output$gd_affiliation <- renderDatagrid2(
    datagrid(
      r_affiliation_current(),
      colwidths = "guess",
      colnames = anames,
      bodyHeight = "auto",
      editingEvent = "click",
      sortable = FALSE
    ) |>
      grid_col_button(
        "affiliation_id",
        inputId = "affiliation_delete",
        label = "Delete",
        icon = icon("trash")
      ) |>
      grid_columns(columns = c("author_id"), hidden = TRUE) |>
      grid_editor(column = "affiliation_name", type = "text") |>
      grid_editor(column = "affiliation_department", type = "text") |>
      grid_editor(column = "affiliation_group", type = "text") |>
      grid_editor(column = "affiliation_address", type = "text") |>
      grid_editor(column = "affiliation_city", type = "text") |>
      grid_editor(column = "affiliation_region", type = "text") |>
      grid_editor(column = "affiliation_country", type = "text") |>
      grid_editor(column = "affiliation_postal_code", type = "text") |>
      grid_editor(column = "affiliation_url", type = "text") |>
      grid_click("affiliation_click")
  )
  
  
  author_row <- function(i) {
    current_author_id = NA
    d_author_current <- input$gd_author_data
    if (length(d_author_current) == 0) {
      return(NULL)
    }
    if (is.null(d_author_current))
      d_author_current <- d_author
    
    if (!is.null(i)) {
      if (i > 0 & nrow(d_author_current) > 0) {
        new_author_id <- d_author_current[i, "author_id", drop = TRUE]
        
        if (!all(i == author_select())) {
          d_affiliation_current <- input$gd_affiliation_data
          if (is.data.frame(d_affiliation_current)) {
            d_affiliation_current <- d_affiliation_current |>
              select(-rowKey) |>
              mutate(across(affiliation_name:affiliation_url, as.character))
            current_author_id <- input$gd_author_data[author_select(), "author_id", drop = TRUE]
          }
          
          if (!is.na(current_author_id) &
              is.data.frame(d_affiliation_current)) {
            if (nrow(d_affiliation_current) > 0) {
              r_affiliation(
                r_affiliation() |>
                  unique() |>
                  filter(author_id != current_author_id) |>
                  bind_rows(unique(d_affiliation_current))
              )
              
            }
            
          }
          
          
        }
        r_affiliation_current(r_affiliation() |>
                                filter(author_id == new_author_id) |>
                                unique())
        
      }
      author_select(i)
    }
  }
  
  # author row ----
  observeEvent(input$author_click, {
    i <- input$author_click$row
    if (!is.null(i)) {
      author_row(i)
    }
  })
  
  
  
  author_n <- reactiveVal(nrow(d_author))
  
  
  
  # add author----
  observeEvent(input$addAuthor, {
    new_author_id <- author_n() + 1
    new_affiliation_id <- ifelse(nrow(r_affiliation()) == 0,
                                 1,
                                 max(r_affiliation()$affiliation_id) + 1)
    new_author <- tibble(
      author_id = new_author_id,
      author_corresponding = FALSE,
      author_deceased = FALSE
    ) |>
      bind_rows(d_author |> filter(FALSE)) |>
      mutate(across(starts_with("role_"), \(x) "No"))
    
    d_author <- bind_rows(unique(d_author), unique(new_author))
    
    r_affiliation(bind_rows(
      r_affiliation() |>
        filter(affiliation_id != new_affiliation_id) |>
        unique(),
      tibble(affiliation_id = new_affiliation_id, author_id = new_author_id) |>
        unique()
    ))
    
    grid_proxy_add_row(proxy = "gd_author", new_author)
    author_n(author_n() + 1)
    
    
    
  })
  
  # delete author----
  observeEvent(input$author_delete, {
    d_author <- d_author |>
      filter(author_id != input$author_delete)
    data = input$gd_author_data
    rowKey <- data$rowKey[data$author_id == input$author_delete]
    grid_proxy_delete_row(proxy = "gd_author", rowKey)
    aff <- r_affiliation() |>
      filter(author_id != input$author_delete)
    aff$rowKey <- NULL
    r_affiliation(aff)
    r_affiliation_current(r_affiliation() |> filter(FALSE))
  })
  
  # add affiliation----
  observeEvent(input$addAffiliation, {
    d_author_current <- input$gd_author_data
    
    if (is.numeric(author_select()) &&
        !is.na(author_select()) &&
        author_select() <= nrow(d_author_current)) {
      current_author_id <- d_author_current |> slice(author_select()) |> pull(author_id)
      
      if (nrow(r_affiliation()) == 0) {
        new_id <- 1
      } else {
        new_id <- max(pull(r_affiliation(), affiliation_id)) + 1L
      }
      
      new_affiliation_row <- bind_rows(
        d_affiliation |> filter(FALSE),
        data.frame(affiliation_id = new_id, author_id = current_author_id)
      )
      
      r_affiliation(bind_rows(r_affiliation(), new_affiliation_row))
      grid_proxy_add_row("gd_affiliation", new_affiliation_row)
    }
    
  })
  
  
  # delete affiliation----
  observeEvent(input$affiliation_delete, {
    delete_id <- as.numeric(input$affiliation_delete)
    r_affiliation(r_affiliation() |>
                    filter(affiliation_id != delete_id))
    
    
    data = input$gd_affiliation_data
    rowKey <- data$rowKey[data$affiliation_id == delete_id]
    grid_proxy_delete_row(proxy = "gd_affiliation", rowKey)
    
  })
  
  # format ----
  observeEvent(input$btnmakedocument, {
    author_row(1)
    
    d_author_current <- input$gd_author_data
    if (is.null(d_author_current))
      d_author_current <- d_author
    author_yaml <- NA
    
    if (!is.null(d_author_current) & length(d_author_current) > 0) {
      if (nrow(d_author_current) > 0) {
        d_author_current$rowKey <- NULL
        author_yaml <- d_author_current |>
          pivot_longer(starts_with("role"), names_to = "role") |>
          mutate(role = str_remove(role, "role_")) |>
          nest(.by = -c(role, value), .key = "role") |>
          mutate(role = map(role, \(d) {
            d <- d |>
              filter(value != "No")
            if (nrow(d) == 0)
              return(NA)
            role_level <- d |>
              filter(value != "Yes") |>
              deframe() |>
              as.list()
            role <- d |>
              filter(value == "Yes") |>
              pull(role) |>
              as.list()
            
            if (length(role_level) > 0) {
              ll <- map2(names(role_level), role_level, \(n, v) {
                l <- list(v)
                names(l) <- n
                l
              })
              role <- append(role, ll)
            }
            role
            
          })) |>
          rename_with(.fn = \(x) str_remove(x, "^author_")) |>
          nest(.by = c(id), .key = "author") |>
          mutate(author = map2(author, id, \(d, i) {
            d <- d[, d |> apply(MARGIN = 2, \(x) !all(is.na(x)))]
            if (all(!d$deceased))
              d$deceased <- NULL
            if (all(!d$corresponding))
              d$corresponding <- NULL
            x <- as.list(d)
            if (!all(is.na(d$role))) {
              x$role <- d$role[[1]]
            }
            
            if (nrow(r_affiliation()) > 0) {
              d_aff <- r_affiliation() |>
                filter(author_id == i) |>
                pivot_longer(-c(affiliation_id, author_id)) |>
                mutate(name = str_remove(name, "^affiliation_")) |>
                filter(!is.na(value))
              
              if (nrow(d_aff) > 0) {
                l_affiliation <- d_aff |>
                  pivot_wider() |>
                  nest(affiliation = -c(affiliation_id, author_id)) |>
                  mutate(affiliation = map(affiliation, as.list))
                x$affiliation <- l_affiliation$affiliation
              }
            }
            
            x
            
          })) |>
          select(-id)
        }}
        
        
        nocite <- NULL
        if (length(input$nocite) > 0) {
          nocite <- lapply(input$nocite, \(x) {
            if (!str_detect(x, "^\\@"))
              x <- paste0("@", x)
            x
          }) |>
            paste(collapse = ", ")
          nocite <- paste0("nocitestart\n", nocite, "\nnociteend")
          
        }
        
        doc_list <- list(
          title = ifempty(input$title),
          shorttitle = ifempty(input$shorttitle),
          bibliography = ifempty(input$bibliography),
          floatsintext = input$floatsintext,
          `numbered-lines` = input$`numbered-lines`,
          mask = input$mask,
          `no-ampersand-parenthetical` = input$`no-ampersand-parenthetical`,
          `meta-analysis` = input$`meta-analysis`,
          `nocite` = nocite
        )
        
        if (!all(is.na(author_yaml))) {
          l_author <- list(author = author_yaml[[1]])
          doc_list <- append(doc_list, l_author, after = 2)
        }
        
        
        if (length(input$suppress) > 0) {
          l_suppress <- rep(TRUE, length(input$suppress))
          names(l_suppress) <- input$suppress
          doc_list <- append(doc_list, l_suppress)
        }
        
        author_note <- list(
          `status-changes` = list(
            `affiliation-change` = ifempty(input$`affiliation-change`),
            deceased = ifempty(input$deceased)
          ),
          disclosures = list(
            `study-registration` = ifempty(input$`study-registration`),
            `data-sharing` = ifempty(input$`data-sharing`),
            `related-report` = ifempty(input$`related-report`),
            `conflict-of-interest` = ifempty(input$`conflict-of-interest`),
            `financial-support` = ifempty(input$`financial-support`),
            `gratitude` = ifempty(input$`gratitude`),
            `authorship-agreements` = ifempty(input$`authorship-agreements`)
          )
        )
        
        author_note <- lapply(author_note, \(x) {
          x[lapply(x, length) == 0] <- NULL
          x
        })
        
        author_note[lapply(author_note, length) == 0] <- NULL
        

        doc_list <- append(doc_list, list(`author-note` = author_note), after = 3)
        
        
        doc_list <- append(
          doc_list,
          list(
            abstract = ifempty(input$abstract),
            `impact-statement` = ifempty(input$`impact-statement`),
            keywords = input$keywords,
            `word-count` = input$`word-count`
          ),
          after = 4
        )
        
        doc_list$lang <- input$lang
        language <- list()
        language$`citation-last-author-separator` <- ifempty(input$`citation-last-author-separator`)
        language$`citation-masked-author` <- ifempty(input$`citation-masked-author`)
        language$`citation-masked-date` <- ifempty(input$`citation-masked-date`)
        
        language$`title-block-author-note` <- ifempty(input$`title-block-author-note`)
        language$`title-block-correspondence-note` <- ifempty(input$`title-block-correspondence-note`)
        language$`title-block-correspondence-note` <- ifempty(input$`title-block-role-introduction`)
        language$`title-impact-statement` <- ifempty(input$`title-impact-statement`)
        language$`title-word-count` <- ifempty(input$`title-word-count`)
        language$`references-meta-analysis` <- ifempty(input$`references-meta-analysis`)
        
        language[lapply(language, length) == 0] <- NULL
        if (length(language) > 0 ) {
          doc_list$language <- language
        }
        

        
        if (length(input$formattype) == 0) {
          doc_list$format <- list(`apaquarto-html` = list(toc = TRUE))
        } else {
          apaformats <- list()
          if ("apaquarto-html" %in% input$formattype) {
            apaformats$`apaquarto-html` = list(toc = TRUE)
          }
          if ("apaquarto-docx" %in% input$formattype) {
            doc_list$`blank-lines-above-title` <- ifempty(input$`blank-lines-above-title`)
            doc_list$`blank-lines-above-author-note` <- ifempty(input$`blank-lines-above-author-note`)
            apaformats$`apaquarto-docx` = list(toc = FALSE)
          }
          if ("apaquarto-typst" %in% input$formattype) {
            doc_list$`blank-lines-above-title` <- ifempty(input$`blank-lines-above-title`)
            doc_list$`blank-lines-above-author-note` <- ifempty(input$`blank-lines-above-author-note`)
            apaformats$`apaquarto-typst` = list(
              toc = FALSE,
              `list-of-figures` = input$`list-of-figures`,
              `list-of-tables` = input$`list-of-tables`
              )
          }
          
          if ("apaquarto-pdf" %in% input$formattype) {
            apaformats$`apaquarto-pdf` = list(
              documentmode = input$documentmode,
              course = ifempty(input$course),
              professor = ifempty(input$professor),
              duedate = as.character(as.Date(input$duedate)),
              note = ifempty(input$note),
              journal = ifempty(input$journal),
              volume = ifempty(input$volume),
              copyrightnotice = ifempty(input$copyrightnotice),
              copyrightext = ifempty(input$copyrightext),
              
              `keep-tex` = FALSE
            )
            if (!input$includeduedate) {
              apaformats$`apaquarto-pdf`$duedate <- NULL
            }
            apaformats$`apaquarto-pdf`[lapply(apaformats$`apaquarto-pdf`, length) == 0] <- NULL
          }
          doc_list$format <- apaformats
          
          
          
        }
        
        doc_list[lapply(doc_list, length) == 0] <- NULL
        
        doc_yaml <- doc_list |>
          as.yaml(
            indent.mapping.sequence = T,
            handlers = list(
              logical = function(x) {
                result <- ifelse(x, "true", "false")
                class(result) <- "verbatim"
                return(result)
              }
            )
          ) |>
          gsub(pattern = "'false'", replacement = "false") |>
          gsub(pattern = "'true'", replacement = "true") |>
          gsub(pattern = "nocitestart\n", replacement = "") |>
          gsub(pattern = "nociteend\n", replacement = "") |>
          gsub(pattern = "\\|\\-", replacement = "|")
        
        doc_yaml <- paste0(
          "---\n",
          trimws(doc_yaml),
          "\n---\n\n",
          "<!-- The introduction should not have a level-1 heading such as Introduction. -->\n\n",
          "## Section in Introduction\n\n",
          "## Another Section in Introduction\n\n",
          "# Method\n\n",
          "## Participants\n\n",
          "## Measures\n\n",
          "## Procedure\n\n",
          "# Results\n\n",
          "# Discussion\n\n",
          "## Limitations and Future Directions\n\n",
          "## Conclusion\n\n",
          "# References\n\n",
          "<!-- References will auto-populate in the refs div below -->\n\n",
          "::: {#refs}\n",
          ":::\n\n",
          "# This Section Is an Appendix {#apx-a}\n\n",
          "# Another Appendix {#apx-b}\n"
        )
        
        output$makedocument <- renderUI(
          tags$div(
            style = "border: 1px solid #ccc; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
            rclipButton(inputId = "copythis",
              label = "Copy to clipboard",
              clipText = doc_yaml,
              icon = icon("clipboard"),
              class = "mb-2"
            ),
            verbatimTextOutput("yaml_output", placeholder = TRUE)
          )
        )
        
        output$yaml_output <- renderText(doc_yaml)
      
    
    
    
    
    
  })
  
  
  
}

if (interactive())
  shinyApp(ui, server)
