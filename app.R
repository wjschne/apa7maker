library(conflicted)
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
library(fresh)
library(rmarkdown)

adding_author <- FALSE

current_year <- format(Sys.Date(), "%Y")

mytheme <- create_theme(
  theme = "default",
  bs_vars_button(
    default_color = "#FFF",
    default_bg = "#112446",
    default_border = "#112446",
    border_radius_base = "15px"
  ),
  bs_vars_wells(bg = "#FFF", border = "#112446")
)

conflict_prefer("page", "bslib", "utils", quiet = TRUE)
conflict_prefer_all("dplyr", c("base", "stats"), quiet = TRUE)
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
# A count, written as a whole number. A numericInput hands back a double, and
# yaml::as.yaml writes that as 2.0, which quarto reads as a number but which
# reads oddly in a file a person is meant to edit.
whole <- function(x, unless = NULL) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (is.na(x)) {
    return(NULL)
  }
  if (!is.null(unless) && x == unless) {
    return(NULL)
  }
  as.integer(x)
}

# The four formats the app writes for itself. A document may name others,
# and those are left as they were written.
apa_formats <- c("apaquarto-docx",
                 "apaquarto-html",
                 "apaquarto-typst",
                 "apaquarto-pdf")

# The top-level fields the app writes for itself, and so the ones it is
# entitled to replace. Everything else an imported document carried --- a csl,
# an execute block, knitr options, an editor setting, a crossref, a field
# apaquarto has not got round to yet --- is written back out untouched, so
# that a document can go through the app and come out whole.
#
# affiliations belongs here because the app reads that block, resolves what
# the authors name in it, and writes the result under each author instead.
owned_fields <- c(
  "title",
  "shorttitle",
  "author",
  "author-note",
  "affiliations",
  "abstract",
  "impact-statement",
  "keywords",
  "supplemental-materials",
  "word-count",
  "bibliography",
  "nocite",
  "masked-citations",
  "floatsintext",
  "numbered-lines",
  "mask",
  "no-ampersand-parenthetical",
  "meta-analysis",
  "documentmode",
  "fontsize",
  "papersize",
  "first-page",
  "mainfont",
  "monofont",
  "linenumber-font",
  "keep-introduction-heading",
  "toc-depth",
  "linkcolor",
  "urlcolor",
  "citecolor",
  "filecolor",
  "toccolor",
  "course",
  "professor",
  "duedate",
  "note",
  "journal",
  "thesis",
  "dedication",
  "acknowledgments",
  "acknowledgements",
  "lang",
  "language",
  "blank-lines-above-title",
  "blank-lines-above-author-note",
  "format"
)

# The same, for what sits inside one of the four format blocks. The import
# reads most of these from either place, and the app writes them back at the
# top level, so leaving them here as well would say the same thing twice.
owned_format_fields <- c(
  setdiff(owned_fields, "format"),
  "toc",
  "pdf-standard",
  "notebook-view",
  "crossref",
  "list-of-contents",
  "list-of-figures",
  "list-of-tables",
  "list-of-illustrations"
)

# The fields of a block that the app does not write, in the order they were
# written. A suppress- field is the app's whatever it is called, since the
# whole family is offered on the Format Options tab.
unowned_fields <- function(block, owned) {
  if (!is.list(block)) {
    return(list())
  }
  keys <- names(block)
  if (is.null(keys)) {
    return(list())
  }
  keys <- keys[keys != "" & !is.na(keys)]
  keys <- keys[!(keys %in% owned)]
  keys <- keys[!str_starts(keys, "suppress-")]
  block[keys]
}

# The document's own crossref settings with the app's added to them, rather
# than in place of them: a document may have set fig-title or a float of its
# own, and replacing the block outright would take those away. Where both
# name a float of the same key, the document's is kept, since it was written
# by hand.
merge_crossref <- function(theirs, ours) {
  if (!is.list(theirs)) {
    return(ours)
  }
  if (!is.list(ours)) {
    return(theirs)
  }
  custom <- theirs[["custom"]]
  if (is.null(custom)) {
    custom <- list()
  } else if (!is.null(names(custom))) {
    # One float written without a list around it.
    custom <- list(custom)
  }
  keys <- vapply(custom, \(x) as.character(ornull(x[["key"]], ""))[1], character(1))
  for (entry in ours[["custom"]]) {
    if (!(as.character(entry[["key"]])[1] %in% keys)) {
      custom[[length(custom) + 1]] <- entry
    }
  }
  theirs[["custom"]] <- custom
  theirs
}

# Everything after the YAML of a .qmd: the prose, the headings, the code
# chunks, the refs div, the appendices. rmarkdown::yaml_front_matter reads the
# options and throws this away, so the file is split here as well, and what
# comes back is handed to the writer untouched when the document is rebuilt.
#
# The front matter is the block between the first --- on a line of its own and
# the next --- or ... that closes it. A file with no front matter at all is
# body from its first line.
document_body <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) == 0) {
    return("")
  }
  # A UTF-8 byte order mark would otherwise hide the opening ---.
  lines[1] <- sub("^\uFEFF", "", lines[1])
  
  opening <- which(trimws(lines) != "")[1]
  if (is.na(opening) || trimws(lines[opening]) != "---") {
    return(paste(lines, collapse = "\n"))
  }
  rest <- seq_along(lines) > opening
  closing <- which(rest & trimws(lines) %in% c("---", "..."))[1]
  if (is.na(closing)) {
    return("")
  }
  if (closing == length(lines)) {
    return("")
  }
  
  body <- lines[seq(closing + 1, length(lines))]
  # The blank lines that separate the front matter from the prose belong to
  # neither, and the writer puts its own back.
  while (length(body) > 0 && trimws(body[1]) == "") {
    body <- body[-1]
  }
  paste(body, collapse = "\n")
}

# Whether a yaml field says yes. A field may be written as a bare true, or
# quoted, or spelled yes, and a field that is there but false is not on ---
# `suppress-title-page: false` asks for the title page to be kept.
is_yes <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(FALSE)
  }
  if (is.logical(x)) {
    return(isTRUE(x[1]))
  }
  tolower(trimws(as.character(x[1]))) %in% c("true", "yes")
}

# The first of its arguments that is not NULL, or NULL when none of them is.
ornull <- function(...) {
  for (x in list(...)) {
    if (!is.null(x)) {
      return(x)
    }
  }
  NULL
}

# What is left of a field once the empty parts of it are taken out, or NULL
# when nothing is left, so that the writer can leave the field out.
#
# NA counts as empty: a grid hands back NA for a cell nobody typed in, and
# the old comparison `x == is.na(x)` turned that into NA, which is neither
# TRUE nor FALSE and so stopped the render. A field holding more than one
# value counts too --- two bibliography files made those comparisons two long
# and `if` refuses a condition of that length.
ifempty <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- x[!is.na(x)]
  if (is.character(x)) {
    x <- x[trimws(x) != ""]
  }
  if (length(x) == 0) {
    return(NULL)
  }
  x
}

my_css <- tags$head(tags$style(
  HTML(
    ".btn-left {margin-left: 15px}
    .multicol {
      -webkit-column-count: 3; /* Chrome, Safari, Opera */
      -moz-column-count: 3;    /* Firefox */
      column-count: 3;         /* Standard */
    }
    .inline-input .form-group {
        display: flex;
        align-items: center;
      }
    .inline-input .form-group label {
        width: 60%;
        margin-bottom: 0;
      }
    .inline-input .form-group .form-control {
        width: 40%;
      }"
  )
))

# ui ----
ui <- page_fluid(
  # header = tagList(
  #   use_theme(mytheme)
  # ),
  rclipboardSetup(),
  # theme = bs_theme(version = 5),
  my_css,
  title = "Writing in APA Style, 7th Edition with Quarto",
  h1("APAQUARTO: A Quarto Extension for Writing in APA Style"),
  navset_pill_list(
    id = "main",
    widths = c(2, 10),
    ## title/general ----
    nav_panel(
      title = "Title and Authors",
      fileInput("importqmd", "Import file (optional)", accept = ".qmd"),
      panel(
        heading = "Title",
        status = "primary",
        
        textInput("title", label = "Title", width = "100%"),
        textInput(
          "shorttitle",
          tooltip(
            trigger = list("Short Title", bs_icon("info-circle")),
            "Running text in header. If blank, the running header is the title in upper case."
          ),
          width = "100%"
        ),
      ),
      ## authors ----
      panel(
        heading = "Authors and Affiliations",
        status = "primary",
        tags$h3("Author(s)"),
        fluidRow(column(
          width = 12, datagridOutput2("gd_author", height = "auto")
        )),
        actionButton(inputId = "addAuthor", label = "Add Author"),
        
        tags$h3("Affiliation(s)", style = "margin-top: 12px"),
        
        fluidRow(column(
          width = 12, datagridOutput2("gd_affiliation", height = "auto")
        )),
        actionButton(inputId = "addAffiliation", label = "Add Affiliation"),
        p(
          "Before clicking outside the table, be sure to finish editing a cell by clicking Enter on the keyboard or by clicking another cell within the table."
        )
      ),
      ## author note ----
      panel(
        status = "primary",
        heading = "Author Note",
        tags$h3("Status Changes"),
        textInput(
          "affiliation-change",
          label = "Affiliation Change",
          width = "100%",
          placeholder = "Example: Fred Jones is now at Generic State University."
        ),
        textInput(
          "deceased-note",
          label = "Author Deceased",
          width = "100%",
          placeholder = "Example: Fred Jones is deceased."
        ),
        tags$h3("Disclosures"),
        textInput(
          "study-registration",
          label = "Study Registration",
          width = "100%",
          placeholder = "Example: This study was registered at ClinicalTrials.gov (Identifier NTC998877)."
        ),
        textInput(
          "data-sharing",
          label = "Data Sharing",
          width = "100%",
          placeholder = "Example: Data from this study can be accessed at https://academicdata.org/jones2024."
        ),
        textInput(
          "related-report",
          label = "Related Report",
          width = "100%",
          placeholder = "Example: This article is based on the dissertation completed by Jones (2018)"
        ),
        textInput(
          "conflict-of-interest",
          label = "Conflict of Interest",
          width = "100%",
          placeholder = "Example: Fred Jones has been a paid consultant for Corporation X, which funded this study."
        ),
        textInput(
          "financial-support",
          label = "Financial Support",
          width = "100%",
          placeholder = "Example: This study was supported by Grant 123 from Academic Funders United."
        ),
        textInput(
          "gratitude",
          label = "Gratitude/Acknowledgements",
          width = "100%",
          placeholder = "Example: The authors are grateful to Sidney Fiero for thoughtful comments on an early draft of this paper."
        ),
        textInput(
          "author-agreements",
          label = "Authorships Agreements",
          width = "100%",
          placeholder = "Example: Because the authors are equal contributors, order of authorship was determined by a fair coin toss."
        ),
        textInput(
          "correspondence-note",
          label = "Custom Correspondence Note",
          width = "100%",
          placeholder = "Example: Any text here will override the correspondence note that would otherwise be generated automatically."
        ),
        radioButtons(
          inputId = "author-note-columns",
          label = "Author Note Columns (Journal Mode Only)",
          choices = c("auto", "1", "2"),
          selected = "auto",
          inline = TRUE,
          width = "100%"
        )
      )
    ),
    ## Formats ----
    nav_panel(
      title = "Format Options",
      # General -----
      panel(
        heading = "General Options",
        status = "primary",
        selectInput(
          "papersize",
          label = "Paper Size",
          choices = c(
            `8.5 × 11in (letter)` = "letter",
            A3 = "a3",
            A4 = "a4",
            A5 = "a5",
            B5 = "b5",
            Executive = "executive",
            Legal = "legal",
            Tabloid = "tabloid"
          ),
          selected = "letter",
          width = "100%"
        ),
        checkboxInput(
          "floatsintext",
          label = "Plots and figures appear in text instead at the end.",
          value = TRUE,
          width = "100%"
        ),
        checkboxInput(
          "numbered-lines",
          label = tooltip(
            trigger = list("Numbered lines", bs_icon("info-circle")),
            "Not available in .html format"
          ),
          value = FALSE,
          width = "100%"
        ),
        checkboxInput("keep-introduction-heading", label = tooltip(
          trigger = list("Keep the Introduction heading", bs_icon("info-circle")),
          "A level-one heading reading Introduction at the start of the body is taken out, because APA does not label the introduction. Tick this to keep it."
        )),
        ## Citations ----
        panel(
          heading = "Citations and References",
          checkboxInput(
            "no-ampersand-parenthetical",
            label = tooltip(
              trigger = list(
                "Use \"and\" in parenthetical citations.",
                bs_icon("info-circle")
              ),
              "If checked, the word \"and\" (or its replacement value in the language options) will appear in parenthetical citions. For example, (Schneider and McGrew, 2018) instead of (Schneider & McGrew, 2018). For standard APA citations, leave this box unchecked."
            ),
            value = FALSE,
            width = "100%"
          ),
          selectizeInput(
            inputId = "bibliography",
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
          
          checkboxInput(
            "mask",
            label = tooltip(
              trigger = list("Mask authors", bs_icon("info-circle")),
              "If checked, omits title page and masks any citations listed in box below."
            ),
            value = FALSE,
            width = "100%"
            
          ),
          selectizeInput(
            inputId = "masked-citations",
            label = "A list of citation keys that should be masked if `mask` is checked. Enter each key.",
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
              trigger = list("List of reference-only citations", bs_icon("info-circle")),
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
            label = span("Reference-only citations are meta-analytic citations."),
            value = TRUE,
            width = "100%"
          )
        ),
        ## Content Lists ----
        panel(
          heading = "Content Lists",
          fluidRow(
            column(3, checkboxInput(
              "list-of-contents",
              label = tooltip(
                trigger = list("Table of Contents", bs_icon("info-circle")),
                "A table of contents in every format. In .html it is added only when Quarto's own contents is not already in the margin, so this turns that one off."
              )
            )),
            column(3, checkboxInput("list-of-figures", label = "List of Figures", )),
            column(3, checkboxInput("list-of-tables", label = "List of Tables")),
            column(3, checkboxInput(
              "list-of-illustrations",
              label = tooltip(
                trigger = list("List of Illustrations", bs_icon("info-circle")),
                "An Illustration is a float of its own, numbered apart from the figures. Tick \"Declare an Illustration float\" below to use one."
              )
            ))
          ),
          checkboxInput(
            "illustration-float",
            label = tooltip(
              trigger = list("Declare an Illustration float", bs_icon("info-circle")),
              "A float a document declares for itself is set the way a figure is, with its number on a line of its own, its caption under it, and its apa-note below that. Written into each format block, because Quarto drops latex-env from an extension's shared options."
            )
          ),
          numericInput(
            "toc-depth",
            label = tooltip(
              trigger = list("Contents depth", bs_icon("info-circle")),
              "How many levels of heading the table of contents lists."
            ),
            value = 3,
            min = 1,
            max = 6,
            width = "200px"
          )
        ),
        ## fonts and colours ----
        panel(
          heading = "Fonts and Font Families",
          radioButtons(
            "fontsize",
            label = "Font Size",
            inline = TRUE,
            choices = c(`10` = "10pt", `11` = "11pt", `12` = "12pt"),
            selected = "12pt",
            width = "100%"
          ),
          fluidRow(
            column(
              width = 4,
              textInput(
                "mainfont",
                "Main font",
                width = "100%",
                placeholder = "Example: Times New Roman"
              )
            ),
            column(
              width = 4,
              textInput(
                "monofont",
                "Monospace font",
                width = "100%",
                placeholder = "Example: Consolas"
              )
            ),
            column(
              width = 4,
              textInput(
                "linenumber-font",
                tooltip(
                  trigger = list("Line number font", bs_icon("info-circle")),
                  "The font of the numbers that numbered-lines puts in the margin, in apaquarto-typst only."
                ),
                width = "100%",
                placeholder = "Default: DejaVu Sans Mono"
              )
            )
          )
        ),
        panel(
          heading = "Link Colors",
          p(
            "Name a colour the way LaTeX names it \u2014 one of ",
            tags$a(
              tags$code("xcolor"),
              href = "https://www.overleaf.com/learn/latex/Using_colors_in_LaTeX",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            "'s own, such as ",
            tags$code("teal"),
            ", or an HTML code such as ",
            tags$code("#cc0000"),
            ". Left blank, a link takes apaquarto's ",
            tags$code("#0074D9"),
            "."
          ),
          fluidRow(
            column(
              2,
              textInput(
                "linkcolor",
                placeholder = "#0074D9",
                tooltip(
                  trigger = list("Links", bs_icon("info-circle")),
                  "A link inside the document: a cross reference, or a link to a heading."
                ),
                width = "100%"
                
              )
            ),
            column(
              2,
              textInput(
                "urlcolor",
                placeholder = "#0074D9",
                tooltip(
                  trigger = list("URLs", bs_icon("info-circle")),
                  "A link that leaves the document, an email address included."
                ),
                width = "100%"
              )
            ),
            column(
              2,
              textInput(
                "citecolor",
                placeholder = "#0074D9",
                tooltip(
                  trigger = list("Citations", bs_icon("info-circle")),
                  "A citation, which points at the reference list."
                ),
                width = "100%"
              )
            ),
            column(
              2,
              textInput(
                "filecolor",
                placeholder = "#0074D9",
                tooltip(trigger = list("Files", bs_icon("info-circle")), "A link to a file."),
                width = "100%"
              )
            ),
            column(
              2,
              textInput(
                "toccolor",
                placeholder = "#0074D9",
                tooltip(
                  trigger = list("Table of Contents", bs_icon("info-circle")),
                  "The entries of the lists of contents, figures and tables. Body-text black by default, so that a list reads as a list rather than as a page of links."
                ),
                width = "100%"
              )
            )
          )
        )
      ),
      panel(
        heading = "Format-Specific Options",
        status = "primary",
        tags$div(
          width = "100%",
          class = "container p-0 m-0",
          tags$div(
            class = "row align-items-center border-bottom py-1, px-0",
            tags$div(tags$strong("Option"), class = "col-5"),
            tags$div(class = "col-3"),
            tags$div(tags$strong("Word"), class = "col-1 text-center p-0"),
            tags$div(tags$strong("Web"), class = "col-1 text-center p-0"),
            tags$div(tags$strong("LaTeX"), class = "text-center", class = "col-1 text-center p-0"),
            tags$div(tags$strong("Typst"), class = "col-1 text-center p-0")
          ),
          tags$div(
            class = "row align-items-center border-bottom p-1",
            tags$div(
              tooltip(
                trigger = list("First page number", bs_icon("info-circle")),
                "The number the first page carries, and the number the pages count on from. Useful for a chapter or an article that begins partway into a volume."
              ),
              class = "col-5"
            ),
            tags$div(
              numericInput(
                "first-page",
                label = NULL,
                value = 1,
                min = 1,
                width = "75px"
              ),
              class = "col-3"
            ),
            tags$div("", class = "col-1 text-center"),
            tags$div("", class = "col-1 text-center"),
            tags$div(icon("check"), class = "col-1 text-center"),
            tags$div(icon("check"), class = "col-1 text-center")
          ),
          tags$div(
            class = "row align-items-center border-bottom p-1",
            tags$div("Number of blank lines above title", class = "col-5"),
            tags$div(
              numericInput(
                "blank-lines-above-title",
                label = NULL,
                value = 2,
                min = 0,
                width = "75px"
              ),
              class = "col-3"
            ),
            tags$div(icon("check"), class = "col-1 text-center"),
            tags$div("", class = "col-1 text-center"),
            tags$div("", class = "col-1 text-center"),
            tags$div(icon("check"), class = "col-1 text-center")
          ),
          tags$div(
            class = "row align-items-center border-bottom p-1",
            tags$div("Lines between Author Names and Notes", class = "col-5"),
            tags$div(
              numericInput(
                "blank-lines-above-author-note",
                label = NULL,
                value = 2,
                min = 0,
                width = "75px"
              ),
              class = "col-3"
            ),
            tags$div(icon("check"), class = "col-1 border-primary text-center"),
            tags$div("", class = "col-1 border-primary text-center"),
            tags$div("", class = "col-1 border-primary text-center"),
            tags$div(icon("check"), class = "col-1 border-primary text-center")
          ),
          tags$div(
            class = "row align-items-center border-bottom p-1",
            tags$div(
              tooltip(
                trigger = list("PDF accessibility", bs_icon("info-circle")),
                "The .pdf is built with lualatex, so a screen-reader standard can be asked for. flextable tables are not yet ua-2 compliant and will stop the build."
              ),
              class = "col-5"
            ),
            tags$div(
              class = "col-3",
              selectInput(
                "pdf-standard",
                label = NULL,
                choices = c(
                  None = "",
                  `ua-2` = "ua-2",
                  `a-2b` = "a-2b"
                ),
                selected = "",
                width = "100%"
              )
            ),
            tags$div("", class = "col-1 text-center"),
            tags$div("", class = "col-1 text-center"),
            tags$div(icon("check"), class = "col-1 text-center"),
            tags$div("", class = "col-1 text-center")
          ),
          tags$div(
            class = "row align-items-center p-1",
            tags$div(
              tooltip(
                trigger = list("Notebook view", bs_icon("info-circle")),
                "The apaquarto-html no longer builds a standalone preview page for a document pulled in with an embed. Tick this to get the Source: link and its preview page back."
              ),
              class = "col-5"
            ),
            tags$div(class = "col-3", checkboxInput("notebook-view", label = NULL)),
            tags$div("", class = "col-1 text-center"),
            tags$div(icon("check"), class = "col-1 text-center"),
            tags$div("", class = "col-1 text-center"),
            tags$div("", class = "col-1 text-center")
          )
        )
      ),
      ## Suppress ----
      panel(
        heading = "Suppress Document Elements",
        status = "primary",
        tags$div(
          align = 'left',
          class = "multicol",
          checkboxGroupInput(
            inputId = "suppress",
            label = NULL,
            inline = FALSE,
            choices = c(
              `Title Page` = "suppress-title-page",
              `Title Page Number` = "suppress-title-page-number",
              Title = "suppress-title",
              `Short Title` = "suppress-short-title",
              `Title in Introduction` = "suppress-title-introduction",
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
              `Corresponding Country` = "suppress-corresponding-country",
              `Corresponding Email` = "suppress-corresponding-email",
              `Abstract` = "suppress-abstract",
              `Impact Statement` = "suppress-impact-statement",
              `Keywords` = "suppress-keywords",
              `Supplemental Materials` = "suppress-supplemental-materials"
            )
          )
        )
      )
  ),
  ### Mode ----
  nav_panel(
    title = "Mode/Output",
    panel(
      heading = "Document Mode and Output Format",
      status = "primary",
      checkboxGroupInput(
        "formattype",
        label = "Output Formats",
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
      radioButtons(
        inline = TRUE,
        "documentmode",
        label = "Document mode",
        choices = c(
          Manuscript = "manuscript",
          Journal = "journal",
          `LaTeX Style Document` = "document",
          Student = "student",
          `Thesis/Dissertation` = "thesis"
        ),
        selected = "manuscript"
      ),
      tabsetPanel(
        id = "modeoptions", 
        selected = "tb-journal",
        ### Journal ----
        tabPanel(
          title = "Journal",
          value = "tb-journal",
          p(
            "Journal and LaTeX Style Document work in apaquarto-typst and apaquarto-pdf. Other document modes work in all output formats."
          ),
          panel(
            heading = "Journal Mode Options (apaquarto-pdf and apaquarto-typst)",
            textInput(
              "journal-title",
              "Journal Title",
              width = "100%",
              placeholder = "Example: Journal of Educational Psychology"
            ),
            fluidRow(
              column(
                4,
                textInput(
                  "journal-volume",
                  "Journal Volume",
                  width = "100%",
                  placeholder = "Example: 10"
                )
              ),
              column(
                4,
                textInput(
                  "journal-issue",
                  "Journal Issue Number",
                  width = "100%",
                  placeholder = "Example: 2"
                )
              ),
              column(
                4,
                textInput(
                  "journal-pages",
                  "Journal Pages",
                  width = "100%",
                  placeholder = "Example: 10--60"
                )
              )
            ),
            fluidRow(
              column(4,textInput(
                "journal-year",
                "Publication Year",
                width = "100%",
                placeholder = paste0("Example: ", current_year)
              )),
              column(4,textInput(
                "journal-url",
                "url/doi",
                width = "100%",
                placeholder = "Example: https://doi.org/10.32614/CRAN.package.apa7"
              )),
              column(4,textInput(
                "journal-logo",
                "Journal Logo",
                width = "100%",
                placeholder = "Examples: default or file name (e.g., logo.png)"
              ))
            ),
            fluidRow(
              column(
                4,
                textInput(
                  "journal-copyrightnotice",
                  "Copyright Year",
                  width = "100%",
                  placeholder = paste0("Example: ", current_year)
                )
              ),
              column(
                4,
                textInput(
                  "journal-copyrighttext",
                  "Copyright Text",
                  width = "100%",
                  placeholder = "Example: The Author(s). All rights reserved."
                )
              ),
              column(
                4,
                textInput(
                  "journal-issn",
                  "ISSN",
                  width = "100%",
                  placeholder = "Example: 0022-0663"
                )
              )
            ))
        ),
        ### Student ----
        tabPanel(
          value = "tb-student", 
          title = "Student",
          panel(
            heading = "Student Paper Options",
            fluidRow(
              column(
                3,
                textInput("course", "Course", width = "100%", placeholder = "Example: Introduction to Statistics (EDUC 5101)")
              ),
              column(
                3,
                textInput(
                  "professor",
                  "Professor",
                  width = "100%",
                  placeholder = "Example: W. Joel Schneider"
                )
              ),
              column(
                3,
                textInput(
                  "student-note",
                  "Student Paper Note",
                  width = "100%",
                  placeholder = "Example: Student ID: 12345"
                )
              ),
              column(
                3,
                dateInput("duedate", "Due Date", width = "100%", format = "yyyy-mm-dd")
              )
            ),
            checkboxInput("includeduedate", "Include due date")
            
            
            
          )
                 ),
        ### Dissertation ----
        tabPanel(
          value = "tb-dissertation",
          title = "Dissertation",
          panel(
            heading = "Dissertation/Thesis Options",
            p(
              "Set ",
              tags$strong("Document Mode"),
              " to Thesis/Dissertation on the ",
              tags$strong("Format Options"),
              " tab, then fill in what belongs on the title page. Every field is optional: a field left out leaves its part of the page out. ",
              tags$a(
                "(More information)",
                href = "https://wjschne.github.io/apaquarto/options.html#dissertations-and-theses",
                rel = "noopener noreferrer",
                target = "_blank"
              )
            ),
            p(
              "apaquarto follows the Temple University Graduate School's 2024\u20132025 handbook. Confirm the current requirements with the Graduate School before submitting."
            ),
            radioButtons(
              inline = TRUE,
              "thesis-type",
              label = tooltip(
                trigger = list("Type of work", bs_icon("info-circle")),
                "The title page reads \"A Dissertation\" or \"A Thesis\"."
              ),
              choices = c(
                Dissertation = "Dissertation",
                Thesis = "Thesis",
                `Dissertation Proposal` = "Dissertation Proposal",
                `Thesis Proposal` = "Thesis Proposal"
              ),
              selected = "Dissertation"
            ),
            textInput(
              "thesis-submitted-to",
              "Submitted to",
              width = "100%",
              placeholder = "Default: the Temple University Graduate Board"
            ),
            textInput(
              "thesis-degree",
              tooltip(
                trigger = list("Degree", bs_icon("info-circle")),
                "The degree the work answers to, as Student Services Banner names it."
              ),
              width = "100%",
              placeholder = "Example: Doctor of Philosophy"
            ),
            textInput(
              "thesis-date",
              tooltip(
                trigger = list("Date", bs_icon("info-circle")),
                "The diploma date, not the date of the defence."
              ),
              width = "100%",
              placeholder = "Example: May 2027"
            ),
            textInput(
              "thesis-copyright",
              tooltip(
                trigger = list("Copyright year", bs_icon("info-circle")),
                "Taken from the year in the date above when left blank."
              ),
              width = "100%",
              placeholder = "Example: 2027"
            ),
            checkboxInput(
              "thesis-copyright-suppress",
              "Leave the copyright page out",
              value = FALSE,
              width = "100%"
            )
          ),
          panel(
            heading = "Examining Committee",
            p(
              "In the order they are to be listed. Each member is set as one line, with commas between the name, the role and the affiliation."
            ),
            fluidRow(column(
              width = 12, datagridOutput2("gd_committee", height = "auto")
            )),
            actionButton(inputId = "addCommittee", label = "Add Committee Member"),
            p(
              "Before clicking outside the table, be sure to finish editing a cell by clicking Enter on the keyboard or by clicking another cell within the table."
            )
          ),
          panel(
            heading = "Dedication and Acknowledgments",
            textAreaInput(
              "dedication",
              label = tooltip(
                trigger = list("Dedication", bs_icon("info-circle")),
                "Centred on a page of its own, with no heading."
              ),
              width = "100%",
              rows = 4,
              resize = "vertical"
            ),
            textAreaInput(
              "acknowledgments",
              label = tooltip(
                trigger = list("Acknowledgments", bs_icon("info-circle")),
                "Set under a heading of its own."
              ),
              width = "100%",
              rows = 6,
              resize = "vertical"
            )
          )
          
                 )
        
                  ),





   
      
    )
  ),
  ## Abstract ----
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
        "impact-statement",
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
      textInput(
        "supplemental-materials",
        width = "100%",
        label = tooltip(
          trigger = list("Supplemental Materials", bs_icon("info-circle")),
          "Usually a URL link to a file or webpage with a additional information."
        )
      ),
      checkboxInput(
        "word-count",
        label = tooltip(
          trigger = list("Word Count", bs_icon("info-circle")),
          "This option is available as a convenience. Strict APA style does not include a word count."
        )
      )
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
          tags$a(
            "(More information)",
            href = "https://quarto.org/docs/authoring/language.html",
            rel = "noopener noreferrer",
            target = "_blank"
          )
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
        a(
          "More information",
          href = "https://wjschne.github.io/apaquarto/options.html#language-options",
          rel = "noopener noreferrer",
          target = "_blank"
        )
      ),
      textInput(
        "citation-last-author-separator",
        "Separator for the last author in narrative citations. For example, Smith, Davis, and Jones (2025)",
        placeholder = "Default: and",
        width = "100%"
      ),
      textInput(
        "citation-masked-author",
        "Replacement phrase for masked citations",
        placeholder = "Default:  Masked Citation",
        width = "100%"
      ),
      textInput(
        "citation-masked-date",
        "Replacement phrase for date in masked citations",
        placeholder = "Default: n.d.",
        width = "100%"
      ),
      textInput(
        "title-block-author-note",
        "The heading of the author note",
        placeholder = "Default: Author Note",
        width = "100%"
      ),
      textInput(
        "title-block-correspondence-note",
        "Correspondence introduction",
        placeholder = "Default: Correspondence concerning this article should be addressed to",
        width = "100%"
      ),
      textInput(
        "title-block-role-introduction",
        "The phrase introducing the author roles",
        placeholder = "Default: Author roles were classified using the Contributor Role Taxonomy (CRediT; https://credit.niso.org/) as follows:",
        width = "100%"
      ),
      textInput(
        "title-impact-statement",
        "Impact statement heading",
        placeholder = "Default: Impact Statement",
        width = "100%"
      ),
      textInput(
        "title-supplemental-materials",
        "Supplemental materials heading",
        placeholder = "Default: Supplemental Matterials",
        width = "100%"
      ),
      textInput(
        "title-word-count",
        span(
          "The phrase before the word count when",
          tags$code("word-count: true")
        ),
        placeholder = "Default: Word Count",
        width = "100%"
      ),
      textInput(
        "references-meta-analysis",
        "Explanation for meta-analytic explanations",
        placeholder = "Default: References marked with an asterisk indicate studies included in the meta-analysis.",
        width = "100%"
      ),
      textInput(
        "section-title-introduction",
        tooltip(
          trigger = list(
            "The heading apaquarto takes out of the body",
            bs_icon("info-circle")
          ),
          "A level-one heading of this name at the start of the body is removed, because APA does not label the introduction. Name it in your language so that the heading is matched."
        ),
        placeholder = "Default: Introduction",
        width = "100%"
      ),
      textInput(
        "figure-panel",
        tooltip(
          trigger = list("The word for a figure panel", bs_icon("info-circle")),
          "The panels of a multipanel figure are labelled Panel A, Panel B, and so on."
        ),
        placeholder = "Default: Panel",
        width = "100%"
      ),
      textInput(
        "language-journal-volume",
        tooltip(
          trigger = list("The word for a volume", bs_icon("info-circle")),
          "The word before the volume number in the journal masthead."
        ),
        placeholder = "Default: Vol.",
        width = "100%"
      ),
      textInput(
        "language-journal-issue",
        tooltip(
          trigger = list("The word for an issue", bs_icon("info-circle")),
          "The word before the issue number in the journal masthead."
        ),
        placeholder = "Default: No.",
        width = "100%"
      ),
      textInput(
        "figure-table-note",
        tooltip(
          trigger = list("The word introducing a note", bs_icon("info-circle")),
          "The word before the full stop in the note under a figure, a table or an illustration."
        ),
        placeholder = "Default: Note",
        width = "100%"
      )
    )
  ),
  ## body ----
  nav_panel(
    title = "Document Body",
    panel(
      heading = "Everything Below the Options",
      status = "primary",
      p(
        "Importing a .qmd puts the whole of the document below its YAML in here \u2014 the prose, the headings, the code chunks, the reference div and the appendices \u2014 and it is written back out underneath the options unchanged. So a document can be brought in, its options changed here, and the result pasted back over the original."
      ),
      p(
        "Left empty, the app writes a skeleton of APA headings instead, as it always has."
      ),
      textAreaInput(
        "body",
        label = NULL,
        width = "100%",
        rows = 24,
        resize = "vertical",
        placeholder = "# Method\n\n## Participants\n\nImport a .qmd above, or write here, or leave this empty for a skeleton."
      ),
      actionButton("clearbody", label = "Clear", class = "mb-2"),
      p("Clearing this brings the skeleton back.")
    )
  ),
  ## make yaml----
  nav_panel(
    title = "Make Document",
    panel(
      heading = "Make Document",
      status = "primary",
      tags$ol(
        tags$li(
          tags$span("In your project folder, install apaquarto "),
          tags$a(
            "(Instructions here).",
            href = "https://wjschne.github.io/apaquarto/installation.html",
            rel = "noopener noreferrer",
            target = "_blank"
          )
        ),
        tags$li("Create an empty .qmd file."),
        tags$li("Click Update button below."),
        tags$li("Paste resulting code into your .qmd file."),
        tags$li("Write a great paper!")
      ),
      actionButton("btnmakedocument", label = "Update", class = "mb-2"),
      tags$br(),
      uiOutput("makedocument")
    )
  )
)
)
# server ----
server <- function(input, output, session) {
  d_author <- read_csv("author.csv", col_types = "icccllcccccccccccccc")
  
  d_affiliation <- read_csv("affiliation.csv", col_types = "iiccccccccc")
  
  # The examining committee of a dissertation. Held the way
  # the affiliations are, so that a member can be added and deleted in the
  # grid, and written out in the order the rows are in.
  d_committee <- tibble(
    committee_id = 1L,
    committee_name = NA_character_,
    committee_role = NA_character_,
    committee_affiliation = NA_character_
  )
  
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
  r_committee <- reactiveVal(d_committee)
  # The document mode the four list checkboxes were last set for. An import
  # writes the mode it read straight in here, so that the observer below sees
  # no change and leaves the lists the imported file asked for alone.
  r_listmode <- reactiveVal(NULL)
  r_affiliation_current <- reactiveVal(d_affiliation)
  r_affiliation <- reactiveVal(d_affiliation)
  r_yaml <- reactiveVal("")
  # The front matter of the last file imported, kept whole so that the
  # fields the app has no box for can be written back out with the rest.
  r_frontmatter <- reactiveVal(NULL)
  
  # gd_author ----
  output$gd_author <- renderDatagrid2({
    datagrid(
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
  
  # gd_committee ----
  output$gd_committee <- renderDatagrid2(
    datagrid(
      r_committee(),
      colwidths = "guess",
      colnames = c("Delete", "Name", "Role", "Affiliation"),
      bodyHeight = "auto",
      editingEvent = "click",
      sortable = FALSE
    ) |>
      grid_col_button(
        "committee_id",
        inputId = "committee_delete",
        label = "Delete",
        icon = icon("trash")
      ) |>
      grid_columns(
        column = c("committee_name", "committee_role"),
        width = c(300, 250)
      ) |>
      grid_columns(column = c("committee_id"), width = 80) |>
      grid_editor(column = "committee_name", type = "text") |>
      grid_editor(column = "committee_role", type = "text") |>
      grid_editor(column = "committee_affiliation", type = "text") |>
      grid_click("committee_click")
  )
  
  # the lists a mode carries by default----
  #
  # A dissertation carries a table of contents and a list for each kind of
  # float unless it says otherwise, which is the other way round from every
  # other mode. The boxes are set to match whenever the mode changes, so that
  # what they show is what the document will do; the YAML then records only
  # the departures from it. Nothing is touched on the first pass, or when an
  # import has already said what the lists are to be.
  observeEvent(input$documentmode, {
    previous <- r_listmode()
    r_listmode(input$documentmode)
    if (is.null(previous) ||
        identical(previous, input$documentmode)) {
      return()
    }
    thesis_mode <- identical(input$documentmode, "thesis")
    for (fd in c(
      "list-of-contents",
      "list-of-figures",
      "list-of-tables",
      "list-of-illustrations"
    )) {
      updateCheckboxInput(session = session,
                          inputId = fd,
                          value = thesis_mode)
    }
  })
  
  # add committee member----
  observeEvent(input$addCommittee, {
    current <- input$gd_committee_data
    if (is.data.frame(current) && nrow(current) > 0) {
      current$rowKey <- NULL
      r_committee(as_tibble(current))
    }
    new_id <- ifelse(nrow(r_committee()) == 0, 1L, as.integer(max(r_committee()$committee_id)) + 1L)
    new_row <- bind_rows(d_committee |> filter(FALSE), tibble(committee_id = new_id))
    r_committee(bind_rows(r_committee(), new_row))
    grid_proxy_add_row("gd_committee", new_row)
  })
  
  # delete committee member----
  observeEvent(input$committee_delete, {
    delete_id <- as.numeric(input$committee_delete)
    r_committee(r_committee() |>
                  filter(committee_id != delete_id))
    data <- input$gd_committee_data
    rowKey <- data$rowKey[data$committee_id == delete_id]
    grid_proxy_delete_row(proxy = "gd_committee", rowKey)
  })
  
  author_row <- function(i) {
    current_author_id = NA
    d_author_current <- input$gd_author_data
    if (length(d_author_current) == 0) {
      return(NULL)
    }
    if (is.null(d_author_current)) {
      d_author_current <- d_author
    }
    
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
    if (!adding_author) {
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
    }
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
      current_author_id <- d_author_current |>
        slice(author_select()) |>
        pull(author_id)
      
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
  
  # clear the body----
  observeEvent(input$clearbody, {
    updateTextAreaInput(session = session,
                        inputId = "body",
                        value = "")
  })
  
  # One affiliation, as the fields quarto reads. Anything the row does not
  # carry is left out rather than written as an empty value, and the column
  # name is given the spelling quarto uses: postal_code is postal-code there,
  # and a column is a column name only because R would not take the hyphen.
  affiliation_fields <- function(row) {
    out <- list()
    for (column in colnames(row)) {
      if (column %in% c("affiliation_id", "author_id", "rowKey")) {
        next
      }
      value <- row[[column]]
      if (length(value) == 0 ||
          all(is.na(value)) || all(trimws(value) == "")) {
        next
      }
      field <- column |>
        str_remove("^affiliation_") |>
        str_replace_all("_", "-")
      out[[field]] <- as.character(value)[1]
    }
    out
  }
  
  # make document ----
  build_document <- function() {
    author_row(1)
    
    d_author_current <- input$gd_author_data
    if (is.null(d_author_current)) {
      d_author_current <- d_author
    }
    author_yaml <- NA
    
    if (!is.null(d_author_current) & length(d_author_current) > 0) {
      if (nrow(d_author_current) > 0) {
        d_author_current$rowKey <- NULL
        author_yaml <- d_author_current |>
          mutate(author_name = ifelse(
            is.na(author_name),
            "Firstname Middlename Lastname",
            author_name
          )) |>
          pivot_longer(starts_with("role"), names_to = "role") |>
          mutate(role = str_replace_all(str_remove(role, "role_"), "_", " ")) |>
          nest(.by = -c(role, value), .key = "role") |>
          mutate(role = map(role, \(d) {
            d <- d |>
              filter(value != "No")
            if (nrow(d) == 0) {
              return(NA)
            }
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
            d <- d[, d |> apply(MARGIN = 2, \(x) ! all(is.na(x)))]
            if (all(!d$deceased)) {
              d$deceased <- NULL
            }
            if (all(!d$corresponding)) {
              d$corresponding <- NULL
            }
            x <- as.list(d)
            if ("role" %in% colnames(d)) {
              if (!all(is.na(d$role))) {
                x$role <- d$role[[1]]
              }
            }
            
            if (nrow(r_affiliation()) > 0) {
              r_affiliation(
                rows_update(
                  r_affiliation(),
                  as_tibble(input$gd_affiliation_data) %>%
                    select(-rowKey) %>%
                    mutate(across(
                      c(affiliation_id, author_id), as.integer
                    ), across(
                      -c(affiliation_id, author_id), as.character
                    )),
                  by = "affiliation_id"
                )
              )
              
              d_aff <- r_affiliation() |>
                filter(author_id == i)
              
              if (nrow(d_aff) > 0) {
                l_affiliation <- d_aff |>
                  rowwise() |>
                  group_split() |>
                  lapply(affiliation_fields) |>
                  Filter(f = \(a) length(a) > 0)
                if (length(l_affiliation) > 0) {
                  x$affiliation <- l_affiliation
                }
              }
            }
            
            x
          })) |>
          select(-id)
      }
    }
    
    nocite <- NULL
    if (length(input$nocite) > 0) {
      nocite <- lapply(input$nocite, \(x) {
        if (!str_detect(x, "^\\@")) {
          x <- paste0("@", x)
        }
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
    
    if (length(doc_list$title) == 0) {
      doc_list$title <- "My Title"
    }
    
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
        deceased = ifempty(input$`deceased-note`)
      ),
      disclosures = list(
        `study-registration` = ifempty(input$`study-registration`),
        `data-sharing` = ifempty(input$`data-sharing`),
        `related-report` = ifempty(input$`related-report`),
        `conflict-of-interest` = ifempty(input$`conflict-of-interest`),
        `financial-support` = ifempty(input$`financial-support`),
        `gratitude` = ifempty(input$`gratitude`),
        `authorship-agreements` = ifempty(input$`author-agreements`)
      ),
      `correspondence-note` = ifempty(input$`correspondence-note`),
      `author-note-columns` = input$`author-note-columns`
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
        `supplemental-materials` = ifempty(input$`supplemental-materials`),
        `word-count` = input$`word-count`
      ),
      after = 4
    )
    
    # Options that belong to the document rather than to one format. Quarto
    # hands a top-level field to every format, and a format that has no use
    # for one ignores it, so the document mode, the fonts, the paper and the
    # link colours are written here rather than repeated per format.
    doc_list$documentmode <- input$documentmode
    doc_list$fontsize <- ifempty(input$fontsize)
    doc_list$`first-page` <- whole(input$`first-page`, unless = 1)
    doc_list$mainfont <- ifempty(input$mainfont)
    doc_list$monofont <- ifempty(input$monofont)
    doc_list$`linenumber-font` <- ifempty(input$`linenumber-font`)
    if (isTRUE(input$`keep-introduction-heading`)) {
      doc_list$`keep-introduction-heading` <- TRUE
    }
    doc_list$`toc-depth` <- whole(input$`toc-depth`, unless = 3)
    
    
    for (cl in c("linkcolor",
                 "urlcolor",
                 "citecolor",
                 "filecolor",
                 "toccolor")) {
      doc_list[[cl]] <- ifempty(input[[cl]])
    }
    
    doc_list$`masked-citations` <- input$`masked-citations`
    
    # The student-paper fields, which since 6.0.1 are read in all four
    # formats rather than in the two pdf ones alone.
    if (input$documentmode == "stu") {
      doc_list$course <- ifempty(input$course)
      doc_list$professor <- ifempty(input$professor)
      if (isTRUE(input$includeduedate)) {
        doc_list$duedate <- as.character(as.Date(input$duedate))
      }
      doc_list$note <- ifempty(input$`student-note`)
    }
    
    # The journal masthead. Quarto reserves `journal` as an object, so the
    # name goes under `title` rather than on `journal` itself, and version 6
    # assembles the issue line out of the year, the volume, the issue and the
    # pages rather than leaving it to be written by hand.
    if (input$documentmode == "journal" ||
        input$documentmode == "jou") {
      journal <- list(
        title = ifempty(input$`journal-title`),
        logo = ifempty(input$`journal-logo`),
        year = ifempty(input$`journal-year`),
        volume = ifempty(input$`journal-volume`),
        issue = ifempty(input$`journal-issue`),
        pages = ifempty(input$`journal-pages`),
        url = ifempty(input$`journal-url`),
        issn = ifempty(input$`journal-issn`),
        copyrightnotice = ifempty(input$`journal-copyrightnotice`),
        copyrighttext = ifempty(input$`journal-copyrighttext`)
      )
      journal[lapply(journal, length) == 0] <- NULL
      if (length(journal) > 0) {
        doc_list$journal <- journal
      }
    }
    
    # A dissertation or thesis, Every field is optional:
    # a field left out leaves its part of the title page out.
    if (input$documentmode == "thesis") {
      committee <- input$gd_committee_data
      if (!is.data.frame(committee) || nrow(committee) == 0) {
        committee <- r_committee()
      }
      l_committee <- NULL
      if (is.data.frame(committee) && nrow(committee) > 0) {
        committee$rowKey <- NULL
        l_committee <- committee |>
          rowwise() |>
          group_split() |>
          lapply(\(d) {
            member <- list(
              name = ifempty(d$committee_name),
              role = ifempty(d$committee_role),
              affiliation = ifempty(d$committee_affiliation)
            )
            member[lapply(member, length) == 0] <- NULL
            member
          })
        l_committee[lapply(l_committee, length) == 0] <- NULL
        if (length(l_committee) == 0) {
          l_committee <- NULL
        }
      }
      
      thesis <- list(
        type = ifempty(input$`thesis-type`),
        `submitted-to` = ifempty(input$`thesis-submitted-to`),
        degree = ifempty(input$`thesis-degree`),
        date = ifempty(input$`thesis-date`),
        copyright = ifempty(input$`thesis-copyright`),
        committee = l_committee
      )
      if (isTRUE(input$`thesis-copyright-suppress`)) {
        thesis$copyright <- FALSE
      }
      thesis[lapply(thesis, length) == 0] <- NULL
      if (length(thesis) > 0) {
        doc_list$thesis <- thesis
      }
      doc_list$dedication <- ifempty(input$dedication)
      doc_list$acknowledgments <- ifempty(input$acknowledgments)
    }
    
    doc_list$lang <- input$lang
    language <- list()
    language$`citation-last-author-separator` <- ifempty(input$`citation-last-author-separator`)
    language$`citation-masked-author` <- ifempty(input$`citation-masked-author`)
    language$`citation-masked-date` <- ifempty(input$`citation-masked-date`)
    
    language$`title-block-author-note` <- ifempty(input$`title-block-author-note`)
    language$`title-block-correspondence-note` <- ifempty(input$`title-block-correspondence-note`)
    language$`title-block-role-introduction` <- ifempty(input$`title-block-role-introduction`)
    language$`title-impact-statement` <- ifempty(input$`title-impact-statement`)
    language$`title-supplemental-materials` <- ifempty(input$`title-supplemental-materials`)
    language$`title-word-count` <- ifempty(input$`title-word-count`)
    language$`references-meta-analysis` <- ifempty(input$`references-meta-analysis`)
    # Added in apaquarto 6
    language$`section-title-introduction` <- ifempty(input$`section-title-introduction`)
    language$`figure-panel` <- ifempty(input$`figure-panel`)
    language$`figure-table-note` <- ifempty(input$`figure-table-note`)
    language$`journal-volume` <- ifempty(input$`language-journal-volume`)
    language$`journal-issue` <- ifempty(input$`language-journal-issue`)
    
    language[lapply(language, length) == 0] <- NULL
    if (length(language) > 0) {
      doc_list$language <- language
    }
    
    # The paper, which typst names differently from the other three: it wants
    # us-letter where .docx and LaTeX want letter, and iso-b5 where they want
    # b5. So the size is written into each format block in the spelling that
    # format understands rather than once at the top. Letter is what every
    # format takes when nothing says otherwise, so it is left unwritten.
    typst_paper <- c(
      letter = "us-letter",
      legal = "us-legal",
      tabloid = "us-tabloid",
      executive = "us-executive",
      b5 = "iso-b5"
    )
    papersize_for <- function(typst = FALSE) {
      size <- ifempty(input$papersize)
      if (is.null(size) || identical(size, "letter")) {
        return(NULL)
      }
      if (typst && size %in% names(typst_paper)) {
        return(unname(typst_paper[size]))
      }
      size
    }
    
    # A float a document declares for itself, Written
    # into each format block rather than once for all of them: quarto drops
    # latex-env on the way through an extension's shared options, and its own
    # LaTeX code then stops on the missing field.
    illustration_float <- NULL
    if (isTRUE(input$`illustration-float`)) {
      illustration_float <- list(custom = list(
        list(
          kind = "float",
          `reference-prefix` = "Illustration",
          key = "ill",
          `latex-env` = "illustration"
        )
      ))
    }
    
    # The lists each format can build. .html has no pages to find, so it takes
    # the contents alone; the other three take all four.
    lists_for <- function(html = FALSE) {
      l <- list(`list-of-contents` = isTRUE(input$`list-of-contents`))
      if (!html) {
        l$`list-of-figures` <- isTRUE(input$`list-of-figures`)
        l$`list-of-tables` <- isTRUE(input$`list-of-tables`)
        l$`list-of-illustrations` <- isTRUE(input$`list-of-illustrations`)
      }
      # In thesis mode all four are true unless the document says otherwise,
      # so only a false is worth writing; in every other mode only a true is.
      keep <- if (input$documentmode == "thesis")
        ! unlist(l)
      else
        unlist(l)
      l[!keep] <- NULL
      l
    }
    
    # ---- what the imported document carried and the app does not write ----
    #
    # Added before the formats so that they stay at the foot of the block,
    # which is where a reader looks for them.
    fm_kept <- r_frontmatter()
    for (field in names(unowned_fields(fm_kept, owned_fields))) {
      doc_list[[field]] <- fm_kept[[field]]
    }
    
    if (length(input$formattype) == 0) {
      doc_list$format <- list(`apaquarto-html` = list(toc = TRUE))
    } else {
      apaformats <- list()
      if ("apaquarto-html" %in% input$formattype) {
        # Quarto's own contents sits in the margin of a web page, which is
        # where a reader looks, and apaquarto adds none of its own while that
        # one is there. So asking for a contents in the body turns it off.
        html_format <- list(toc = !isTRUE(input$`list-of-contents`))
        html_format <- append(html_format, lists_for(html = TRUE))
        if (isTRUE(input$`notebook-view`)) {
          html_format$`notebook-view` <- TRUE
        }
        html_format$crossref <- illustration_float
        apaformats$`apaquarto-html` <- html_format
      }
      if ("apaquarto-docx" %in% input$formattype) {
        doc_list$`blank-lines-above-title` <- whole(input$`blank-lines-above-title`)
        doc_list$`blank-lines-above-author-note` <- whole(input$`blank-lines-above-author-note`)
        docx_format <- list(toc = FALSE)
        docx_format <- append(docx_format, lists_for())
        docx_format$papersize <- papersize_for(typst = FALSE)
        docx_format$crossref <- illustration_float
        apaformats$`apaquarto-docx` <- docx_format
      }
      if ("apaquarto-typst" %in% input$formattype) {
        doc_list$`blank-lines-above-title` <- whole(input$`blank-lines-above-title`)
        doc_list$`blank-lines-above-author-note` <- whole(input$`blank-lines-above-author-note`)
        typst_format <- list(toc = FALSE)
        typst_format <- append(typst_format, lists_for())
        typst_format$papersize <- papersize_for(typst = TRUE)
        typst_format$crossref <- illustration_float
        apaformats$`apaquarto-typst` <- typst_format
      }
      
      if ("apaquarto-pdf" %in% input$formattype) {
        pdf_format <- lists_for()
        pdf_format$papersize <- papersize_for()
        pdf_format$`pdf-standard` <- ifempty(input$`pdf-standard`)
        pdf_format$crossref <- illustration_float
        pdf_format$`keep-tex` <- FALSE
        apaformats$`apaquarto-pdf` <- pdf_format
      }
      doc_list$format <- apaformats
    }
    
    # The same for what sat inside a format block --- an include-in-header, a
    # theme, execute options --- and for a format the app does not write at
    # all, such as a revealjs or a plain docx alongside the apaquarto ones.
    imported_formats <- purrr::pluck(fm_kept, "format")
    if (is.list(imported_formats)) {
      for (fmt in names(doc_list$format)) {
        theirs <- imported_formats[[fmt]]
        for (field in names(unowned_fields(theirs, owned_format_fields))) {
          doc_list$format[[fmt]][[field]] <- theirs[[field]]
        }
        doc_list$format[[fmt]]$crossref <- merge_crossref(purrr::pluck(theirs, "crossref"),
                                                          doc_list$format[[fmt]]$crossref)
      }
      for (fmt in names(imported_formats)) {
        # An apaquarto format the writer left out is one the user unticked,
        # so it stays out; any other format is the document's own business.
        if (!(fmt %in% apa_formats) &&
            !(fmt %in% names(doc_list$format))) {
          doc_list$format[[fmt]] <- imported_formats[[fmt]]
        }
      }
    }
    
    doc_list[lapply(doc_list, length) == 0] <- NULL
    
    doc_yaml <- doc_list |>
      as.yaml(indent.mapping.sequence = T,
              handlers = list(
                logical = function(x) {
                  result <- ifelse(x, "true", "false")
                  class(result) <- "verbatim"
                  return(result)
                }
              )) |>
      gsub(pattern = "'false'", replacement = "false") |>
      gsub(pattern = "'true'", replacement = "true") |>
      gsub(pattern = "nocitestart\n", replacement = "") |>
      gsub(pattern = "\\s{2,}nociteend", replacement = "") |>
      gsub(pattern = "\\|\\-", replacement = "|")
    
    # The document's own body, when one was imported or written on the
    # Document Body tab. It is written back exactly as it came in: the point
    # of bringing a document in is to change its options and paste the result
    # back over the original, and anything rewritten here would be lost work.
    kept_body <- NULL
    if (length(input$body) > 0) {
      kept_body <- ifempty(trimws(input$body))
    }
    
    # A dissertation is written in chapters, and its level-one headings are
    # set in capitals with CHAPTER and a number above them, so the skeleton
    # under the YAML follows the mode.
    if (input$documentmode == "thesis") {
      body_skeleton <- paste0(
        "# Introduction\n\n",
        "<!-- Each level-1 heading begins a chapter of its own, on a page of",
        " its own, with CHAPTER and its number above it. -->\n\n",
        "## Background\n\n",
        "## Statement of the Problem\n\n",
        "# Review of the Literature\n\n",
        "# Method\n\n",
        "## Participants\n\n",
        "## Measures\n\n",
        "## Procedure\n\n",
        "# Results\n\n",
        "# Discussion\n\n",
        "## Limitations and Future Directions\n\n",
        "## Conclusion\n\n"
      )
    } else {
      body_skeleton <- paste0(
        "<!-- The introduction should not have a level-1 heading such as",
        " Introduction. If one is written here, apaquarto takes it out",
        " for you; set keep-introduction-heading: true to keep it. -->\n\n",
        "## Section in Introduction\n\n",
        "## Another Section in Introduction\n\n",
        "# Method\n\n",
        "## Participants\n\n",
        "## Measures\n\n",
        "## Procedure\n\n",
        "# Results\n\n",
        "# Discussion\n\n",
        "## Limitations and Future Directions\n\n",
        "## Conclusion\n\n"
      )
    }
    
    # Since apaquarto 6 the abstract and the impact statement may be written
    # as sections instead of in the YAML, which is easier on a long abstract:
    # it is spell checked and counted with the rest of the prose, and an
    # apostrophe in it stops nothing. The section is shown when the field has
    # been left empty.
    abstract_skeleton <- ""
    if (length(ifempty(input$abstract)) == 0) {
      abstract_skeleton <- paste0(
        "<!-- The abstract may be written here as a",
        " section rather than in the YAML above. Delete this if you would",
        " rather write it there. -->\n\n",
        "# Abstract\n\n",
        "Write the abstract here.\n\n"
      )
    }
    
    illustration_skeleton <- ""
    if (isTRUE(input$`illustration-float`)) {
      # Shown as a comment rather than as live markup, so that the document
      # the app hands back renders as it stands: a picture named here would
      # be one the writer has yet to put in the folder.
      illustration_skeleton <- paste0(
        "<!-- An Illustration is a float of its own, numbered apart from the\n",
        "figures. Its apa-note works as a figure's does. Uncomment this and\n",
        "point it at a picture of your own:\n\n",
        "::: {#ill-example apa-note=\"The note below the illustration.\"}\n",
        "![](your-image.png)\n\n",
        "The illustration caption.\n",
        ":::\n",
        "-->\n\n"
      )
    }
    
    if (is.null(kept_body)) {
      document_text <- paste0(
        abstract_skeleton,
        body_skeleton,
        illustration_skeleton,
        "# References\n\n",
        "<!-- References will auto-populate in the refs div below -->\n\n",
        "::: {#refs}\n",
        ":::\n\n",
        "# This Section Is an Appendix {#apx-a}\n\n",
        "# Another Appendix {#apx-b}\n"
      )
    } else {
      document_text <- paste0(kept_body, "\n")
    }
    
    doc_yaml <- paste0("---\n", trimws(doc_yaml), "\n---\n\n", document_text)
    
    output$makedocument <- renderUI(
      tags$div(
        style = "border: 1px solid #ccc; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
        rclipButton(
          inputId = "copythis",
          label = "Copy to clipboard",
          clipText = doc_yaml,
          icon = icon("clipboard"),
          class = "mb-2"
        ),
        verbatimTextOutput("yaml_output", placeholder = TRUE)
      )
    )
    r_yaml(doc_yaml)
    output$yaml_output <- renderText(doc_yaml)
  }
  
  observeEvent(input$btnmakedocument, {
    # Shiny leaves an output spinning for ever when the observer feeding it
    # stops on an error, and the render says nothing, so what a person sees
    # is a button that hangs. Saying what went wrong instead turns that into
    # something they can report or work around.
    tryCatch(
      build_document(),
      error = function(e) {
        message("apa7maker could not build the document: ",
                conditionMessage(e))
        output$makedocument <- renderUI(tags$div(
          class = "alert alert-danger",
          tags$strong("The document could not be built."),
          tags$p(conditionMessage(e)),
          tags$p(
            "Nothing has been lost: the Document Body tab still holds what",
            " was imported. Please report this, with the .qmd that caused",
            " it, at ",
            tags$a(
              "the apa7maker issue tracker",
              href = "https://github.com/wjschne/apa7maker/issues",
              rel = "noopener noreferrer",
              target = "_blank"
            ),
            "."
          )
        ))
      }
    )
  })
  
  # import doc ----
  observe({
    req(input$importqmd)
    fm <- rmarkdown::yaml_front_matter(input$importqmd$datapath)
    r_frontmatter(fm)
    
    
    
    # The fields that are written under `language`, as against the ones that
    # share a name with something else.
    language_fields <- c(
      "citation-last-author-separator",
      "citation-masked-author",
      "citation-masked-date",
      "citation-masked-title",
      "title-block-author-note",
      "title-block-correspondence-note",
      "title-block-role-introduction",
      "title-impact-statement",
      "title-supplemental-materials",
      "title-word-count",
      "references-meta-analysis"
    )
    
    ## text inputs
    for (fd in c(
      "title",
      "shorttitle",
      "journal-title",
      "journal-logo",
      "journal-year",
      "journal-volume",
      "journal-issue",
      "journal-pages",
      "journal-url",
      "journal-issn",
      "journal-copyrightnotice",
      "journal-copyrighttext",
      "course",
      "professor",
      "student-note",
      "affiliation-change",
      "deceased-note",
      "study-registration",
      "data-sharing",
      "related-report",
      "conflict-of-interest",
      "financial-support",
      "gratitude",
      "author-agreements",
      "correspondence-note",
      "citation-last-author-separator",
      "citation-masked-author",
      "citation-masked-date",
      "title-block-author-note",
      "title-block-correspondence-note",
      "title-block-role-introduction",
      "title-impact-statement",
      "title-supplemental-materials",
      "title-word-count",
      "references-meta-analysis"
    )) {
      for (fmt in fm[["format"]]) {
        if (!is.character(fmt)) {
          updateTextInput(session = session,
                          inputId = fd,
                          value = fmt[[fd]])
        }
        updateTextInput(
          session = session,
          inputId = fd,
          value = purrr::pluck(fm, "journal", str_remove(fd, "journal\\-"))
        )
      }
      
      updateTextInput(
        session = session,
        inputId = fd,
        value = purrr::pluck(fm, "author-note", fd)
      )
      
      updateTextInput(
        session = session,
        inputId = fd,
        value = purrr::pluck(fm, "journal", str_remove(fd, "journal\\-"))
      )
      
      updateTextInput(
        session = session,
        inputId = fd,
        value = purrr::pluck(fm, "author-note", "status-changes", fd)
      )
      
      updateTextInput(
        session = session,
        inputId = fd,
        value = purrr::pluck(fm, "author-note", "disclosures", fd)
      )
      
      # Only where the field really is a language key. Version 6 added
      # journal-volume and journal-issue to that block, and looking every
      # field up there put the word "Vol." into the masthead's volume box.
      if (fd %in% language_fields) {
        updateTextInput(
          session = session,
          inputId = fd,
          value = purrr::pluck(fm, "language", fd)
        )
      }
      
      updateTextInput(session = session,
                      inputId = fd,
                      value = fm[[fd]])
    }
    
    ## the body, which is everything the front matter is not
    updateTextAreaInput(
      session = session,
      inputId = "body",
      value = document_body(input$importqmd$datapath)
    )
    
    ## text area inputs
    for (fd in c("abstract", "impact-statement", "supplemental-materials")) {
      for (fmt in fm[["format"]]) {
        if (!is.character(fmt)) {
          updateTextAreaInput(session = session,
                              inputId = fd,
                              value = fmt[[fd]])
        }
      }
      
      updateTextAreaInput(session = session,
                          inputId = fd,
                          value = fm[[fd]])
    }
    
    ## date inputs
    for (fd in c("duedate")) {
      for (fmt in fm[["format"]]) {
        if (!is.character(fmt)) {
          updateDateInput(session = session,
                          inputId = fd,
                          value = fmt[[fd]])
        }
      }
      
      updateDateInput(session = session,
                      inputId = fd,
                      value = fm[[fd]])
    }
    
    ## list imputs
    for (fd in c("bibliography", "masked-citations", "nocite", "keywords")) {
      updateSelectizeInput(
        session = session,
        inputId = fd,
        selected = fm[[fd]],
        choices = fm[[fd]]
      )
    }
    
    for (fd in c("lang")) {
      updateSelectInput(session = session,
                        inputId = fd,
                        selected = fm[[fd]])
    }
    
    ## checkbox inputs
    for (fd in c(
      "floatsintext",
      "numbered-lines",
      "no-ampersand-parenthetical",
      "mask",
      "meta-analysis",
      "list-of-figures",
      "list-of-tables",
      "list-of-illustrations",
      "includeduedate",
      "word-count"
    )) {
      for (fmt in fm[["format"]]) {
        if (!is.character(fmt)) {
          updateCheckboxInput(session = session,
                              inputId = fd,
                              value = fmt[[fd]])
        }
      }
      
      updateCheckboxInput(session = session,
                          inputId = fd,
                          value = fm[[fd]])
    }
    
    ## checkboxgroup
    #
    # Only the ones the document actually turns on. A suppress field written
    # as false asks for the element to be kept, and ticking it because it was
    # mentioned took the title page away from every document that had said,
    # in so many words, to keep it. Written every time, so that importing a
    # second document clears what the first one ticked.
    suppress_fields <- names(fm)[str_starts(names(fm), "suppress\\-")]
    updateCheckboxGroupInput(session = session,
                             inputId = "suppress",
                             selected = suppress_fields[vapply(suppress_fields, \(nm) is_yes(fm[[nm]]), logical(1))])
    
    updateCheckboxGroupInput(session = session,
                             inputId = "formattype",
                             selected = names(fm[["format"]]))
    
    ## radio button
    for (fd in c("fontsize",
                 "a4paper",
                 "documentmode",
                 "author-note-columns")) {
      for (fmt in fm[["format"]]) {
        if (!is.character(fmt)) {
          updateNumericInput(session = session,
                             inputId = fd,
                             value = fmt[[fd]])
        }
      }
      
      updateRadioButtons(session = session,
                         inputId = fd,
                         selected = fm[[fd]])
    }
    
    ## numeric input
    
    for (fd in c("blank-lines-above-title",
                 "blank-lines-above-author-note")) {
      for (fmt in fm[["format"]]) {
        if (!is.character(fmt)) {
          updateNumericInput(session = session,
                             inputId = fd,
                             value = fmt[[fd]])
        }
      }
      
      updateNumericInput(session = session,
                         inputId = fd,
                         value = fm[[fd]])
    }
    
    # ---- the options version 6 and 7 added ----
    #
    # Several of these may be written at the top level or inside a format
    # block, and a file written by hand may put them in either place, so both
    # are looked at. The first value found wins, which is also how quarto
    # reads them.
    from_anywhere <- function(field) {
      v <- fm[[field]]
      if (!is.null(v)) {
        return(v)
      }
      for (fmt in fm[["format"]]) {
        if (is.list(fmt) && !is.null(fmt[[field]])) {
          return(fmt[[field]])
        }
      }
      NULL
    }
    
    ## the document mode, which since version 6 belongs to every format
    mode <- from_anywhere("documentmode")
    if (!is.null(mode)) {
      # Version 6 accepts each mode spelled out in full.
      mode <- c(
        manuscript = "manuscript",
        journal = "journal",
        document = "document",
        student = "student",
        dissertation = "thesis",
        man = "manuscript",
        jou = "journal",
        doc = "document",
        stu = "student",
        thesis = "thesis"
      )[mode] |>
        unname() |>
        coalesce(mode)
      updateRadioButtons(session = session,
                         inputId = "documentmode",
                         selected = mode)
      # Told to the observer that keeps the list checkboxes in step with the
      # mode, so that it leaves the values read below alone.
      r_listmode(mode)
    }
    
    ## text fields written at the top level or in a format block
    for (fd in c(
      "mainfont",
      "monofont",
      "linenumber-font",
      "linkcolor",
      "urlcolor",
      "citecolor",
      "filecolor",
      "toccolor",
      "supplemental-materials"
    )) {
      updateTextInput(session = session,
                      inputId = fd,
                      value = from_anywhere(fd))
    }
    
    ## the journal masthead, which may be written at the top level or inside
    ## a format block. Version 6 assembles the issue line out of the year, the
    ## volume, the issue and the pages, so each is read separately.
    journal_block <- from_anywhere("journal")
    if (is.list(journal_block)) {
      for (fd in c(
        "title",
        "logo",
        "year",
        "volume",
        "issue",
        "pages",
        "url",
        "issn",
        "copyrightnotice",
        "copyrighttext"
      )) {
        if (!is.null(journal_block[[fd]])) {
          updateTextInput(
            session = session,
            inputId = paste0("journal-", fd),
            value = as.character(journal_block[[fd]])
          )
        }
      }
    }
    
    ## the student note, which apaquarto calls `note`
    updateTextInput(session = session,
                    inputId = "student-note",
                    value = from_anywhere("note"))
    
    ## the language words version 6 added
    for (fd in c("section-title-introduction",
                 "figure-panel",
                 "figure-table-note")) {
      updateTextInput(
        session = session,
        inputId = fd,
        value = purrr::pluck(fm, "language", fd)
      )
    }
    updateTextInput(
      session = session,
      inputId = "language-journal-volume",
      value = purrr::pluck(fm, "language", "journal-volume")
    )
    updateTextInput(
      session = session,
      inputId = "language-journal-issue",
      value = purrr::pluck(fm, "language", "journal-issue")
    )
    
    ## a dissertation or thesis
    for (fd in c("type", "submitted-to", "degree", "date")) {
      v <- purrr::pluck(fm, "thesis", fd)
      if (fd == "type") {
        if (!is.null(v)) {
          updateRadioButtons(session = session,
                             inputId = "thesis-type",
                             selected = v)
        }
      } else {
        updateTextInput(
          session = session,
          inputId = paste0("thesis-", fd),
          value = v
        )
      }
    }
    copyright <- purrr::pluck(fm, "thesis", "copyright")
    if (!is.null(copyright)) {
      if (isFALSE(copyright)) {
        updateCheckboxInput(session = session,
                            inputId = "thesis-copyright-suppress",
                            value = TRUE)
      } else {
        updateTextInput(
          session = session,
          inputId = "thesis-copyright",
          value = as.character(copyright)
        )
      }
    }
    
    ## the dedication and the acknowledgments, which may be written at the top
    ## level or under thesis, and which apaquarto also reads spelled the
    ## British way
    for (fd in c("dedication", "acknowledgments")) {
      v <- ornull(fm[[fd]], purrr::pluck(fm, "thesis", fd))
      if (fd == "acknowledgments") {
        v <- ornull(v, fm[["acknowledgements"]], purrr::pluck(fm, "thesis", "acknowledgements"))
      }
      updateTextAreaInput(session = session,
                          inputId = fd,
                          value = v)
    }
    
    committee <- purrr::pluck(fm, "thesis", "committee")
    if (length(committee) > 0) {
      r_committee(
        tibble(
          committee_id = seq_along(committee),
          committee_name = map_chr(committee, \(x) ornull(x$name, NA_character_)),
          committee_role = map_chr(committee, \(x) ornull(x$role, NA_character_)),
          committee_affiliation = map_chr(committee, \(x) ornull(x$affiliation, NA_character_))
        )
      )
    }
    
    ## the lists
    #
    # A dissertation carries a contents and a list for each kind of float
    # unless it says otherwise, so a field the document does not mention is
    # on in that mode and off in every other. Read this way round, a
    # dissertation that named none of them comes back with all four.
    thesis_mode <- identical(mode, "thesis")
    for (fd in c(
      "list-of-contents",
      "list-of-figures",
      "list-of-tables",
      "list-of-illustrations"
    )) {
      v <- from_anywhere(fd)
      updateCheckboxInput(
        session = session,
        inputId = fd,
        value = if (is.null(v))
          thesis_mode
        else
          isTRUE(v)
      )
    }
    
    ## the rest of the checkboxes version 6 and 7 added
    for (fd in c("keep-introduction-heading", "notebook-view")) {
      v <- from_anywhere(fd)
      if (!is.null(v)) {
        updateCheckboxInput(session = session,
                            inputId = fd,
                            value = isTRUE(v))
      }
    }
    
    ## a float the document declares for itself
    has_illustration <- FALSE
    for (fmt in fm[["format"]]) {
      if (is.list(fmt)) {
        custom <- ornull(purrr::pluck(fmt, "crossref", "custom"), list())
        for (entry in custom) {
          if (identical(entry[["key"]], "ill")) {
            has_illustration <- TRUE
          }
        }
      }
    }
    updateCheckboxInput(session = session,
                        inputId = "illustration-float",
                        value = has_illustration)
    
    ## counts and choices
    for (fd in c("toc-depth", "first-page")) {
      v <- from_anywhere(fd)
      if (!is.null(v)) {
        updateNumericInput(session = session,
                           inputId = fd,
                           value = v)
      }
    }
    for (fd in c("papersize", "pdf-standard")) {
      v <- from_anywhere(fd)
      if (!is.null(v)) {
        updateSelectInput(session = session,
                          inputId = fd,
                          selected = v)
      }
    }
    
    d_author <<- tibble(
      author_name = map(fm$author, "name") |>
        map_chr(\(x) {
          ifelse(is.null(x), NA_character_, ifelse(is.list(x), paste(unlist(x), collapse = " "), x))
        }),
      author_orcid = map(fm$author, "orcid") |>
        map_chr(\(x) ifelse(is.null(x), NA_character_, x)),
      author_email = map(fm$author, "email") |>
        map_chr(\(x) ifelse(is.null(x), NA_character_, x)),
      author_corresponding = map(fm$author, "corresponding") |>
        map_lgl(\(x) ifelse(is.null(x), FALSE, x)),
      author_deceased = map(fm$author, "deceased") |>
        map_lgl(\(x) ifelse(is.null(x), FALSE, x)),
      affiliations = map(fm$author, "affiliations"),
      role = map(fm$author, \(a) {
        if (is.list(a) && "roles" %in% names(a)) {
          print(a)
          ornull(a[["roles"]], a[["role"]])
        } else {
          NULL  # Or whatever default value you want
        }
      }) |>
        map_df(\(x) {
          d <- tibble::tibble(
            role_conceptualization = "No",
            role_data_curation = "No",
            role_formal_analysis = "No",
            role_funding_acquisition = "No",
            role_investigation = "No",
            role_methodology = "No",
            role_project_administration = "No",
            role_resources = "No",
            role_software = "No",
            role_supervision = "No",
            role_validation = "No",
            role_visualization = "No",
            role_writing = "No",
            role_editing = "No"
          )
          for (entry in as.list(x)) {
            label <- names(entry)
            if (is.null(label) ||
                length(label) == 0 || label[1] == "") {
              # The role on its own, which is a yes and nothing more.
              label <- as.character(unlist(entry))[1]
              degree <- "Yes"
            } else {
              label <- label[1]
              degree <- str_to_title(as.character(unlist(entry))[1])
            }
            if (is.na(label) || label == "") {
              next
            }
            column <- paste0("role_", to_snake_case(label))
            if (column %in% colnames(d)) {
              d[1, column] <- degree
            }
          }
          
          d
        })
    ) |>
      unnest(role) |>
      mutate(author_id = seq(length(fm$author)))
    
    # imported affiliations----
    #
    # An affiliation may be written three ways, and all three are read:
    # inline under the author as an object, inline as a plain name, or held
    # once in a top-level `affiliations` block and named there by its id.
    # Quarto takes `affiliations` and `affiliation` alike as the key.
    affiliation_columns <- setdiff(colnames(d_affiliation),
                                   c("affiliation_id", "author_id"))
    # The column each of quarto's affiliation fields is held in. region and
    # state are the same column, and postal-code is read with either spelling.
    affiliation_column <- c(
      name = "affiliation_name",
      department = "affiliation_department",
      group = "affiliation_group",
      address = "affiliation_address",
      city = "affiliation_city",
      region = "affiliation_region",
      state = "affiliation_region",
      country = "affiliation_country",
      `postal-code` = "affiliation_postal_code",
      postal_code = "affiliation_postal_code",
      url = "affiliation_url"
    )
    
    # The affiliations an author carries, as a list of entries whichever way
    # they were written: under `affiliations` or under `affiliation`, as a
    # list of them or as a single one, as objects or as plain names.
    affiliation_entries <- function(author) {
      aff <- NULL
      if (is.list(author) && "affiliations" %in% names(author)) {
        aff <- author[["affiliations"]]
      } else if (is.list(author) && "affiliation" %in% names(author)) {
        aff <- author[["affiliation"]]
      } else {
        NULL  # Or whatever default value you want
      }
      entries <- ornull(aff)
      if (is.list(entries) && !is.null(names(entries))) {
        entries <- list(entries)
      }
      if (is.character(entries)) {
        entries <- as.list(entries)
      }
      entries
    }
    
    # Every affiliation that carries an id, from wherever it is written: a
    # top-level `affiliations` block, or --- as example.qmd writes it ---
    # inline under the first author to use it, for the authors after them to
    # point at.
    shared_affiliations <- list()
    if (!is.null(fm[["affiliations"]])) {
      for (entries in c(list(fm[["affiliations"]]), lapply(fm$author, affiliation_entries))) {
        for (a in entries) {
          if (is.list(a) && !is.null(a[["id"]])) {
            shared_affiliations[[as.character(a[["id"]])[1]]] <- a
          }
        }
      }
    }
    
    
    as_affiliation_row <- function(entry) {
      # An author may point at an affiliation someone else spelled out,
      # written either as `ref: id1` or as the id on its own. The app holds
      # one row per author and has no notion of an affiliation shared between
      # them, so the whole of the one referred to is copied here. Its id is
      # not a column, so it is dropped along the way.
      reference <- NULL
      if (is.character(entry) && length(entry) == 1) {
        reference <- entry
      } else if (is.list(entry) && !is.null(entry[["ref"]])) {
        reference <- as.character(entry[["ref"]])[1]
      }
      if (!is.null(reference)) {
        shared <- shared_affiliations[[reference]]
        # A name on its own that matches no id is a name, not a reference.
        # A `ref` that matches none is a fault in the document, and putting
        # what it said in the name column says so rather than losing it.
        entry <- if (is.null(shared))
          list(name = reference)
        else
          shared
      }
      if (!is.list(entry)) {
        return(NULL)
      }
      row <- as.list(rep(NA_character_, length(affiliation_columns)))
      names(row) <- affiliation_columns
      for (nm in names(entry)) {
        column <- unname(affiliation_column[nm])
        value <- entry[[nm]]
        if (!is.na(column) && length(value) > 0) {
          row[[column]] <- as.character(unlist(value))[1]
        }
      }
      as_tibble(row)
    }
    
    imported_affiliations <- list()
    next_affiliation_id <- 0L
    for (i in seq_along(fm$author)) {
      entries <- affiliation_entries(fm$author[[i]])
      for (entry in entries) {
        row <- as_affiliation_row(entry)
        if (is.null(row)) {
          next
        }
        next_affiliation_id <- next_affiliation_id + 1L
        imported_affiliations[[length(imported_affiliations) + 1]] <- bind_cols(tibble(affiliation_id = next_affiliation_id, author_id = i),
                                                                                row)
      }
      # An author with none still gets an empty row, so that the grid has
      # something to show and to edit when that author is selected.
      if (is.null(entries) || length(entries) == 0) {
        next_affiliation_id <- next_affiliation_id + 1L
        imported_affiliations[[length(imported_affiliations) + 1]] <- bind_rows(
          filter(d_affiliation, FALSE),
          tibble(affiliation_id = next_affiliation_id, author_id = i)
        )
      }
    }
    
    if (length(imported_affiliations) > 0) {
      d_affiliation_imported <- bind_rows(imported_affiliations)
    } else {
      d_affiliation_imported <- d_affiliation |> filter(FALSE)
    }
    
    r_affiliation(d_affiliation_imported)
    author_select(1)
    r_affiliation_current(d_affiliation_imported |>
                            filter(author_id == 1))
    
    # The grid has no column for the affiliations, which are held apart.
    d_author$affiliations <- NULL
    
    # add imported authors----
    if (!adding_author) {
      adding_author <- TRUE
      
      for (i in seq(author_n())) {
        grid_proxy_delete_row("gd_author", i - 1)
      }
      
      author_n(nrow(d_author))
      if (author_n() > 0) {
        grid_proxy_add_row("gd_author", d_author)
      }
      
      adding_author <- FALSE
    }
  })
}

shinyApp(ui, server)
