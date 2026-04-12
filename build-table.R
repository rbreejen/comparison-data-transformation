# build-table.R — generic comparison table engine
# Source this file, then call build_comparison_table(cfg).

icon_map <- c(
  ":n:" = "<span><img alt='' src='./assets/icons/icon.png' style='width: 16.38px; height: 12.8px; margin-left: 0.00px; margin-top: 0.00px; transform: rotate(0.00rad) translateZ(0px); -webkit-transform: rotate(0.00rad) translateZ(0px);' title=''></i></span>",
  ":y:" = "<span><img alt='' src='./assets/icons/62fc3b327bf0d9337241e112_check.png' style='width: 16.38px; height: 12.8px; margin-left: 0.00px; margin-top: 0.00px; transform: rotate(0.00rad) translateZ(0px); -webkit-transform: rotate(0.00rad) translateZ(0px);' title=''></i></span>",
  ":u:" = "<span><img alt='' src='./assets/icons/62fc3b3276df8460a3e8d91b_output-onlinepngtools.png' style='width: 20.46px; height: 16.00px; margin-left: 0.00px; margin-top: 0.00px; transform: rotate(0.00rad) translateZ(0px); -webkit-transform: rotate(0.00rad) translateZ(0px);' title=''></i></span>"
)

column_transformer <- function(string) {
  for (token in names(icon_map)) {
    if (str_detect(string, token)) {
      string <- str_replace(string, stringr::fixed(token), icon_map[[token]])
    }
  }
  markdown::mark(text = as.character(string))
}

# Build a colDef for one comparison column from a structured spec:
#   label       — display text (NULL = no text label)
#   logo_url    — image src (NULL = no logo)
#   logo_w/h    — logo dimensions in px
#   label_style — optional inline CSS for the <p> label tag
#   highlight   — badge text shown above the column title (NULL = no badge)
make_col <- function(spec) {
  if (!is.null(spec$logo_url)) {
    img_tag <- sprintf(
      "<img alt='' src='%s' style='width: %.2fpx; height: %.2fpx'>",
      spec$logo_url, spec$logo_w, spec$logo_h
    )
    if (!is.null(spec$label)) {
      label_style <- if (!is.null(spec$label_style)) sprintf(" style='%s'", spec$label_style) else ""
      content <- sprintf("%s<p%s>%s</p>", img_tag, label_style, spec$label)
    } else {
      content <- img_tag
    }
  } else {
    content <- spec$label
  }

  highlight_div <- if (!is.null(spec$highlight)) sprintf("<div class='highlight-tag'>%s</div>", spec$highlight) else ""
  header_html   <- sprintf("<div class='header-wrapper'>%s<div class='column-title'>%s</div></div>", highlight_div, content)

  colDef(
    html     = TRUE,
    align    = "center",
    cell     = function(value) column_transformer(value),
    class    = "border-left",
    minWidth = 90,
    name     = header_html
  )
}

build_comparison_table <- function(cfg) {
  # --- Load data -----------------------------------------------------------
  comparison_data <- fread(here::here(cfg$csv_path), quote = "", header = TRUE)

  # Extract all col_* metadata rows, then drop them from the data
  col_meta_names  <- c("col_group", "col_label", "col_logo_url", "col_logo_w",
                       "col_logo_h", "col_label_style", "col_highlight")
  col_meta        <- comparison_data[item %in% col_meta_names]
  comparison_data <- comparison_data[!item %in% col_meta_names]

  # Derive comparison columns from col_group row: any column with a non-empty group value
  col_group_row   <- col_meta[item == "col_group"]
  comparison_cols <- Filter(
    function(nm) {
      nm != "item" && !is.na(col_group_row[[nm]][[1]]) && nzchar(as.character(col_group_row[[nm]][[1]]))
    },
    names(col_group_row)
  )

  # Build column groups from col_group row
  group_map  <- split(comparison_cols, unlist(col_group_row[, ..comparison_cols]))
  col_groups <- unname(mapply(
    function(grp_name, cols) colGroup(name = grp_name, columns = cols, html = TRUE),
    names(group_map), group_map,
    SIMPLIFY = FALSE
  ))

  # Any column not in comparison_cols or "item" is metadata — hide automatically
  hidden_cols <- setdiff(names(comparison_data), c("item", comparison_cols))
  hidden_defs <- setNames(lapply(hidden_cols, function(x) colDef(show = FALSE)), hidden_cols)

  # Build column specs from col_* metadata rows
  get_meta_val <- function(prop, col_name) {
    row <- col_meta[item == prop]
    if (nrow(row) == 0) return(NULL)
    val <- as.character(row[[col_name]][[1]])
    if (is.na(val) || !nzchar(val)) NULL else val
  }

  col_specs <- setNames(lapply(comparison_cols, function(col_name) {
    list(
      label       = get_meta_val("col_label",       col_name),
      logo_url    = get_meta_val("col_logo_url",     col_name),
      logo_w      = as.numeric(get_meta_val("col_logo_w",      col_name)),
      logo_h      = as.numeric(get_meta_val("col_logo_h",      col_name)),
      label_style = get_meta_val("col_label_style",  col_name),
      highlight   = get_meta_val("col_highlight",    col_name)
    )
  }), comparison_cols)

  # --- Build column definitions --------------------------------------------
  item_def <- colDef(
    html      = TRUE,
    name      = "",
    align     = "left",
    cell      = function(value, index) {
      title <- column_transformer(paste0(value, comparison_data[index, "description"][[1]]))
      title <- as.character(div(class = "item-content-left", HTML(title)))
      if (comparison_data[index, "is_advanced"][[1]]) {
        advanced_tag <- div(class = "item-content-right", span(class = "tag", "Advanced"))
        title <- paste0(title, tagList(advanced_tag))
      }
      paste0("<div class='item-wrapper'>", title, "</div>")
    },
    minWidth  = 280
  )

  col_defs <- c(
    list(item = item_def),
    lapply(col_specs, make_col),
    hidden_defs
  )

  # --- Build table ---------------------------------------------------------
  tbl_core <- reactable(
    comparison_data,
    pagination    = FALSE,
    borderless    = TRUE,
    columnGroups  = col_groups,
    defaultColDef = colDef(
      vAlign      = "center",
      headerVAlign = "bottom",
      class       = "cell",
      headerClass = "header"
    ),
    columns         = col_defs,
    highlight       = FALSE,
    theme           = reactableTheme(
      headerStyle = list(borderColor = "hsl(0, 0%, 90%)"),
      cellStyle   = list(display = "flex", flexDirection = "column", justifyContent = "center")
    ),
    defaultExpanded = TRUE,
    rowStyle        = local({
      unique_groups <- unique(comparison_data[comparison_data$is_group_header == FALSE, "group_name"][[1]])
      group_colors  <- setNames(
        ifelse(seq_along(unique_groups) %% 2 == 1, "rgb(247, 246, 235)", "rgb(237, 241, 246)"),
        unique_groups
      )
      function(index) {
        if (comparison_data[index, "is_group_header"][[1]]) return(NULL)
        list(backgroundColor = group_colors[[comparison_data[index, "group_name"][[1]]]])
      }
    }),
    sortable = FALSE,
    class    = "comparison-tbl"
  )

  tbl_with_header <- div(
    class = "comparison",
    div(class = "comparison-header", id = "header01",
        tags$br(),
        h2(class = "comparison-title", cfg$title),
        format(Sys.Date(), "%B %Y")
    ),
    tbl_core
  )

  tbl_browsable <- browsable(
    tagList(
      tags$head(
        tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
        tags$link(rel = "stylesheet", type = "text/css", href = "./assets/styles/styles.css")
      ),
      tbl_with_header
    )
  )

  # --- Save ----------------------------------------------------------------
  save_html(tbl_browsable, file = cfg$html_out, libdir = "lib")

  webshot2::webshot(url = cfg$html_out, file = cfg$img_out, delay = 3, vwidth = 2000)
}
