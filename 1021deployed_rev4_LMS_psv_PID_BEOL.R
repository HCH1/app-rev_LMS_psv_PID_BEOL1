library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(openxlsx)
library(rvest)
library(stringr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ====================================================
# General helpers
# ====================================================
trim_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

read_uploaded_table <- function(path, filename = "") {
  if (is.null(path) || path == "") return(data.frame())
  ext <- tolower(tools::file_ext(filename))
  if (ext == "") ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                   na.strings = c(""))
  } else if (ext == "txt") {
    df <- tryCatch(
      read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                 check.names = FALSE, quote = "", fill = TRUE,
                 na.strings = c("")),
      error = function(e) data.frame()
    )
    if (ncol(df) <= 1) {
      df <- read.table(path, header = TRUE, stringsAsFactors = FALSE,
                       check.names = FALSE, fill = TRUE, na.strings = c(""))
    }
  } else if (ext %in% c("xls", "xlsx")) {
    df <- as.data.frame(read_excel(path), stringsAsFactors = FALSE,
                        check.names = FALSE)
  } else {
    stop(paste("Unsupported file format:", filename))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  df[] <- lapply(df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  df
}

read_file <- function(path) read_uploaded_table(path, path)

tech_variant_match <- function(values, keyword) {
  kw <- trimws(as.character(keyword %||% ""))
  vals <- trim_chr(values)
  if (kw == "") return(rep(TRUE, length(vals)))

  vapply(vals, function(v) {
    tokens <- trimws(unlist(strsplit(v, ";", fixed = TRUE)))
    any(tokens == kw)
  }, logical(1))
}

make_gds_from_cols <- function(df) {
  if (!("GDS Number" %in% names(df)) || !("GDS Datatype" %in% names(df))) {
    return(rep("", nrow(df)))
  }
  paste0(trim_chr(df[["GDS Number"]]), ":", trim_chr(df[["GDS Datatype"]]))
}

apply_search <- function(df, keyword) {
  kw <- trimws(as.character(keyword %||% ""))
  if (nrow(df) == 0 || kw == "") return(df)
  mask <- apply(df, 1, function(row) {
    any(grepl(kw, as.character(row), ignore.case = TRUE, fixed = FALSE))
  })
  df[mask, , drop = FALSE]
}

safe_sheet_name <- function(x) {
  x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
  substr(x, 1, 31)
}

write_df_xlsx <- function(df, file, sheet_name = "Sheet1") {
  sheet_name <- safe_sheet_name(sheet_name)
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)
  saveWorkbook(wb, file, overwrite = TRUE)
}

# ====================================================
# LMS > cdslib helpers
# ====================================================
build_cdslib_df <- function(input1_df, tech_variant) {
  required_cols <- c("Layer Status", "Design Layer",
                     "Tech Variant - Included in PDK")
  missing_cols <- setdiff(required_cols, names(input1_df))
  if (length(missing_cols) > 0) {
    stop(paste("Input1 file is missing required columns:",
               paste(missing_cols, collapse = ", ")))
  }

  mask <- trim_chr(input1_df[["Layer Status"]]) == "Active" &
    trim_chr(input1_df[["Design Layer"]]) == "Yes" &
    tech_variant_match(input1_df[["Tech Variant - Included in PDK"]],
                       tech_variant)

  as.data.frame(input1_df[mask, , drop = FALSE],
                stringsAsFactors = FALSE, check.names = FALSE)
}

build_diff_df <- function(dm_df, cdslib_df, input1_df) {
  if (nrow(dm_df) == 0 || nrow(cdslib_df) == 0) return(data.frame())
  if (!("GDS" %in% names(dm_df))) {
    stop("Input2 / DM tab is missing required column: GDS")
  }
  if (!("GDS Number" %in% names(cdslib_df)) ||
      !("GDS Datatype" %in% names(cdslib_df))) {
    stop("cdslib result is missing required columns: GDS Number and/or GDS Datatype")
  }

  dm_base <- dm_df
  dm_base[["GDS"]] <- trim_chr(dm_base[["GDS"]])
  dm_summary <- data.frame(
    `CAD Level` = if ("CAD Level" %in% names(dm_base))
      dm_base[["CAD Level"]] else rep("", nrow(dm_base)),
    Description = if ("Description" %in% names(dm_base))
      dm_base[["Description"]] else rep("", nrow(dm_base)),
    GDS = dm_base[["GDS"]], stringsAsFactors = FALSE, check.names = FALSE
  ) %>% distinct()

  cdslib_base <- cdslib_df
  cdslib_base[["GDS"]] <- make_gds_from_cols(cdslib_base)
  cdslib_summary <- data.frame(
    `CAD Level` = if ("Data Layer Name" %in% names(cdslib_base))
      cdslib_base[["Data Layer Name"]] else rep("", nrow(cdslib_base)),
    Description = if ("Layer Description" %in% names(cdslib_base))
      cdslib_base[["Layer Description"]] else rep("", nrow(cdslib_base)),
    GDS = cdslib_base[["GDS"]], stringsAsFactors = FALSE,
    check.names = FALSE
  ) %>% distinct()

  dm_gds <- unique(trim_chr(dm_summary[["GDS"]]))
  cdslib_gds <- unique(trim_chr(cdslib_summary[["GDS"]]))

  only_dm <- dm_summary[dm_summary[["GDS"]] %in%
                          setdiff(dm_gds, cdslib_gds), , drop = FALSE]
  if (nrow(only_dm) > 0) only_dm[["Delta"]] <- "Only_in_DM"

  only_cdslib <- cdslib_summary[cdslib_summary[["GDS"]] %in%
                                  setdiff(cdslib_gds, dm_gds), , drop = FALSE]
  if (nrow(only_cdslib) > 0) only_cdslib[["Delta"]] <- "Only_in_cdslib"

  diff_summary <- bind_rows(only_dm, only_cdslib) %>%
    select(`CAD Level`, Description, GDS, Delta) %>% distinct()

  if (nrow(diff_summary) == 0) {
    return(data.frame(`CAD Level` = character(), Description = character(),
                      GDS = character(), Delta = character(),
                      check.names = FALSE))
  }

  if (nrow(input1_df) > 0 &&
      all(c("GDS Number", "GDS Datatype") %in% names(input1_df))) {
    lms_full <- input1_df
    lms_full[["GDS"]] <- make_gds_from_cols(lms_full)
    rename_cols <- intersect(c("CAD Level", "Description", "Delta"),
                             names(lms_full))
    for (col in rename_cols) {
      names(lms_full)[names(lms_full) == col] <- paste0("LMS_", col)
    }
    final_df <- left_join(diff_summary, lms_full, by = "GDS")
  } else {
    final_df <- diff_summary
  }

  first_cols <- c("CAD Level", "Description", "GDS", "Delta")
  final_df <- final_df[, c(first_cols, setdiff(names(final_df), first_cols)),
                       drop = FALSE]
  final_df %>% arrange(Delta, GDS, `CAD Level`)
}

# ====================================================
# PID > YX > BEOL helpers
# ====================================================
read_beol_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    df <- read.csv(path, header = FALSE, stringsAsFactors = FALSE,
                   check.names = FALSE)
  } else {
    df <- readxl::read_excel(path, col_names = FALSE)
  }
  as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
}

extract_beol_sections <- function(file_path) {
  df <- read_beol_file(file_path)
  if (is.null(df) || nrow(df) == 0) return(list())
  vec <- trimws(as.character(df[[1]]))
  vec <- vec[!is.na(vec) & vec != ""]
  beol_idx <- grep("^Process ID For BEOL", vec)
  if (length(beol_idx) == 0) return(list())

  result <- list()
  for (i in seq_along(beol_idx)) {
    start <- beol_idx[i]
    end <- if (i < length(beol_idx)) beol_idx[i + 1] - 1 else length(vec)
    section <- vec[start:end]
    section <- section[!is.na(section) & section != ""]
    if (length(section) == 0) next
    beol_name <- section[1]
    mask_idx <- grep("^\\#\\Mask Layer", section)
    if (length(mask_idx) == 0 || mask_idx[1] >= length(section)) next
    layer_data <- section[(mask_idx[1] + 1):length(section)]
    layer_data <- layer_data[!is.na(layer_data) & layer_data != ""]
    result[[beol_name]] <- layer_data
  }
  result
}

# ====================================================
# HTML > rev helpers - FIXED
# ====================================================
html_clean_text <- function(x) {
  if (length(x) == 0) return(character())
  x <- html_text2(x)
  x <- str_replace_all(x, "\\u00a0", " ")
  x <- str_replace_all(x, "\\s+", " ")
  str_trim(x)
}

html_parse_target_table <- function(doc, table_id) {
  tbl <- html_element(doc, paste0("table#", table_id))
  if (length(tbl) == 0 || is.na(tbl)) return(data.frame())
  headers <- tbl %>% html_elements("thead th") %>% html_clean_text()
  rows <- tbl %>% html_elements("tbody tr")
  if (length(rows) == 0) return(data.frame())

  out <- lapply(rows, function(r) {
    cells <- r %>% html_elements("td") %>% html_clean_text()
    if (length(cells) < length(headers)) {
      cells <- c(cells, rep("", length(headers) - length(cells)))
    }
    cells[seq_along(headers)]
  })
  df <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE,
                      check.names = FALSE)
  names(df) <- headers
  df
}

# Fix shortened section titles created by malformed/fragmented HTML cells.
# A shorter title is replaced only when a longer title starts with it and the
# missing continuation begins with an operator such as <=, >=, <, >, or =.
html_repair_section_titles <- function(section_vec) {
  sections <- str_squish(as.character(section_vec))
  sections[is.na(sections)] <- ""
  unique_sections <- unique(sections[sections != ""])
  if (length(unique_sections) <= 1) return(sections)

  full_candidates <- unique_sections[
    order(nchar(unique_sections), decreasing = TRUE)
  ]

  repaired <- vapply(sections, function(current_section) {
    if (current_section == "") return("")
    candidates <- full_candidates[
      nchar(full_candidates) > nchar(current_section) &
        startsWith(full_candidates, current_section)
    ]
    if (length(candidates) == 0) return(current_section)

    valid_candidates <- candidates[vapply(candidates, function(candidate) {
      remaining_text <- substr(candidate, nchar(current_section) + 1,
                               nchar(candidate))
      remaining_text <- str_trim(remaining_text)
      grepl("^(<=|>=|<|>|=|\\)|\\]|\\})", remaining_text)
    }, logical(1))]

    if (length(valid_candidates) == 0) return(current_section)
    valid_candidates[which.max(nchar(valid_candidates))]
  }, character(1))

  unname(repaired)
}

html_compact_impact_lines <- function(diff_vec, item_vec) {
  diff_vec <- str_trim(as.character(diff_vec))
  item_vec <- str_trim(as.character(item_vec))
  impact_lines <- character()
  seen_diff <- character()

  for (d in diff_vec) {
    d_key <- tolower(str_trim(d))
    if (d_key == "" || d_key %in% seen_diff) next
    seen_diff <- c(seen_diff, d_key)
    idx <- which(tolower(str_trim(diff_vec)) == d_key)
    items <- unique(item_vec[idx][str_trim(item_vec[idx]) != ""])
    if (length(items) == 0) next
    impact_lines <- c(impact_lines,
                      str_squish(paste(d, paste(items, collapse = ", "))))
  }
  impact_lines
}

html_make_summary_one_table <- function(df) {
  if (nrow(df) == 0 || ncol(df) < 3) {
    return(data.frame(section = character(), impact = character(),
                      stringsAsFactors = FALSE))
  }

  tmp <- data.frame(
    diff = str_trim(as.character(df[[1]])),
    item = str_trim(as.character(df[[2]])),
    section = str_trim(as.character(df[[3]])),
    stringsAsFactors = FALSE
  ) %>% filter(section != "", item != "", diff != "")

  if (nrow(tmp) == 0) {
    return(data.frame(section = character(), impact = character(),
                      stringsAsFactors = FALSE))
  }

  tmp$section <- html_repair_section_titles(tmp$section)
  tmp %>%
    group_by(section) %>%
    summarise(impact = paste(html_compact_impact_lines(diff, item),
                             collapse = "\n\n"), .groups = "drop")
}

html_merge_same_section <- function(summary_df) {
  if (nrow(summary_df) == 0) return(summary_df)
  summary_df$section <- html_repair_section_titles(summary_df$section)
  section_order <- unique(summary_df$section)

  out <- lapply(section_order, function(current_section) {
    g <- summary_df[summary_df$section == current_section, , drop = FALSE]
    diff_order <- character()
    diff_label <- list()
    diff_items <- list()

    for (impact in g$impact) {
      lines <- unlist(str_split(impact, "\\r?\\n+"))
      for (line in lines) {
        line <- str_trim(line)
        if (line == "") next
        parts <- str_split_fixed(line, "\\s+", 2)
        diff_name <- parts[1]
        item_text <- parts[2]
        diff_key <- tolower(diff_name)
        if (!(diff_key %in% diff_order)) {
          diff_order <- c(diff_order, diff_key)
          diff_label[[diff_key]] <- diff_name
          diff_items[[diff_key]] <- character()
        }
        items <- str_trim(unlist(str_split(item_text, "\\s*,\\s*")))
        items <- items[items != ""]
        diff_items[[diff_key]] <- unique(c(diff_items[[diff_key]], items))
      }
    }

    impact_lines <- vapply(diff_order, function(k) {
      paste0(diff_label[[k]], " ", paste(diff_items[[k]], collapse = ", "))
    }, character(1))

    data.frame(section = current_section,
               impact = paste(impact_lines, collapse = "\n\n"),
               stringsAsFactors = FALSE)
  })
  bind_rows(out)
}

html_make_summary <- function(file_path) {
  doc <- read_html(file_path, options = c("RECOVER", "NOERROR", "NOWARNING"))
  glossary <- html_parse_target_table(doc, "GLOSSARY_TBL")
  design_rule <- html_parse_target_table(doc, "DESIGN_RULE_TBL")
  summary_df <- bind_rows(html_make_summary_one_table(glossary),
                          html_make_summary_one_table(design_rule))
  if (nrow(summary_df) == 0) {
    return(data.frame(section = character(), impact = character(),
                      stringsAsFactors = FALSE))
  }
  summary_df$section <- html_repair_section_titles(summary_df$section)
  html_merge_same_section(summary_df)
}

html_write_summary_xlsx <- function(summary_df, file) {
  wb <- createWorkbook()
  addWorksheet(wb, "Summary")
  writeData(wb, "Summary", summary_df)

  header_style <- createStyle(textDecoration = "bold", halign = "center",
                              valign = "center", fgFill = "#D9EAF7",
                              border = "Bottom")
  body_style <- createStyle(valign = "top", wrapText = TRUE)
  addStyle(wb, "Summary", header_style, rows = 1,
           cols = seq_len(ncol(summary_df)), gridExpand = TRUE)

  if (nrow(summary_df) > 0) {
    addStyle(wb, "Summary", body_style,
             rows = 2:(nrow(summary_df) + 1),
             cols = seq_len(ncol(summary_df)), gridExpand = TRUE)
    line_counts <- str_count(summary_df$impact, "\n") + 1
    setRowHeights(wb, "Summary", rows = 2:(nrow(summary_df) + 1),
                  heights = pmax(30, line_counts * 18))
  }

  setColWidths(wb, "Summary", cols = 1, widths = 48)
  setColWidths(wb, "Summary", cols = 2, widths = 110)
  freezePane(wb, "Summary", firstRow = TRUE)
  addFilter(wb, "Summary", rows = 1, cols = seq_len(ncol(summary_df)))
  saveWorkbook(wb, file, overwrite = TRUE)
}

# ====================================================
# DR > PSV helpers
# ====================================================
drc_cols <- c("Name", "Notes", "Description", "Operator", "Value",
              "Multiple", "Table Title", "Table ID", "Rule Hint",
              "Rule Class", "Wafer", "Tolerance", "Priority",
              "Priority Consumer", "Priority Automotive", "DFM Category",
              "Reliability")

internal_cols <- c("Name", "Note ID", "Notes", "Description", "Operator",
                   "Value", "Multiple", "Table Title", "Table ID",
                   "Table Number", "Source File", "Chapter/Topic Full Path",
                   "Rule Hint", "Rule Class", "Wafer", "Tolerance",
                   "Priority", "Priority Consumer", "Priority Automotive",
                   "DFM Category", "Reliability")

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\r\n|\r|\n", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

empty_to <- function(x, value = "") {
  x <- clean_text(x)
  ifelse(x == "", value, x)
}

normalize_operator <- function(x) {
  x <- clean_text(x)
  x <- gsub("≥", ">=", x, fixed = TRUE)
  x <- gsub("≤", "<=", x, fixed = TRUE)
  x
}

make_valid_description <- function(desc, rule_name = "") {
  desc <- clean_text(desc)
  rule_name <- clean_text(rule_name)
  if (desc == "" && rule_name != "") desc <- paste(rule_name, "design rule.")
  desc
}

read_excel_raw <- function(path, sheet = 1) {
  df <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                           .name_repair = "minimal", guess_max = 10000)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- paste0("V", seq_len(ncol(df)))
  for (nm in names(df)) df[[nm]] <- clean_text(df[[nm]])
  df
}

detect_header_rows <- function(raw_df) {
  if (nrow(raw_df) == 0) return(integer(0))
  header_rows <- c()
  for (i in seq_len(nrow(raw_df))) {
    row_vals <- tolower(clean_text(unlist(raw_df[i, , drop = TRUE])))
    row_vals <- row_vals[row_vals != ""]
    has_rule <- any(row_vals == "rule")
    has_desc <- any(row_vals %in% c("description", "desc"))
    has_op <- any(row_vals %in% c("operator", "op"))
    has_val <- any(row_vals %in% c("value", "values", "val"))
    if (has_rule && has_desc && (has_op || has_val)) {
      header_rows <- c(header_rows, i)
    }
  }
  header_rows
}

get_col_map <- function(raw_df, header_row) {
  row_vals <- tolower(clean_text(unlist(raw_df[header_row, , drop = TRUE])))
  find_col <- function(candidates) {
    idx <- which(row_vals %in% candidates)
    if (length(idx) == 0) NA_integer_ else idx[1]
  }
  list(rule = find_col(c("rule", "name")),
       notes = find_col(c("notes", "note")),
       description = find_col(c("description", "desc")),
       operator = find_col(c("operator", "op")),
       value = find_col(c("value", "values", "val")))
}

find_section_title_before <- function(raw_df, row_index) {
  if (row_index <= 1) return("")
  for (i in seq(row_index - 1, 1, by = -1)) {
    vals <- clean_text(unlist(raw_df[i, , drop = TRUE]))
    vals <- vals[vals != ""]
    if (length(vals) == 0) next
    low <- tolower(vals)
    if (any(low == "rule") && any(low == "description")) next
    hit <- grep("design\\s+rules", vals, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
    if (length(vals) == 1) return(vals[1])
  }
  ""
}

parse_rule_excel <- function(path, sheet = 1) {
  raw_df <- read_excel_raw(path, sheet)
  empty_base <- function() data.frame(
    Name = character(), Notes = character(), Description = character(),
    Operator = character(), Value = character(), `Table Title` = character(),
    stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(raw_df) == 0 || ncol(raw_df) == 0) return(empty_base())

  header_rows <- detect_header_rows(raw_df)
  if (length(header_rows) == 0) header_rows <- 1L
  out <- list()

  for (h_idx in seq_along(header_rows)) {
    h <- header_rows[h_idx]
    next_h <- if (h_idx < length(header_rows)) header_rows[h_idx + 1] else nrow(raw_df) + 1
    col_map <- get_col_map(raw_df, h)
    if (is.na(col_map$rule) || is.na(col_map$description)) next
    section_title <- find_section_title_before(raw_df, h)
    if (section_title == "") section_title <- "Design Rules"
    start_row <- h + 1
    end_row <- next_h - 1
    if (start_row > end_row) next

    for (r in start_row:end_row) {
      rule_name <- clean_text(raw_df[r, col_map$rule])
      notes <- if (!is.na(col_map$notes)) clean_text(raw_df[r, col_map$notes]) else ""
      desc <- clean_text(raw_df[r, col_map$description])
      op <- if (!is.na(col_map$operator)) normalize_operator(raw_df[r, col_map$operator]) else ""
      val <- if (!is.na(col_map$value)) clean_text(raw_df[r, col_map$value]) else ""
      if (rule_name == "" && notes == "" && desc == "" && op == "" && val == "") next
      if (rule_name == "" && grepl("design\\s+rules", desc, ignore.case = TRUE)) next
      if (tolower(rule_name) %in% c("rule", "name")) next
      if (rule_name == "" && desc == "") next
      out[[length(out) + 1]] <- data.frame(
        Name = rule_name, Notes = ifelse(notes == "", "-", notes),
        Description = make_valid_description(desc, rule_name), Operator = op,
        Value = val, `Table Title` = section_title,
        stringsAsFactors = FALSE, check.names = FALSE)
    }
  }

  if (length(out) == 0) return(empty_base())
  base <- do.call(rbind, out)
  base$Name <- empty_to(base$Name, "")
  base$Notes <- empty_to(base$Notes, "-")
  base$Description <- empty_to(base$Description, "")
  base$Operator <- normalize_operator(base$Operator)
  base$Value <- empty_to(base$Value, "")
  base$`Table Title` <- empty_to(base$`Table Title`, "Design Rules")
  rownames(base) <- NULL
  base
}

make_drc_psv_like <- function(base) {
  n <- nrow(base)
  df <- data.frame(matrix("", nrow = n, ncol = length(drc_cols)),
                   stringsAsFactors = FALSE)
  names(df) <- drc_cols
  if (n == 0) return(df)
  df$Name <- base$Name; df$Notes <- base$Notes
  df$Description <- base$Description; df$Operator <- base$Operator
  df$Value <- base$Value; df$`Table Title` <- base$`Table Title`
  for (cc in c("Rule Class", "Wafer", "Tolerance", "Priority",
               "Priority Consumer", "Priority Automotive", "DFM Category",
               "Reliability")) df[[cc]] <- "^"
  df
}

make_internal_psv_like <- function(base) {
  n <- nrow(base)
  df <- data.frame(matrix("", nrow = n, ncol = length(internal_cols)),
                   stringsAsFactors = FALSE)
  names(df) <- internal_cols
  if (n == 0) return(df)
  df$Name <- base$Name; df$`Note ID` <- "-"; df$Notes <- base$Notes
  df$Description <- base$Description; df$Operator <- base$Operator
  df$Value <- base$Value; df$`Table Title` <- base$`Table Title`
  df$`Chapter/Topic Full Path` <- base$`Table Title`
  for (cc in c("Rule Class", "Wafer", "Tolerance", "Priority",
               "Priority Consumer", "Priority Automotive", "DFM Category",
               "Reliability")) df[[cc]] <- "^"
  df
}

write_psv <- function(df, file) {
  utils::write.table(df, file = file, sep = "|", row.names = FALSE,
                     col.names = TRUE, quote = FALSE, na = "",
                     fileEncoding = "UTF-8")
}

one_page_dt <- function(df) {
  DT::datatable(df, rownames = FALSE, filter = "top",
                options = list(paging = FALSE, lengthChange = FALSE,
                               dom = "t", scrollX = TRUE,
                               scrollY = "650px"))
}

# ====================================================
# UI
# ====================================================
ui <- fluidPage(
  titlePanel("app for rev/LMS/psv/PID/BEOL etc"),
  tabsetPanel(
    tabPanel(
      "HTML>rev",
      sidebarLayout(
        sidebarPanel(
          fileInput("html_file", "Upload HTML diff", accept = c(".html", ".htm")),
          downloadButton("download_xlsx", "Download rev XLSX")
        ),
        mainPanel(h4("Preview"), DTOutput("preview"))
      )
    ),
    tabPanel(
      "LMS>cdslib",
      sidebarLayout(
        sidebarPanel(
          fileInput("input1_file", "Upload input1 LMS rawdata (.csv)", accept = ".csv"),
          fileInput("input2_file", "Upload input2 DM common layers",
                    accept = c(".txt", ".csv", ".xlsx", ".xls")),
          textInput("tech_variant", "Tech Variant - Included in PDK",
                    placeholder = "Example: 55BCD"),
          div(style = "background-color:#eef6ff;border-left:4px solid #2c7be5;padding:10px;margin-top:10px;",
              "cdslib == Filter: Active + Tech Variant + Design Layer=Yes"),
          br(),
          helpText("Tech Variant filter matches an exact semicolon-separated token."),
          hr(),
          downloadButton("download_dm", "Download DM Excel"), br(), br(),
          downloadButton("download_cdslib", "Download cdslib Excel"), br(), br(),
          downloadButton("download_diff", "Download Diff Excel")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("DM", h3("DM preview"),
                     textInput("dm_search", "Search"),
                     fluidRow(column(6, h4(textOutput("dm_total_count"))),
                              column(6, h4(textOutput("dm_shown_count")))),
                     DTOutput("dm_table")),
            tabPanel("cdslib", h3("cdslib result"),
                     textInput("cdslib_search", "Search"),
                     fluidRow(column(6, h4(textOutput("cdslib_total_count"))),
                              column(6, h4(textOutput("cdslib_shown_count")))),
                     DTOutput("cdslib_table")),
            tabPanel("Diff", h3("Diff report: cdslib GDS vs DM GDS"),
                     textInput("diff_search", "Search"),
                     fluidRow(column(6, h4(textOutput("diff_total_count"))),
                              column(6, h4(textOutput("diff_shown_count")))),
                     DTOutput("diff_table"))
          )
        )
      )
    ),
    tabPanel(
      "DR>PSV",
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Upload DR Excel (5 col format)", accept = c(".xlsx", ".xls")),
          tags$hr(),
          downloadButton("download_drc", "Download drc.psv"), br(), br(),
          downloadButton("download_internal", "Download internal.psv"),
          tags$hr(),
          helpText("Auto-detects Rule / Notes / Description / Operator / Value(s) headers.")
        ),
        mainPanel(tabsetPanel(
          tabPanel("Tab1: drc.psv-like preview", br(), DTOutput("drc_preview")),
          tabPanel("Tab2: internal.psv-like preview", br(), DTOutput("internal_preview")),
          tabPanel("Tab3: input file preview", br(), DTOutput("input_preview"))
        ))
      )
    ),
    tabPanel(
      "PID>YX>BEOL#",
      sidebarLayout(
        sidebarPanel(
          fileInput("files_app2", "Upload YX# Files (only excel)", multiple = TRUE,
                    accept = c(".xls", ".xlsx")),
          hr(), downloadButton("downloadCombined", "Download PID masks"),
          br(), br(), downloadButton("downloadMatrix", "Download BEOL 2D Matrix")
        ),
        mainPanel(tabsetPanel(
          tabPanel("preview1", DTOutput("combinedTable")),
          tabPanel("preview2", DTOutput("matrixTable"))
        ))
      )
    )
  )
)

# ====================================================
# Server
# ====================================================
server <- function(input, output, session) {
  # HTML > rev
  summary_data <- reactive({
    req(input$html_file)
    html_make_summary(input$html_file$datapath)
  })

  output$preview <- renderDT({
    datatable(summary_data(), rownames = FALSE,
              options = list(paging = FALSE, scrollX = TRUE), escape = FALSE)
  })

  output$download_xlsx <- downloadHandler(
    filename = function() paste0("DMDRC_diff_summary_by_section_compact_",
                                 format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) html_write_summary_xlsx(summary_data(), file)
  )

  # LMS > cdslib
  input1_df <- reactive({
    req(input$input1_file)
    read_uploaded_table(input$input1_file$datapath, input$input1_file$name)
  })
  input2_df <- reactive({
    req(input$input2_file)
    read_uploaded_table(input$input2_file$datapath, input$input2_file$name)
  })
  dm_df <- reactive({
    df <- input2_df()
    if (nrow(df) > 0 && "CAD Level" %in% names(df)) df <- df %>% arrange(`CAD Level`)
    df
  })
  cdslib_df <- reactive(build_cdslib_df(input1_df(), input$tech_variant))
  diff_df <- reactive(build_diff_df(dm_df(), cdslib_df(), input1_df()))
  dm_view <- reactive(apply_search(dm_df(), input$dm_search))
  cdslib_view <- reactive(apply_search(cdslib_df(), input$cdslib_search))
  diff_view <- reactive(apply_search(diff_df(), input$diff_search))

  output$dm_total_count <- renderText(paste("Total rows:", nrow(dm_df())))
  output$dm_shown_count <- renderText(paste("Shown rows:", nrow(dm_view())))
  output$cdslib_total_count <- renderText(paste("Total rows:", nrow(cdslib_df())))
  output$cdslib_shown_count <- renderText(paste("Shown rows:", nrow(cdslib_view())))
  output$diff_total_count <- renderText(paste("Total rows:", nrow(diff_df())))
  output$diff_shown_count <- renderText(paste("Shown rows:", nrow(diff_view())))

  output$dm_table <- renderDT({
    datatable(dm_view(), rownames = FALSE, filter = "top",
              options = list(scrollX = TRUE, paging = FALSE,
                             lengthChange = FALSE, autoWidth = TRUE)) %>%
      formatStyle(columns = names(dm_view()), `white-space` = "nowrap")
  })
  output$cdslib_table <- renderDT({
    datatable(cdslib_view(), rownames = FALSE, filter = "top",
              options = list(scrollX = TRUE, paging = FALSE,
                             lengthChange = FALSE, autoWidth = TRUE)) %>%
      formatStyle(columns = names(cdslib_view()), `white-space` = "nowrap")
  })
  output$diff_table <- renderDT({
    datatable(diff_view(), rownames = FALSE, filter = "top",
              options = list(scrollX = TRUE, paging = FALSE,
                             lengthChange = FALSE, autoWidth = TRUE)) %>%
      formatStyle(columns = names(diff_view()), `white-space` = "normal",
                  `word-wrap` = "break-word")
  })

  output$download_dm <- downloadHandler(
    filename = function() paste0("DM_preview_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(dm_view(), file, "DM_preview"))
  output$download_cdslib <- downloadHandler(
    filename = function() paste0("cdslib_result_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(cdslib_view(), file, "cdslib_result"))
  output$download_diff <- downloadHandler(
    filename = function() paste0("Diff_report_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(diff_view(), file, "Diff_report"))

  # PID > YX > BEOL
  combined_data <- reactive({
    req(input$files_app2)
    all_sections <- list()
    for (f in input$files_app2$datapath) {
      all_sections <- c(all_sections, extract_beol_sections(f))
    }
    if (length(all_sections) == 0) return(data.frame())
    max_len <- max(vapply(all_sections, length, integer(1)))
    all_sections <- lapply(all_sections, function(x) {
      length(x) <- max_len
      x
    })
    as.data.frame(all_sections, stringsAsFactors = FALSE, check.names = FALSE)
  })

  matrix_data <- reactive({
    df <- combined_data()
    req(ncol(df) > 0)
    unique_items <- unique(unlist(df, use.names = FALSE))
    unique_items <- unique_items[!is.na(unique_items) & unique_items != ""]
    mat <- data.frame(Mask_Layer = unique_items, stringsAsFactors = FALSE,
                      check.names = FALSE)
    for (col in names(df)) {
      vals <- unique(na.omit(df[[col]]))
      mat[[col]] <- ifelse(mat$Mask_Layer %in% vals, "x", "")
    }
    mat
  })

  output$combinedTable <- renderDT({
    datatable(t(combined_data()),
              options = list(scrollX = TRUE, paging = FALSE,
                             lengthChange = FALSE, searching = TRUE, info = TRUE),
              class = "nowrap display")
  })
  output$matrixTable <- renderDT({
    datatable(matrix_data(), rownames = FALSE,
              options = list(scrollX = TRUE, paging = FALSE,
                             lengthChange = FALSE, searching = TRUE, info = TRUE),
              class = "nowrap display")
  })
  output$downloadCombined <- downloadHandler(
    filename = function() paste0("PID_masks_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(combined_data(), file, "PID_masks"))
  output$downloadMatrix <- downloadHandler(
    filename = function() paste0("BEOL_matrix_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(matrix_data(), file, "BEOL_matrix"))

  # DR > PSV
  selected_sheet <- reactive({
    req(input$file1)
    readxl::excel_sheets(input$file1$datapath)[1]
  })
  base_rules <- reactive({
    req(input$file1)
    parse_rule_excel(input$file1$datapath, sheet = selected_sheet())
  })
  input_raw_preview <- reactive({
    req(input$file1)
    read_excel_raw(input$file1$datapath, sheet = selected_sheet())
  })
  drc_result <- reactive(make_drc_psv_like(base_rules()))
  internal_result <- reactive(make_internal_psv_like(base_rules()))
  output$drc_preview <- renderDT(one_page_dt(drc_result()))
  output$internal_preview <- renderDT(one_page_dt(internal_result()))
  output$input_preview <- renderDT(one_page_dt(input_raw_preview()))
  output$download_drc <- downloadHandler(
    filename = function() "drc_converted.psv",
    content = function(file) write_psv(drc_result(), file))
  output$download_internal <- downloadHandler(
    filename = function() "internal_converted.psv",
    content = function(file) write_psv(internal_result(), file))
}

shinyApp(ui, server)
