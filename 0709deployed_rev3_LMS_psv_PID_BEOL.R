library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(openxlsx)
library(rvest)
library(stringr)

# ====================================================
# Helpers 
# ====================================================
read_uploaded_table <- function(path, filename = "") {
  if (is.null(path) || path == "") return(data.frame())
  ext <- tolower(tools::file_ext(filename))
  if (ext == "") ext <- tolower(tools::file_ext(path))
  
  if (ext == "csv") {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c(""))
  } else if (ext == "txt") {
    # input2 format is tab-separated. Keep text columns unchanged.
    df <- tryCatch(
      read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                 quote = "", fill = TRUE, na.strings = c("")),
      error = function(e) data.frame()
    )
    
    # Fallback if not parsed into columns.
    if (ncol(df) <= 1) {
      df <- read.table(path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                       fill = TRUE, na.strings = c(""))
    }
  } else if (ext %in% c("xls", "xlsx")) {
    df <- as.data.frame(read_excel(path), stringsAsFactors = FALSE, check.names = FALSE)
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

trim_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

tech_variant_match <- function(values, keyword) {
  kw <- trimws(as.character(keyword %||% ""))
  vals <- trim_chr(values)
  
  if (kw == "") return(rep(TRUE, length(vals)))
  
  sapply(vals, function(v) {
    tokens <- trimws(unlist(strsplit(v, ";", fixed = TRUE)))
    any(tokens == kw)
  })
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
  wb <- createWorkbook()
  addWorksheet(wb, safe_sheet_name(sheet_name))
  writeData(wb, safe_sheet_name(sheet_name), df)
  saveWorkbook(wb, file, overwrite = TRUE)
}


read_file <- function(path) {
  read_uploaded_table(path, path)
}

read_beol_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    df <- read.csv(path, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
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
    if (length(mask_idx) == 0) next
    
    layer_data <- section[(mask_idx[1] + 1):length(section)]
    layer_data <- layer_data[!is.na(layer_data) & layer_data != ""]
    result[[beol_name]] <- layer_data
  }
  
  result
}

build_cdslib_df <- function(input1_df, tech_variant) {
  required_cols <- c("Layer Status", "Design Layer", "Tech Variant - Included in PDK")
  missing_cols <- setdiff(required_cols, names(input1_df))
  if (length(missing_cols) > 0) {
    stop(paste("Input1 file is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  mask <- trim_chr(input1_df[["Layer Status"]]) == "Active" &
    trim_chr(input1_df[["Design Layer"]]) == "Yes" &
    tech_variant_match(input1_df[["Tech Variant - Included in PDK"]], tech_variant)
  
  input1_df[mask, , drop = FALSE] %>%
    as.data.frame(stringsAsFactors = FALSE, check.names = FALSE)
}

build_diff_df <- function(dm_df, cdslib_df, input1_df) {
  if (nrow(dm_df) == 0 || nrow(cdslib_df) == 0) return(data.frame())
  
  if (!("GDS" %in% names(dm_df))) {
    stop("Input2 / DM tab is missing required column: GDS")
  }
  if (!("GDS Number" %in% names(cdslib_df)) || !("GDS Datatype" %in% names(cdslib_df))) {
    stop("cdslib result is missing required columns: GDS Number and/or GDS Datatype")
  }
  
  dm_base <- dm_df
  dm_base[["GDS"]] <- trim_chr(dm_base[["GDS"]])
  
  dm_summary <- data.frame(
    `CAD Level` = if ("CAD Level" %in% names(dm_base)) dm_base[["CAD Level"]] else rep("", nrow(dm_base)),
    Description = if ("Description" %in% names(dm_base)) dm_base[["Description"]] else rep("", nrow(dm_base)),
    GDS = dm_base[["GDS"]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) %>% distinct()
  
  cdslib_base <- cdslib_df
  cdslib_base[["GDS"]] <- make_gds_from_cols(cdslib_base)
  
  cdslib_summary <- data.frame(
    `CAD Level` = if ("Data Layer Name" %in% names(cdslib_base)) cdslib_base[["Data Layer Name"]] else rep("", nrow(cdslib_base)),
    Description = if ("Layer Description" %in% names(cdslib_base)) cdslib_base[["Layer Description"]] else rep("", nrow(cdslib_base)),
    GDS = cdslib_base[["GDS"]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) %>% distinct()
  
  dm_gds <- unique(trim_chr(dm_summary[["GDS"]]))
  cdslib_gds <- unique(trim_chr(cdslib_summary[["GDS"]]))
  
  only_dm <- dm_summary[dm_summary[["GDS"]] %in% setdiff(dm_gds, cdslib_gds), , drop = FALSE]
  if (nrow(only_dm) > 0) only_dm[["Delta"]] <- "Only_in_DM"
  
  only_cdslib <- cdslib_summary[cdslib_summary[["GDS"]] %in% setdiff(cdslib_gds, dm_gds), , drop = FALSE]
  if (nrow(only_cdslib) > 0) only_cdslib[["Delta"]] <- "Only_in_cdslib"
  
  diff_summary <- bind_rows(only_dm, only_cdslib) %>%
    select(`CAD Level`, Description, GDS, Delta) %>%
    distinct()
  
  if (nrow(diff_summary) == 0) {
    return(data.frame(
      `CAD Level` = character(),
      Description = character(),
      GDS = character(),
      Delta = character(),
      check.names = FALSE
    ))
  }
  
  if (nrow(input1_df) > 0 && all(c("GDS Number", "GDS Datatype") %in% names(input1_df))) {
    lms_full <- input1_df
    lms_full[["GDS"]] <- make_gds_from_cols(lms_full)
    
    rename_cols <- intersect(c("CAD Level", "Description", "Delta"), names(lms_full))
    for (col in rename_cols) {
      names(lms_full)[names(lms_full) == col] <- paste0("LMS_", col)
    }
    
    final_df <- left_join(diff_summary, lms_full, by = "GDS")
  } else {
    final_df <- diff_summary
  }
  
  first_cols <- c("CAD Level", "Description", "GDS", "Delta")
  other_cols <- setdiff(names(final_df), first_cols)
  final_df <- final_df[, c(first_cols, other_cols), drop = FALSE]
  
  final_df %>% arrange(Delta, GDS, `CAD Level`)
}

# `%||%` helper
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ====================================================
# UI
# ====================================================


# ====================================================
# HTML REV helpers (from 0708HTMLrev4.R)
# ====================================================
# app.R
# DM Diff HTML -> Section Impact Summary
# Refined version: column 2 removes duplicate Diff terms within each section.
# Example:
#   Added DPW.S.RX.1
#   Modified DPW.EN.LVPW.1
#   Modified DPW.EN.NW.1
# becomes:
#   Added DPW.S.RX.1
#   Modified DPW.EN.LVPW.1 DPW.EN.NW.1 
#
# How to run:
#   install.packages(c("shiny", "rvest", "dplyr", "stringr", "openxlsx", "DT"))
#   shiny::runApp("path/to/folder containing app.R")


html_clean_text <- function(x) {
  x <- html_text2(x)
  x <- str_replace_all(x, "\\u00a0", " ")
  x <- str_replace_all(x, "\\s+", " ")
  str_trim(x)
}

html_parse_target_table <- function(doc, table_id) {
  tbl <- html_element(doc, paste0("table#", table_id))
  if (length(tbl) == 0 || is.na(tbl)) return(data.frame())

  headers <- tbl %>%
    html_elements("thead th") %>%
    html_clean_text()

  rows <- tbl %>% html_elements("tbody tr")
  if (length(rows) == 0) return(data.frame())

  out <- lapply(rows, function(r) {
    cells <- r %>% html_elements("td") %>% html_clean_text()
    if (length(cells) < length(headers)) {
      cells <- c(cells, rep("", length(headers) - length(cells)))
    }
    cells[seq_along(headers)]
  })

  df <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  names(df) <- headers
  df
}

html_compact_impact_lines <- function(diff_vec, item_vec) {
  # Preserve first-seen order of Diff terms, then combine all unique items under each Diff.
  impact_lines <- character()
  seen_diff <- character()

  for (d in diff_vec) {
    d_key <- tolower(str_trim(d))
    if (d_key == "" || d_key %in% seen_diff) next

    seen_diff <- c(seen_diff, d_key)
    idx <- which(tolower(str_trim(diff_vec)) == d_key)

    items <- item_vec[idx]
    items <- items[str_trim(items) != ""]
    items <- unique(items)

    impact_lines <- c(impact_lines, str_squish(paste(d, paste(items, collapse = " "))))
  }

  impact_lines
}

html_make_summary_one_table <- function(df) {
  # Required logic kept:
  #   input column 3 -> output column 1 = section
  #   input column 1 + input column 2 -> output column 2 = impact
  #   group by section
  # Refined display:
  #   within same section, repeated Diff terms are printed once only.
  if (nrow(df) == 0 || ncol(df) < 3) {
    return(data.frame(section = character(), impact = character(), stringsAsFactors = FALSE))
  }

  tmp <- data.frame(
    diff = str_trim(as.character(df[[1]])),
    item = str_trim(as.character(df[[2]])),
    section = str_trim(as.character(df[[3]])),
    stringsAsFactors = FALSE
  ) %>%
    filter(section != "", item != "", diff != "")

  if (nrow(tmp) == 0) {
    return(data.frame(section = character(), impact = character(), stringsAsFactors = FALSE))
  }

  tmp %>%
    group_by(section) %>%
    summarise(
      impact = paste(html_compact_impact_lines(diff, item), collapse = "\n"),
      .groups = "drop"
    )
}

html_merge_same_section <- function(summary_df) {
  # Handles cases where the same section appears from both GLOSSARY_TBL and DESIGN_RULE_TBL.
  # Keeps the same calculation, but avoids repeating the same Diff word in output impact.
  if (nrow(summary_df) == 0) return(summary_df)

  out <- lapply(split(summary_df, summary_df$section), function(g) {
    diff_order <- character()
    diff_label <- list()
    diff_items <- list()

    for (impact in g$impact) {
      lines <- unlist(str_split(impact, "\n"))
      for (line in lines) {
        line <- str_trim(line)
        if (line == "") next

        parts <- str_split_fixed(line, "\\s+", 2)
        d <- parts[1]
        rest <- parts[2]
        d_key <- tolower(d)

        if (!(d_key %in% diff_order)) {
          diff_order <- c(diff_order, d_key)
          diff_label[[d_key]] <- d
          diff_items[[d_key]] <- character()
        }

        items <- unlist(str_split(rest, "\\s+"))
        items <- items[items != ""]
        diff_items[[d_key]] <- unique(c(diff_items[[d_key]], items))
      }
    }

    impact <- paste(
      sapply(diff_order, function(k) {
        str_squish(paste(diff_label[[k]], paste(diff_items[[k]], collapse = " ")))
      }),
      collapse = "\n"
    )

    data.frame(section = g$section[1], impact = impact, stringsAsFactors = FALSE)
  })

  bind_rows(out)
}

html_make_summary <- function(file_path) {
  doc <- read_html(file_path)

  glossary <- html_parse_target_table(doc, "GLOSSARY_TBL")
  design_rule <- html_parse_target_table(doc, "DESIGN_RULE_TBL")

  bind_rows(
    html_make_summary_one_table(glossary),
    html_make_summary_one_table(design_rule)
  ) %>%
    html_merge_same_section()
}

html_write_summary_xlsx <- function(summary_df, file) {
  wb <- createWorkbook()
  addWorksheet(wb, "Summary")
  writeData(wb, "Summary", summary_df)

  header_style <- createStyle(
    textDecoration = "bold",
    halign = "center",
    fgFill = "#D9EAF7"
  )

  body_style <- createStyle(
    valign = "top",
    wrapText = TRUE
  )

  addStyle(
    wb,
    "Summary",
    header_style,
    rows = 1,
    cols = 1:ncol(summary_df),
    gridExpand = TRUE
  )

  if (nrow(summary_df) > 0) {
    addStyle(
      wb,
      "Summary",
      body_style,
      rows = 2:(nrow(summary_df) + 1),
      cols = 1:ncol(summary_df),
      gridExpand = TRUE
    )
  }

  setColWidths(wb, "Summary", cols = 1, widths = 48)
  setColWidths(wb, "Summary", cols = 2, widths = 110)
  freezePane(wb, "Summary", firstRow = TRUE)
  addFilter(wb, "Summary", rows = 1, cols = 1:ncol(summary_df))
  saveWorkbook(wb, file, overwrite = TRUE)
}


# ====================================================
# DR > PSV helpers (from 0708DRtoPSV1.R)
# ====================================================

# app.R
# One-file R Shiny app: Excel rule table -> drc.psv-like and internal.psv-like outputs
# Workflow:
# 1) Upload Excel file
# 2) Convert uploaded Excel rule table to drc.psv format preview / download
# 3) Convert uploaded Excel rule table to internal.psv format preview / download
# 4) Preview tables show one full page, no "entries" selector
# 5) Tab3 previews the uploaded input Excel table
# 6) No Excel sheet selector in left sidebar; app automatically uses the first sheet

# ============================================================
# Package check 
# ============================================================
required_pkgs <- c("shiny", "readxl", "DT")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    paste0(
      "Missing required package(s): ",
      paste(missing_pkgs, collapse = ", "),
      "\nPlease install first, for example:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}


# ============================================================
# Template PSV column formats
# ============================================================
drc_cols <- c(
  "Name", "Notes", "Description", "Operator", "Value", "Multiple",
  "Table Title", "Table ID", "Rule Hint", "Rule Class", "Wafer",
  "Tolerance", "Priority", "Priority Consumer", "Priority Automotive",
  "DFM Category", "Reliability"
)

internal_cols <- c(
  "Name", "Note ID", "Notes", "Description", "Operator", "Value", "Multiple",
  "Table Title", "Table ID", "Table Number", "Source File",
  "Chapter/Topic Full Path", "Rule Hint", "Rule Class", "Wafer",
  "Tolerance", "Priority", "Priority Consumer", "Priority Automotive",
  "DFM Category", "Reliability"
)

# ============================================================
# Small helpers
# ============================================================
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
  x <- gsub("\u2265", ">=", x, fixed = TRUE)
  x <- gsub("\u2264", "<=", x, fixed = TRUE)
  x <- gsub("\u003e", ">", x, fixed = TRUE)
  x <- gsub("\u003c", "<", x, fixed = TRUE)
  x
}

make_valid_description <- function(desc, rule_name = "") {
  desc <- clean_text(desc)
  rule_name <- clean_text(rule_name)

  if (desc == "" && rule_name != "") {
    desc <- paste(rule_name, "design rule.")
  }

  desc
}

# Read uploaded Excel as raw text; no header because source may contain title/header blocks.
read_excel_raw <- function(path, sheet = 1) {
  df <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal",
    guess_max = 10000
  )

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- paste0("V", seq_len(ncol(df)))

  for (nm in names(df)) {
    df[[nm]] <- clean_text(df[[nm]])
  }

  df
}

# Find repeated header rows that contain Rule / Notes / Description / Operator / Value(s).
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

# Get column positions from a header row.
get_col_map <- function(raw_df, header_row) {
  row_vals <- tolower(clean_text(unlist(raw_df[header_row, , drop = TRUE])))

  find_col <- function(candidates) {
    idx <- which(row_vals %in% candidates)
    if (length(idx) == 0) return(NA_integer_)
    idx[1]
  }

  list(
    rule = find_col(c("rule", "name")),
    notes = find_col(c("notes", "note")),
    description = find_col(c("description", "desc")),
    operator = find_col(c("operator", "op")),
    value = find_col(c("value", "values", "val"))
  )
}

# Detect nearest section/table title before a header row or data row.
# Example: "M1 Design Rules", "TD Design Rules"
find_section_title_before <- function(raw_df, row_index) {
  if (row_index <= 1) return("")

  for (i in seq(row_index - 1, 1, by = -1)) {
    vals <- clean_text(unlist(raw_df[i, , drop = TRUE]))
    vals <- vals[vals != ""]

    if (length(vals) == 0) next

    # Ignore ordinary header rows
    low <- tolower(vals)
    if (any(low == "rule") && any(low == "description")) next

    # Prefer cells containing Design Rules
    hit <- grep("design\\s+rules", vals, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])

    # Otherwise use a single non-empty line as the title
    if (length(vals) == 1) return(vals[1])
  }

  ""
}

# Main parser: convert source Excel blocks to normalized base table
parse_rule_excel <- function(path, sheet = 1) {
  raw_df <- read_excel_raw(path, sheet)

  if (nrow(raw_df) == 0 || ncol(raw_df) == 0) {
    return(data.frame(
      Name = character(),
      Notes = character(),
      Description = character(),
      Operator = character(),
      Value = character(),
      `Table Title` = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  header_rows <- detect_header_rows(raw_df)

  # Fallback: if no header row found, try to use first row as a simple header.
  if (length(header_rows) == 0) {
    header_rows <- 1L
  }

  out <- list()

  for (h_idx in seq_along(header_rows)) {
    h <- header_rows[h_idx]
    next_h <- if (h_idx < length(header_rows)) header_rows[h_idx + 1] else (nrow(raw_df) + 1)

    col_map <- get_col_map(raw_df, h)

    # Need at least Rule/Name and Description to convert
    if (is.na(col_map$rule) || is.na(col_map$description)) next

    section_title <- find_section_title_before(raw_df, h)
    if (section_title == "") {
      section_title <- "Design Rules"
    }

    start_row <- h + 1
    end_row <- next_h - 1
    if (start_row > end_row) next

    for (r in start_row:end_row) {
      rule_name <- clean_text(raw_df[r, col_map$rule])

      notes <- ""
      if (!is.na(col_map$notes)) notes <- clean_text(raw_df[r, col_map$notes])

      desc <- clean_text(raw_df[r, col_map$description])

      op <- ""
      if (!is.na(col_map$operator)) op <- normalize_operator(raw_df[r, col_map$operator])

      val <- ""
      if (!is.na(col_map$value)) val <- clean_text(raw_df[r, col_map$value])

      # Skip fully blank rows
      if (rule_name == "" && notes == "" && desc == "" && op == "" && val == "") next

      # Skip section title rows inside a block
      if (rule_name == "" && grepl("design\\s+rules", desc, ignore.case = TRUE)) next

      # Skip non-rule lines
      if (tolower(rule_name) %in% c("rule", "name")) next

      # Keep rows that have a rule name or meaningful description
      if (rule_name == "" && desc == "") next

      out[[length(out) + 1]] <- data.frame(
        Name = rule_name,
        Notes = ifelse(notes == "", "-", notes),
        Description = make_valid_description(desc, rule_name),
        Operator = op,
        Value = val,
        `Table Title` = section_title,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  if (length(out) == 0) {
    return(data.frame(
      Name = character(),
      Notes = character(),
      Description = character(),
      Operator = character(),
      Value = character(),
      `Table Title` = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  base <- do.call(rbind, out)

  # Extra cleanup
  base$Name <- empty_to(base$Name, "")
  base$Notes <- empty_to(base$Notes, "-")
  base$Description <- empty_to(base$Description, "")
  base$Operator <- normalize_operator(base$Operator)
  base$Value <- empty_to(base$Value, "")
  base$`Table Title` <- empty_to(base$`Table Title`, "Design Rules")

  rownames(base) <- NULL
  base
}

# Convert normalized base table to drc.psv-like format
make_drc_psv_like <- function(base) {
  n <- nrow(base)

  df <- data.frame(matrix("", nrow = n, ncol = length(drc_cols)), stringsAsFactors = FALSE)
  names(df) <- drc_cols

  if (n == 0) return(df)

  df$Name <- base$Name
  df$Notes <- base$Notes
  df$Description <- base$Description
  df$Operator <- base$Operator
  df$Value <- base$Value
  df$Multiple <- ""
  df$`Table Title` <- base$`Table Title`
  df$`Table ID` <- ""
  df$`Rule Hint` <- ""

  caret_cols <- c(
    "Rule Class", "Wafer", "Tolerance", "Priority",
    "Priority Consumer", "Priority Automotive", "DFM Category", "Reliability"
  )

  for (cc in caret_cols) {
    df[[cc]] <- "^"
  }

  df
}

# Convert normalized base table to internal.psv-like format
make_internal_psv_like <- function(base) {
  n <- nrow(base)

  df <- data.frame(matrix("", nrow = n, ncol = length(internal_cols)), stringsAsFactors = FALSE)
  names(df) <- internal_cols

  if (n == 0) return(df)

  df$Name <- base$Name
  df$`Note ID` <- "-"
  df$Notes <- base$Notes
  df$Description <- base$Description
  df$Operator <- base$Operator
  df$Value <- base$Value
  df$Multiple <- ""
  df$`Table Title` <- base$`Table Title`
  df$`Table ID` <- ""
  df$`Table Number` <- ""
  df$`Source File` <- ""
  df$`Chapter/Topic Full Path` <- base$`Table Title`
  df$`Rule Hint` <- ""

  caret_cols <- c(
    "Rule Class", "Wafer", "Tolerance", "Priority",
    "Priority Consumer", "Priority Automotive", "DFM Category", "Reliability"
  )

  for (cc in caret_cols) {
    df[[cc]] <- "^"
  }

  df
}

# Write pipe-separated file safely.
write_psv <- function(df, file) {
  utils::write.table(
    df,
    file = file,
    sep = "|",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}

# DataTable option: one page, no entries selector
one_page_dt <- function(df) {
  DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    options = list(
      paging = FALSE,
      lengthChange = FALSE,
      dom = "t",
      scrollX = TRUE,
      scrollY = "650px"
    )
  )
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  titlePanel("app for rev/LMS/psv/PID/BEOL etc"),
  
  tabsetPanel(

    tabPanel(
      "HTML>rev",
      sidebarLayout(
        sidebarPanel(
          fileInput(
            "html_file",
            "Upload HTML diff",
            accept = c(".html", ".htm")
          ),
          downloadButton("download_xlsx", "Download rev XLSX")
        ),
        mainPanel(
          h4("Preview"),
          DTOutput("preview")
        )
      )
    ),
    tabPanel(
      "LMS>cdslib",
      sidebarLayout(
        sidebarPanel(
          fileInput("input1_file", "Upload input1 LMS rawdata (.csv)", accept = c(".csv")),
          fileInput("input2_file", "Upload input2 DM common layers (.txt)", accept = c(".txt", ".csv", ".xlsx", ".xls")),
          textInput("tech_variant", "Tech Variant - Included in PDK", placeholder = "Example: 55BCD"),
          div(
            style = "background-color:#eef6ff; border-left:4px solid #2c7be5; padding:10px; margin-top:10px;",
            "cdslib == Filter: Active + Tech Variant + Design Layer=Yes"
          ),
          br(),
          helpText("Tech Variant filter matches exact token only: exact value, value before ';', value after ';', or middle token."),
          hr(),
          downloadButton("download_dm", "Download DM Excel"),
          br(), br(),
          downloadButton("download_cdslib", "Download cdslib Excel"),
          br(), br(),
          downloadButton("download_diff", "Download Diff Excel")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "DM",
              h3("DM preview"),
              textInput("dm_search", "Search", placeholder = "Type keyword to search table..."),
              fluidRow(
                column(6, h4(textOutput("dm_total_count"))),
                column(6, h4(textOutput("dm_shown_count")))
              ),
              DTOutput("dm_table")
            ),
            tabPanel(
              "cdslib",
              h3("cdslib result"),
              p("Filter: Layer Status=Active + Design Layer=Yes + Tech Variant token match"),
              textInput("cdslib_search", "Search", placeholder = "Type keyword to search table..."),
              fluidRow(
                column(6, h4(textOutput("cdslib_total_count"))),
                column(6, h4(textOutput("cdslib_shown_count")))
              ),
              DTOutput("cdslib_table")
            ),
            tabPanel(
              "Diff",
              h3("Diff report: cdslib GDS vs DM GDS"),
              p("cdslib GDS = GDS Number:GDS Datatype. Diff GDS is mapped back to full LMS input1 rows."),
              textInput("diff_search", "Search", placeholder = "Type keyword to search table..."),
              fluidRow(
                column(6, h4(textOutput("diff_total_count"))),
                column(6, h4(textOutput("diff_shown_count")))
              ),
              DTOutput("diff_table")
            )
          )
        )
      )
    ),
    
    #tabPanel(
      #"diff>rev_history",
      #sidebarLayout(
        #sidebarPanel(
          #fileInput("file_tab3", "Upload DRCtrack.csv", accept = c(".csv", ".xls", ".xlsx")),
          #downloadButton("download_tab3", "Download rev history")
        #),
        #mainPanel(
          #tableOutput("impact_table")
        #)
      #)
    #),

    tabPanel(
      "DR>PSV",
      sidebarLayout(
        sidebarPanel(
          fileInput(
            "file1",
            "Upload DR Excel (5 col format)",
            accept = c(".xlsx", ".xls")
          ),
          tags$hr(),
          downloadButton("download_drc", "Download drc.psv"),
          br(), br(),
          downloadButton("download_internal", "Download internal.psv"),
          tags$hr(),
          helpText("The app auto-detects Rule / Notes / Description / Operator / Value(s) header rows.")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Tab1: drc.psv-like preview",
              br(),
              DTOutput("drc_preview")
            ),
            tabPanel(
              "Tab2: internal.psv-like preview",
              br(),
              DTOutput("internal_preview")
            ),
            tabPanel(
              "Tab3: input file preview",
              br(),
              DTOutput("input_preview")
            )
          )
        )
      )
    ),
    
    tabPanel(
      "PID>YX>BEOL#",
      sidebarLayout(
        sidebarPanel(
          fileInput("files_app2", "Upload YX# Files (only excel)", multiple = TRUE, accept = c(".xls", ".xlsx")),
          hr(),
          downloadButton("downloadCombined", "Download PID masks"),
          br(), br(),
          downloadButton("downloadMatrix", "Download BEOL 2D Matrix")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("preview1", DTOutput("combinedTable")),
            tabPanel("preview2", DTOutput("matrixTable"))
          )
        )
      )
    )

  )
)


# ====================================================
# Server
# ====================================================
server <- function(input, output, session) {
  
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
    if (nrow(df) > 0 && "CAD Level" %in% names(df)) {
      df <- df %>% arrange(`CAD Level`)
    }
    df
  })
  
  cdslib_df <- reactive({
    build_cdslib_df(input1_df(), input$tech_variant)
  })
  
  diff_df <- reactive({
    build_diff_df(dm_df(), cdslib_df(), input1_df())
  })
  
  dm_view <- reactive({
    apply_search(dm_df(), input$dm_search)
  })
  
  cdslib_view <- reactive({
    apply_search(cdslib_df(), input$cdslib_search)
  })
  
  diff_view <- reactive({
    apply_search(diff_df(), input$diff_search)
  })
  
  # =====================
  # Counts
  # =====================
  output$dm_total_count <- renderText({ paste("Total rows:", nrow(dm_df())) })
  output$dm_shown_count <- renderText({ paste("Shown rows:", nrow(dm_view())) })
  
  output$cdslib_total_count <- renderText({ paste("Total rows:", nrow(cdslib_df())) })
  output$cdslib_shown_count <- renderText({ paste("Shown rows:", nrow(cdslib_view())) })
  
  output$diff_total_count <- renderText({ paste("Total rows:", nrow(diff_df())) })
  output$diff_shown_count <- renderText({ paste("Shown rows:", nrow(diff_view())) })
  
  # =====================
  # Tables
  # =====================
  output$dm_table <- renderDT({
    datatable(
      dm_view(),
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        lengthChange = FALSE,
        autoWidth = TRUE
      )
    ) %>%
      formatStyle(
        columns = names(dm_view()),
        `white-space` = "nowrap"
      )
  })
  
  output$cdslib_table <- renderDT({
    datatable(
      cdslib_view(),
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        lengthChange = FALSE,
        autoWidth = TRUE
      )
    ) %>%
      formatStyle(
        columns = names(cdslib_view()),
        `white-space` = "nowrap"
      )
  })
  
  output$diff_table <- renderDT({
    datatable(
      diff_view(),
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        lengthChange = FALSE,
        autoWidth = TRUE
      )
    ) %>%
      formatStyle(
        columns = names(diff_view()),
        `white-space` = "normal",
        `word-wrap` = "break-word"
      )
  })
  
  # =====================
  # Downloads: Excel format
  # =====================
  output$download_dm <- downloadHandler(
    filename = function() paste0("DM_preview_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(dm_view(), file, "DM_preview")
  )
  
  output$download_cdslib <- downloadHandler(
    filename = function() paste0("cdslib_result_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(cdslib_view(), file, "cdslib_result")
  )
  

  output$download_diff <- downloadHandler(
    filename = function() paste0("Diff_report_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(diff_view(), file, "Diff_report")
  )
  
  # ==================================================
  # REV HISTORY
  # ==================================================
  source_data_tab3 <- reactive({
    req(input$file_tab3)
    read_uploaded_table(input$file_tab3$datapath, input$file_tab3$name)
  })
  
  rev_history_df <- reactive({
    df <- source_data_tab3()
    req(all(c("Rule", "Section", "Type") %in% names(df)))
    
    df %>%
      filter(!is.na(Rule), !is.na(Section), !is.na(Type), Rule != "", Section != "", Type != "") %>%
      distinct(Section, Type, Rule) %>%
      group_by(Section, Type) %>%
      summarise(Rules = paste(sort(unique(Rule)), collapse = " "), .groups = "drop") %>%
      mutate(ImpactText = paste(Type, Rules)) %>%
      group_by(Section) %>%
      summarise(impact = paste(ImpactText, collapse = " "), .groups = "drop")
  })
  
  output$impact_table <- renderTable(rev_history_df())
  
  output$download_tab3 <- downloadHandler(
    filename = function() paste0("Impact_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(rev_history_df(), file, "Impact")
  )
  
  # ==================================================
  # PID > YX > BEOL#
  # ==================================================
  combined_data <- reactive({
    req(input$files_app2)
    
    all_sections <- list()
    for (f in input$files_app2$datapath) {
      all_sections <- c(all_sections, extract_beol_sections(f))
    }
    
    if (length(all_sections) == 0) return(data.frame())
    
    max_len <- max(sapply(all_sections, length))
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
    
    mat <- data.frame(Mask_Layer = unique_items, stringsAsFactors = FALSE, check.names = FALSE)
    
    for (col in names(df)) {
      vals <- unique(na.omit(df[[col]]))
      mat[[col]] <- ifelse(mat$Mask_Layer %in% vals, "x", "")
    }
    
    mat
  })
  
  output$combinedTable <- renderDT({
    datatable(
      t(combined_data()),
      options = list(scrollX = TRUE, paging = FALSE, lengthChange = FALSE, searching = TRUE, info = TRUE),
      class = "nowrap display"
    )
  })
  
  output$matrixTable <- renderDT({
    datatable(
      matrix_data(),
      options = list(scrollX = TRUE, paging = FALSE, lengthChange = FALSE, searching = TRUE, info = TRUE),
      rownames = FALSE,
      class = "nowrap display"
    )
  })
  
  output$downloadCombined <- downloadHandler(
    filename = function() paste0("PID_masks_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(combined_data(), file, "PID_masks")
  )
  
  output$downloadMatrix <- downloadHandler(
    filename = function() paste0("BEOL_matrix_", Sys.Date(), ".xlsx"),
    content = function(file) write_df_xlsx(matrix_data(), file, "BEOL_matrix")
  )

  # ==================================================
  # HTML REV
  # ==================================================
  summary_data <- reactive({
    req(input$html_file)
    html_make_summary(input$html_file$datapath)
  })

  output$preview <- renderDT({
    datatable(
      summary_data(),
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollX = TRUE
      ),
      escape = FALSE
    )
  })

  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0("DMDRC_diff_summary_by_section_compact_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      html_write_summary_xlsx(summary_data(), file)
    }
  )


  # ==================================================
  # DR > PSV
  # ==================================================
  selected_sheet <- reactive({
    req(input$file1)
    sheets <- readxl::excel_sheets(input$file1$datapath)
    sheets[1]
  })

  base_rules <- reactive({
    req(input$file1)
    parse_rule_excel(input$file1$datapath, sheet = selected_sheet())
  })

  input_raw_preview <- reactive({
    req(input$file1)
    read_excel_raw(input$file1$datapath, sheet = selected_sheet())
  })

  drc_result <- reactive({
    make_drc_psv_like(base_rules())
  })

  internal_result <- reactive({
    make_internal_psv_like(base_rules())
  })

  output$drc_preview <- renderDT({
    one_page_dt(drc_result())
  })

  output$internal_preview <- renderDT({
    one_page_dt(internal_result())
  })

  output$input_preview <- renderDT({
    one_page_dt(input_raw_preview())
  })

  output$download_drc <- downloadHandler(
    filename = function() {
      "drc_converted.psv"
    },
    content = function(file) {
      write_psv(drc_result(), file)
    }
  )

  output$download_internal <- downloadHandler(
    filename = function() {
      "internal_converted.psv"
    },
    content = function(file) {
      write_psv(internal_result(), file)
    }
  )


}


shinyApp(ui, server)
