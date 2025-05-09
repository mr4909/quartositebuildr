#' Create Project Site Structure for Code for America Research Projects
#'
#' This function automates the creation of a file and directory structure
#' specifically designed for hosting research projects. It facilitates the rapid
#' deployment of project content by organizing files and directories in a manner
#' compatible with static site generators like Quarto.
#'
#' The function supports predefined types of sites, currently limited to "cfa"
#' projects. It performs parameter validation to ensure compatibility and prevent
#' errors during the structure setup.
#'
#' Upon execution, the user is prompted to confirm the creation process to avoid
#' unintended directory changes. The function then proceeds to create a series
#' of directories and files:
#'
#' - Directories for R, site content (_site), logos are created.
#'
#' - A custom CSS file (`styles.css`) is generated, pre-populated with basic
#'   styling and custom fonts.
#'
#' - A set of Quarto markdown files (`.qmd`) are created for various sections
#'   of the research project, such as executive summaries, analysis sections,
#'   and data dictionaries. These files include default YAML headers and are
#'   ready for further customization.
#'
#' - A `.gitignore` file to exclude certain data and temporary files from version
#'   control.
#'
#' - Lastly, the function configures a `_quarto.yml` file to define the site's
#'   structure, navigation, and thematic elements for the site.
#'
#' It's important to note that while the function provides a foundation for site
#' structure and initial file setup, further customization of content, styling,
#' and functionality is expected to be carried out by the research team
#' according to specific project needs and design guidelines.
#'
#' @param type The type of site to create, currently supporting only "cfa" as a valid type.
#'             This allows for future expansion to support different project templates.
#' @param subfolder The subfolder in which to create the site structure. Defaults to the top-level directory.
#'
#' @return Invisibly returns `NULL`. The function is executed for its side effects
#'         of creating files and directories, without a need for a return value.
#'
#' @examplesIf interactive()
#' create_site_structure(type = "cfa")
#' @export
#'
create_site_structure <- function(type, subfolder = ".") {

  # Check if 'type' is missing
  if (missing(type)) {
    stop("'type' must be provided.", call. = FALSE)
  }

  # Validate parameter 'type'
  # Currently only cfa is supported
  validate_parameters <- function(type, supported_types) {
    if (is.null(type)) {
      stop("'type' must be a character string.", call. = FALSE)
    }
    if (!is.character(type)) {
      stop("'type' must be a character string.", call. = FALSE)
    }
    if (length(type) != 1) {
      stop("'type' must be a single character string.", call. = FALSE)
    }
    if (!type %in% supported_types) {
      stop(paste("Type", sQuote(type), "is not supported."), call. = FALSE)
    }
  }

  # Confirm action with the user
  confirm_action <- function(prompt_message) {
    repeat {
      response <- tolower(readline(prompt = prompt_message))
      if (response %in% c("yes", "no")) {
        return(response == "yes")
      }
      cat("Please enter 'yes' or 'no'.\n")
    }
  }

  # Specify supported types
  # Currently, only "cfa" is supported
  supported_types <- c("cfa")
  validate_parameters(type, supported_types)

  # Ask user to confirm creation of file structure
  if (!confirm_action("Do you want to create the project site structure? (yes/no): ")) {
    cat("Operation cancelled by the user.\n")
    return(invisible(NULL))
  }

  # Ensure subfolder path exists
  if (!dir.exists(subfolder)) {
    dir.create(subfolder, recursive = TRUE)
  }

  # Default YAML header for qmd files
  default_qmd_header <- c(
    "---",
    "title: 'Title Placeholder'",
    "format:",
    "  html:",
    "    css: styles.css",
    "    embed-resources: TRUE",
    "    code-fold: true",
    "    page-layout: full",
    "    fig_caption: yes",
    "    toc: TRUE",
    "---",
    "",
    "## Header",
    "",
    "### Subheader"
  )

  # Identify qmd files to create
  files_to_create <- c("styles.css",
                       "index.qmd",
                       "executive_summary.qmd",
                       "analysis.qmd",
                       "analysis_plan.qmd",
                       "data_codebooks.qmd",
                       "contact.qmd")

  # Define directories to create
  dirs_to_create <- c("R", "_site", "img")

  # Create directories
  for (dir in dirs_to_create) {
    normalized_dir <- normalizePath(file.path(subfolder, dir), mustWork = FALSE)
    if (!file.exists(normalized_dir)) {
      dir.create(normalized_dir, recursive = TRUE)
      cat(paste0("Directory created: ", normalized_dir, "\n"))
    }
  }

  # Copy favicon.png to the img folder
  favicon_src <- cfa_sys_file("img/favicon.png")
  if (favicon_src == "") {
    stop("favicon.png not found in the inst/img directory.")
  }
  favicon_dest <- file.path(subfolder, "img", "favicon.png")
  file.copy(favicon_src, favicon_dest)
  cat("Favicon copied to img folder.\n")

  # Copy code_for_america_black.jpg to the img folder
  logo_src <- cfa_sys_file("logos/code_for_america_black.jpg")
  if (logo_src == "") {
    stop("code_for_america_black.jpg not found in the inst/logos directory.")
  }
  logo_dest <- file.path(subfolder, "img", "code_for_america_black.jpg")
  file.copy(logo_src, logo_dest)
  cat("code_for_america_black.jpg copied to img folder.\n")

  # Create .gitignore file if it doesn't exist
  gitignore_path <- file.path(subfolder, ".gitignore")
  if (!file.exists(gitignore_path)) {
    gitignore_content <- c(
      "# Ignore data files",
      "*.csv",
      "*.xlsx",
      "*.xls",
      "*.json",
      "*.xml",
      "*.rdata",
      "*.RData",
      "*.rds",
      "*.rda",
      "*.RDS",
      "*.db",
      "*.sql",
      "*.sqlite",
      "# Log and temporary files",
      "*.log",
      "*.out",
      "*~",
      "*.bak",
      "*.swp",
      "# Environment files",
      ".Renv*",
      ".Rhistory",
      ".Rproj.user",
      ".RData",
      ".Ruserdata",
      "# Directory exclusions",
      "data/",
      "cache/",
      "tmp/",
      "# Configuration files",
      "*.conf",
      ".env"
    )
    writeLines(gitignore_content, gitignore_path)
    cat(".gitignore file created.\n")
  } else {
    cat(".gitignore already exists, no changes made.\n")
  }

  # CSS styling
  css_content <- c(
    "/* styles.css */",
    "",
    "/* Import Source Sans 3 from Google Fonts */",
    "@import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&display=swap');",
    "",
    "/* Apply font site-wide */",
    "body {",
    "  font-family: \"Source Sans 3\", sans-serif;",
    "  margin: 0;",
    "  padding: 0;",
    "}",
    "",
    "/* Style for navigation menu */",
    ".navbar,",
    ".navbar-dark {",
    "  background-color: #2b1a78 !important;",
    "}",
    "",
    "/* Adjust text color in navbar */",
    ".navbar .navbar-brand,",
    ".navbar .nav-link,",
    ".navbar .navbar-nav .nav-link {",
    "  color: white !important;",
    "}",
    "",
    "/* Bold active menu item */",
    ".navbar .nav-item.active .nav-link,",
    ".navbar .nav-link.active {",
    "  font-weight: 600;",
    "}",
    "",
    "/* Improve spacing and hover states */",
    ".navbar .nav-link:hover {",
    "  text-decoration: underline;",
    "}",
    "",
    "/* Uniform dropdown style */",
    ".dropdown-menu {",
    "  background-color: white;",
    "  border: 1px solid #ccc;",
    "  border-radius: 0px;",
    "  padding: 0;",
    "  box-shadow: none !important;",
    "  margin-top: 8px;",
    "  min-width: 200px;",
    "}",
    "",
    "/* Dropdown items */",
    ".dropdown-item {",
    "  padding: 10px 16px;",
    "  font-weight: 400;",
    "  color: #2b1a78;",
    "  border-radius: 0;",
    "  transition: background-color 0.2s ease;",
    "}",
    "",
    "/* Hover effect */",
    ".dropdown-item:hover {",
    "  background-color: #f1f1f1;",
    "  color: #2b1a78;",
    "}",
    "",
    "/* Ensure consistent border radius on first/last items */",
    ".dropdown-item:first-child {",
    "  border-top-left-radius: 0px;",
    "  border-top-right-radius: 0px;",
    "}",
    ".dropdown-item:last-child {",
    "  border-bottom-left-radius: 0px;",
    "  border-bottom-right-radius: 0px;",
    "}"
  )

  # Create styles.css file if it doesn't exist
  if (!file.exists(file.path(subfolder, "styles.css"))) {
    # Create styles.css with CSS content only if it doesn't exist
    writeLines(css_content, file.path(subfolder, "styles.css"))
    cat("styles.css file created.\n")
  } else {
    cat("styles.css already exists. No changes made.\n")
  }

  # Function to generate common YAML header
  generate_yaml_header <- function(title) {
    c(
      "---",
      paste("title: '", title, "'", sep=""),
      "format:",
      "  html:",
      "    css: styles.css",
      "    embed-resources: TRUE",
      "    code-fold: true",
      "    page-layout: full",
      "    fig_caption: yes",
      "    toc: TRUE",
      "---",
      ""
    )
  }

  # Mapping between file names and their titles
  title_mapping <- list(
    "index.qmd"                      = "Project Overview",
    "executive_summary.qmd"          = "Executive Summary",
    "analysis.qmd"                   = "Analysis",
    "analysis_plan.qmd"              = "Analysis Plan",
    "data_codebooks.qmd"             = "Codebooks",
    "contact.qmd"                    = "Contact"
  )

  # Create files and add qmd header and content
  for (file in files_to_create) {
    file_path <- file.path(subfolder, file)
    if (!file.exists(file_path)) {
      file.create(file_path)
      cat(paste0("File created: ", file_path, "\n"))
      if (grepl("\\.qmd$", file)) {
        file_title <- title_mapping[[file]]  # Get the title from the mapping

        # Initial common header setup
        file_content <- generate_yaml_header(file_title)

        # Define additional content based on file
        additional_content <- switch(file,
                                     "index.qmd" = {
                                       c(
                                         "## Introduction",
                                         "Welcome to the ___ project website. This is a private, password-protected website that should not be shared externally.",
                                         "This section provides a comprehensive introduction to the project, detailing the scope and goals of ___.",
                                         "",

                                         "## Objectives",
                                         "Details of the project's primary goals and expected outcomes. This may include reducing recidivism, optimizing justice system resources, and enhancing public safety.",
                                         "",

                                         "## Key Questions",
                                         "What are the major challenges the project aims to address?",
                                         "",

                                         "## Project Links",
                                         "Access to project resources and repositories is critical for team collaboration and transparency. Below are the links to key project tools and platforms:",
                                         "",
                                         "- **SharePoint**: [Link to the project's files](#)",
                                         "- **GitHub Repository**: [Link to the project's GitHub repository](#)",
                                         "- **Asana**: [Link to the project's Asana page](#)",
                                         "",

                                         "## Stakeholders",
                                         "This project involves collaboration between multiple stakeholders, including government agencies, community organizations, and justice system partners. This section lists key stakeholders and describes their roles and contributions to the project.",
                                         "",
                                         ""
                                       )
                                     },
                                     "executive_summary.qmd" = {
                                       c(
                                         "## Executive Summary"
                                       )
                                     },
                                     "analysis.qmd" = {
                                       c(
                                         "## Analysis"
                                       )
                                     },
                                     "analysis_plan.qmd" = {
                                       c(
                                         "## Analysis Plan"
                                       )
                                     },
                                     "data_codebooks.qmd" = {
                                       c(
                                         "## About This Page",
                                         "Detailed descriptions of all datasets, variables, and classifications used in the project.",
                                         "",
                                         "## File 1 Codebook",
                                         "",
                                         "## File 2 Codebook"
                                       )
                                     },
                                     "contact.qmd" = {
                                       c(
                                         "## About This Page",
                                         "Information for contacting project team members.",
                                         "",
                                         "## Team",
                                         "Names and roles of the project team members.",
                                         "",
                                         "## Reach Out",
                                         "Contact details for further communication."
                                       )
                                     },
                                     c()
        )

        # Combine the header and additional content
        file_content <- c(file_content, additional_content)
        writeLines(file_content, file_path)
        cat(paste0("qmd content added to: ", file_path, "\n"))
      }
    } else {
      cat(paste0("File already exists: ", file_path, ". No changes made.\n"))
    }
  }

  # Create template R files if they don't exist
  R_files <- c("file1.R", "file2.R")
  for (file in R_files) {
    file_path <- file.path(subfolder, "R", file)  # Construct the full file path with subfolder
    if (!file.exists(file_path)) {
      file_header <- c(
        "####################",
        "# Author:",
        "# Date Last Updated:",
        "# File Name:",
        "# File Description:",
        "####################",
        ""
      )
      writeLines(file_header, file_path)
      cat(paste0("R script created: ", file_path, "\n"))
    } else {
      cat(paste0("R script already exists: ", file_path, ". No changes made.\n"))
    }
  }

  # Populate _quarto.yml with provided content
  quarto_content <- c(
    "project:",
    "  type: website",
    "",
    "website:",
    "  title: \"Project Name\"",
    "  favicon: img/favicon.png",
    "  navbar:",
    "    right:",
    "      - text: \"Project Overview\"",
    "        href: index.html",
    "      - text: \"Executive Summary\"",
    "        href: executive_summary.html",
    "      - text: \"Analysis\"",
    "        href: analysis.html",
    "      - text: \"Analysis Plan\"",
    "        href: analysis_plan.html",
    "      - text: \"Codebooks\"",
    "        href: data_codebooks.html",
    "      - text: \"Contact\"",
    "        href: contact.html",
    "      - text: \"Github\"",
    "        href: https://github.com/",
    "",
    "format:",
    "  html:",
    "    theme: flatly",
    "    css: styles.css",
    "    toc: true",
    "    toc-location: left",
    "",
    "execute:",
    "  freeze: auto"
  )

  # Create _quarto.yml file if it doesn't exist
  quarto_path <- file.path(subfolder, "_quarto.yml")
  if (!file.exists(quarto_path)) {
    writeLines(quarto_content, quarto_path)
    cat("_quarto.yml file created.\n")
  } else {
    cat("_quarto.yml already exists. No changes made.\n")
  }

}
