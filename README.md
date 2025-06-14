
# quartositebuildr

The `quartositebuildr` R package automates the setup of a project
website using [Quarto](https://quarto.org/), specifically designed for
Code for America-style research projects. It creates a consistent folder
structure, prepopulated Quarto files, and branded assets — making it
easy to get started on data-sharing or internal collaboration websites.

------------------------------------------------------------------------

## Features

- Creates folders for R scripts, site content, fonts, and images
- Generates `.qmd` files with YAML headers and placeholder content
- Adds a CSS stylesheet with custom fonts and formatting
- Copies default images and logos (favicon, branding)
- Writes a fully configured `_quarto.yml` for site navigation
- Includes a `.gitignore` for reproducible version control

------------------------------------------------------------------------

## Installation

To install the package from GitHub:

``` r
# Install remotes if needed
install.packages("remotes")

# Install quartositebuildr
remotes::install_github("mr4909/quartositebuildr")
```

## Usage

To create a new site structure:

``` r
library(quartositebuildr)

# Create a site in the folder "my_project_site"
create_site_structure(type = "cfa", subfolder = "my_project_site")
```

You’ll be prompted to confirm creation before anything is written.

## Output Structure

Running the function will generate a folder like this:

my_project_site/ 
├── R/  
├── \_site/  
├── fonts/  
├── img/  
├── styles.css  
├── .gitignore  
├── \_quarto.yml  
├── index.qmd  
├── executive_summary.qmd  
├── analysis.qmd  
├── analysis_plan.qmd  
├── decision_making.qmd  
├── data_codebooks.qmd  
├── data_diagram.qmd  
└── contact.qmd

## Quarto Files Included

Each .qmd file contains a Quarto-compatible YAML header and default
section text:

- index.qmd: Project overview, objectives, links, and stakeholders
- executive_summary.qmd: High-level summary of findings
- analysis.qmd: Data analysis pages
- analysis_plan.qmd: Research analysis plan
- decision_making.qmd: Documentation of key decisions
- data_codebooks.qmd: Codebooks and definitions
- data_diagram.qmd: Data flow diagrams and linkages
- contact.qmd: Team contacts

These are copied automatically into the appropriate folders.

## Tips

- This package assumes Quarto is installed.
- After creation, open the .qmd files in RStudio or VS Code to edit
  content.
- Run quarto preview to preview the site locally.
- You can deploy to GitHub Pages or Netlify using Quarto’s built-in
  tools.

## Feedback

Please open an issue on GitHub if you encounter bugs or have suggestions
for new features.
