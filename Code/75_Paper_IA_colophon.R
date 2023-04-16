# Colophon

# Additional packages ---------------------------------------------------
library(ggplot2) # Packages are not needed but loaded to show up in the colophon
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(lubridate)
library(forcats)
library(zoo)

# Local colophon --------------------------------------------------------
dependencies <- renv::dependencies(progress = FALSE) |> count(Package)
packages <- jsonlite::fromJSON("renv.lock", flatten = TRUE)
colophon_local <- do.call("rbind", packages$Packages) |>
  as_tibble() |>
  select(Package, Version) |>
  unnest(cols = c("Package", "Version")) |>
  inner_join(dependencies, by = "Package") |>
  select(Package, Local = Version)

base_packages <- sessionInfo()$basePkgs
installation <- installed.packages(fields = c("Package", "Version"))[,c("Package", "Version")]
base_packages <- installation |> 
  as_tibble() |> 
  filter(Package %in% base_packages) |> 
  rename(Local = Version)

colophon_local <- colophon_local |> 
  bind_rows(base_packages)


# Cluster colophon ------------------------------------------------------
# Cluster code
## 1. Load all packages in files and call: loaded <- (.packages())
## 2. Then get package versions: installed <- installed.packages(fields = c("Package", "Version"))[,c("Package", "Version")]
## 3. Package versions via: used_packages <- installed |> as_tibble() |> filter(Package %in% loaded)
## 4. saveRDS(used_packages, file = "Project_NSE/used_packages.rds")

colophon_cluster <- readRDS("Data/used_packages.rds")


# Full and Print --------------------------------------------------------
colophon <- colophon_local |> 
  full_join(colophon_cluster |> 
              rename(Cluster = Version), 
            by = "Package") |> 
  mutate(Citation = NA_character_) |> 
  arrange(tolower(Package))

# Special citations
R_cited <- FALSE

for(j in 1:nrow(colophon)) {
  # Get citation
  the_citation <- colophon$Package[j] |> 
    citation() |> 
    toBibtex()
  
  # Escape empty
  if(length(the_citation) < 2) {
    next()
  }
  
  # R core?
  if(any(str_detect(the_citation, "R Core"))) {
    # Put citation
    colophon$Citation[j] <- "\\cite{R-Core}"
    
    # If not in citations, add citation
    if(!R_cited) {
      the_citation[1] <- str_replace(the_citation[1], ",", "R-Core,")
      
      # Print
      the_citation |> 
        cat(file = "packages.bib", sep = "\n", append = (j != 1))
      
      # Update index
      R_cited <- TRUE
    }
  } else {
    # Add citation key
    colophon$Citation[j] <- paste0("\\cite{R-", colophon$Package[j], "}")
    the_citation[1] <- str_replace(the_citation[1], ",", paste0("R-", colophon$Package[j], ","))
    
    # Print
    the_citation |> 
      cat(file = "packages.bib", sep = "\n", append = (j != 1))
  }
}

print(xtable(colophon),
      include.rownames = FALSE,
      only.contents	= TRUE,
      comment = FALSE,
      timestamp = FALSE,
      file = "Paper_Tables/IA_colophon.tex",
      booktabs = TRUE,
      sanitize.text.function = function(x) return(x))

