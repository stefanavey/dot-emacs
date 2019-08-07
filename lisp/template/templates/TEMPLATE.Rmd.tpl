---
title: "(>>>docTitle<<<)"
subtitle: "(>>>docSubTitle<<<)"
author: "(>>>USER_NAME<<<) <<(>>>LOGIN_NAME<<<)@its.jnj.com>>"
date: "`r format(Sys.Date(), format = '%d %b %Y')`"
fontsize: 12pt
geometry: margin=1in
linkcolor: blue
urlcolor: blue
citecolor: blue
bibliography: ../lib/library.bib
link-citations: true
output:
    html_document:
       css: ../lib/custom.css
       code_folding: hide
       toc_float: true
       toc: true
       df_print: paged
       highlight: pygments
       fig_width: 6
       fig_height: 4.5
    pdf_document:
       toc: true
       toc_depth: 2
       number_sections: true
       df_print: kable
       latex_engine: xelatex
       highlight: pygments
    word_document:
       toc_depth: 2
       toc: true
       df_print: kable
       highlight: pygments
       reference_docx: ../lib/WordTemplate.dotx
       fig_width: 6
       fig_height: 4.5
header-includes:
    - \usepackage{wallpaper}
---
\URCornerWallPaper{0.175}{../lib/Janssen_Prof_RGB.jpg}

```{r setup, eval=TRUE, echo=FALSE, message=FALSE}

###################
## Chunk options ##
###################
knitr::opts_chunk$set(echo = FALSE, cache = TRUE)
if (knitr::is_html_output()) {
    knitr::opts_chunk$set(echo = TRUE)
}

##############
## Packages ##
##############
library(devtools)
library(pander)
(>>>2<<<)

###############
## Variables ##
###############
munge_dir <- file.path("..", "munge")

###############
## Functions ##
###############
source(file.path("..", "lib", "helpers.R"))

#################
## Import Data ##
#################

```

(>>>3<<<)(>>>POINT<<<)

\clearpage

# References {-}

<div id="refs"></div>

\clearpage

# Appendix {-}

## Source Code {-}

R Source code is available online [here](<PATH_TO_FILE>)

## R Session Info {-}

```{r sessionInfo, eval=TRUE, echo=FALSE, warning=FALSE}

pander(sessionInfo())

```

>>>TEMPLATE-DEFINITION-SECTION<<<
("docTitle" "Document Title: ")
("docSubTitle" "Document Subtitle: ")
