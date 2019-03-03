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
    word_document:
       toc_depth: 2
       toc: true
	   number_sections: true
       df_print: kable
       highlight: pygments
       reference_docx: ~/Downloads/FormalStatReport.dotx
       fig_width: 6
       fig_height: 4.5
---

```{r setup, eval=TRUE, echo=FALSE, message=FALSE}

###################
## Chunk options ##
###################
knitr::opts_chunk$set(echo = FALSE, cache = TRUE)
if (knitr::is_html_output()) {
    knitr::opts_chunk$set(echo = TRUE)
    htmltools::img(src = knitr::image_uri(file.path("..", "lib",
                                                    "Janssen_Prof_RGB.jpg")),
                   width = "250px",
                   alt = "logo", 
                   style = "position:absolute; top:0; right:0; padding:10px;")
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

```

(>>>3<<<)(>>>POINT<<<)


\clearpage

# ABBREVIATIONS

\clearpage

# EXECUTIVE SUMMARY

# INTRODUCTION

## Background

# OBJECTIVES AND HYPOTHESIS

# EXPERIMENTAL DESIGN

# ENDPOINTS

# MISSING DATA AND EXTREME VALUES

# STATISTICAL ANALYSIS METHODS

## Primary Endpoints

## Secondary/Exploratory Endpoints

## Power and Sample Size

# RESULTS

# CONCLUSIONS

\clearpage

# Appendix {-}

## Source Code {-}

R Source code is available online [here](<PATH_TO_FILE>)

## R Session Info {-}

```{r sessionInfo, eval=TRUE, echo=FALSE, warning=FALSE}

si <- devtools::session_info()
pkgs <- si$packages$package
cutoff <- 16
pkgs_trunc <- ifelse(nchar(pkgs) < cutoff, pkgs,
                     paste0(substr(pkgs, 1, cutoff), "..."))
si$packages$package <- pkgs_trunc
pander(si)

```

\clearpage

# References {-}

>>>TEMPLATE-DEFINITION-SECTION<<<
("docTitle" "Document Title: ")
("docSubTitle" "Document Subtitle: ")
