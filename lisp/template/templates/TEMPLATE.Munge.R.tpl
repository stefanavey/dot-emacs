#################################################################################
## (>>>FILE<<<)
## 
##
## Short Summary: (>>>summary<<<)(>>>0<<<)
##
## Copyright (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>USER_NAME<<<) <<(>>>LOGIN_NAME<<<)@its.jnj.com>>
## Date Created: (>>>VC_DATE<<<)
## Project Directory: (>>>DIR<<<)
## Keywords: (>>>keywords<<<)(>>>1<<<)
##
## Description:
##
## (>>>description<<<)(>>>2<<<)
##
## Header generated automatically by TEMPLATE.Munge.R.tpl
#################################################################################

current_vars <- ls()

##############
## Packages ##
##############
(>>>POINT<<<)

###############
## Variables ##
###############
(>>>3<<<)


## Only keep required variables I intend to pass along
output <- c((>>>output<<<))
message("(>>>FILE<<<) munging completed with output:\n",
        paste(output, collapse = ', '))
rm(list = setdiff(ls(), c(current_vars, output)))

>>>TEMPLATE-DEFINITION-SECTION<<<
("summary" "Short Summary: ")
("keywords" "Keywords: ")
("description" "Description: ")
("output" "Enter names of output(s): ")
