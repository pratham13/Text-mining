
# The start of text mining ------------------------------------------------
install.packages("rvest", dependencies =T)
library(rvest)
library(dplyr)

# weblink
url <- "https://www.theguardian.com/environment/2015/jan/08/mayors-failure-clean-up-londons-air-pollution-risks-childrens-health"

# Read the HTML document using try to handle 404 errors

try(html_document <- read_html(url))
print(html_document)


title_xpath <- "//h1[contains(@class, 'content__headline')]"
title_text <- html_document %>%
  html_node(xpath = title_xpath) # Only provides the node.

