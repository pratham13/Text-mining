
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

# In order to get the information we want, we need html_text, which extracts attributes, text and tag name from html
title_text <- title_text %>%
  html_text(trim = T) # Stores title in title_text

# Access author information (CSS)
author_css <- ".tone-colour span" # Using SelectorGadget ('.byline span' does also work)
author_text <- html_document %>%
  html_node(css = author_css) %>%
  html_text(trim = T) # Stores author in author_text

# Access article text information (XPath)
body_xpath <- "//div[contains(@class, 'content__article-body')]//p" # '.js-article__body > p' is also possible, but needs css option in html_nodes
# The above location can be found when searching for the first two words of the article in the source code (or when inspecting the first to lines of the article).
# This provides you with the location information 

body_text <- html_document %>%
  html_nodes(xpath = body_xpath) %>%
  html_text(trim = T) %>%
  paste0(collapse = "\n")

# Access publishing date information (XPath)
date_xpath <- "//time" # '.content__dateline-wpd--modified' does not work for some reason, although it is the output of SelectorGadget. 
# In such a case just try to look for alternatives witht he other methods outlined above
library(lubridate) # to handle date information (important for later analysis including time)
date_text <- html_document %>%
  html_node(xpath = date_xpath) %>%
  html_attr(name = "datetime") %>% # accesses the attribute information datetime in //time (different from html_text above)
  as.Date() %>% 
  parse_date_time(., "ymd", tz = "UTC") 

# Store all information in a data frame called article
article <- data.frame(
  url = url,
  date = date_text,
  title = title_text,
  author = author_text,
  body = body_text
)

print(as_tibble(article))
## # A tibble: 1 x 5
##                                                                           url
##                                                                        
## 1 https://www.theguardian.com/environment/2015/jan/08/mayors-failure-clean-up
## # ... with 4 more variables: date , title , author ,
## #   body 

# The next step would be to wrap this code in a function, in order to be able to run it for multiple The Guardian articles.

# # In order to get the information we want, we need html_text, wh --------


# Define the function
scrape_guardian_article <- function(url) {
  try(html_document <- read_html(url))
  title_xpath <- "//h1[contains(@class, 'content__headline')]"
  title_text <- html_document %>%
    html_node(xpath = title_xpath)
  
  title_text <- title_text %>%
    html_text(trim = T) 
  
  author_css <- ".tone-colour span" 
  author_text <- html_document %>%
    html_node(css = author_css) %>%
    html_text(trim = T) 
  
  body_xpath <- "//div[contains(@class, 'content__article-body')]//p" 
  body_text <- html_document %>%
    html_nodes(xpath = body_xpath) %>%
    html_text(trim = T) %>%
    paste0(collapse = "\n")
  
  date_xpath <- "//time" 
  library(lubridate) 
  date_text <- html_document %>%
    html_node(xpath = date_xpath) %>%
    html_attr(name = "datetime") %>% 
    as.Date() %>% 
    parse_date_time(., "ymd", tz = "UTC") 
  
  article <- data.frame(
    url = url,
    date = date_text,
    title = title_text,
    author = author_text,
    body = body_text
  )
  return(article)
}

# Run the function for multiple links
articles <- data.frame()
links <- c("https://www.theguardian.com/environment/2015/jan/08/mayors-failure-clean-up-londons-air-pollution-risks-childrens-health", "https://www.theguardian.com/world/2016/dec/07/marshall-islands-natives-return-mass-exodus-climate-change", "https://www.theguardian.com/environment/2016/dec/14/queenslands-largest-solar-farm-plugs-into-the-grid-a-month-early")

for (i in seq_along(links)) { # Iterate over number of links
  cat("Downloading", i, "of", length(links), "URL:", links[i], "\n")
  article <- scrape_guardian_article(links[i]) # Use downloder function specified above for link[i]
  articles <- rbind(articles, article) # Append new article to old
}
## Downloading 1 of 3 URL: https://www.theguardian.com/environment/2015/jan/08/mayors-failure-clean-up-londons-air-pollution-risks-childrens-health 
## Downloading 2 of 3 URL: https://www.theguardian.com/world/2016/dec/07/marshall-islands-natives-return-mass-exodus-climate-change 
## Downloading 3 of 3 URL: https://www.theguardian.com/environment/2016/dec/14/queenslands-largest-solar-farm-plugs-into-the-grid-a-month-early
print(as_tibble(articles))
## # A tibble: 3 x 5
##                                                                           url
##                                                                        
## 1 https://www.theguardian.com/environment/2015/jan/08/mayors-failure-clean-up
## 2 https://www.theguardian.com/world/2016/dec/07/marshall-islands-natives-retu
## 3 https://www.theguardian.com/environment/2016/dec/14/queenslands-largest-sol
## # ... with 4 more variables: date , title , author ,
## #   body 
# You can also modify the function to use it with lapply. To do that, use the following code modifications

# Change the return code in the functin defined above to:
article =NULL
articles <- rbind(articles, article)

# Run the function over vector of links
text_df <- as.data.frame(lapply(links, scrape_guardian_article))

(as_tibble(text_df))
