library(httr)
library(rvest)
# Random User Generator web site: https://randomuser.me/
# Website that loads random user data for API 
# if you use https://randomuser.me/api, you can see all the HTML info about the user
# We are going to transform this data with R into useful data

library(httr)
GET(url="https://randomuser.me/api")
# 
# Output:
# Response [https://randomuser.me/api] #SITE
# Date: 2017-02-07 23:49   #the date, the time where the server is located
# Status: 200 #the status of request (200=all good, 404=not good, more at https://http.cat/)
# Content-Type: application/json; charset=utf-8  #the file format (e.g.,json file format and the character type)
# Size: 919 B #Size of the data (doesn't download yet because there is often quite a bit)
req<-GET(url="https://randomuser.me/api")
content(req) #translates the content into a list, allows you to see the details of content
headers(req) #information you will get even if not succesful, not usually very useful except for troubleshooting why not ablke to get info
http_status(req) #additional details on status of request


# OK, READY TO GET INFO AND DO SOMETHING WITH IT
# best to start with some simple functions

# You could start here and just try and download the info
ru_GET<-function(){
  req<-GET(url="https://randomuser.me/api")
  req
}
req

# BUT what if there is a problem with the server?
# this function will check the status of the request sent to the server. 
#Is the status code <400? that means the request is ok, act like nothing happened, and keep going.
ru_check<-function(){
  req<-GET(url="https://randomuser.me/api")
  if (req$status_code<400) return(invisible())
  msg<-http_status(req)$message
  stop("HTTP failure: ", req$status_code," ", msg)
}

# BUT
# sometimes there's an error not with the status code, but with downloading the data. this will test for that
ru_parse <- function(req){
  text<-content(req, as="text")
  if (identical(text,""))
    stop("No output to parse")
  jsonlite::fromJSON(text, simplifyVector = FALSE)  #jsonlite converts JSON to a list
}

# SO WHY NOT PUT THESE ALL TOGETHER IN THE ORIGINAL SIMPLE FUNCTION?
ru_GET<-function(){
  req<-GET(url="https://randomuser.me/api")
  ru_check(req)
  ru_parse(req)
  req
}
req

# A good api might allow you to request
# 1) multiple profiles back at same time instead of one at a time
# 2) male vs. femal
# 3) differet format (JSON, XML, etc)
# 4) What country users form, etc.


# LETS TRY THAT by adding function to do that
ru_GET<-function(....){
  req<-GET(url="https://randomuser.me/api")
  ru_check(req)
  ru_parse(req)
  req
}




# for instance from a specific country - 
# add modifier after url and country code we want, in this case "https://randomuser.me/api?c=FR"
ru_GET<-function(url=ru_url,...){
  req<-GET(url=url,...)
  ru_check(req)
  ru_parse(req)
}
ru_check<-function(req){
  if (req$status_code<400) return(invisible())
  msg<-http_status(req)$message
  stop("HTTP failure: ", req$status_code," ", msg)
}
ru_parse <- function(req){
  text<-content(req, as="text")
  if (identical(text,""))
    stop("No output to parse")
  jsonlite::fromJSON(text, simplifyVector = FALSE)  #jsonlite converts JSON to a list
}

ru_url <- "https://randomuser.me/api" #create a global variable for the generic url
ru_from_countries <- function(country, n_profile=1) {
  country<-match.arg(country, 
                     c("AU", "BR",
                       "CA","CH",
                       "DE", "DK", 
                       "ES", "FI", 
                       "FR", "GB", 
                       "IE", "IR", 
                       "NL", "NZ", 
                       "TR", "US"), 
                     several.ok=TRUE)   #These countries came from documentation of randomuser.me website 
  country<-paste(country, collapse=",")
  
  ru_GET(url=modify_url(ru_url, 
                        query=list(nat = country, 
                                   results=n_profile)))
}
ru_from_countries(country=c("FI","DE"),n_profile=1) #NB: STILL NOT WORKING!!!








####################################################################################  
#
# WEB PAGE SCRAPING
#
####################################################################################  


# some sites have no api, or api is need-to-pay, so will use a script to harvest the information from web pages

library(rvest)
library(xml2)

#1st the package will go to web page and download all the html()
html<-read_html("http://brunalab.org/")  
OR
html<-read_html("http://www.imdb.com/title/tt1490017/")  
# on firefox can right click and inspect element to see html code
# html: gives content, tells you it is an xml document, the header, the beginning of the html, etc.

xml_find_all(html,".//h1") #finding the title of the movie, which in this case after inspection looks like it is only 1 header tag
html_text(xml_find_all(html,".//h1")) #this will make it text


# can pipe to make it easier
html<-read_html("http://www.imdb.com/title/tt1490017/")  
html %>% 
  xml_find_all(".//h1") %>% 
  html_text()

#Lets try to get the cast. find the elements with path widgets
cast<-html_nodes(html, "#titleCast span.itemprop") %>% 
  html_text()
cast
# 

package "rentrez" for NCBI, including pubmed

#can download the web page itslef
# some websites are resistant to this because of proprietary issues. (eg google scholar)
# for those that might care but get traffic (eg IMDB) you can add a for loop telling it to wait .25 seconds, o 0.5 deconds between loops

# XPath tutorial may help to identify paths, whihc willhelp you figure out how to parse your html into text that you are interested in
# also useful is "selector gadget", its a widget you drag into your browser, and wehn you put your mouse over it it will show you wehre you are on the text and will 
  # give you the xpath you need
