library(tidyverse)
library(XML)
library(httr)
library(data.table)

###Enter values and run code to obtain desired hansard

First.debate <- 
Last.debate <- 
Parliament <- 
Session <- 

obtain_hansard <- function(Last.debate){

hans_debnum <- First.debate:Last.debate %>%
               str_pad(3,pad="0") %>%
               as.character()
            
Parl_sess <- as.character(paste0(as.character(Parliament),Session))
	           
create_url <- function(hans_debnum){
	           	            paste0("https://www.ourcommons.ca/Content/House/",Parl_sess,"/Debates/",hans_debnum,"/HAN",hans_debnum,"-E.XML")%>%
	                            GET() %>%
	                            XML::xmlParse()
                                     }	            

url_list <- lapply(hans_debnum, create_url)
	                      
node_set <- function(url){
	                df <- setnames(xmlToDataFrame(node = getNodeSet(url,"//Intervention")), c("speaker", "text"))
	                sitting <- as.numeric(setnames(xmlToDataFrame(node = getNodeSet(url,"//ExtractedItem[@Name='MetaNumberNumber']"), stringsAsFactors = FALSE), "sitting"))
                        day <- setnames(xmlToDataFrame(node = getNodeSet(url,"//ExtractedItem[@Name='MetaDateNumDay']"), stringsAsFactors = FALSE),"day")
                        month <- setnames(xmlToDataFrame(node = getNodeSet(url,"//ExtractedItem[@Name='MetaDateNumMonth']"), stringsAsFactors = FALSE),"month")
                        year <- setnames(xmlToDataFrame(node = getNodeSet(url,"//ExtractedItem[@Name='MetaDateNumYear']"), stringsAsFactors = FALSE), "year")

df_new <- cbind(df, sitting, day, month, year)
                        }

text <- lapply(url_list, node_set)%>%
        bind_rows()%>%
        as.data.frame()                     
}

hansard <- obtain_hansard(Last.debate)
