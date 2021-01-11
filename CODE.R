First.debate <- 1
Last.debate <- 2
Parliament <- 43
Session <- 1

obtain_hansard <- function(Last.debate){

###1
hans_debnum <- 
	        First.debate:Last.debate %>%
            str_pad(3,pad="0") %>%
            as.character()
            
###2
Parl_sess <- as.character(paste0(as.character(Parliament),Session))
	           

create_url <- function(hans_debnum){
	           	                         paste0("https://www.ourcommons.ca/Content/House/",Parl_sess,"/Debates/",hans_debnum,"/HAN",hans_debnum,"-E.XML")%>%
	                                     GET() %>%
	                                     XML::xmlParse()
 
                                        }	            


	           url_list <- lapply(hans_debnum, create_url)
	           
	           
###3	           
node_set <- function(x){
	df <- setnames(xmlToDataFrame(node = getNodeSet(x,"//Intervention")), c("speaker", "text"))
	sitting <- as.numeric(setnames(xmlToDataFrame(node = getNodeSet(x,"//ExtractedItem[@Name='MetaNumberNumber']"), stringsAsFactors = FALSE), "sitting"))
    day <- setnames(xmlToDataFrame(node = getNodeSet(x,"//ExtractedItem[@Name='MetaDateNumDay']"), stringsAsFactors = FALSE),"day")
    month <- setnames(xmlToDataFrame(node = getNodeSet(x,"//ExtractedItem[@Name='MetaDateNumMonth']"), stringsAsFactors = FALSE),"month")
    year <- setnames(xmlToDataFrame(node = getNodeSet(x,"//ExtractedItem[@Name='MetaDateNumYear']"), stringsAsFactors = FALSE), "year")

###NEED TO FIX BINDS ALL COLUMNS TO EACH COLUMN IN DF   
df_new <- cbind(df, sitting, day, month, year)

}

text <- lapply(url_list, node_set)%>%
                       bind_rows()%>%
                       as.data.frame()                     

}

hansard <- obtain_hansard(Last.debate)







df_new <- lapply(df, function(x) 
  cbind(x, "sitting" = sitting))
	           }


	           
df_new <- mapply(cbind, df, "sitting"= sitting, "day"= day, "month" = month, "year" = year, SIMPLIFY=F)
}





