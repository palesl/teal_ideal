# helpers


# cutline -- modified add.OCcutline function from 'oc' package to port for use with ggplot
cutline <- function(cutData) {

  slope <- -cutData[1]/cutData[2]
  if (is.na(slope)) {
    x <- c(cutData[1]*cutData[3],cutData[1]*cutData[3])
    y <- c(sqrt(1-(cutData[1]*cutData[3])^2),-sqrt(1-(cutData[1]*cutData[3])^2))
    slope <- NA
    intercept <- NA
  }
  else {
    intercept <- -slope*cutData[1]*cutData[3]+cutData[2]*cutData[3]
    x <- c( (-slope*intercept + sqrt( (slope*intercept)^2 -
                                        (1+slope*slope)*(intercept*intercept-1)))/(1+slope*slope),
            (-slope*intercept - sqrt( (slope*intercept)^2 -
                                        (1+slope*slope)*(intercept*intercept-1)))/(1+slope*slope) )
    if (is.na(x[1])) {
      warning("Couldn't solve for points on the unit circle!\n")
      x<-NA
      y<-NA
      slope<-NA
      intercept<-NA
    }
    else {
      y <- intercept + slope*x
      y[y < -1] <- -sqrt(1-x[y<1]^2)
      y[y >  1] <-  sqrt(1-x[y>1]^2)
    }
  }

  return(dplyr::bind_cols(x=x[1],xend=x[2],y=y[1],yend=y[2]))
}

# summary<-division[["summary"]]

get_introducer <- function(summary){

  sum_vec<-summary|>str_split_1( "\n|\r")
  person_raw<-sum_vec[str_detect(sum_vec,"people/representatives")][1]

  if(is.na(person_raw)){return("Unknown")}

  person_refined <-person_raw|>str_split_1( fixed(". "))

  person_single<-person_refined[!grepl("original amendment|original",person_refined)]

  person_single<-person_single[grepl("people/representatives",person_single)]

  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+" # https://stackoverflow.com/questions/26496538/extract-urls-with-regex-into-a-new-data-frame-column

  person_url<-str_extract_all(person_single,url_pattern)[[1]]
  person_url<-person_url[grepl("people/representatives",person_url)]
  person_url<-gsub("\\)","",person_url)

  html<-read_html(person_url)
  party_introducer<-html|>html_element(".org")|>html_text()
  name_introducer<- html|>html_element(".fn") |>html_text()

  introducer<- paste(name_introducer,party_introducer,sep="*")
  return(introducer)
}


