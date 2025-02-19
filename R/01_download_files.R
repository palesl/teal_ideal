# Purpose of this code. To download the divisions, MP characteristics, and votes
# from aph.gov.au. To format for analysis and save in working folder.

# packages

library(rvest);library(tidyverse);library(here);library(jsonlite)

# location

here::i_am("R/01_download_files.R")

# helpers

source("R/00_helpers.R")
# get all house divisions in the 47th Parliament


divisionsPH<-read_json("https://divisions.aph.gov.au/api/division?f=2022-7-26&t=2025-2-6&page=0&ps=1000")[[3]]
divisionsPH_list<-list()

for(i in 1:length(divisionsPH)){
  divisionsPH_list[[i]]<-unlist(divisionsPH[[i]])
}
divisions<-bind_rows(divisionsPH_list)


# save divisions data...

saveRDS(divisions, 'working_data/divisions.rds')


# divisions...
vote_info_list<-list()

for(i in 1:nrow(divisions)){

  while(i<=length(vote_info_list)){i<-i+1}
  Sys.sleep(2)
  print(paste(i,"out of", nrow(divisions), "(votes)"))
  url<-paste0('https://divisions.aph.gov.au/api/division/',divisions$divisionId[i])
  division<-read_json(url)
  votes<-division[["votes"]]
  division_name<-paste0('div',division$divisionNumber)

  votes<-votes|>bind_rows()|>select(parliamentarianId,partyName,partyColour, voteResultId)
  names(votes)[4]<-division_name


  vote_info_list[[i]] <-  votes

}


# get personal information...




# append_lists


vote_frame<-vote_info_list|> reduce(full_join, by = c('parliamentarianId',
                                                      'partyName' ,
                                                      'partyColour'))

vote_frame$parliamentarianId<-toupper(vote_frame$parliamentarianId)

vote_frame <- vote_frame |> left_join(ausPH::getIndividuals()|>select(DisplayName,PHID), by= join_by("parliamentarianId"=="PHID"))



# identifying climate 200 MPs

vote_frame$teal<-FALSE
vote_frame$teal_plus<-FALSE

vote_frame$teal[vote_frame$parliamentarianId==
                              "008CH"|
                  vote_frame$parliamentarianId==
                              "286042"|
                  vote_frame$parliamentarianId==
                              "297660"|
                  vote_frame$parliamentarianId==
                              "299623"|
                  vote_frame$parliamentarianId==
                              "300006"|
                  vote_frame$parliamentarianId==
                              "300124"]<-TRUE

vote_frame$teal_plus[vote_frame$parliamentarianId==
                             "008CH"|
                             vote_frame$parliamentarianId==
                             "286042"|
                             vote_frame$parliamentarianId==
                             "297660"|
                             vote_frame$parliamentarianId==
                             "299623"|
                             vote_frame$parliamentarianId==
                             "300006"|
                             vote_frame$parliamentarianId==
                             "300124"|
                             vote_frame$parliamentarianId==
                             "282335"|
                             vote_frame$parliamentarianId==
                             "175696"|
                             vote_frame$parliamentarianId==
                             "C2T"|
                             vote_frame$parliamentarianId==
                             "265980"]<-TRUE

# reordering variables

latest_div<-paste0('div',nrow(divisions))

vote_frame<-vote_frame|>
  relocate(parliamentarianId,DisplayName,partyName,partyColour,teal,teal_plus,
           div1:all_of(latest_div))

# renaming division variables


new_names<-paste0("div", divisions$divisionNumber,"_", lubridate::date(divisions$date)|>format("%d%b%Y"))

names(vote_frame)[7:ncol(vote_frame)]<- new_names[length(new_names):1]

#  accounting for 'out of legislature' due to byelection [coded=9]

# Aston by-election

vote_frame[grep("TUDGE,", vote_frame$DisplayName) , 110:ncol(vote_frame)]<- 9
vote_frame[grep("DOYLE,", vote_frame$DisplayName), 7:139]<- 9

# Fadden byelection


vote_frame[grep("ROBERT,", vote_frame$DisplayName), 146:ncol(vote_frame)]<- 9
vote_frame[grep("CALDWELL,", vote_frame$DisplayName), 7:171]<- 9


# Dunkley by-election

vote_frame[grep("MURPHY,", vote_frame$DisplayName), 271:ncol(vote_frame)]<- 9
vote_frame[grep("BELYEA,", vote_frame$DisplayName), 7:307]<- 9

# Cook by-election


vote_frame[grep("MORRISON,", vote_frame$DisplayName), 306:ncol(vote_frame)]<- 9
vote_frame[grep("KENNEDY,", vote_frame$DisplayName), 7:354]<- 9

#  accounting for 'out of legislature' due to party switch

vote_frame[grepl("GEE,", vote_frame$DisplayName) & vote_frame$partyName=="Independent",
           7:95]<- 9
vote_frame[grepl("GEE,", vote_frame$DisplayName) & vote_frame$partyName=="The Nationals",
           96:ncol(vote_frame)]<- 9

vote_frame[grepl("BROADBENT,", vote_frame$DisplayName) & vote_frame$partyName=="Independent",
           7:223]<- 9
vote_frame[grepl("BROADBENT,", vote_frame$DisplayName) & vote_frame$partyName=="Liberal Party of Australia",
           224:ncol(vote_frame)]<- 9
# abstain

vote_frame[is.na(vote_frame)]<-5

# save file

saveRDS(vote_frame, "working_data/house_votes.rds")



