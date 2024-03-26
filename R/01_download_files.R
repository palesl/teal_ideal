# Purpose of this code. To download the divisions, MP characteristics, and votes 
# from theyvoteforyou.org.au. To format for analysis and save in working folder.

# packages

library(rvest);library(tidyverse);library(here);library(jsonlite)

# location

here::i_am("R/01_download_files.R")

# get all house divisions in the 47th Parliament

divisions_2022 <-read_json('https://theyvoteforyou.org.au/api/v1/divisions.json?end_date=2022-12-31&house=representatives&key=PTtX2PaLu0P2KMjGvXhZ&start_date=2022-05-22')|>bind_rows()
divisions_2023_H1 <-read_json('https://theyvoteforyou.org.au/api/v1/divisions.json?start_date=2023-01-01&end_date=2023-06-31&house=representatives&key=PTtX2PaLu0P2KMjGvXhZ')|>bind_rows()
divisions_2023_H2 <-read_json('https://theyvoteforyou.org.au/api/v1/divisions.json?start_date=2023-07-01&end_date=2023-12-31&house=representatives&key=PTtX2PaLu0P2KMjGvXhZ')|>bind_rows()
divisions_2023_Q1 <-read_json('https://theyvoteforyou.org.au/api/v1/divisions.json?start_date=2024-01-01&end_date=2024-03-15&house=representatives&key=PTtX2PaLu0P2KMjGvXhZ')|>bind_rows()

divisions_47<-bind_rows(divisions_2023_Q1,divisions_2023_H2,divisions_2023_H1,divisions_2022)

# divisions...
vote_info_list<-list()

for(i in 1:nrow(divisions_47)){
  
  print(paste(i,"out of", nrow(divisions_47)))
  
  url<-paste0("https://theyvoteforyou.org.au/api/v1/divisions/",divisions_47$id[i],".json?key=PTtX2PaLu0P2KMjGvXhZ")
  division<-read_json(url)
  votes<-division[["votes"]]
  division_name<-paste0('div',division$id)
  
  extract_vote <- function(votes, division_name) {
    person_id <- votes$member$person$id
    vote <- votes$vote
    
    out<-list(person_id = person_id, vote = vote)
    
    names(out)[2]<-division_name
    return(out)
  }
  
  vote_info_list[[i]] <- lapply(votes, extract_vote, division_name)|>bind_rows()
  
}


# get personal information...

current_people<-read_json("https://theyvoteforyou.org.au/api/v1/people.json?key=PTtX2PaLu0P2KMjGvXhZ")


people_tibble<-lapply(current_people, unlist)|>bind_rows()|>select(-latest_member.id)

names(people_tibble)<-gsub("latest_member.","",names(people_tibble), fixed = T)

names(people_tibble)[1]<-'person_id'


# append_lists


vote_frame<-vote_info_list|> reduce(full_join, by = "person_id")

vote_frame$person_id<-as.character(vote_frame$person_id)

vote_frame_joined<-vote_frame|>full_join(people_tibble)

# re attaching former MPs to the dataset

mp_ids<- vote_frame_joined$person_id[is.na(vote_frame_joined$name.first)]

mp_ids<-data.frame(person_id=mp_ids)
leftover_mps<-list()


for(i in 1:nrow(mp_ids)){
  url<-paste0("https://theyvoteforyou.org.au/api/v1/people/",mp_ids$person_id[i],".json?key=PTtX2PaLu0P2KMjGvXhZ")
  person<-read_json(url)
  flat<-person[2]|>unlist()
  names(flat)<-gsub("latest_member.","",names(flat))
  flat<-t(flat)|>as.data.frame()|>select(-id)
  flat<-bind_cols(person_id=mp_ids[i,1], flat)
  leftover_mps[[i]]<-flat
}

leftover_mps<-bind_rows(leftover_mps)

# merging back to the main data...

vote_frame_current<-vote_frame_joined|>filter(!is.na(electorate))

vote_frame_past<-vote_frame_joined|>filter(is.na(electorate))|>
  select(-(name.first:party))|>left_join(leftover_mps)

vote_frame_full<-bind_rows(vote_frame_current,vote_frame_past)

# restricting to house members

vote_frame_house<-vote_frame_full|>filter(house=="representatives")


# identifying climate 200 MPs

vote_frame_house$teal<-FALSE
vote_frame_house$teal_plus<-FALSE

vote_frame_house$teal[vote_frame_house$electorate==
                              "North Sydney"|
                              vote_frame_house$electorate==
                              "Goldstein"|
                              vote_frame_house$electorate==
                              "Kooyong"|
                              vote_frame_house$electorate==
                              "Mackellar"|
                              vote_frame_house$electorate==
                              "Wentworth"|
                              vote_frame_house$electorate==
                              "Curtin"]<-TRUE

vote_frame_house$teal_plus[vote_frame_house$electorate==
                                   "North Sydney"|
                                   vote_frame_house$electorate==
                                   "Goldstein"|
                                   vote_frame_house$electorate==
                                   "Kooyong"|
                                   vote_frame_house$electorate==
                                   "Mackellar"|
                                   vote_frame_house$electorate==
                                   "Wentworth"|
                                   vote_frame_house$electorate==
                                   "Curtin"|
                                   vote_frame_house$electorate==
                                   "Warringah"|
                                   vote_frame_house$electorate==
                                   "Mayo"|
                                   vote_frame_house$electorate==
                                   "Clark"|
                                   vote_frame_house$electorate==
                                   "Indi"]<-TRUE

# reordering variables
vote_frame_house<-vote_frame_house|>
  relocate(person_id,name.first,name.last,house,electorate,party, teal,teal_plus)


# save file

saveRDS(vote_frame_house, "working_data/house_votes.rds")



