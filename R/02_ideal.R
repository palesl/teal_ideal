# Purpose of this code. To run and evaluate fit for some ideal point models
# making use of:
# Armstrong, David A., et al.2020.
# Analyzing Spatial Models of Choice and Judgment. 2nd ed.
# New York: Chapman and Hall/CRC. doi:10.1201/9781315197609.

set.seed(1234)

dat<-readRDS('working_data/house_votes.rds')
divisions<-readRDS( 'working_data/divisions.rds')
library(pscl);library(tidyverse);library(oc);library(politicsR)



# rice scores.

rice_dat<-cbind(dat[,1:6],apply(dat[7:ncol(dat)],2,as.character))


rice_dat[rice_dat=='1']<-'Yay'
rice_dat[rice_dat=='2']<-'Nay'
rice_dat[rice_dat=='5']<-NA
rice_dat[rice_dat=='9']<-NA

teal_rice<- rice_dat|>filter(teal==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

teal_splits<-rice_dat|>filter(teal==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  t() |>as.data.frame()

teal_splits$division_name<-rownames(teal_splits)
names(teal_splits)[1]<-"Rice_teal"

divisions$division_name<-paste0("div",divisions$divisionNumber,"_",
                                as.Date(divisions$date)|>format("%d%b%Y"))

divisions<-divisions|>left_join(teal_splits)

divisions$url<-paste0("https://www.aph.gov.au/Parliamentary_Business/Chamber_documents/HoR/Divisions/Details?id=",
                      divisions$divisionId)




teal_plus_rice<- rice_dat|>filter(teal_plus==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()


teal_plus_splits<-rice_dat|>filter(teal_plus==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  t() |>as.data.frame()

teal_plus_splits$division_name<-rownames(teal_plus_splits)


names(teal_plus_splits)[1]<-"Rice_teal_plus"




divisions<-divisions|>left_join(teal_plus_splits)
divisions$date<-as.Date(divisions$date)
ggplot(data=divisions, aes(date, Rice_teal))+geom_smooth()+geom_point()
teal_split_table<-divisions|>filter(Rice_teal<1)|>
  select(title, question)|>
  mutate(division_type=case_when(grepl("Fair Work",title)~"Bills, Fair Work Legislation",
                                 grepl("Bill",title)~"Bills, other",
                                 grepl("Suspension of", title)~"Suspension of Standing Orders, other procedure",
                                 grepl("Hamas",title)~"Israel Gaza Conflict",
                                 grepl("Two-state",title)~"Israel Gaza Conflict",
                                 .default = "Motions, other")
  )|>group_by(division_type)|>
  summarise(n=n(),
            teals =paste0(n()," (",round(100*n()/nrow(divisions|>filter(Rice_teal_plus<1)),1),"%)")
  )|>arrange(-n)|>select(-n)


teal_split_divisions<-divisions|>filter(Rice_teal<1)|>
  select(divisionId, divisionNumber, url, title, question,Rice_teal)|>
  mutate(division_type=case_when(grepl("Fair Work",title)~"Bills, Fair Work Legislation",
                                 grepl("Bill",title)~"Bills, other",
                                 grepl("Suspension of", title)~"Suspension of Standing Orders, other procedure",
                                 grepl("Hamas",title)~"Israel Gaza Conflict",
                                 grepl("Two-state",title)~"Israel Gaza Conflict",
                                 .default = "Motions, other")
  )

write_excel_csv(teal_split_divisions, 'working_data/teal_splits_afr.csv')

teal_plus_split_table<-divisions|>filter(Rice_teal_plus<1)|>
  select(title, question)|>
  mutate(division_type=case_when(grepl("Fair Work",title)~"Bills, Fair Work Legislation",
                                 grepl("Bill",title)~"Bills, other",
                                 grepl("Suspension of", title)~"Suspension of Standing Orders, other procedure",
                                 grepl("Hamas",title)~"Israel Gaza Conflict",
                                 grepl("Two-state",title)~"Israel Gaza Conflict",
                                 .default = "Motions, other")
  )|>group_by(division_type)|>
  summarise(n=n(),
            teals_plus=paste0(n()," (",round(100*n()/nrow(divisions|>filter(Rice_teal_plus<1)),1),"%)")
            )|>arrange(-n)|>select(-n)

split_tab<-teal_split_table|>left_join(teal_plus_split_table)





alp_rice<-rice_dat|>filter(partyName=="Australian Labor Party")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

lib_rice<-rice_dat|>filter(partyName=="Liberal Party of Australia"|
                             partyName=="Liberal National Party of Queensland")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

nat_rice<-rice_dat|>filter(partyName=="The Nationals")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

green_rice <-rice_dat|>filter(partyName=="Australian Greens")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ rice(.x)))|>
  rowMeans()

rice_scores <- bind_rows(teal=teal_rice,
                         teal_plus=teal_plus_rice,
                         alp=alp_rice,
                         lib=lib_rice,
                         nat=nat_rice,
                         green=green_rice)

## percent agreement for the 8 teals ####

Mode <- function(x, na.rm=F) {
  ux <- unique(x)
  if(na.rm==T){ux<-ux[!is.na(ux)]}
  ux[which.max(tabulate(match(x, ux)))]
}


pos_alp<-rice_dat|>filter(partyName=="Australian Labor Party")|>
  select(7:ncol(rice_dat))|>
  apply(2,Mode, na.rm=T)

pos_green<-rice_dat|>filter(partyName=="Australian Greens")|>
  select(7:ncol(rice_dat))|>
  apply(2,Mode, na.rm=T)

pos_lib<-rice_dat|>filter(partyName=="Liberal Party of Australia"|
                            partyName=="Liberal National Party of Queensland")|>
  select(7:ncol(rice_dat))|>
  apply(2,Mode, na.rm=T)

pos_teal_skeleton<-rice_dat|>filter(teal_plus==TRUE&
                             DisplayName!="WILKIE, Andrew Damien"&
                             DisplayName!="SHARKIE, Rebekha Carina Che")|>
  t()

pos_teal<-pos_teal_skeleton[7:nrow(pos_teal_skeleton),]|>as.data.frame()
names(pos_teal)<-gsub(",.*$",'',pos_teal_skeleton[2,])

teal_vote_with<-bind_cols(pos_teal, "LIB"=pos_lib, "ALP"=pos_alp,"GRN"=pos_green)

teal_agreement<-teal_vote_with|>
  mutate(
    CHANEY_LIB=CHANEY==LIB,
    CHANEY_ALP=CHANEY==ALP,
    CHANEY_GRN=CHANEY==GRN,

    DANIEL_LIB=DANIEL==LIB,
    DANIEL_ALP=DANIEL==ALP,
    DANIEL_GRN=DANIEL==GRN,

    HAINES_LIB=HAINES==LIB,
    HAINES_ALP=HAINES==ALP,
    HAINES_GRN=HAINES==GRN,

    RYAN_LIB=RYAN==LIB,
    RYAN_ALP=RYAN==ALP,
    RYAN_GRN=RYAN==GRN,

    SCAMPS_LIB=SCAMPS==LIB,
    SCAMPS_ALP=SCAMPS==ALP,
    SCAMPS_GRN=SCAMPS==GRN,

    SPENDER_LIB=SPENDER==LIB,
    SPENDER_ALP=SPENDER==ALP,
    SPENDER_GRN=SPENDER==GRN,

    STEGGALL_LIB=STEGGALL==LIB,
    STEGGALL_ALP=STEGGALL==ALP,
    STEGGALL_GRN=STEGGALL==GRN,

    TINK_LIB=TINK==LIB,
    TINK_ALP=TINK==ALP,
    TINK_GRN=TINK==GRN
  )|>select(CHANEY_LIB:TINK_GRN)

div_types<-divisions|>
  select(title, question)|>
  mutate(legislation=case_when(
    grepl("Bill",title)~T,
    .default = F)
  )

library(flextable)

ta <-teal_agreement|>colSums(na.rm = T)|>
  matrix(ncol=3, byrow=T)
rownames(ta)<-names(pos_teal)
colnames(ta)<-c("LIB", "ALP", "GRN")
ta

ta_all<-round(ta*100/554)
ta_all

lengths<-colSums(!is.na(pos_teal))
lengths<-matrix(lengths,ncol=3, nrow=8)
lengths

ta_voted<-round(ta*100/lengths)
ta_voted

ta_voted|>as.data.frame()|>rownames_to_column(" ")|>
  flextable()|>set_caption("Percent agreement, all votes")



teal_agreement_bills<-teal_agreement[div_types$legislation,]

ta_b <-teal_agreement_bills|>colSums(na.rm = T)|>
  matrix(ncol=3, byrow=T)
rownames(ta_b)<-names(pos_teal)
colnames(ta_b)<-c("LIB", "ALP", "GRN")
ta_b

lengths_bills<-colSums(!is.na(pos_teal[div_types$legislation,]))
lengths_bills<-matrix(lengths_bills,ncol=3, nrow=8)
lengths_bills


ta_b_voted<-round(ta_b*100/lengths_bills)

ta_b_voted|>as.data.frame()|>rownames_to_column(" ")|>
  flextable()|>set_caption("Percent agreement, votes on legislation")


## votes with katter ##

KATTER <- rice_dat|>filter(DisplayName=="KATTER, the Hon. Robert (Bob) Carl")|>
  select(7:ncol(rice_dat))|>as.character()

vote_with_katter<-teal_vote_with

for(i in 1:ncol(vote_with_katter)){
  vote_with_katter[,i]<-KATTER==teal_vote_with[,i]
}

katter_lengths<-colSums(!is.na(vote_with_katter))
katter_agreement<-vote_with_katter|>colSums(na.rm = T)
katter_perc<-round(katter_agreement*100/katter_lengths)
katter_names<-names(katter_lengths)
data.frame(`name`= katter_names,
           `Votes` = katter_agreement,
           `n`=katter_lengths,
           `perc`= katter_perc)|>flextable()|>
  set_caption("Percent agreement, teals and major parties with Bob Katter (all votes)")

# agreement index scores (Hix, Noury and Roland)
# (max{Yi, Ni, Ai} - 1/2*[(Yi + Ni + Ai) - max{Yi, Ni, Ai}] )/(Yi + Ni + Ai)


ai<- function(x){
  y<-length(x[x==1])
  n<-length(x[x==2])
  a<-length(x[x==5])

  out<- (max(y,n,a) - 0.5*((y+n+a)-max(y,n,a)))/(y+n+a)

  return(out)
}

teal_ai<- dat|>filter(teal==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

teal_plus_ai<- dat|>filter(teal_plus==T)|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

alp_ai<-dat|>filter(partyName=="Australian Labor Party")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

lib_ai<-dat|>filter(partyName=="Liberal Party of Australia"|
                      partyName=="Liberal National Party of Queensland")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

nat_ai<-dat|>filter(partyName=="The Nationals")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

green_ai <-dat|>filter(partyName=="Australian Greens")|>
  select(7:ncol(rice_dat))|>
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )|>
  mutate(across(where(is.character), as.factor))|>
  summarise(across(starts_with("div"), ~ ai(.x)))|>
  rowMeans()

ai_scores <- bind_rows(teal=teal_ai,
                       teal_plus=teal_plus_ai,
                       alp=alp_ai,
                       lib=lib_ai,
                       nat=nat_ai,
                       green=green_ai)

# model_mat <- dat|>dplyr::select(starts_with('div'))|>as.matrix()

# model_mat[model_mat=="aye"] <- 1
# model_mat[model_mat=="no"] <- 0
# rownames(model_mat)<-paste(dat$name.first,dat$name.last,sep = "_")
#
#
# #cleaning matrix
#
# rownames(model_mat) <- gsub("\\-","",rownames(model_mat))
#

# first with bayesian IRT...

# now with some model tuning, including only MPs who have voted more than 50 times
# this removes milton dick, who is the Speaker

# cutoff <- 50
# model_mat <- model_mat[rowSums(!is.na(model_mat)) >= cutoff,]
#
# head(sort(rowSums(!is.na(model_mat))))
#
# posterior2d <- MCMCirtKd(model_mat, dimensions=2,
#                          mcmc=15000, burnin=15000, thin=3,
#                          theta.start=NA, alpha.start=NA, beta.start=NA,
#                          t0=0, T0=1, a0=0, A0=0.25, b0=0, B0=0.25,
#                          seed=NA, verbose=0, store.item=FALSE,
#                          store.ability=TRUE, drop.constant.items=TRUE)
#
#
# idealpt1 <- colMeans(posterior2d[,seq(1, ncol(posterior2d), by=2)])
# idealpt2 <- colMeans(posterior2d[,seq(2, ncol(posterior2d), by=2)])
#
#
# # diagnostics for the tuned model...
#
# plot(posterior2d)
#
#
# theta1<-posterior2d[,seq(1, ncol(posterior2d), by=2)]
# theta2<-posterior2d[,seq(2, ncol(posterior2d), by=2)]
#
# sort(abs(geweke.diag(theta1)$z), decreasing=T)
# sort(abs(geweke.diag(theta2)$z), decreasing=T)
#
# geweke<-as.mcmc(theta1[,"theta.Joanne_Ryan.1"])

# the diagnostics fail pretty badly here to produce something that looks stable


# Now try with optimal classification

library(oc)

rc <- rollcall(data=dat[,7:ncol(dat)],
               yea=1,
               nay=2,
               missing	= 5,
               notInLegis = 9,
               legis.names=dat$DisplayName,
               vote.names=names(dat[,7:ncol(dat)]),
               legis.data=dat[,1:6],
               vote.data=NULL,
               desc="47th Parliament of the Australian House of Representatives")


dutton_no<- grep('DUTTON,',dat$DisplayName)

oc_teal_1dim <- oc(rc, dims=1, minvotes=25, lop=0.05,
                   polarity=c(dutton_no), verbose=T)

oc_teal_2dim <- oc(rc, dims=2, minvotes=25, lop=0.05,
                   polarity=c(dutton_no,dutton_no), verbose=T)

# checking fits

summary(oc_teal_2dim)
summary(oc_teal_1dim)

fits <- cbind(oc_teal_1dim$fits, oc_teal_2dim$fits)

colnames(fits) <- c("1 Dim", "2 Dim")
rownames(fits) <- c("% Correct", "APRE")

fits

# clearly we have a winner in terms of fit, both average proportional
# reduction of error and %correct are excellent

# how about the cutlines...

cutlines <- oc_teal_2dim$rollcalls




# output data


ip_frame <- oc_teal_2dim[["legislators"]]


saveRDS(ip_frame, 'working_data/oc_out_2d.rds')
saveRDS(cutlines, 'working_data/oc_cutlines_2d.rds')
