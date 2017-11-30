## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ---- message=FALSE,warning=FALSE----------------------------------------

library(OpenCancer)
datadir <- paste0(getwd(),"/inst")


## ---- message=FALSE,warning=FALSE----------------------------------------

df_FAO <- import_FAO(path = datadir, colstokeep = 1:100)
knitr::kable(head(df_FAO[!is.na(df_FAO[,10]),1:10]))


## ---- message=FALSE,warning=FALSE----------------------------------------

df_WB <- import_WB(path = datadir, colstokeep = 1:100)
knitr::kable(head(df_WB[!is.na(df_WB[,6]),1:6]))

## ---- message=FALSE,warning=FALSE----------------------------------------
df_ILO <- import_ILO(path = datadir, colstokeep = 1:100)
knitr::kable(head(df_ILO[!is.na(df_ILO[,6]),1:6]))

## ---- message=FALSE,warning=FALSE----------------------------------------

#df_training <- import_training(path = datadir, colstokeep = NULL)
df_training <- import_training(path = datadir, colstokeep = NULL,
                               filename = "training_IARC.csv")

## ---- message=FALSE,warning=FALSE----------------------------------------
knitr::kable(head(na.omit(df_training)))

## ---- message=FALSE,warning=FALSE----------------------------------------

df_label <- import_coding()


## ---- message=FALSE,warning=FALSE----------------------------------------

codes <- readxl::read_excel(path = paste0(datadir,"/creation zonier.xls"),
                            sheet = "Transco_Country")

knitr::kable(head(codes))


## ---- message=FALSE,warning=FALSE----------------------------------------

df_FAO = matchkeys(df_FAO,codes)
df_WB = matchkeys(df_WB,codes,origin = "World Bank")
df_ILO = matchkeys(df_ILO,codes, origin = "IloStat")


## ---- message=FALSE,warning=FALSE----------------------------------------

# LEFT JOIN FAO AND WORLD BANK DATA: NO RESTRICTION TO POST 1970 DATA
df_covariates <- dplyr::full_join(df_FAO,df_WB, by = c("Country_Transco","year","Zonier"))

# JOIN AVEC ILO
df_covariates <- dplyr::left_join(df_covariates,df_ILO, by = c("Country_Transco","year","Zonier"))



## ---- message=FALSE,warning=FALSE----------------------------------------

# CHANGING MISMATCHED NAMES WITH TRAINING DATASET
oldnames <- unique(df_training$country[!df_training$country %in% 
                                         intersect(df_covariates$Country_Transco,df_training$country)])
newnames <- c("Netherlands","Slovakia","United States")

# IF DATAFRAME IS training.csv UNCOMMENT
#newnames <- c("Netherlands","Slovakia","United States","Gambia, The","Australian Capital Territory",
#              "South Korea","VietNam","Yugoslavia")
df_training$country <- plyr::mapvalues(df_training$country, from=oldnames, to=newnames,
                                       warn_missing = FALSE)


## ---- message=FALSE,warning=FALSE----------------------------------------

# HARMONIZING COLUMN NAMES
df_training  <- df_training %>% dplyr::rename(Country_Transco = country)
df_training$ethnicity[is.na(df_training$ethnicity)] <- "All population" 
df_training$region[is.na(df_training$region)] <- "All regions" 

# JOIN WITH TRAINING DATASET
df_full <- dplyr::left_join(df_training,df_covariates)

# REMOVE UNNECESSARY VARIABLES
df_full <- df_full %>% select_(.dots = paste0("-", c("cancer", "area.x","area.y",
             "area_code","ref_area",
             "area","sourceFAO","sourceWB","sourceILO"))
)

df_full <- df_full[,!check.emptycolumn(df_full)]

knitr::kable(head(df_full[!is.na(df_full[,10]),1:10]))


## ---- message=FALSE,warning=FALSE----------------------------------------

pryr::mem_change(rm(df_ILO,df_FAO,df_WB,df_training,
   df_covariates))

df_full <- df_full %>% filter(year>=1970)


## ---- message=FALSE,warning=FALSE----------------------------------------

if (sum(check.emptycolumn(df_full))>0) df_full <- check.emptycolumn(df_full)


## ---- message=FALSE,warning=FALSE----------------------------------------

colstokeep <- !check.emptycolumn(df_full,proportion = T)>0.4
if (!colstokeep["ethnicity"]) colstokeep["ethnicity"] <- TRUE
df_full <- df_full[,colstokeep]


## ---- message=FALSE,warning=FALSE----------------------------------------

head(check.factor(df_full,check.levels = T,threshold = 12))


## ---- message=FALSE,warning=FALSE----------------------------------------

df_full <- check.factor(df_full,check.levels = F,threshold = 12)


## ---- message=FALSE,warning=FALSE----------------------------------------

if (sum(apply(df_full[,!sapply(df_full, is.character)], 2, var, na.rm=TRUE) == 0)>0){
  df_full <- df_full[,-which(apply(df_full[,!sapply(df_full, is.character)], 2, var, na.rm=TRUE) == 0)]
}

dim(df_full)

## ---- message=FALSE,warning=FALSE----------------------------------------
 
head(plyr::mapvalues(names(which(convert_fakezero(df_full,convert = F))),
                from = df_label$code,
                to = df_label$label, warn_missing = FALSE)
)

## ---- message=FALSE,warning=FALSE----------------------------------------

potential.miscoding <- 
  names(which(convert_fakezero(df_full,convert = F)))
miscoded <- potential.miscoding[!potential.miscoding %in% "incidence"]
is.na(df_full[,miscoded]) <- !df_full[,miscoded]


## ------------------------------------------------------------------------

df <- data.frame(year = c(1980,1983:1985,1990),y = rnorm(length(c(1980,1983:1985,1990))))
knitr::kable(df,caption = "Initial incomplete dataframes")

# FILL DATAFRAME
df <- fill_dataframe(df,year.var = 'year',label.var = NULL)
knitr::kable(df, caption = "Missing years have been appended")


## ------------------------------------------------------------------------

interpo <- interpolate_data(df_full[,c(1:15, which(colnames(df_full) == "Zonier"))],
                 groupingvar = c("sex","age","Country_Transco", "region","ethnicity"),
                 year.var = 'year', label.var = 'Zonier',
                 threshold = 0.2)


## ------------------------------------------------------------------------
df_full2 <- interpo$data 

## ------------------------------------------------------------------------

conversion <- convert.character(df_full2)
names(conversion)


## ------------------------------------------------------------------------

knitr::kable(head(conversion$data[,1:10]))
knitr::kable(head(conversion$correspondance[[1]]))

df_full2 <- conversion$data


## ------------------------------------------------------------------------

df_full2$fakefactor <- factor(sample(1:8, size = nrow(df_full2),
                              replace = T))

knitr::kable(head(expand.dummies(df_full2,labelvar = "sex")))


## ---- eval = F-----------------------------------------------------------
#  
#  factorvar <- which(sapply(df_full2,is.factor))
#  factorvar <- factorvar[-which(names(factorvar) == 'sex')]
#  
#  df_full2 <- cbind(df[,-factorvar],expand.dummies(df_full2,labelvar = "sex"))
#  

## ---- eval = F-----------------------------------------------------------
#  
#  df_full2 <- createlag(df_full2[,1:8], groupingvar = c("sex","age","Country_Transco",
#                                      "region","ethnicity"), k = 2,
#                        labelvar = c("year","incidence"))
#  
#  knitr::kable(df_full2[,colnames(df_full2)[8]])
#  

## ---- eval = F-----------------------------------------------------------
#  
#  X <- df_full[,1:15]
#  X <- convert.character(X)$data
#  Y <- as.big.matrix(data.frame(X))
#  
#  preprocess.model <- performPreProcess(Y, groupingvar = c("sex","age","Country_Transco",
#                                       "region","ethnicity"),
#                    labelvar = "year")
#  

