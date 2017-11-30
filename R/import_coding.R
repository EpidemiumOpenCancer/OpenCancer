
import_coding <- function(){

  codesILO <- readr::read_csv("http://qa.epidemium.cc/data/epidemiology_dataset/ilo_data/Ilostat_Indicators.csv") %>%
    select(collection.label, indicator_full_code) %>%
    rename(label = collection.label, code = indicator_full_code) %>%
    dplyr::mutate(source = "ILO")

  codesWB <- readr::read_csv("http://qa.epidemium.cc/data/epidemiology_dataset/world_bank_data/WorldBank_Indicators.csv") %>%
    select(Code,`Indicator Name`) %>%
    rename(label = `Indicator Name`, code = Code) %>%
    dplyr::mutate(source = "WB")

  codesFAO <- readr::read_csv("http://qa.epidemium.cc/data/epidemiology_dataset/fao_data/Faostat_Indicators.csv") %>%
    dplyr::mutate(label = paste0(Item," ",Element," (",Unit,")")) %>%
    select(Indicateur,label) %>%
    rename(code = Indicateur) %>% dplyr::mutate(source = "FAO")

  df <- dplyr::full_join(
    dplyr::full_join(codesILO,codesWB),codesFAO)

  return(df)
}
