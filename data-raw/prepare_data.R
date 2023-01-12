## code to to creatae internal data objects goes here

library(here)
library(dplyr)
library(tidyr)




# data for domain and indicator snakey chart ------------------------------

gnh_nodes <- data.frame(name = c("GNH Index","Psychological wellbeing (1/9)",
                                 "Life satisfaction (1/3)",
                                 "Positive emotion (1/6)",
                                 "Negative emotion (1/6)",
                                 "Spirituality (1/3)",
                                 "Health (1/9)",
                                 "Self-reported health status (1/10)",
                                 "Number of healthy days (3/10)",
                                 "Disability (3/10)",
                                 "Mental Health (3/10)",
                                 "Time Use (1/9)",
                                 "Work (1/2)",
                                 "Sleep (1/2)",
                                 "Education (1/9)",
                                 "Schooling (3/10)",
                                 "Literacy (3/10)",
                                 "Value (1/5)",
                                 "Knowledge (1/5)",
                                 "Cultural diversity & resilience (1/9)",
                                 "Zorig chusum skills (Artisan Skills) (3/10)",
                                 "Speak native language (1/5)",
                                 "Cultural Participation (3/10)",
                                 "Driglam Namzha (code of etiquette and conduct) (1/5)",
                                 "Good governance (1/9)",
                                 "Government performance (1/10)",
                                 "Fundamental rights (1/10)",
                                 "Services (2/5)",
                                 "Political participation (2/5)",
                                 "Community vitality (1/9)",
                                 "Donation (time & money) (3/10)",
                                 "Community realationship (1/5)",
                                 "Family (1/5)",
                                 "Safety (3/10)",
                                 "Ecological diversity & resilience (1/9)",
                                 "Ecological issues (1/10)",
                                 "Responsibility towards environment (1/10)",
                                 "Wildlife damage (2/5)",
                                 "Urban issues (2/5)",
                                 "Living Standard (1/9)",
                                 "Household per capita income (1/3)",
                                 "Housing (1/3)",
                                 "Assets (1/3)"
)
)





gnh_links = data.frame(

  source = c(rep(0,9),
             rep(1,4),
             rep(6,4),
             rep(11,2),
             rep(14,4),
             rep(19,4),
             rep(24,4),
             rep(29,4),
             rep(34,4),
             rep(39,3)),
  target = c(1,6,11,14,19,24,29,34,39,
             2:5,
             7:10,
             12:13,
             15:18,
             20:23,
             25:28,
             30:33,
             35:38,
             40:42

  ),
  value = c(rep(1/9,9),
            c(1/3,1/6,1/6,1/3)*1/9,
            c(1/10,3/10,3/10,3/10)*1/9,
            c(1/2,1/2)*1/9,
            c(3/10,3/10,1/5,1/5)*1/9,
            c(3/10,1/5,3/10,1/5)*1/9,
            c(1/10,1/10,2/5,2/5)*1/9,
            c(3/10,1/5,1/5,3/10)*1/9,
            c(1/10,1/10,2/5,2/5)*1/9,
            c(1/3,1/3,1/3)*1/9
  )

)

gnh_links$ln_grp <- c(rep("a",9),rep("b",4),
                      rep("c",4),
                      rep("d",2),
                      rep("e",4),
                      rep("f",4),
                      rep("g",4),
                      rep("h",4),
                      rep("i",4),
                      rep("j",3)
)



# Indicator colours -------------------------------------------------------

indi_col <- readr::read_csv(here("data-raw/colours_indicators.csv"))


# get data,clean and create sysobject -------------------------------------


gnh_data <- readr::read_csv(here("data-raw/missing_ind_corrected_gnh_2015_all_measures.csv"),
                            col_select = c(1:23))

# remove empty rows

gnh_data |>
  janitor::remove_empty("rows") -> gnh_data


# ~~~~~~~~~~~~~~~~ primary measures data at national level


gnh_data|>
  filter(measure_lab %in% c("GNH (suf)","GNH/MPI (suf)",
                            "Headcount ratio (suf)",
                            "Headcount ratio (Not-Yet-Happy)",
                            "Intensity (suf) among Nnot-Yet-Happy",
                            "Intensity (suf) among Not-Yet-Happy") &
           area_lab %in% c("Rural", "Urban", "National"))|>
  mutate(
    b = ifelse(measure_lab == "Headcount ratio (Not-Yet-Happy)",
               1-b,b),
    measure_lab = ifelse(measure_lab == "Headcount ratio (Not-Yet-Happy)",
                         "Headcount ratio (suf)",measure_lab),
    measure_lab = ifelse(measure_lab == "Intensity (suf) among Nnot-Yet-Happy",
                         "Intensity (suf) among Not-Yet-Happy",
                         measure_lab),
    measure_lab = ifelse(measure_lab == "GNH/MPI (suf)",
                         "GNH (suf)", measure_lab),
    b = ifelse(measure_lab == "GNH (suf)",
               round(b,3), round(b*100,2)),
    area_lab = forcats::fct_relevel(area_lab,c("National","Rural","Urban"))
  ) -> gnh_data_mod_primary_measures



gnh_data_mod_primary_measures |>
  left_join(
    gnh_data |>
      filter(measure_lab == "Population share" &
               area_lab %in% c("Rural", "Urban", "National"))|>
      select(area_lab,measure_lab,share_val=b)|>
      distinct()|>
      mutate(
        share_val = round(share_val*100,2)
      )|>
      add_row(area_lab = "National",measure_lab ="Population share",share_val= 100),
    by = c("area_lab" = "area_lab")
  ) -> gnh_data_mod_primary_measures


gnh_data|>
  filter(measure_lab %in% c("GNH (suf)", "Headcount ratio (suf)",
                            "Intensity (suf)") & !is.na(region_lab))|>
  mutate(
    b =  ifelse(measure_lab == "GNH (suf)",
                round(b,3),round(b*100,2))
  ) -> gnh_data_mod_primay_measures_district_overview

gnh_data_mod_primay_measures_district_overview |>
  left_join(
    gnh_data |>
      filter(measure_lab == "Population share" &
               !is.na(region_lab))|>
      select(region_lab,share_val=b)|>
      distinct()|>
      mutate(
        share_val = round(share_val*100,2)
      ),
    by = c("region_lab" = "region_lab")
  ) -> gnh_data_mod_primay_measures_district_overview


# ~~~~~~~~~~~~~~~~ sufficiency in indicators at national level

gnh_data |>
  filter(measure_lab %in% sort(unique(gnh_data$measure_lab))[c(6,36)] &
                  area_lab %in% c("National", "Urban", "Rural"))|>
  mutate(
    b = round(b*100,2)
    )-> gnh_data_mod_sufficiency_in_indicators

gnh_data_mod_sufficiency_in_indicators|>
  left_join(indi_col, by = c("ind_lab" = "ind")) -> gnh_data_mod_sufficiency_in_indicators

gnh_data |>
  filter(measure_lab %in% sort(unique(gnh_data$measure_lab))[c(6,34)] &
           !is.na(region_lab))|>
  mutate(
    b = round(b*100,2)
  ) -> gnh_data_mod_sufficiency_in_indicators_district_overview

gnh_data_mod_sufficiency_in_indicators_district_overview |>
  left_join(
    indi_col, by = c("ind_lab" = "ind")
  )|>
  left_join(
    gnh_data |>
      filter(measure_lab == "Population share" &
               !is.na(region_lab))|>
      select(region_lab,share_val=b)|>
      distinct()|>
      mutate(
        share_val = round(share_val*100,2)
      ),
    by = c("region_lab" = "region_lab")
  )-> gnh_data_mod_sufficiency_in_indicators_district_overview



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~contribution of indicators to happiness


gnh_data |>
  filter(measure_lab %in% c("Absolute contribution (suf. adj)",
                                   "Relative contribution (suf. adj)" ) &
                  area_lab %in% c("National","Urban","Rural"))|>
  mutate(
    b = ifelse(b<=0.005,round(b*100,5),round(b*100,2))
  ) -> gnh_data_mod_contribution_indicators_national


gnh_data_mod_contribution_indicators_national|>
  left_join(indi_col, by = c("ind_lab" = "ind")) -> gnh_data_mod_contribution_indicators_national


gnh_data |>
  filter(measure_lab %in% c("Absolute contribution (suf. adj)",
                            "Relative contribution (suf. adj)" ) &
           !is.na(region_lab))|>
  mutate(
    b = ifelse(b<=0.005,round(b*100,5),round(b*100,2))
  ) -> gnh_data_mod_contribution_indicators_district_overview


gnh_data_mod_contribution_indicators_district_overview|>
  left_join(indi_col, by = c("ind_lab" = "ind"))|>
  left_join(
    gnh_data |>
      filter(measure_lab == "Population share" &
               !is.na(region_lab))|>
      select(region_lab,share_val=b)|>
      distinct()|>
      mutate(
        share_val = round(share_val*100,2)
      ),
    by = c("region_lab" = "region_lab")
    )-> gnh_data_mod_contribution_indicators_district_overview



# handle shp files and get to geojson format ------------------------------


readRDS("data-raw/gadm36_BTN_1_sf.rds") |>
  mutate(
    NAME_1 = case_when(
      NAME_1 == "Chhukha" ~ "Chukha",
      NAME_1 == "Lhuentse" ~ "Lhuntse",
      NAME_1 == "Monggar" ~ "Mongar",
      NAME_1 == "Pemagatshel" ~ "Pema Gatshel",
      NAME_1 == "Samdrupjongkhar" ~ "Samdrup Jongkhar",
      NAME_1 == "Trashigang" ~ "Tashigang",
      NAME_1 == "Yangtse" ~ "Tashi Yangtse",
      NAME_1 == "Wangduephodrang" ~ "Wangdue Phodrang",
      TRUE ~ NAME_1
    )
  )|>
  rename(region_shp = NAME_1, iso = GID_0)|>select(
    iso,region_shp,geometry
  )|>
  sf::write_sf("data-raw/btn_shp_use_this_to_convert_to_json.shp")

geojsonio::file_to_geojson("data-raw/btn_shp_use_this_to_convert_to_json.shp",
                           method = "local",
                           output = "data-raw/btn_use_for_highchart")

jsonlite::read_json("data-raw/btn_use_for_highchart.geojson") -> btn_poly_json

# write objects in sys data -----------------------------------------------


usethis::use_data(gnh_links,
                  gnh_nodes,
                  gnh_data,
                  gnh_data_mod_primary_measures,
                  gnh_data_mod_primay_measures_district_overview,
                  gnh_data_mod_sufficiency_in_indicators,
                  gnh_data_mod_sufficiency_in_indicators_district_overview,
                  gnh_data_mod_contribution_indicators_national,
                  gnh_data_mod_contribution_indicators_district_overview,
                  btn_poly_json,
                  overwrite = TRUE,internal = T)
