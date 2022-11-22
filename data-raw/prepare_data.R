## code to to creatae internal data objects goes here



# data for domain and indicator snakey chart ------------------------------

gnh_nodes <- data.frame(name = c("GNH","Psychological wellbeing (1/9)",
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





# write objects in sys data -----------------------------------------------


usethis::use_data(gnh_links,
                  gnh_nodes,
                  overwrite = TRUE,internal = T)
