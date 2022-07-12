
### Generació d'informe




rmarkdown::render("codi/met_lect_prep_anal.Rmd", 
                  output_file = here::here("resultats",paste0("Informe_Meta_Analisi_COVID_",Sys.Date()))
                  
                  )




