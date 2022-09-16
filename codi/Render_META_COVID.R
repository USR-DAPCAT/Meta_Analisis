
### Generació d'informe



#0)
rmarkdown::render("codi/met_lect_prep_anal.Rmd", 
                  output_file = here::here("resultats",paste0("Informe_Meta_Analisi_COVID_",Sys.Date()))
                  
                  )



#i)
rmarkdown::render("codi/met_lect_prep_anal_PVD.Rmd", 
                  output_file = here::here("resultats",paste0("Informe_Meta_Analisi_COVID_PVD_",Sys.Date()))
                  
)


#ii)
rmarkdown::render("codi/met_lect_prep_anal_IHD.Rmd", 
                  output_file = here::here("resultats",paste0("Informe_Meta_Analisi_COVID_IHD_",Sys.Date()))

)



#iii)
rmarkdown::render("codi/met_lect_prep_anal_TIA.Rmd", 
                  output_file = here::here("resultats",paste0("Informe_Meta_Analisi_COVID_TIA_",Sys.Date()))
                  
)