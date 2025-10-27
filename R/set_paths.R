set_paths <- function(info = Sys.info()) {

  # you can add your user and node if you want to use other than the generic paths

  if (info["user"] == "ffinger") {
    # paths for Flavio's laptop
    
    mountpath <- "/media/ffinger/sharepoint/grp-epi-proj-ebolalinelist"

    if(length(dir(path = mountpath, all.files=TRUE))<=2) { #if not mounted (dir is empty) --> mount sharepoint
      system(paste0("rclone --vfs-cache-mode full mount grp-epi-proj-ebolalinelist: ", mountpath ," &"))
      Sys.sleep(15) #give time to mount
    }

    paths = list(
      dir_data = mountpath,
      file_data_vax = paste0(mountpath, "/data/vaccination/rvsv_vaccination_list_pev_latest.feather"),
      file_data_vax_weeklytotal=paste0(mountpath, "/data/vaccination/df_weekly_nb_vaccinations.xlsx"),
      file_data_etc = paste0(mountpath, "/data/linelist/TC_ETC/full/msf_ebola_linelist.rds"),
      file_data_mll = paste0(mountpath, "/data/linelist/MASTER_LL/mll_clean_simple_2020-11-14.rds"),
      file_data_clinical = paste0(mountpath, "/BD_Clinique_MVE_2018/Données\ Exports/export_2021-10-12/Ebola_BDC.rda"),
      dir_shp = paste0(mountpath, "/data/shapefile")
    )
  } else if (info["user"] == "aminatand") {
    # paths for Aminata's laptop
    paths = list(
      dir_data = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data",
      file_data_vax = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/vaccination/rvsv_vaccination_list_pev_latest.feather",
      file_data_vax_weeklytotal= "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/vaccination/df_weekly_nb_vaccinations.xlsx",
      file_data_etc = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/linelist/TC_ETC/full/msf_ebola_linelist.rds",
      file_data_mll = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/line,list/MASTER_LL/mll_clean_simple_2020-11-14.rds",
      dir_shp = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/shapefile"
    )
    #Aminata'access locally via onedrive
    } else if (info["user"] == "ALS-NDIAYE") {
      # paths for Aminata's laptop
      paths = list(
        dir_data = "D:/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data",
        file_data_vax = "D:/MSF/GRP-EPI-PROJ-EbolaLineList - data/vaccination/rvsv_vaccination_list_pev_latest.feather",
        file_data_vax_weeklytotal= "D:/MSF/GRP-EPI-PROJ-EbolaLineList - data/vaccination/df_weekly_nb_vaccinations.xlsx",
        file_data_etc = "D:/MSF/GRP-EPI-PROJ-EbolaLineList - data/linelist/TC_ETC/full/msf_ebola_linelist.rds",
        file_data_mll = "D:/MSF/GRP-EPI-PROJ-EbolaLineList - data/linelist/MASTER_LL/mll_clean_simple_2020-11-14.rds",
        file_data_clinical = "D:/MSF/GRP-EPI-PROJ-EbolaLineList - export_2021-10-12/Ebola_BDC.rda",
        dir_shp = "D:/MSF/GRP-EPI-PROJ-EbolaLineList - data/shapefile"
      )
  } else if (info["user"] == "ntncmch") {
    # paths for Anton's laptop
    paths = list(
      dir_data = "~/Library/CloudStorage/OneDrive-SharedLibraries-MSF/Ebola Outbreaks - COD-10/data",
      file_data_vax = "~/Library/CloudStorage/OneDrive-SharedLibraries-MSF/Ebola Outbreaks - COD-10/data/vaccination/rvsv_vaccination_list_pev_latest.feather",
      file_data_vax_weeklytotal= "~/Library/CloudStorage/OneDrive-SharedLibraries-MSF/Ebola Outbreaks - COD-10/data/vaccination/df_weekly_nb_vaccinations.xlsx",
      file_data_etc = "~/Library/CloudStorage/OneDrive-SharedLibraries-MSF/Ebola Outbreaks - COD-10/data/linelist/TC_ETC/full/msf_ebola_linelist.rds",
      file_data_mll = "~/Library/CloudStorage/OneDrive-SharedLibraries-MSF/Ebola Outbreaks - COD-10/data/linelist/MASTER_LL/mll_clean_simple_2020-11-14.rds",
      file_data_clinical = "~/Library/CloudStorage/OneDrive-SharedLibraries-MSF/Ebola Outbreaks - COD-10/BD_Clinique_MVE_2018/Données Exports/export_2021-10-12/Ebola_BDC.rda",
      dir_shp = "~/Library/CloudStorage/OneDrive-SharedLibraries-MSF/Ebola Outbreaks - COD-10/data/shapefile"
    )
  } else if (info["user"] == "sophiemeakin") {
    # paths for Sophie's laptop
    paths = list(
      dir_data = "~/OneDrive - MSF/RDC10/data",
      file_data_vax = "~/OneDrive - MSF/RDC10/data/vaccination/rvsv_vaccination_list_pev_latest.feather",
      file_data_vax_weeklytotal= "~/OneDrive - MSF/RDC10/data/vaccination/df_weekly_nb_vaccinations.xlsx",
      file_data_etc = "~/OneDrive - MSF/RDC10/data/linelist/TC_ETC/anonymized/ebola_etc_linelist_combined.csv",
      file_data_mll = "~/OneDrive - MSF/RDC10/data/linelist/MASTER_LL/mll_clean_simple_2020-11-14.rds",
      file_data_clinical = "~/OneDrive - MSF/RDC10/BD_Clinique_MVE_2018/Données Exports/export_2021-10-12/Ebola_BDC.rda",
      dir_shp = "~/OneDrive - MSF/RDC10/data/shapefile"
    )
  } else {
    # generic paths, if the machine is not above
    warning("User and computer not recognized, generic paths set.")
    paths = list(
      dir_data = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data",
      file_data_vax = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/vaccination/rvsv_vaccination_list_pev_latest.feather",
      file_data_vax_weeklytotal= "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/vaccination/df_weekly_nb_vaccinations.xlsx",
      file_data_etc = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/linelist/TC_ETC/full/msf_ebola_linelist.rds",
      file_data_mll = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/linelist/MASTER_LL/mll_clean_simple_2020-11-14.rds",
      dir_shp = "~/MSF/GRP-EPI-PROJ-EbolaLineList - Documents/data/shapefile"
    )
  }

  return(paths)
}
