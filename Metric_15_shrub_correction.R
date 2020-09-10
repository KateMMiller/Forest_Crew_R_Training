#----- M15: Invasive frequency table -----
library(tidyverse)
library(forestMIDN)
importCSV("../data")

inv_list <- read.csv("../data/Invasive_List.csv") %>% select(Latin_Name, Year_added)

# Create species list of exotics found in park in all years
park = "VAFO"
park_exo_spplist <- makeSppList(park = park, from = 2007, to = 2019, speciesType = "exotic")
sort(unique(park_exo_spplist$Latin_Name))

# Merge species list of occurrences with MIDN invasive list
park_inv_spplist2 <- merge(inv_list, 
                           park_exo_spplist[ , c("Plot_Number", "Latin_Name", "Common", "present", "cycle")], 
                           by = "Latin_Name", 
                           all.x = FALSE, all.y = TRUE)
head(park_exo_spplist)

# olive <- c(27769, 27776, 27770)
# olive_df <- park_exo_spplist %>% filter(TSN %in% olive) %>% select(Plot_Name, cycle, Latin_Name)
table(park_inv_spplist2$Latin_Name)

# Summarize by species

park_inv_spplist3 <- park_inv_spplist2 %>% #filter(!is.na(Year_added)) %>% 
  group_by(Latin_Name, Common, cycle, Year_added) %>% 
  summarize(numplots = sum(present)) %>% 
  ungroup() #added 7/22

table(unique(park_inv_spplist2$Latin_Name))

# Sometimes multiple common names are listed in plants table, but we only want 1. Code below fixes that
park_inv_spplist3$Common <- gsub(",.*$", "\\1", park_inv_spplist3$Common) # The first two arguments are java
# code to split the string using , and then take the first string.
sort(unique(park_inv_spplist3$Common)) # I prefer calling Persicaria perfoliata mile-a-minute
park_inv_spplist3$Common[park_inv_spplist3$Common == "Asiatic tearthumb"] <- "mile-a-minute"
# I also prefer calling Vinca minor lesser periwinkle
park_inv_spplist3$Common[park_inv_spplist3$Common == "myrtle"] <- "lesser periwinkle"
sort(unique(park_inv_spplist3$Common)) # I prefer calling Persicaria perfoliata mile-a-minute
# That looks better!
# Pivot wide so there's a column for each cycle and a 
park_invlist_wide <- park_inv_spplist3 %>% pivot_wider(names_from = cycle, 
                                                       values_from = numplots, 
                                                       values_fill = 0) 

park_plot_events <- joinLocEvent(park = park, from = 2007, to = 2019)
park_plot_nums <- park_plot_events %>% group_by(cycle) %>% 
  summarize(numvisits = sum(!is.na(Plot_Name))) %>% 
  pivot_wider(names_from = cycle, values_from = numvisits) %>% 
  ungroup()

names(park_invlist_wide)
park_plot_nums$Latin_Name <- as.character("Number of plots")
park_plot_nums$Common <- as.character(NA)
park_plot_nums$Year_added <- as.numeric(NA)
park_plot_nums <- park_plot_nums[ , names(park_invlist_wide)] #using names from park_invlist2 to order columns the same
park_invtbl <- rbind(park_invlist_wide, park_plot_nums)
exospp_list <- c("Cirsium arvense", "Prunus avium", "Duchesnea indica", "Glechoma hederacea", "Persicaria longiseta", "Vinca minor")
park_invtbl <- park_invtbl %>% mutate(Latin_Name = ifelse(Latin_Name %in% exospp_list, paste0(Latin_Name, "*"),
                                                          paste0(Latin_Name)))

park_invtbl <- park_invtbl %>% select(Latin_Name, Common, Year_added, Cycle1, Cycle2, Cycle3, Cycle4)
View(park_invtbl)
path <- c("D:/NETN/Monitoring_Projects/Forest_Health/2020_data/Crew_Maps/Deliverables/")
write.csv(park_invtbl, paste0(path, park, "/", park, "_tables/", park, "_invspp_plot_freq_M15_20200910.csv"), row.names = FALSE)
