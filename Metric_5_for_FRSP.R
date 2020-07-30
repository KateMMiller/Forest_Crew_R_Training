#-----M5-----
FRSP_regen_tbl <- joinRegenData(park = "FRSP", speciesType = "native", canopyForm = "canopy", 
                                units = "sq.m", from = 2007, to = 2019)

FRSP_regen_tbl2 <- FRSP_regen_tbl %>% group_by(Plot_Name, cycle) %>% 
  summarise(sd15_30 = sum(seed15.30, na.rm = TRUE), 
            sd30_100 = sum(seed30.100, na.rm = TRUE), 
            sd100_150 = sum(seed100.150, na.rm = TRUE), 
            sd150p = sum(seed150p, na.rm = TRUE),
            seed = sd15_30 + sd30_100 + sd100_150 + sd150p,
            sap = sum(sap.den, na.rm = TRUE),
            stock = sum(stock, na.rm = TRUE)) %>% # note the addition of stock here
  select(Plot_Name, cycle, seed, sap, stock)


FRSP_regen_tbl3 <- FRSP_regen_tbl2 %>% pivot_wider(names_from = "cycle", 
                                                   values_from = c("seed", "sap", "stock"),
                                                   values_fill = NA)

FRSP_plots <- joinLocEvent(park = "FRSP", from = 2016, to = 2019, output='verbose') %>% 
              mutate(subunit = substr(Unit_ID, 6, nchar(Unit_ID))) %>% 
              select(Plot_Name, subunit) 
              

FRSP_regen_tbl4 <- merge(FRSP_plots, FRSP_regen_tbl3, by = "Plot_Name", all.x = TRUE, all.y = TRUE) %>% arrange(subunit, Plot_Name)

write.csv(PARK_regen_tbl4, './tables/Seedlings_saplings_stocking_table_M5.csv', row.names = FALSE)
#row.names=FALSE removes the first unnamed column that's numbered.