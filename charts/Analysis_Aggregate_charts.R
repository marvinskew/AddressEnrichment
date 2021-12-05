

#Analysis
selected= c("WELRS_OBX_ID","PATIENT_ADDRESS_COUNTY","PAT_ADDR_COUNTY_ENHANCED", "countyName",
            "ACCOUNTABLE_COUNTY" ,"Code", "ENRICH_ADD_COUNTY", "ORIG_ACCOUNTABLE_COUNTY")
df= enriched_neglab_fips[,colnames(enriched_neglab_fips) %in% selected]

orig_ac_miss_complete= enriched_neglab_fips%>%
                              dplyr::mutate(missingness_completeness= ifelse(ORIG_ACCOUNTABLE_COUNTY=="","missing","complete"))%>%
                              dplyr::group_by(missingness_completeness)%>%
                              dplyr::summarise(total_acctable_county= n(), 
                                               pct_acctable_county=paste0(round(n()*100/nrow(enriched_neglab_fips),2), "%"))%>%
                              dplyr::mutate(county="ACCOUNTABLE COUNTY(ORIGNAL)")

enhanced_ac_miss_complete= enriched_neglab_fips%>%
                              dplyr::mutate(missingness_completeness= ifelse(ACCOUNTABLE_COUNTY=="","missing","complete"))%>%
                              dplyr::group_by(missingness_completeness)%>%
                              dplyr::summarise(total_acctable_county= n(), 
                                               pct_acctable_county=paste0(round(n()*100/nrow(enriched_neglab_fips),2), "%"))%>%
                              dplyr::mutate(county="ACCOUNTABLE COUNTY(ENHANCED)")

ac_orig_enhanced_df= bind_rows(orig_ac_miss_complete, enhanced_ac_miss_complete)

plot_ac<-ac_orig_enhanced_df%>%
              dplyr::mutate(total_acctable_county= total_acctable_county/1000)%>%
  ggplot(aes(fill= missingness_completeness, y= total_acctable_county, x=county)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label= pct_acctable_county), position= position_dodge(0.9), vjust= -0.5, size= 3.0) +
  xlab(label= "Accountable Counties") + ylab("Counts('000)") +
  ggtitle("Accountable Counties - Pre & Post Enrichment") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face= "bold"),
        axis.text.x= element_text(angle=0),
        axis.title.x=  element_text(size= 8, face= "bold.italic"),
        axis.title.y=  element_text(size= 8, face= "bold.italic")) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank())



