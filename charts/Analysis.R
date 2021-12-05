
#Analysis
selected= c("WELRS_OBX_ID","PATIENT_ADDRESS_COUNTY","PAT_ADDR_COUNTY_ENHANCED", "countyName",
            "ACCOUNTABLE_COUNTY" ,"Code", "ENRICH_ADD_COUNTY", "ORIG_ACCOUNTABLE_COUNTY")
df= enriched_neglab_fips[,colnames(enriched_neglab_fips) %in% selected]



orig_patient_county= df%>%
  dplyr::group_by(PATIENT_ADDRESS_COUNTY)%>%
  dplyr::summarise(orig_pat_county_cases_num=n())%>%
  arrange(desc(orig_pat_county_cases_num))

enhanced_patient_county= df%>%
  dplyr::group_by(PAT_ADDR_COUNTY_ENHANCED)%>%
  dplyr::summarise(enhanced_pat_county_cases_num=n())%>%
  arrange(desc(enhanced_pat_county_cases_num))

orig_accountable_county= df%>%
  dplyr::mutate(label= rep("Pre-Enchancement"), nrow(df),
                counties= ORIG_ACCOUNTABLE_COUNTY)%>%
  dplyr::group_by(ORIG_ACCOUNTABLE_COUNTY)%>%
  dplyr::summarise(orig_ac_county_cases_num=n())%>%
  arrange(desc(orig_ac_county_cases_num))

enhanced_accountable_county= df%>%
  dplyr::mutate(label= rep("Post-Enchancement"), nrow(df),
                counties=ACCOUNTABLE_COUNTY)%>%
  dplyr::group_by(ACCOUNTABLE_COUNTY)%>%
  dplyr::summarise(enhanced_ac_county_cases_num=n())%>%
  dplyr::arrange(desc(enhanced_ac_county_cases_num))

pre_post_AC_enrichment= orig_accountable_county %>%
                            full_join(enhanced_accountable_county, 
                                      by=c("ORIG_ACCOUNTABLE_COUNTY" = "ACCOUNTABLE_COUNTY" ),
                                      keep = TRUE)%>%
          dplyr::mutate(diff_pre_post_AC_enhanced= enhanced_ac_county_cases_num - orig_ac_county_cases_num)%>%
          dplyr::arrange(desc(diff_pre_post_AC_enhanced))

pre_post_pat_county_enrich= orig_patient_county %>%
                            full_join(enhanced_patient_county, 
                                      by=c("PATIENT_ADDRESS_COUNTY" = "PAT_ADDR_COUNTY_ENHANCED"),
                                      keep = TRUE)%>%
          dplyr::mutate(diff_pat_county_pre_post_enhanced= enhanced_pat_county_cases_num - orig_pat_county_cases_num)%>%
          dplyr::arrange(desc(diff_pat_county_pre_post_enhanced))

orig_acctable_county= orig_accountable_county%>%
  dplyr::mutate(label= "Pre-Enchancement",
                counties= ORIG_ACCOUNTABLE_COUNTY)%>%
  dplyr::rename(test_case_counts= orig_ac_county_cases_num)
enhanced_acctable_county= enhanced_accountable_county%>%
  dplyr::mutate(label=   "Post-Enchancement",
                counties=ACCOUNTABLE_COUNTY)%>%
  dplyr::rename(test_case_counts= enhanced_ac_county_cases_num)
charts_data= bind_rows(orig_acctable_county, enhanced_acctable_county)

plott<-charts_data[,!colnames(charts_data) %in% c("ORIG_ACCOUNTABLE_COUNTY","ACCOUNTABLE_COUNTY")]%>%
     dplyr::mutate(test_case_counts= test_case_counts/1000,
                    pct= paste0(round(test_case_counts/sum(test_case_counts)*100,2), "%")) %>%
     dplyr::arrange(desc(test_case_counts))%>%
  ggplot(aes(fill=label, y= test_case_counts, x=counties)) + 
  geom_bar(position="dodge", stat="identity") +
  #geom_text(aes(label= pct), size= 2) +
  xlab(label= "Accountable Counties") + ylab("Counts('000)") +
  ggtitle("Accountable Counties - Pre & Post Enrichment @Test Case Level") +
  scale_fill_manual(values = c("#468189", "#9DBEBB")) +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face= "bold"),
        axis.text.x= element_text(angle= 90),
        axis.title.x=  element_text(size= 8, face= "bold.italic"),
        axis.title.y=  element_text(size= 8, face= "bold.italic")) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank())
pdf(file= paste0(outfile_path,"/","Acctable_Counties_Charts",".pdf"))
print(plot1)
dev.off()

png(file= paste0(outfile_path,"/","Acctable_Counties_Charts",".png"))
print(plot1)
dev.off()

write.csv(data.frame(pre_post_AC_enrichment), paste0(outfile_path,"/","ac_pre_post_enriched",".csv"))
write.csv(data.frame(pre_post_pat_county_enrich), paste0(outfile_path,"/","pat_county_pre_post_enriched",".csv"))
