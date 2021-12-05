
##############################################################################################################
# Script Name: 7_enrich_address_summary_charts.R
#
# Purpose: This scripts generates frequency table and charts of the enriched addresses.
#
# Author: Marvin Akuffo, Jon Brown
#
# Date Created :07/06/2021
#
# Email: marvin.akuffo@doh.wa.gov
##############################################################################################################

enrich_add_stat_tt= data.frame(table(all_together2$enrich_add_stat))%>%
                        dplyr::mutate(pct= paste0(round(prop.table(Freq), 4)*100, "%"),
                                      Freq= Freq/1000)%>%
                        dplyr::filter(!Var1== "")

enrich_add_tt= ggplot(enrich_add_stat_tt, aes(Var1, Freq, fill= Var1)) + geom_bar(stat= "identity") +
               geom_text(aes(label= pct), vjust= -0.5, size= 3.0) +
               xlab(label= "Enrich Address Category") + ylab("Counts('000)") +
               ggtitle("Negative Labs Data - Address Enrichment") +
               theme(plot.title= element_text(hjust= 0.5, size= 9, face= "bold"),
                     axis.text.x= element_text(angle= 45),
                     axis.title.x=  element_text(size= 8, face= "bold.italic"),
                     axis.title.y=  element_text(size= 8, face= "bold.italic")) +
               theme(legend.title= element_blank())

bad_address_nl_tt= data.frame(table(nl$WELRS_ASSIGN_ADDR_TYPE))%>%
  dplyr::mutate(pct= paste0(round(prop.table(Freq), 4)*100, "%"),
                Freq= Freq/1000)%>%
  dplyr::filter(!Var1== "")
bad_address_nl= ggplot(bad_address_nl_tt, aes(Var1, Freq, fill= Var1)) + geom_bar(stat= "identity") +
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) +
  xlab(label= "WELRS Assign Address Type") + ylab("Counts('000)") +
  ggtitle(" Count of WELRS Assign Address Type \n In Negative Covid Dataset") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face= "bold"),
        axis.text.x= element_text(angle= 45),
        axis.title.x=  element_text(size= 8, face= "bold.italic"),
        axis.title.y=  element_text(size= 8, face= "bold.italic")) +
  theme(legend.title= element_blank())
