library(tidyverse)

#counts total complaints by district
district_summary <- ppd_complaints %>%
  count(district_occurrence)

#counts complaints by district
district_complaint <- ppd_complaints %>%
  count(district_occurrence, general_cap_classification)

#counts the complaint type 
complaint_type <- count(ppd_complaints, c('district_occurrence','general_cap_classification'))

#pivot table
Complaint_by_district <- cast(ppd_complaints, district_occurrence ~ general_cap_classification)

#removes empty row
test <- Complaint_by_district[-c(1),] 

#removes empty column
test[2] <- NULL

--------------------------------------------------
#removes NA entries from dataset 
 clean <- na.omit(ppd_complaint_disciplines)

#counts total complaints by district
district_total <- clean %>% count(po_assigned_unit)

#classifies allegations by findings
allegations_findings <-  clean %>% count(allegations_investigated, investigative_findings)

#counts all allegations
allegations <- clean %>% count(allegations_investigated)

#counts investigative findings by race
race_allegations <- clean %>% count(po_race, investigative_findings)

#counts allegations by officer ID -- some officers are charged with multiple alligations
by_officer <- clean %>% count(officer_id, allegations_investigated)


#counts office ID by district
officer_totals <- clean %>% count(officer_id, po_assigned_unit)

#removes NA from officer_totals

officier_totals <- na.omit(officer_totals)

#pivot tables district by race
District_race <- cast(clean, po_assigned_unit ~ po_race)

#adds total column to District_race
District_race$Total_officers <- District_race$asian + District_race$black + District_race$indian + District_race$latino + District_race$other + District_race$white

#pivot tables police district by officer's gender
district_gender <- cast(clean, po_assigned_unit ~ po_sex)

#pivot tables police district by investigative finding
district_allegations <- cast(clean, po_assigned_unit ~ investigative_findings)

#adds total column to district_allegations
district_allegations$total_findings <- district_allegations$`No Sustained Findings`+ district_allegations$`Not Applicable`+ district_allegations$Pending + district_allegations$`Sustained Finding`


by_officer$Total <- by_officer %>% count(officer_id)

Organized <- clean %>% 
  select(officer_id, po_race, po_sex, po_assigned_unit, allegations_investigated, investigative_findings)

officer_allegations <- cast(clean, officer_id ~ allegations_investigated)

#joins the officer_total crime to the pivot allegations
join <- merge(x=officer_totals, y=officer_allegations, by='officer_id')

#creates gender ID by officer ID but its counted by number of offenses
officer_gender <- cast(clean, officer_id ~ po_sex)

#creates a dummy variable
officer_gender <- mutate(officer_gender, total = ifelse(female > 0, 0,1))

#joins the first join to the newly created gender variable
join1 <- merge(x=join, y=officer_gender, by='officer_id')

#creates officer race
officer_race <- cast(clean, officer_id ~ po_race)

#creates a dummy variable for race: asian =1, black =2, indian = 3, latino =4, white=5 
officer_race <- mutate(officer_race, race = ifelse(asian > 0,1, 
                                            ifelse(black > 0,2,
                                            ifelse(indian> 0,3,
                                            ifelse(latino> 0,4,
                                            ifelse(white > 0,5,0))))))

#joins the second join to the newly created race variable
join2 <- merge(x=join1, y=officer_race, by='officer_id')

officer_invest <- cast(clean, officer_id ~ investigative_findings)

#joins the third join to the newly created investigative findings

join3 <- merge(x=join2, y=officer_invest, by='officer_id')

Final_Complaint <- join3 %>% 
  select(officer_id,po_assigned_unit, n, `Civil Rights Complaint`, `Criminal Allegation`,
         `Departmental Violation`, `Disciplinary Code Violation`, `Domestic`,`Drugs`,
         `Falsification`, `Harassment`, `Investigation OnGoing`, `Lack of Service`,
         `No C.A.P. Investigation`, `Other Misconduct`, `Physical Abuse`, 
         `Referred to Other Agency/C.A.P. Investigation`, `Sexual Crime/Misconduct`,
         `Unprofessional Conduct`, `Verbal Abuse`, total, race, `No Sustained Findings`,
         `Not Applicable`, `Pending`, `Sustained Finding`)



colnames(Final_Complaint)[colnames(Final_Complaint) == 'n'] <- 'TotalComplaints'

colnames(Final_Complaint)[colnames(Final_Complaint) == 'total'] <- 'Gender'

write_csv2(Final_Complaint, '/Users/Esther/Library/Mobile Documents/com~apple~CloudDocs/Documents/Crime Research/Final_CopCompaint.csv')
