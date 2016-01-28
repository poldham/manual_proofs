pcy <- select(pizza, publication_country_name, publication_number, publication_year)
pcy_total <- mutate(pcy, n = sum(publication_number = 1))
pcy_total <- count(pcy_total, publication_country_name, publication_year, wt = n)
pcy_spread <- spread(pcy_total, publication_country_name, n)
write_csv(pcy_spread, "pcy_spread.csv")

#applicant_summary
applicant_count <- select(df3, applicants, publication_number) %>%
  mutate(n = sum(publication_number = 1)) %>%
  count(applicants, wt = n) %>%
  arrange(desc(n)) %>%
  filter(n >= 64)
write_csv(applicant_count, "applicant_top10.csv")

#word cloud 

phrase_count <- select(pizza, title_nlp_multiword_phrases, publication_number) %>%
  separate(title_nlp_multiword_phrases, 1:30, sep = ";", fill = "right") %>%
  mutate(n = sum(publication_number = 1)) %>%
  select(1:30, 32) %>%
  gather(x, phrases, 1:30, na.rm = TRUE) %>%
  select(phrases, n) %>%
  count(phrases, wt = n) %>%
  arrange(desc(n)) %>%
  filter(n >= 15)
write_csv(phrase_count, "title_phrases.csv") 
  
  