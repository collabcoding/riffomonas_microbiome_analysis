
install.packages("tidyverse")
library(tidyverse)

## Manipulate database 1 - subsamples

# useful to leave a line for the base version of the data so you can refer to it as you manipulate below

read_tsv("C:/Users/Brad_/OneDrive/Learning R/Recreating_Riffomonas_microbiome_analysis/minimalR-raw_data/baxter.subsample.shared")

shared <- read_tsv("C:/Users/Brad_/OneDrive/Learning R/Recreating_Riffomonas_microbiome_analysis/minimalR-raw_data/baxter.subsample.shared",
                 col_types = cols(Group = col_character(),
                                  .default = col_double())) %>%
  rename_all(tolower) %>%
  select(group, starts_with("otu")) %>%
  pivot_longer(-group, names_to="otu", values_to="count") # pivot_longer turns certain columns into rows, towards panel data



## Manipulate database 2 - taxonomy 

read_tsv("C:/Users/Brad_/OneDrive/Learning R/Recreating_Riffomonas_microbiome_analysis/minimalR-raw_data/baxter.cons.taxonomy")

taxonomy <- read_tsv("C:/Users/Brad_/OneDrive/Learning R/Recreating_Riffomonas_microbiome_analysis/minimalR-raw_data/baxter.cons.taxonomy") %>%
  rename_all(tolower) %>%
  select(otu,taxonomy) %>%
  mutate(otu = tolower(otu),
         taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";unclassified", "_unclassified"),
         taxonomy = str_replace_all(taxonomy, ";$", ""), # ";$" the dollar sign means everything up to the last ;, so removes all names but the last
         taxonomy = str_replace_all(taxonomy, ".*;", "")
         )
         
         
# \\ matches literal characters, "\\(" refers to ( and "\\d+" refers to d+
# "\\(\\d+\\)" looks for a substring within a string that is more than 1 digit in brackets
# we then remove them by replacing then with nothing 
# without this the data has lots of (123) type substrings

## Manipulate metadata 

read_tsv("C:/Users/Brad_/OneDrive/Learning R/Recreating_Riffomonas_microbiome_analysis/minimalR-raw_data/baxter.metadata.tsv" %>% tail)

metadata <- read_tsv("C:/Users/Brad_/OneDrive/Learning R/Recreating_Riffomonas_microbiome_analysis/minimalR-raw_data/baxter.metadata.tsv", 
         col_types=cols(sample = col_character())) %>%
  rename_all(tolower) %>%
  rename(group = sample) %>%
  mutate(srn = dx_bin == "Adv Adenoma" | dx_bin == "Cancer",
         lesion = dx_bin == "Adv Adenoma" | dx_bin == "Cancer" | dx_bin == "Adenoma") 

metadata

# change the sample column data to the character data type                                     
# make everything lower case
# create a category (does not alter the data) called srn and another called lesion, useful to count how many in the sample fall in these categories

## Merge the databases 

inner_join(shared, taxonomy, by="otu") %>% 
  group_by(group, taxonomy) %>%
  summarize(count = sum(count), .groups="drop") %>%
  group_by(group) %>%
  mutate(rel_abund = count / sum(count)) %>%
  ungroup() %>%
  select(-count) %>%
  inner_join(., metadata, by="group") 

# merge shared and taxonomy by 'otu'
# group the data in the table using the group column, allows you to create the rel_abund variable properly  
# create a column called rel_abund and remove the count column 
# remove the group_by info that was created to return the dataset to its previous state
# join the metadata by group




















