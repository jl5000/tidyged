

#' Title
#'
#' @description 
#' 
#' @details
#'
#' @param city The name of the city/town used in the address. Isolated for sorting or indexing.
#' @param country The name of the country that pertains to the associated address. Isolated by some systems for 
#' sorting or indexing. Used in most cases to facilitate automatic sorting of mail.
#' @param email An electronic address that can be used for contact such as an email address.
#' @param fax A FAX telephone number appropriate for sending data facsimiles.
#' @param all_address_lines The address lines usually contain the addressee’s street and city 
#' information so that it forms an address that meets mailing requirements. 
#' @param postal_code The ZIP or postal code used by the various localities in handling of mail. 
#' Isolated for sorting or indexing.
#' @param state The name of the US state/UK county used in the address. Isolated for sorting or indexing.
#' @param web_page The world wide web page address.
#' @param adoptive_parent A code which shows which parent in the associated family record adopted this person.
#' Use "HUSB" for husband, "WIFE" for wife, or "BOTH" for both.
#' @param age_at_event A string that indicates the age in years, months, and days that the 
#' principal was at the time of the associated event. Any labels must come after their 
#' corresponding number, for example; 4y 8m 10d.
#' @param ancestral_file_number A unique permanent record number of an individual record 
#' contained in the Family History Department's Ancestral File.
#' @param approved_system_id A system identification name which was obtained through the GEDCOM 
#' registration process. This name must be unique from any other product. Spaces within the name 
#' must be substituted with an underscore.
#' @param attribute_descriptor Text describing a particular characteristic or attribute assigned to an individual. 
#' @param attribute_type An attribute which may have caused name, addresses, phone numbers, 
#' family listings to be recorded. Its application is in helping to classify sources used for information.
#' @param automated_record_id A unique record identification number assigned to the record by 
#' the source system. This number is intended to serve as a more sure means of identification 
#' of a record for reconciling differences in data between two interfacing systems.
#' @param caste A name assigned to a particular group that this person was associated with, 
#' such as a particular racial group, religious group, or a group with an inherited status.
#' @param cause_of_event Used in special cases to record the reasons which precipitated an event. 
#' Normally this will be used subordinate to a death event to show cause of death, such as 
#' might be listed on a death certificate.
#' @param certainty_assessment A quantitative evaluation of the credibility of a piece of 
#' information, based upon its supporting evidence. Some systems use this feature to rank multiple
#' conflicting opinions for display of most likely information first. It is not intended to 
#' eliminate the receiver's need to evaluate the evidence for themselves.
#' 0 = unreliable/estimated data
#' 1 = Questionable reliability of evidence 
#' 2 = Secondary evidence, data officially recorded sometime after event
#' 3 = Direct and primary evidence used, or by dominance of the evidence
#' @param change_date The date that this data was changed.
#' @param character_set A code value that represents the character set to be used to 
#' interpret this data. Currently, the preferred character set is ANSEL, which 
#' includes ASCII as a subset. UNICODE is not widely supported by most operating systems; 
#' therefore, GEDCOM produced using the UNICODE character set will be limited in its 
#' interchangeability for a while but should eventually provide the international flexibility 
#' that is desired. Allowed values: "ANSEL", "UTF-8", "UNICODE", "ASCII"
#' @param child_linkage_status A status code that allows passing on the users opinion of the 
#' status of a child to family link.
#' "challenged": Linking this child to this family is suspect, but the linkage has been 
#' neither proven nor disproven.
#' "disproven": There has been a claim by some that this child belongs to this family, but 
#' the linkage has been disproven.
#' "proven" = There has been a claim by some that this child does not belongs to this family, 
#' but the linkage has been proven.
#' @param gedcom_copyright A copyright statement needed to protect the copyrights of the 
#' submitter of this GEDCOM file.
#' @param source_data_copyright A copyright statement required by the owner of data from 
#' which this information was downloaded. For example, when a GEDCOM down-load is requested 
#' from the Ancestral File, this would be the copyright statement to indicate that the data 
#' came from a copyrighted source.
#' @param children_count The known number of children of this individual from all marriages or, 
#' if subordinate to a family record, the reported number of children known to belong to this 
#' family, regardless of whether the associated children are represented in the corresponding 
#' structure. This is not necessarily the count of children listed in a family structure.
#' @param marriage_count The number of different families that this person was known to have 
#' been a member of as a spouse or parent, regardless of whether the associated families are 
#' represented in the GEDCOM file.
#' @param LOADS OF DATES
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
parameter_definitions <- function() {return(0)}

