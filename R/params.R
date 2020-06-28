

#' Title
#'
#' @description 
#' 
#' @details
#'
#' @param address_city The name of the city/town used in the address. Isolated for sorting or indexing.
#' @param address_country The name of the country that pertains to the associated address. Isolated by some systems for 
#' sorting or indexing. Used in most cases to facilitate automatic sorting of mail.
#' @param address_email An electronic address that can be used for contact such as an email address.
#' @param address_fax A FAX telephone number appropriate for sending data facsimiles.
#' @param address_postal_code The ZIP or postal code used by the various localities in handling of mail. 
#' Isolated for sorting or indexing.
#' @param address_state The name of the US state/UK county used in the address. Isolated for sorting or indexing.
#' @param address_web_page The world wide web page address.
#' @param adopted_by_which_parent A code which shows which parent in the associated family record adopted this person.
#' Use "HUSB" for husband, "WIFE" for wife, or "BOTH" for both.
#' @param age_at_event,husband_age_at_event,wife_age_at_event A string that indicates the age in years, months, and days that the 
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
#' @param caste_name A name assigned to a particular group that this person was associated with, 
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
#' @param copyright_gedcom_file A copyright statement needed to protect the copyrights of the 
#' submitter of this GEDCOM file.
#' @param copyright_source_data A copyright statement required by the owner of data from 
#' which this information was downloaded. For example, when a GEDCOM down-load is requested 
#' from the Ancestral File, this would be the copyright statement to indicate that the data 
#' came from a copyrighted source.
#' @param count_of_children The known number of children of this individual from all marriages or, 
#' if subordinate to a family record, the reported number of children known to belong to this 
#' family, regardless of whether the associated children are represented in the corresponding 
#' structure. This is not necessarily the count of children listed in a family structure.
#' @param count_of_marriages The number of different families that this person was known to have 
#' been a member of as a spouse or parent, regardless of whether the associated families are 
#' represented in the GEDCOM file.
#' @param descriptive_title The title of a work, record, item, or object.
#' @param entry_recording_date A date_value() object giving the date that this event data 
#' was entered into the original source document. 
#' @param event_attribute_type A code that classifies the principal event or happening that caused the 
#' source record entry to be created. If the event or attribute doesn't translate to one of these tag codes, 
#' then a user supplied value is expected and will be generally classified in the category of other. 
#' @param event_descriptor Text describing a particular event pertaining to the individual or family. 
#' This event value is usually assigned to the EVEN tag. The classification as to the difference 
#' between this specific event and other occurrences of the EVENt tag is indicated by the use of 
#' a subordinate TYPE tag selected from the EVENT_DETAIL structure.
#' @param event_or_fact_classification A descriptive word or phrase used to further classify the parent 
#' event or attribute tag. This should be used whenever either of the generic EVEN or FACT tags are used. 
#' The value of this primative is responsible for classifying the generic event or fact being cited.  
#' @param event_type_cited_from A code that indicates the type of event which was responsible for the 
#' source entry being recorded. For example, if the entry was created to record a birth of a child, 
#' then the type would be BIRT regardless of the assertions made from that record, such as the mother's 
#' name or mother's birth date. This will allow a prioritized best view choice and a determination of 
#' the certainty associated with the source used in asserting the cited fact. 
#' @param event_type_family A code used to indicate the type of family event. 
#' @param event_type_individual A code used to indicate the type of individual event. 
#' @param events_recorded An enumeration of the different kinds of events that were recorded in a 
#' particular source. Each enumeration is separated by a comma. Such as a parish register of births, 
#' deaths, and marriages would be BIRT, DEAT, MARR. These can be enumerated over more than one
#' vector element. 
#' @param file_name The name of the GEDCOM transmission file. If the file name includes a file 
#' extension it must be shown in the form (filename.ext).
#' @param gedcom_content_description A note that a user enters to describe the contents of the 
#' lineage-linked file in terms of "ancestors or descendants of" so that the person receiving 
#' the data knows what genealogical information the transmission contains. 
#' @param gedcom_form The GEDCOM form used to construct this transmission. There maybe other 
#' forms used such as CommSoft's "EVENT_LINEAGE_LINKED" but these specifications define only 
#' the LINEAGE-LINKED Form. Systems will use this value to specify GEDCOM compatible with these specifications. 
#' @param generations_of_ancestors The number of generations of ancestors included in this transmission. 
#' This value is usually provided when FamilySearch programs build a GEDCOM file for a patron requesting 
#' a download of ancestors.
#' @param generations_of_descendants The number of generations of descendants included in this transmission. 
#' This value is usually provided when FamilySearch programs build a GEDCOM file for a patron requesting 
#' a download of descendants. 
#' @param language_of_text The human language in which the data in the transmission is normally read or 
#' written. It is used primarily by programs to select language-specific sorting sequences and phonetic 
#' name matching algorithms.
#' @param language_preference The language in which a person prefers to communicate. Multiple language 
#' preference is shown by using multiple occurrences in order of priority.
#' @param multimedia_file_reference A complete local or remote file reference to the auxiliary data 
#' to be linked to the GEDCOM context. Remote reference would include a network address where the multimedia 
#' data may be obtained. 
#' @param multimedia_format Indicates the format of the multimedia data associated with the specific 
#' GEDCOM context. This allows processors to determine whether they can process the data object. 
#' Any linked files should contain the data required, in the indicated format, to process the file data. 
#' @param name_of_business Name of the business, corporation, or person that produced or commissioned the product.
#' @param name_of_family_file Name under which family names for ordinances are stored in the temple's family file.
#' @param name_of_product The name of the software product that produced this transmission.
#' @param name_of_repository The official name of the archive in which the stated source material is stored.
#' @param name_of_source_data The name of the electronic data source that was used to obtain the data in 
#' this transmission. For example, the data may have been obtained from a CD-ROM disc that was named 
#' "U.S. 1880 CENSUS CD-ROM vol. 13."
#' @param name_personal The surname of an individual, if known, is enclosed between two slash (/) characters. 
#' The order of the name parts should be the order that the person would, by custom of their culture, 
#' have used when giving it to a recorder.
#' @param name_phonetic_variation A character vector of phonetic variations of the name.
#' @param name_piece_given Given name or earned name. Different given names are separated by a comma.
#' @param name_piece_nickname A descriptive or familiar name used in connection with one's proper name.
#' @param name_piece_prefix Non indexing name piece that appears preceding the given name and surname parts. 
#' Different name prefix parts are separated by a comma.
#' @param name_piece_suffix Non-indexing name piece that appears after the given name and surname parts. 
#' Different name suffix parts are separated by a comma.
#' @param name_piece_surname Surname or family name. Different surnames are separated by a comma.
#' @param name_piece_surname_prefix Surname prefix or article used in a family name. Different surname 
#' articles are separated by a comma, for example in the name "de la Cruz", this value would be "de, la".
#' @param name_romanized_variation A character vector giving romanized variations of the name. 
#' @param name_type Indicates the name type, for example the name issued or assumed as an immigrant.
#' @param national_id_number A nationally-controlled number assigned to an individual. Commonly 
#' known national numbers should be assigned their own tag, such as SSN for U.S. Social Security Number. 
#' The use of the IDNO tag requires a subordinate TYPE tag to identify what kind of number is being stored.
#' @param national_or_tribal_origin The person's division of national origin or other folk, house, kindred, 
#' lineage, or tribal interest.
#' @param nobility_type_title The title given to or used by a person, especially of royalty or other noble 
#' class within a locality.
#' @param occupation The kind of activity that an individual does for a job, profession, or principal activity.
#' @param ordinance_process_flag A flag that indicates whether submission should be processed for clearing 
#' temple ordinances. 
#' @param pedigree_linkage_type A code used to indicate the child to family relationship for pedigree navigation 
#' purposes.
#' @param permanent_record_file_number The record number that uniquely identifies this record within a 
#' registered network resource.
#' @param phone_number A phone number.
#' @param phonetic_type Indicates the method used in transforming the text to the phonetic variation.
#' @param physical_description An unstructured list of the attributes that describe the physical 
#' characteristics of a person, place, or object. Commas separate each attribute.
#' @param place_hierarchy This shows the jurisdictional entities that are named in a sequence from the 
#' lowest to the highest jurisdiction. The jurisdictions are separated by commas, and any jurisdiction's 
#' name that is missing is still accounted for by a comma. 
#' @param place_latitude The value specifying the latitudinal coordinate of the place name. 
#' The latitude coordinate is the direction North or South from the equator in degrees and fraction of 
#' degrees carried out to give the desired accuracy. For example: 18 degrees, 9 minutes, and 3.4 seconds North 
#' would be formatted as N18.150944.
#' @param place_longitude The value specifying the longitudinal coordinate of the place name. 
#' The longitude coordinate is Degrees and fraction of degrees east or west of the zero or base 
#' meridian coordinate. For example: 168 degrees, 9 minutes, and 3.4 seconds East would be formatted as E168.150944. 
#' @param place_name The jurisdictional name of the place where the event took place. 
#' Jurisdictions are separated by commas.
#' @param place_phonetic_variation A character vector of phonetic variations of the place name.
#' @param place_romanized_variation A character vector of romanized variations of the place name. 
#' @param possessions A list of possessions (real estate or other property) belonging to this individual.
#' @param publication_date_source_data A date_exact() object giving the date this source was published or created.
#' @param receiving_system_name The name of the system expected to process the GEDCOM-compatible transmission.
#' @param relation_is_descriptor A word or phrase that states object 1's relation is object 2. 
#' @param religious_affiliation A name of the religion with which this person, event, or record was affiliated.
#' @param responsible_agency The organization, institution, corporation, person, or other entity that has 
#' responsibility for the associated context. For example, an employer of a person of an associated occupation, 
#' or a church that administered rites or events, or an organization responsible for creating and/or archiving 
#' records.
#' @param restriction_notice The restriction notice is defined for Ancestral File usage. Ancestral File 
#' download GEDCOM files may contain this data.
#' @param role_descriptor A word or phrase that identifies a person's role in an event being described. 
#' This should be the same word or phrase, and in the same language, that the recorder used to define 
#' the role in the actual record. 
#' @param role_in_event Indicates what role this person played in the event that is being cited in this context. 
#' @param romanized_type Indicates the method used in transforming the text to a romanized variation.
#' @param scholastic_achievement A description of a scholastic or educational achievement or pursuit.
#' @param sex_value A code that indicates the sex of the individual. 
#' @param social_security_number A number assigned to a person in the United States for identification purposes. 
#' @param source_call_number An identification or reference description used to file and retrieve items 
#' from the holdings of a repository. 
#' @param source_description A free form text block used to describe the source from which information was 
#' obtained. This text block is used by those systems which cannot use a pointer to a source record. 
#' It must contain a descriptive title, who created the work, where and when it was created, and where 
#' the source data stored.
#' @param source_descriptive_title The title of the work, record, or item and, when appropriate, 
#' the title of the larger work or series of which it is a part.
#' @param source_filed_by_entry This entry is to provide a short title used for sorting, filing, 
#' and retrieving source records.
#' @param source_jurisdiction_place The name of the lowest jurisdiction that encompasses all lower-level places 
#' named in this source. 
#' @param source_media_type A code, selected from one of the media classifications choices above, 
#' that indicates the type of material in which the referenced source is stored.
#' @param source_originator The person, agency, or entity who created the record. For a published work, 
#' this could be the author, compiler, transcriber, abstractor, or editor. For an unpublished source, 
#' this may be an individual, a government agency, church organization, or private organization, etc. 
#' @param source_publication_facts When and where the record was created.
#' @param submitter_name The name of the submitter formatted for display and address generation. 
#' @param submitter_registered_rfn A registered number of a submitter of Ancestral File data. 
#' This number is used in subsequent submissions or inquiries by the submitter for identification purposes.
#' @param submitter_text Comments or opinions from the submitter. 
#' @param temple_code An abbreviation of the temple in which LDS temple ordinances were performed.
#' @param text_from_source A verbatim copy of any description contained within the source. 
#' This indicates notes or text that are actually contained in the source document, not the submitter's 
#' opinion about the source.  
#' @param time_value,transmission_time The time of a specific event. 
#' @param transmission_date A date_exact() object giving the date that this transmission was created. 
#' @param user_reference_number A user-defined number or text that the submitter uses to identify this record. 
#' @param user_reference_type A user-defined definition of the user_reference_number.
#' @param system_version_number An identifier that represents the version level assigned to the system. 
#' It is defined and changed by the creators of the product.
#' @param character_set_version_number An identifier that represents the version level assigned to the character set. 
#' It is defined and changed by the creators of the product. 
#' @param where_within_source Specific location with in the information referenced. 
#' @param xref_fam An xref ID of a family record.  
#' @param xref_indi An xref ID of an individual record. 
#' @param xref_sour An xref ID of a source record. 
#' @param xref_repo An xref ID of a repository record. 
#' @param xref_obje An xref ID of a multimedia record.
#' @param xref_note An xref ID of a note record. 
#' @param xref_subm An xref ID of a submitter record.
#' @param xref_subn An xref ID of a submission record.
#' @param date_changed A CHANGE_DATE() object giving the time this record was last modified. If not provided,
#' the current date is used.
#' @param notes A list of NOTE_STRUCTURE() objects.
#' @param source_citations A list of SOURCE_CITATION() objects.
#' @param source_repository_citations A list of SOURCE_REPOSITORY_CITATION() objects.
#' @param multimedia_links A list of MULTIMEDIA_LINK() objects
parameter_definitions <- function() {return(0)}

