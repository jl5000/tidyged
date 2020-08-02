# Stop the NOTES from R CMD CHECK when creating columns with mutate()
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("level", "record", "tag", "value", ".", "num_children", "xref",
                           "mother_xref", "father_xref", "num_siblings", "full"))
