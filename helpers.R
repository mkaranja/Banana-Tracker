labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

## Culture Initiation Module
new_culture_initiation_MandatoryFields <- c("new_culture_initiation_ExplantIdentity","new_culture_initiation_ExplantIdentityType","new_culture_initiation_Cultivar","new_culture_initiation_CultivarConfirmed",
                                            "new_culture_initiation_Source","new_culture_initiation_VirusIndexed","new_culture_initiation_DateOfStarterCulture","new_culture_initiation_Media","new_culture_initiation_LabBookNumber",
                                            "new_culture_initiation_PageNumber","new_culture_initiation_VirusIndexedDate")

