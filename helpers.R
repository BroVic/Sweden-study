# Various objects and functions built for this project

################################################################################
# Functions for the analysis
#  2 very similar functions for multiple Yes/No responses
yesno1 <- function(i) {
  yn <- c("Yes" = 1, "No" = 2, "Unknown" = 3)
  rawdata[, i] <- factor(rawdata[, i], levels = yn, labels = names(yn))
  rawdata[, i]
}

yesno2 <- function(i) {
  yn <- c("Yes" = 1, "No" = 2, "No answer" = 3)
  rawdata[, i] <- factor(rawdata[, i], levels = yn, labels = names(yn))
  rawdata[, i]
}

# A function for multiple major/minor responses
majorminor <- function(i) {
  mm <- c("No" = 3, "Minor" = 2, "Major" = 1, "Unknown" = 4)
  rawdata[, i] <- factor(rawdata[, i], levels = mm, labels = names(mm),
                         ordered = TRUE)
  rawdata[, i]
}

# A function for multiple responses raising "concerns"
concerns <- function(i) {
  con <- c("No concern" = 3, "Some concern" = 2,
           "Major concern" = 1, "Unknown" = 4)
  rawdata[, i] <- factor(rawdata[, i],
                         levels = con, ordered = TRUE,
                         labels = names(con))
  rawdata[, i]
}

# A function for the responses with percentage groups
props <- function(i) {
  pp <- c("None at all" = 1, "< 20%" = 2, "20% - 40%" = 3,
          "40% - 60%" = 4, "60% - 80%" = 5, "80% - 100%" = 6,
          "All" = 7, "Unknown" = 8)
  rawdata[, i] <- factor(rawdata[, i], levels = pp, 
                         ordered = TRUE, labels = names(pp))
  rawdata[, i]
}

# A function for response on quest for interviews
interview <- function(i) {
  inv <- c("Refused" = 3, "Don't know but OK to interview" = 2,
           "Information given" = 1, "Respondent is this person" = 4)
  rawdata[, i] <- factor(rawdata[, i], levels = inv, labels = names(inv),
                         ordered = TRUE)
  rawdata[, i]
}

##############################################################################
# Objects created for the analysis
# a. Variable names
var.names <- c("id", "type.org", "type.site", "public.sector", "established",
               "use.hss.occhealthdoc", "use.hss.safetyexp", "use.hss.psych",
               "use.hss.ergo", "use.hss.consult", "analys.sick.absence",
               "support.return", "monitor.health", "documented.policy",
               "policy.impact", "no.policy.benefit", "no.policy.time",
               "no.policy.expertise", "no.policy.risks", "no.policy.finance",
               "hs.mgt.issue", "mgt.involvment", "check.hs", "checks.by",
               "checked.at.change", "checked.at.request", "checked.regul.int",
               "checked.equipment", "checked.workorganised",
               "checked.workinghours", "checked.relationships",
               "followup.equipment", "followup.workorganised",
               "followup.workinghours", "followup.training", "nocheck.expertise",
               "nocheck.timeconsuming", "nocheck.complex", "nocheck.unnecessary",
               "inspectorate.check", "importance.legal", "importance.request",
               "importance.staff", "importance.economic",
               "importance.requirement", "importance.pressure",
               "difficulties.lackresources", "difficulties.lackawareness",
               "difficulties.lackexpertise", "difficulties.lacktechnical",
               "difficulties.culture", "difficulties.sensitivity",
               "information.institutes", "information.europe",
               "information.inhouse", "information.inspectorate",
               "information.employers", "information.tradeunions",
               "information.contract", "information.insurance", "europe.week",
               "concern.dangerous.subst", "concern.accidents", "concern.noise",
               "concern.musculoskeletal", "concern.stress", "concern.violence",
               "concern.bullying", "risks.time", "risks.communication",
               "risks.poor.cooperation", "risks.lackemployeecontrol",
               "risks.jobinsecurity", "risks.difficultpeople",
               "risks.relationships", "risks.workinghours", "risks.unclearPolicy",
               "risks.discrimination", "procedure.stress", "procedure.bullying",
               "procedure.violence", "measures.workorganised",
               "measures.workarea", "measures.counselling",
               "measures.conflictResolution", "measures.workinghours",
               "measures.training", "action.longHours", "informEmployees",
               "informed.whoAddress", "reasons.legalObligation",
               "reasons.request", "reasons.absenteeism", "reasons.declineOutput",
               "reasons.requirement", "reasons.labourInsp",
               "management.psychosocialRisk", "role.employees",
               "employees.participation", "compare.psychosocialRisk",
               "psych.lackResources", "psych.lackAwareness", "psych.lackTraining",
               "psych.lackTechnical", "psych.culture", "psych.sensitivity",
               "externalSupport", "additionalSupport", "info.helpful",
               "helpful.violence", "helpful.psychosocialRisk",
               "helpful.preventiveMeasures", "worksCouncil", "tradeUnion",
               "rep.importance", "controversies", "internal.hsRep",
               "member.hsRep", "hsCommittee", "femaleEmployees",
               "est.femaleEmployees", "over50", "est.over50", "rate.absenteeism",
               "economicSituation", "nationality", "est.nationality",
               "sum.nationality.est", "name.employeeRep", "spokesperson",
               "name.spokesperson", "name.hsRep.hsCommittee",
               "name.memberWorksCouncil", "name.hsRep", "nonDisclosure",
               "laterContact", "permanent.committee", "meetingFrequency",
               "controversies.often", "timeOff.employeeRep", "difficulties.time",
               "difficulties.getEmp", "difficulties.cooperationMgt",
               "provide.info", "timely.info", "informed.sickness",
               "informed.accidents", "informed.workorganised",
               "informed.equipment", "training.fireSafety", "training.accidents",
               "training.chemicalEtc", "training.ergo", "training.violence",
               "training.stress", "training.discrimination",
               "training.sufficient", "addTraining.fireSafety",
               "addTraining.accidents", "addTraining.chemicalEtc",
               "addTraining.ergo", "addTraining.violence", "addTraining.stress",
               "addTraining.discrimination", "needTraining.fireSafety",
               "needTraining.accidents", "needTraining.chemicalEtc",
               "needTraining.ergo", "needTraining.violence",
               "needTraining.stress", "needTraining.discrimination",
               "lowTraining.timeOff", "lowTraining.lackInfo",
               "lowTraining.lackCourses", "lowTraining.finance",
               "er.documented.policy", "er.impact", "er.reasons.benefit",
               "er.reasons.expertise", "er.reasons.unnecessary",
               "employees.informed", "workplaces.checked",
               "decision.riskAssessment", "action.taken", "involved.actions",
               "est.expertiseLacking", "est.timeConsuming", "est.legalObligation",
               "est.unnecessary", "involveManagers", "hs.managementPhilosophy",
               "hs.managementOpen", "hs.managementConsider", "er.concern.subst",
               "er.concern.accidents", "er.concern.noise",
               "er.concern.musculoskeletal", "er.concern.stress",
               "er.concern.violence", "er.concern.bullying", "er.psych.time",
               "er.psych.communication", "er.psych.cooperation",
               "er.psych.lackemployeecontrol", "er.psych.jobinsecurity",
               "er.psych.diffCustomers", "er.psych.relationships",
               "er.psych.workinghours", "er.psych.unclearPolicy",
               "er.psych.discrimination", "er.psychosocialRisk.workorganised",
               "er.psychosocialRisk.workarea", "er.psychosocialRisk.counselling",
               "er.psychosocialRisk.conflictResolution",
               "er.psychosocialRisk.workinghours",
               "er.psychosocialRisk.training", "er.measures.workorganised",
               "er.measures.workarea", "er.measures.counselling",
               "er.measures.conflictResolution", "er.measures.workinghours",
               "er.measures.training", "er.informEmployees", "er.workStress",
               "er.workBullying", "er.workViolence",
               "er.compare.psychosocialRisk", "er.mgtTackle",
               "er.measures.sufficient", "size_5", "sector", "nace", "country",
               "size_10", "hsr_exists", "hsr_int", "sum.femaleEmployees",
               "sum.over50", "est_wei1", "est_wei2", "emp_wei1", "emp_wei2",
               "country_2")

###################################################################################
# Functions used for plotting charts
# 1. This is a function built to generate 5 - 6 bar charts in a 2 by 3 display
multiplot <- function(x, y) {
  col <- c("green", "yellow", "red")
  layout(matrix(c(1:6), nrow = 2, byrow = TRUE))
  for (i in y) {
    ht <- table(x, mydata[, i])
    par(mar = c(5, 3, 2, 1))
    barplot(ht,
            beside = TRUE,
            legend = FALSE,
            ylim = c(0, max(ht)),
            yaxt = "s",
            col = col,
            xlab = colnames(mydata[i]))
  }
  plot(1, type = "n", axes = FALSE, ylab = "", xlab = "")
  legend("top", inset = 0, legend = levels(x),
         horiz = FALSE, fill = col, col = col,
         title = paste("Concern about", attr(x, which = "name")))
  layout(matrix(1))
}

###################################################################################
# Other functions
# A function doing Chi-squared Test of Independence and printing out result
printchisq <- function(x, vec) {
  for (i in vec) {
    result <- chisq.test(x, mydata[, i])
    print(result)
  }
}