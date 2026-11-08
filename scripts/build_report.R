#!/usr/bin/env Rscript
# Render the report from results/ and figures/ to Word and PDF. The table of
# contents is a Word field, so the PDF is produced through LibreOffice with a
# one-off macro that updates the field before export (a plain --convert-to leaves
# the contents page blank). The macro runs in a throwaway profile, so the user's
# LibreOffice configuration is untouched. Rendering uses a space-free temporary
# path because LibreOffice's file:// URLs do not accept spaces.
#
# Run after scripts/run_study.R, from the repository root:
#   Rscript scripts/build_report.R

suppressMessages(library(rmarkdown))

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  getwd()
}

repo <- normalizePath(file.path(get_script_dir(), ".."))
final_docx <- file.path(repo, "LSE Project Report.docx")
final_pdf <- file.path(repo, "LSE Project Report.pdf")

work <- file.path(tempdir(), "report_build")
dir.create(work, showWarnings = FALSE)
tmp_docx <- file.path(work, "report.docx")
tmp_pdf <- file.path(work, "report.pdf")

rmarkdown::render(file.path(repo, "report", "report.Rmd"),
                  output_file = tmp_docx, quiet = TRUE)

write_toc_macro <- function(profile) {
  scripts <- file.path(profile, "user", "Scripts", "python")
  dir.create(scripts, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "from com.sun.star.beans import PropertyValue",
    "def run(*args):",
    "    doc = XSCRIPTCONTEXT.getDocument()",
    "    try:",
    "        idx = doc.getDocumentIndexes()",
    "        for i in range(idx.getCount()):",
    "            idx.getByIndex(i).update()",
    "    except Exception:",
    "        pass",
    "    try:",
    "        doc.getTextFields().refresh()",
    "    except Exception:",
    "        pass",
    "    url = doc.getURL()",
    "    out = url[:-5] + '.pdf' if url.lower().endswith('.docx') else url + '.pdf'",
    "    p = PropertyValue(); p.Name = 'FilterName'; p.Value = 'writer_pdf_Export'",
    "    doc.storeToURL(out, (p,))",
    "    doc.close(False)",
    "    XSCRIPTCONTEXT.getDesktop().terminate()",
    "    return None"
  ), file.path(scripts, "toc.py"))
}

soffice <- Sys.which("soffice")
if (nzchar(soffice)) {
  if (file.exists(tmp_pdf)) file.remove(tmp_pdf)
  profile <- file.path(work, "lo_profile")
  unlink(profile, recursive = TRUE)
  write_toc_macro(profile)
  system2(soffice,
          c("--headless", "--norestore", "--invisible",
            paste0("-env:UserInstallation=file://", profile),
            "vnd.sun.star.script:toc.py$run?language=Python&location=user",
            paste0("file://", tmp_docx)),
          stdout = FALSE, stderr = FALSE)
  if (!file.exists(tmp_pdf)) {
    system2(soffice, c("--headless", "--convert-to", "pdf", "--outdir", work,
                       tmp_docx), stdout = FALSE, stderr = FALSE)
  }
}

file.copy(tmp_docx, final_docx, overwrite = TRUE)
message("Wrote ", final_docx)
if (file.exists(tmp_pdf)) {
  file.copy(tmp_pdf, final_pdf, overwrite = TRUE)
  message("Wrote ", final_pdf)
} else {
  message("PDF was not produced (LibreOffice unavailable or failed).")
}
message("Report build complete.")
