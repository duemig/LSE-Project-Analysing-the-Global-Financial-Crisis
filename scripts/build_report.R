#!/usr/bin/env Rscript
# Render the report from results/ and figures/ to PDF. A Word file is produced in
# a temporary folder as an intermediate (not kept in the repository). The table of
# contents is a Word field, so the PDF is produced through LibreOffice with a
# one-off macro that updates the field before export (a plain --convert-to leaves
# the contents page blank). The macro runs in a throwaway profile, so the user's
# LibreOffice configuration is untouched. The macro writes a marker reporting the
# number of indexes it updated, and the build retries until the contents page is
# filled, so a blank one is never shipped silently.
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
final_pdf <- file.path(repo, "LSE Project Report.pdf")

work <- file.path(tempdir(), "report_build")
dir.create(work, showWarnings = FALSE)
tmp_docx <- file.path(work, "report.docx")
tmp_pdf <- file.path(work, "report.pdf")
marker <- file.path(work, "toc_marker.txt")

rmarkdown::render(file.path(repo, "report", "report.Rmd"),
                  output_file = tmp_docx, quiet = TRUE)

write_toc_macro <- function(profile, marker_path) {
  scripts <- file.path(profile, "user", "Scripts", "python")
  dir.create(scripts, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "from com.sun.star.beans import PropertyValue",
    "def run(*args):",
    "    doc = XSCRIPTCONTEXT.getDocument()",
    "    n = 0",
    "    try:",
    "        idx = doc.getDocumentIndexes()",
    "        n = idx.getCount()",
    "        for i in range(n):",
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
    paste0("    try:\n        m = open('", marker_path,
           "', 'w'); m.write('index_count=' + str(n)); m.close()\n    except Exception:\n        pass"),
    "    doc.close(False)",
    "    XSCRIPTCONTEXT.getDesktop().terminate()",
    "    return None"
  ), file.path(scripts, "toc.py"))
}

soffice <- Sys.which("soffice")
# LibreOffice ships its own Python for macros; a conda/pixi environment breaks
# it, so launch soffice with a pristine environment (only HOME, PATH, LANG).
# Every argument is shell-quoted so the macro URL's "$run" is not expanded, and
# the command is run through the shell because soffice's homebrew wrapper does
# not run the macro when invoked directly.
home <- Sys.getenv("HOME")
path_dirs <- paste(unique(c("/usr/bin", "/bin", "/usr/sbin", "/sbin",
                            if (nzchar(soffice)) dirname(soffice),
                            "/opt/homebrew/bin",
                            "/Applications/LibreOffice.app/Contents/MacOS")),
                   collapse = ":")
soffice_cmd <- function(tail_args) {
  paste("/usr/bin/env -i", paste0("HOME=", shQuote(home)),
        paste0("PATH=", path_dirs), "LANG=en_US.UTF-8", "soffice", tail_args)
}
await <- function(seconds) {
  for (w in seq_len(seconds)) {
    if (file.exists(marker) && file.exists(tmp_pdf)) {
      return(invisible())
    }
    Sys.sleep(1)
  }
}

filled <- FALSE
if (nzchar(soffice)) {
  for (attempt in 1:3) {
    profile <- file.path(work, paste0("lo_profile_", attempt))
    unlink(profile, recursive = TRUE)
    if (file.exists(tmp_pdf)) file.remove(tmp_pdf)
    if (file.exists(marker)) file.remove(marker)
    write_toc_macro(profile, marker)
    cmd <- soffice_cmd(paste(
      "--headless --norestore --invisible",
      shQuote(paste0("-env:UserInstallation=file://", profile)),
      shQuote("vnd.sun.star.script:toc.py$run?language=Python&location=user"),
      shQuote(paste0("file://", tmp_docx))
    ))
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    await(30)
    if (file.exists(marker) && file.exists(tmp_pdf)) {
      report <- paste(readLines(marker, warn = FALSE), collapse = "")
      if (grepl("index_count=[1-9]", report)) {
        filled <- TRUE
        break
      }
    }
    Sys.sleep(2)
  }
  if (!filled) {
    if (!file.exists(tmp_pdf)) {
      system(soffice_cmd(paste("--headless --convert-to pdf --outdir",
                               shQuote(work), shQuote(tmp_docx))),
             ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    message("WARNING: the table of contents could not be updated automatically ",
            "(LibreOffice may be busy). Re-run this script, or open the Word ",
            "intermediate and update the field.")
  }
}

if (file.exists(tmp_pdf)) {
  file.copy(tmp_pdf, final_pdf, overwrite = TRUE)
  message("Wrote ", final_pdf, if (filled) " (contents filled)." else ".")
} else {
  message("PDF was not produced (LibreOffice unavailable or failed).")
}
message("Report build complete.")
