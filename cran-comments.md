---
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
---
## Test environments
* local: Manjaro Linux "Wynsdey 24.0.3", R "4.4.1", Kernel "6.9.5-1"
* win: tested with win-builder.r-project.org using the r-devel version
* github: R-CMD-check]

## R CMD check results
* no ERRORs
* no WARNINGs
* 1 NOTEs:
❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wformat’ ‘-Wp,-D_FORTIFY_SOURCE=2’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’ ‘-march=x86-64’
According to discussions on https://stackoverflow.com/, it can be ignored.

### Comment
* The delay between 2 releases has been shortened due to the necessity of fixing two
critic bugs.

### bugs fixed
* see `NEWS.md`

### changes
* see `NEWS.md`

### addings
* see `NEWS.md`
