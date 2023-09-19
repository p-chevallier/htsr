---
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
---
## Test environments
* local: Manjaro Linux "Uranos 23.0.2", R "4.3.1", Kernel "6.5.1-1"
* win: tested with win-builder.r-project.org using the r-devel version
* github: R-CMD-check]

## R CMD check results
* no ERRORs
* no WARNINGs
* 1 NOTE:
❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wformat’ ‘-Wp,-D_FORTIFY_SOURCE=2’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’ ‘-march=x86-64’
    
Reply: according to discussions on stackoverflow, it can be ignored.

### Comment
* no comment

### bugs fixed
* see `NEWS.md`

### changes
* see `NEWS.md`

### addings
* see `NEWS.md`
