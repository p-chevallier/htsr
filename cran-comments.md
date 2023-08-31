---
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
---
## Test environments
* local: Manjaro Linux "Uranos 23.0.0", R "4.3.1", Kernel "6.5.0-1"
* win: tested with win-builder.r-project.org using the r-devel version

## R CMD check results
* no ERRORs
* no WARNINGs
* 2 NOTEs:
❯ checking package dependencies ... NOTE
  Imports includes 21 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wformat’ ‘-Wp,-D_FORTIFY_SOURCE=2’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’ ‘-march=x86-64’
    
Both notes cannot be fixed in the context of the package. I regret.

### Comment
* no comment

### bugs fixed
* see `NEWS.md`

### changes
* see `NEWS.md`

### addings
* see `NEWS.md`
