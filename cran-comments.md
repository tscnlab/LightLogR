## LightLogR 0.10.6

This update adds ActLumus Plus and LYS Button PRO support through the existing
import functions, while preserving legacy imports. It also supports older
ActTrust exports, replaces deprecated dplyr::case_match() with
dplyr::recode_values(), and restores the caller's dplyr.summarise.inform option
after pulses_above_threshold(). Minimum dependency versions are now dplyr 1.2.0
and ggtext 0.2.0. See NEWS.md for changes since CRAN version 0.10.3.

## Check results

* macOS Golden Gate 27.0, arm64, R 4.6.1: R CMD check --as-cran on the source
  archive, including examples, tests, and both PDF and HTML manuals.
  0 errors, 0 warnings, 0 notes.
* Ubuntu 24.04.5 LTS, x86_64, R 4.6.1: R CMD check --no-manual --as-cran in
  GitHub Actions. 0 errors, 0 warnings, 0 notes.
  https://github.com/tscnlab/LightLogR/actions/runs/35981139135
* Windows Server 2022 x64, R-devel (2026-09-21 r90579 ucrt): Win-builder,
  including examples, tests, and both PDF and HTML manuals.
  0 errors, 0 warnings, 0 notes.
  https://win-builder.r-project.org/F5eTQ11CgMl1/

The Linux check covered the same package code before the subsequent README
link update. The local and Windows checks include the final README.

The full test suite passed 1,088 assertions without failures, warnings, or
skips. The local --as-cran run passed 1,080 assertions and skipped four snapshot
tests under CRAN settings; those tests passed in the full suite and Linux CI.
There are no vignettes in the source archive; the website articles are excluded
by .Rbuildignore.

Older R versions have not been checked for this release.

## Reverse dependencies

CRAN metadata checked on 2026-09-24 lists no strong reverse dependencies and
one reverse suggested dependency, melidosData 1.0.6.

Its unit tests passed 22 assertions with both CRAN LightLogR 0.10.3 and the
0.10.6 candidate. Both runs produced the same two existing warnings about
text-mode file connections; there were no new failures or warnings. This was
a comparison of unit tests, not a full reverse-dependency R CMD check.

## Documentation URL

The supplemental urlchecker check reports HTTP 500 for https://www.euramet.org
in README.md. Direct HEAD and GET requests both returned HTTP 200, and the
EURAMET homepage loads successfully. The existing link has been retained.
