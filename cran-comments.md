## Resubmission: weasel 0.4.4

This resubmission addresses review of weasel 0.4.3:

* Removed `\dontrun{}` from the short, self-contained `weasel_example()`
  example. It now runs during the standard help-example check.
* Scope reports, the logo, headings and demonstration tables now use
  suppressible messages. `options(weasel.verbose = FALSE)` also suppresses
  explicit table display while retaining its formatted return value.
* Removed helpers that saved, assigned or removed `.Random.seed` in the
  caller's global environment. Cosmetic colours rotate using private package
  state without random draws. Synthetic random draws run in a separate R
  process from the same installation, leaving the caller's global bindings,
  complete RNG configuration and cached Box-Muller normals untouched.
* Explicit seeds retain the sampled panels from 0.4.3. With `seed = NULL`,
  the separate process draws and reports its own seed, so repeated unseeded
  calls can produce different panels. Help and the vignette document this
  change, process startup time and temporary-file cleanup.

Dependencies and all 29 exported signatures are unchanged. Selection rules,
scores, diagnostics and generated justifications are unchanged.

## Completed native verification

On 2026-10-17, the following source archive was checked with native R 4.6.1
(2026-06-24), aarch64-apple-darwin23, on macOS Tahoe 26.5.1:

* Archive: `weasel_0.4.4.tar.gz` (145,765 bytes).
* SHA-256: `308f3f0fa1b87bc221dbf703cf59ddc4e9e0287924ebc5f6c75656b7d1ff9381`.
* Command: `R CMD check --as-cran --run-donttest weasel_0.4.4.tar.gz`.
* Result: 0 errors, 0 warnings, 1 NOTE: `New submission`.
* Examples, tests, both vignettes and their rebuilds, PDF and HTML manuals,
  CRAN incoming feasibility, leftover-file and temporary-file checks pass.
  The complete result is recorded in `weasel.Rcheck/00check.log`.

The separate native development suite passed 3,878 assertions. That count
comes from its development test report, not the final CRAN-mode check log.
Ten explicitly seeded panels were checked against the public 0.4.3 source.
Guide snippet and semantics checks and the repository inclusion policy pass.
The working package source was compared with the checked archive; it matches
apart from standard build metadata and generated vignette files.

Earlier Linux validation used Ubuntu 24.04.3, x86_64 and R 4.3.3 with an
offline, modified check setup. Its HTML-tool and leftover-directory NOTEs
are historical limitations, not results of the completed native check above.
The native check includes the repaired TeX fonts and Homebrew HTML Tidy.

## Completed cross-platform CI verification

On 2026-10-18, all five jobs in
[R-CMD-check run 39108642407](https://github.com/siardv/weasel/actions/runs/39108642407)
completed with `Status: OK` (0 errors, 0 warnings, 0 NOTEs):

| Platform | Actual R | Log |
| --- | --- | --- |
| Windows, x86_64-w64-mingw32 | 4.6.1 (2026-06-24 ucrt) | [Windows release](https://github.com/siardv/weasel/actions/runs/39108642407/job/117619820173) |
| Ubuntu 24.04.5 LTS, x86_64-pc-linux-gnu | Under development (2026-10-16 r90685) | [R-devel](https://github.com/siardv/weasel/actions/runs/39108642407/job/117619820271) |
| Ubuntu 24.04.5 LTS, x86_64-pc-linux-gnu | 4.6.1 (2026-06-24) | [Linux release](https://github.com/siardv/weasel/actions/runs/39108642407/job/117619820278) |
| Ubuntu 24.04.5 LTS, x86_64-pc-linux-gnu | 4.5.3 (2026-03-11) | [Linux oldrel](https://github.com/siardv/weasel/actions/runs/39108642407/job/117619820214) |
| macOS Tahoe 26.6.2, aarch64-apple-darwin23 | 4.6.1 (2026-06-24) | [macOS release](https://github.com/siardv/weasel/actions/runs/39108642407/job/117619820377) |

The tested correction is commit
`af6599c25fe63b8ee3ade996887f2383ae8f5c40`. CI checked out PR merge
`c7c8bccd6e77778e16dd4adc94038f9eefd35578`; its complete tree
`acbbefa928f7ddbdca6cf974571f2774de30a023` is identical to that correction.
Every job rebuilt `weasel_0.4.4.tar.gz` from this source. Successful jobs
retained snapshot artifacts, not the rebuilt source archives, so their
archive checksums are unavailable. These are source-matched platform checks,
not checks of byte-identical copies of the native archive above.

CI used `--no-manual --as-cran`, with `NOT_CRAN=true` and incoming checks
disabled. Its results complement the native manual/incoming check rather
than replace it. Test summaries report 0 failures, 0 warnings and 0 skips;
Windows reports 3,872 assertions and the other platforms report 3,878.
These observed counts are recorded separately from the native reports.
Inclusion policy, all nine guide snippets, guide semantics, source version
coherence and test coverage checks also pass. Measured coverage is 94.25%.

## Separate win-builder uploads

The native archive above was also uploaded to win-builder for R-release and
R-devel on 2026-10-17. Both HTTPS responses acknowledged its filename and
size, but the emailed result links have not been obtained. Those uploads
remain unverified separately from the completed CI checks; acknowledgements
are not passes. Historical 0.4.3 checks are not used as 0.4.4 evidence.

## Source provenance

This revision uses public 0.4.3 commit
`3b2abae0b8f8ee06980ba4d758d706a43dc67734`. Its package version and
DESCRIPTION match the reviewed version, but the original submitted tarball
was not available in this revision's validation inputs for byte comparison.
No submitted-tarball equivalence or CRAN acceptance is claimed.
