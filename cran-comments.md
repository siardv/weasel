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

## Windows and R-devel verification pending

The same archive was uploaded to win-builder for both R-release and R-devel
on 2026-10-17. Both HTTPS upload responses acknowledged the expected filename
and size. Check completion, actual R versions, platforms, results and log
locations have not yet been obtained; upload acknowledgements are not passes.
Record and review those results before resubmission. Historical 0.4.3 checks
do not validate this 0.4.4 archive.

## Source provenance

This revision uses public 0.4.3 commit
`3b2abae0b8f8ee06980ba4d758d706a43dc67734`. Its package version and
DESCRIPTION match the reviewed version, but the original submitted tarball
was not available in this revision's validation inputs for byte comparison.
No submitted-tarball equivalence or CRAN acceptance is claimed.
