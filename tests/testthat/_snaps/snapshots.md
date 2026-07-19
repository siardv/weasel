# plan print output is stable

    Code
      print(p)
    Output
      <weasel_plan>
        span: 1:8 (full, L = 8)
        respondents observed in span: 7
        population: 7 of 7 distinct ids observed in span (denominator: observed_in_span)
                scenario n_ids mean_prop_present endpoint_rate score recommended
         anchored_strict     2             1.000           1.0 3.520        TRUE
       anchored_balanced     3             0.958           1.0 3.488       FALSE
        lenient_info_max     5             0.875           0.8 3.310       FALSE
        data: 42 row(s) attached

# scope info output is stable

    Code
      weasel_scope_info()
    Output
      weasel scope
        data:        42 rows; id = 'id', wave = 'time'
        grid:        consecutive
        span:        1:8 (8 waves)
        constraints: min_present >= 2, max interior gap <= 1
        stage:       set -> evaluated -> reshaped (6 kept) -> summarized (5 patterns)

# comparison and subset sentences are stable

    Code
      cat(weasel_compare_to_sentence(cmp))
    Output
      Recommended scenario (highest composite score under the declared weights): "anchored_strict". "anchored_strict" keeps 2 respondent(s), coverage 1.000, endpoints 1.000. "anchored_balanced" keeps 3 respondent(s), coverage 0.958, endpoints 1.000. "lenient_info_max" keeps 5 respondent(s), coverage 0.875, endpoints 0.800.

---

    Code
      cat(weasel_subset_to_sentence(s))
    Output
      Scenario "anchored_balanced" selects 3 respondent(s) and yields 23 row(s) in long format for waves 1 to 8 (L = 8). Mean observed waves: 7.667 (missing: 0.333). Endpoint rate: 1.000. Missingness: 2 respondent(s) have 0 missing wave(s); 1 respondent(s) have 1 missing wave(s). Lowest wave coverage is wave 3 with 2 respondent(s).

# justification paragraphs are stable

    Code
      cat(weasel_justify_subset(p, "anchored_balanced"))
    Output
      To construct a longitudinal analysis sample, we selected respondents whose wave participation satisfied explicit structural criteria using the WEASEL framework (Wave-based Extraction and Selection for Longitudinal Data) (van den Bosch, 2026). Specifically, we focused on waves 1 to 8 (L = 8) and required observed endpoints to ensure temporal anchoring, allowed at most one missing wave within the window, and restricted the missingness structure (at most 1 interior missing block(s), each no longer than 1 wave(s)). This strategy retained 3 respondent(s), reflecting an explicit trade-off between sample size and within-window completeness. The planning population comprised 7 respondent(s) observed at least once within the analysis window, out of 7 distinct respondent(s) in the supplied data; retention figures are relative to this in-window population. In the resulting subset, mean within-window coverage was 0.958, endpoint coverage was 1.000. This scenario is characterized as: good balance, anchored endpoints. The analysis window was selected using the package's span rule (full), which prioritizes a coherent window with comparatively strong participation. All selection decisions were rule-based and reproducible, and can be regenerated from the same inputs and parameters using the weasel workflow.

---

    Code
      cat(weasel_justify_subset(p, "anchored_strict", style = "concise"))
    Output
      We selected a longitudinal analysis subset using the WEASEL framework (Wave-based Extraction and Selection for Longitudinal Data) (van den Bosch, 2026). Within waves 1 to 8, we required observed endpoints to ensure temporal anchoring, required complete participation within the window (no missing waves) and restricted the missingness structure (at most 0 interior missing block(s), each no longer than 0 wave(s)). This strategy retained 2 respondent(s). In the resulting subset, mean within-window coverage was 1.000; endpoint coverage was 1.000.

---

    Code
      cat(weasel_justify_subset(p, "lenient", style = "extended"))
    Output
      We used the WEASEL framework (Wave-based Extraction and Selection for Longitudinal Data) (van den Bosch, 2026) to derive a reproducible longitudinal analysis subset from the available panel waves. The goal was to avoid ad hoc inclusion rules by explicitly defining admissible participation patterns and selecting a subset that balances longitudinal completeness with sample size. We defined a target window of waves 1 to 8 (L = 8) and applied structural constraints to respondent trajectories. Within this window, we did not require observed endpoints, allowing unanchored participation, allowed up to 2 missing waves within the window, and restricted the missingness structure (at most 2 interior missing block(s), each no longer than 2 wave(s)). Under these criteria, 5 respondent(s) were retained. The planning population comprised 7 respondent(s) observed at least once within the analysis window, out of 7 distinct respondent(s) in the supplied data; retention figures are relative to this in-window population. Subset diagnostics indicated that mean within-window coverage was 0.875 and endpoint coverage was 0.800. Relative to alternative scenarios considered by the package, this configuration is described as: largest N, endpoints not guaranteed. The chosen window was produced by the package's span rule (full). In practice, this emphasizes a stable segment of the panel rather than maximizing the nominal wave range. This approach improves transparency because the inclusion set is fully determined by declared constraints (window bounds, endpoint handling, and permitted missingness structure) rather than subjective post hoc decisions.

