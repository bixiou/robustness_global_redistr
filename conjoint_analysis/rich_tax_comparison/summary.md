# Conjoint: international millionaire tax vs domestic taxes on the rich

(Produced by code_robustness/conjoint_rich_tax.R; not included in the paper.)

Measured against "no policy in that domain", a domestic tax on the rich raises program choice by about 8 pp and the international millionaire tax by about 5 pp. That gap of about 3 pp is only borderline significant, and it reverses into a +2.4 pp premium for the international tax once a generic "having any proposal in this domain" effect is netted out.

I could not create the requested `summary.md`: the tool environment blocks report files from subagents. The full content is below instead. My script ran from start to finish and reproduces the paper exactly: +5.05 pp (s.e. 1.02) for the international tax, −2.93 pp (0.99) for cut aid, and 28.54% of programs inconsistent.

**How the data were built.** There is no cached `.RData`, and the full pipeline cannot run here (Windows library paths, missing Qualtrics credentials, many missing packages). So the script works read-only from what already exists:
- The per-country conjoint files `conjoint_analysis/ca_[country]_.csv` (final sample, policy codes, choice, `weight_country`, respondent `n`).
- The raw files in `data_raw/`, used only to rebuild `vote_factor` with the same rules as `2_prepare.R`. Respondent ids and choices match exactly.
- `questionnaire/sources.xlsx`, used to rebuild `consistent_conjoints_party` and the population weights.

**Conventions.** These follow the paper:
- OLS linear probability model on `program_preferred`, with "Neither" counted as 0 for both programs.
- Pooled regressions use `weight` (country weights rescaled to adult population); country regressions use `weight_country`.
- Standard errors clustered by respondent `n`. I computed them by hand because `sandwich` is not installed; the formula matches `vcovCL`'s default.
- Every policy in every domain gets a dummy relative to "no policy" in its domain. Pooled models add country fixed effects and country-specific dummies, with one common coefficient for the international tax (and for cut aid).
- Sample: 9,993 respondents. Seven respondents with no program information are dropped; the paper keeps them as empty programs.

## 1. Classification

All candidate items are in the `tax_system` domain, so a program contains at most one domestic rich tax, but it can contain one alongside the international tax.

| Country | Code | English label | Main | Narrow | Broad |
|---|---|---|---|---|---|
| FR | tax_system4 | Reinstate a reinforced wealth tax | yes | yes | yes |
| FR | tax_system3 | Reduce income tax up to €4,000/month net and increase it above that | no | no | yes |
| DE | tax_system4 | Higher taxes for the richest 1% to finance higher child benefit, citizen's income and minimum pension | yes | yes | yes |
| ES | tax_system1 | Lower the income tax on the middle class and increase it on rich households | yes | no | yes |
| CH | tax_system2 | Increase inheritance taxes on large fortunes | yes | yes | yes |
| JP | tax_system1 | Increase taxes (on income and inheritance) on the richest and reduce them on the poorest | yes | no | yes |
| US | tax_system2 | Raise the capital gains tax rate to 28% for individuals earning over $1 million | yes | yes | yes |
| GB | tax_system1 | Fight tax avoidance by abolishing the non-domiciled tax status | no | no | yes |
| PL | tax_system2 | Taxes on the profits of large digital corporations and fossil fuel companies | no | no | yes |
| IT | – | none | – | – | – |

**Excluded because they cut taxes for the rich:** DE2 abolish the inheritance tax, ES2 abolish the wealth tax, GB2 abolish the inheritance tax under £2M, IT2 15% flat tax, US3 extend the Trump tax cuts.

**How the borderline cases were decided:**
- **Main set:** the label explicitly raises taxes on the rich, the richest, large fortunes or millionaires.
- **Narrow set:** drops ES and JP, because those items also cut taxes for the middle class or the poor.
- **Broad set adds three items:**
  - FR3 is a progressive reform with a threshold near the top decile, and the label does not mention "the rich".
  - GB1 targets wealthy non-doms, framed as fighting tax avoidance.
  - PL2 taxes corporations, not rich individuals.

The pooled main sample is therefore FR, DE, ES, CH, JP and US.

## 2. Main results (pp; Diff = international − domestic, tested with the joint clustered covariance)

Ratio CIs are delta-method, and in brackets a respondent-cluster bootstrap (999 draws, stratified by country). N = programs / respondents.

| Sample | Int'l tax | Domestic rich tax | Diff (s.e.) | p | Ratio [delta CI] [bootstrap CI] | N |
|---|---|---|---|---|---|---|
| FR | 2.8 (3.3) | 11.6 (3.6) | −8.7 (5.0) | .079 | 0.24 [−0.34, 0.83] [−0.39, 1.04] | 1,596 / 798 |
| DE | 5.6 (3.1) | 8.9 (3.4) | −3.3 (4.5) | .47 | 0.63 [−0.18, 1.44] [−0.04, 2.47] | 2,094 / 1,047 |
| ES | 8.7 (4.1) | 21.2 (3.9) | −12.5 (5.9) | .032 | 0.41 [−0.01, 0.83] [0.03, 0.93] | 1,204 / 602 |
| CH | −4.3 (4.7) | 6.2 (4.7) | −10.5 (6.5) | .10 | −0.70 (unstable) | 936 / 468 |
| JP | 5.1 (2.1) | 7.9 (2.1) | −2.8 (3.0) | .35 | 0.64 [0.02, 1.27] [0.11, 1.72] | 3,994 / 1,997 |
| US | 5.2 (1.8) | 5.8 (1.8) | −0.6 (2.5) | .82 | 0.90 [0.08, 1.71] [0.26, 2.36] | 6,000 / 3,000 |
| **Pooled, single indicator (6 countries)** | **5.1 (1.1)** | **8.2 (1.2)** | **−3.1 (1.6)** | **.056** (bootstrap .052) | **0.62 [0.30, 0.94] [0.33, 1.00]** | 15,824 / 7,912 |
| Pooled, separate item dummies (population-weighted mean) | 5.1 | 8.3 | −3.2 (1.6) | .051 | 0.62 | same |
| Population-weighted average of country estimates | 5.1 (1.1) | 8.3 (1.2) | −3.2 (1.6) | .051 | – | same |
| Pooled, narrow set (FR, DE, CH, US) | 4.8 (1.4) | 6.9 (1.4) | −2.1 (2.0) | .29 | 0.69 [0.21, 1.18] | 10,626 / 5,313 |
| Pooled, broad set (8 countries) | 5.0 (1.05) | 8.3 (1.0) | −3.4 (1.5) | .024 | 0.60 [0.31, 0.89] | 18,474 / 9,237 |

For reference, the international tax in the same full-dummy specification over all 9 countries is 5.15 (1.01).

## 3. Secondary results

**(a) By vote (pooled, main set):**

| Vote | Int'l tax | Domestic rich tax | Diff (s.e.) | p |
|---|---|---|---|---|
| Left | 11.9 (1.9) | 13.3 (2.0) | −1.3 (2.7) | .63 (ratio 0.90) |
| Center-right or Right | 2.0 (1.8) | 5.5 (1.9) | −3.4 (2.7) | .21 |
| Far right | 1.2 (4.1) | 7.0 (4.4) | −5.9 (6.2) | .35 |
| Non-voter, PNR or Other | 3.5 (2.4) | 5.6 (2.3) | −2.1 (3.3) | .52 |

- Left minus Center-right/Right: the international tax effect differs by +9.9 (p<.001) and the domestic effect by +7.8 (p=.004), but the gap between them does not differ (+2.1, p=.59).
- In the US, Trump voters are coded Center-right or Right, so there is no US far-right group.

**(b) Interaction (pooled, main set):**
- The international tax effect is 5.2 (1.2) when the program has no domestic rich tax and 4.7 (2.0) when it has one.
- The interaction is −0.5 (2.2), p=.83, with a 95% CI of about [−4.8, +3.8].
- A program with both taxes beats one with neither by 13.1 (2.0).
- The broad set gives −1.0 (2.0) and the unweighted main set −0.3 (2.1). No country-level interaction is significant.
- So the effects simply add up: no sign of substitution or complementarity.

**(c) Robustness (pooled):**

| Variant | Set | Diff (s.e.) | p | Ratio |
|---|---|---|---|---|
| Unweighted | main | −4.1 (1.5) | .008 | 0.52 |
| Unweighted | broad | −4.3 (1.4) | .002 | 0.50 |
| Consistent programs (`consistent_conjoints_party`) | main | −2.2 (1.8) | .23 | 0.69 |
| Consistent programs | broad | −2.5 (1.7) | .15 | 0.65 |

In JP and the US every program counts as consistent under the party-based leaning, so the subset does not change those two countries.

**(d) Extra sensitivity check (I added this; you did not ask for it):**
- Almost every tax-system item has a positive effect relative to "no tax policy", including tax cuts for the rich: ES abolish the wealth tax +9.8, GB abolish inheritance tax +12.0, DE abolish inheritance tax +6.2.
- The other tax items in the six countries average +6.4 (1.0); the other foreign-policy items average −0.5 (1.0).
- Netting out these domain-level averages — [international − mean of foreign-policy items] − [domestic − mean of tax items] — gives a population-weighted contrast of **+2.4 (1.0), p=.012**, i.e. a premium for the international tax.
- By country: FR −4.2, DE +3.8, ES −2.2, CH +2.9, JP +3.9, US +3.5.

## 4. Caveats

- **The raw discount is fragile.** It is about 3 pp (international ≈ 60% of domestic), is driven by ES, FR and CH, and is near zero in the US, JP and DE. It is significant in the broad and unweighted versions but not in the narrow or consistent-program versions. It flips sign under the domain-demeaned contrast, so it should not be read as a clean penalty for the low-income-country component.
- **The items are not like for like.** ES and JP also cut taxes for others (ES at 21 pp drives much of the pooled gap), and DE earmarks revenue for domestic transfers. The international tax earmarks only 30% to low-income countries, so part of it is implicitly domestic.
- **Coverage is partial.** Only 6 of 9 countries have a main-set item. Country estimates are imprecise and ratios are unstable when the denominator is small (CH).
- **Other limits.** The interaction test can only rule out large effects, not moderate ones. There is no multiple-testing correction. As in the paper, "Neither" is coded 0 in a linear probability model.

## Files

- `/home/fabre/www/robustness_global_redistr/code_robustness/conjoint_rich_tax.R` — run with `Rscript --vanilla` from the repository root. It takes about 15–30 min because of the bootstrap; `B_BOOT` sets the number of draws.
- In `/home/fabre/www/robustness_global_redistr/conjoint_analysis/rich_tax_comparison/`:
  - `classification.csv`
  - `amce_full_by_country.csv`
  - `comparison_by_country.csv`
  - `comparison_pooled.csv`
  - `comparison_pooled_items.csv`
  - `comparison_pooled_summary.csv`
  - `intl_tax_all9_fulldummy.csv`
  - `heterogeneity_vote.csv`
  - `heterogeneity_vote_contrasts.csv`
  - `interaction.csv`
  - `robustness.csv`
  - `demeaned_contrast.csv`
  - `log.txt`

**Git check:** `git diff --stat` is empty, so no tracked file was modified. Besides my two new paths, `git status` shows new untracked files in `papers/` that I did not create: `support_global_redistr_AEJ.tex`, `support_global_redistr_AEJ_appendix.tex`, `title_page_AEJ.tex` (created around 15:45 while my script was running) and later `support_global_redistr_AEJ_diff.tex`. My script writes only to the output folder, so these probably come from you or another session. I left them untouched; there is nothing to restore with `git checkout` since they are untracked.