<!-- Referee report on support_global_redistr_AEJEP.tex + _appendix.tex -->

# Referee report

**Manuscript:** "Public Acceptance of International Redistribution in High-Income Countries"
**Submitted to:** American Economic Journal: Economic Policy

---

## 1. Summary of the paper

The paper reports a pre-registered online survey of 12,001 respondents in eleven high-income countries: the US, Japan, Russia, Saudi Arabia, and seven European countries. It asks why international redistribution gets little attention from politicians even though surveys find majority support for it, a pattern the paper calls the "preference–prominence gap." A stylized probabilistic-voting framework sorts four explanations: overstated acceptance (E1), partial coverage (E2), low salience (E3), and misperceived acceptance (E4). The empirical modules are:

- an opening block meant to measure priorities before respondents can infer the topic (open-ended questions, a two-candidate conjoint, a five-item budget allocation);
- direct Likert and Yes/No questions on ten "plausible" policies and several more radical ones;
- coverage experiments for an International Climate Scheme (ICS) and a millionaire wealth tax;
- two "warm glow" tests: a donation opportunity as a moral substitute, and an information treatment used as an instrument for the perceived likelihood of transfers.

The headline findings are:

- the international millionaire tax raises a program's choice probability by 5.1 p.p., and cutting aid lowers it by 2.9 p.p.;
- LIC spending receives 17.5% of a hypothetical budget;
- majorities "accept" (support among non-indifferent answers) nearly all policies;
- acceptance falls only modestly with coverage;
- the information treatment raises support (reduced form +1.3 p.p.; 2SLS +18 p.p.).

The paper concludes that public opinion is not the binding constraint and that low salience and pluralistic ignorance explain political neglect.

## 2. Overall assessment

The question matters for policy and fits AEJ:EP. Several international negotiations (the UN tax convention, the G20 billionaire tax, the NCQG, MDB reform) turn on whether HIC electorates would tolerate transfers. The fielding period also coincided with announced ODA cuts in several surveyed countries, which makes the gap especially salient. The data are a real asset. The sample is large and multi-country and includes under-studied publics. The design is careful in several respects: country-specific conjoint attribute pools drawn from party platforms, an explicit anti-aid attribute, incentivized comprehension and belief questions, and balance, attrition, placebo, vote-reweighting and extended-sample checks. Pre-registration is a plus. The conjoint result that a globally redistributive tax shifts candidate choice by about as much as typical platform items is the most novel and policy-relevant finding.

Weighed against this, I have four main concerns:

1. **Incremental contribution.** Much of the paper is incremental relative to Fabre, Douenne and Mattauch (2025) and Cappelen, Støstad and Tungodden (2025). Several instruments are reused: the GCS, the revenue split, the "my taxes" item, and the Dis/Adv custom-redistribution algorithm.
2. **Confounded experiments.** The two experiments that carry the "robustness" claims (coverage and warm glow) have design features that confound the intended variation. These confounds are not resolved.
3. **Language stronger than the evidence.** The headline "majority acceptance" rests on a measure that drops roughly 28% indifferent answers; absolute support is below 50% for 7 of the 10 plausible policies. Several pre-registered hypotheses are reported as "confirmed" when the pre-registered test does not support that wording. The exploratory, correlational evidence on salience and misperception is summarized in causal terms in the abstract and conclusion.
4. **Focus.** The paper tries to do too much. About fifteen survey modules compete for space in 30 pages and a 122-page appendix, and the conceptual framework is mostly a taxonomy.

A focused revision built around the conjoint, a cleaned-up coverage analysis, and a properly designed feasibility test could be a good AEJ:EP paper. The current version is not ready.

## 3. Major comments

**1. The "majority acceptance" headline depends on excluding indifferent respondents.**
- *The concern.* Acceptance is defined as support among non-indifferent answers. Table 1 reports that the mean respondent supports 51% of plausible policies and opposes 21%, so about 28% are indifferent. Online Appendix Figure S38 shows absolute pooled support of 37% for the aviation levy and 46–49% for the UNSC expansion, shipping levy, NCQG, 0.7% aid, debt relief, and the 35% minimum profit tax. In Japan, absolute support ranges from 26% to 54%.
- *Why it matters.* The statement in Section IX that "conclusions on majority acceptance are robust to the measure for the most popular policies" holds for three of ten policies. The policy conclusion that "public opinion in HICs is not the binding constraint" (p. 29) rests on how the indifferent are classified, and for electoral supply the indifferent are not obviously supporters. Yes/No items (GCS, ICS, wealth tax) also offer no neutral or "don't know" option, which inflates "Yes" relative to the Likert items.
- *Request.* Report absolute support, opposition and indifference side by side in the main Figure 4 and Figure 5. Rephrase the abstract and conclusion as "more support than oppose" where that is what the data show. Discuss what "Indifferent" means (no opinion, satisficing, or genuine indifference). If possible, use the conjoint, a forced-choice measure that is closer to vote choice, as the headline measure of electoral viability.

**2. Several pre-registered hypotheses are reported as "confirmed" when the pre-registered tests do not support it.**
- *H2.* The pre-registered test is one-sided at the 5% level by region. Table 1 and lines 343–345 state that it is "confirmed in every country … at the 10% level in Japan." Japan, a pre-registered region, therefore fails at 5%.
- *H3a.* Table 1 says "confirmed for all policies in all countries, except an aviation levy in Japan." Figure 4, however, shows 50% for the minimum profit tax in Russia and 51% for the aviation levy in Russia, the US and Switzerland. With control-group samples (half the sample, minus the indifferent), these cannot be significantly above 50%.
- *H1a and H5 framing.* H1a ("no positive effect") and the regional parts of H5 are null hypotheses. Failing to reject is not confirmation, especially in Russia and Saudi Arabia, where the H5a point estimates are negative and the standard errors are 2.2–2.5 p.p. (Table S20).
- *Dependence on controls.* The pooled H5a claim that "the one-sided 95% bound excludes any negative effect" holds only with controls. Without controls the reduced form is 0.011 (0.007) (Table S21), so t ≈ 1.57 and the one-sided test does not reject.
- *Request.* Append the anonymized PAP. Add a table listing each pre-registered test, its exact specification (controls, one-sided direction, region), the result, and every deviation. Deviations to cover include the Russian questionnaire changes and the separate Russian fielding wave, dropping the conjoint in Russia and Saudi Arabia, dichotomizing the belief variable, and the choice of controls. Use equivalence or non-inferiority tests with stated margins for the "does not reduce" hypotheses. Report minimum detectable effects and power, by region. Apply a multiple-testing correction across the full family of hypotheses × regions, not only to H1b.

**3. The information treatment does not isolate perceived likelihood, and the exclusion-restriction argument is weak.**
- *The concern.* The treatment text (Question 36) describes the very policies that make up the outcome index (Question 38): the shipping levy, 0.7% aid, the $300bn climate-finance goal, loss and damage, the 15% minimum tax, the billionaire tax, the UNSC, and Bridgetown. It presents them as already agreed or adopted.
- *Why it matters.* Learning that a policy is the status quo, endorsed by governments, or simply described in more detail plausibly raises support directly through status-quo bias, elite cues, norms, or comprehension. The bound argument in Section VII.B says warm glow would require the direct effect δ to exceed the 1.3 p.p. reduced form. That is a very low bar for the content of this treatment, not a demanding condition. The placebo table (Table S14) shows that an unrelated earlier branch (the High-color map) shifts the same outcome by −1.9 p.p.; the outcome moves by about 1–2 p.p. after small unrelated primes. The belief measured is generic ("significant transfers … in the next 15 years"). It is not the perceived likelihood or perceived personal cost of each policy, and personal cost is what drives the hypothetical-bias version of the warm-glow story. Dichotomizing belief also opens an intensive-margin violation of the exclusion restriction. Finally, the concept tested here is closer to hypothetical or expressive bias than to Andreoni-style warm glow, and the terminology should reflect that.
- *Request.* At minimum:
  - reframe H5a as a test of the effect of "optimistic information about negotiations" on stated support, not as a test of warm glow;
  - report effects on outcomes not mentioned in the text (for example the wealth-tax module, if it falls after the treatment);
  - use the full four-point belief;
  - report heterogeneity by baseline plausibility.

  Ideally, a follow-up wave would include a length-matched placebo text and a treatment that varies feasibility without describing the outcome policies. The abstract should drop "contrary to warm glow." Also, the factual accuracy of the treatment needs checking. For example, the 2025 IMO levy is described as "adopted," whereas the appendix reference is to a draft. This is especially relevant for the September–October Russian wave.

**4. The coverage experiments confound coverage with partner identity, own-bloc membership and question format.**
- *Partner identity.* For European respondents, the Low coalition includes the EU, and hence their own country, while the Mid coalition replaces the EU with China. The Low→Mid contrast for Europeans is therefore not "China joins" (line 511). It is a swap between one's own bloc and China.
- *Format.* All ICS variants are accepted more than the "global" GCS: 62–68% versus 55% (Figure 6). The ICS is accepted more even at 33% coverage, and even with visible costs, as footnote 11 acknowledges. This pattern, and the fact that the GCS text itself says the scheme starts "as soon as 100 countries agree," means the GCS is not a universal-coverage benchmark. The ICS question asks about one's country "joining … in case it is adopted by the above countries." It follows the GCS within subject, and it does not restate costs, even though costs differ by coalition (Appendix D.2 assumes High coverage for the GCS transfer in Europe and Japan).
- *Wealth tax.* The "Global" wealth-tax variant raises acceptance by 4.6 p.p. over All-HICs, which is significant. Yet the narrative says acceptance depends "only mildly" on coverage.
- *Request.* Present the ICS contrasts by respondent region with a clear statement of who is in each coalition relative to the respondent. Separate coverage from partner identity where possible, for example by comparing Mid and High within region. Discuss the ICS > GCS anomaly prominently. Avoid inferring a(θ) across question formats. State the policy implication ("coalitions of willing countries need not wait") with matching caution.

**5. The conjoint estimates need a domestic benchmark and attention to who gains.**
- *Domestic benchmark.* The international tax bundles "tax millionaires" with "fund LICs." Several countries' tax pools contain domestic taxes on the rich. In France, "reinstate a reinforced wealth tax" has an AMCE of about 0.10 against 0.047 for the international tax (Figure S16). In Germany, "higher taxes for the richest 1%" is about 0.08 against 0.053. A systematic comparison would identify the premium or discount attached to the LIC component, which is the parameter of interest.
- *Who gains.* Figure S15 shows that the international-tax effect is essentially zero among centre-right and far-right voters and is driven by left voters. In the probabilistic-voting framework, what matters is the effect among swing voters and the effect within a party's own electorate, not the average AMCE.
- *Robustness.* Country-level sign claims (line 303) do not survive the robustness checks. Switzerland's "cut aid" effect becomes significantly positive with vote weights and in the extended sample (Figures S86 and S92).
- *Request.* Report AMCEs of the international tax relative to domestic rich-tax items within the same pools. Report effects by vote and among self-identified swing or non-partisan voters. Explain how "Neither" (27%) is handled. Report the comparison to platform items as a ratio with a confidence interval rather than "4% smaller." The claim that the opening block precedes knowledge of the topic should be qualified: the millionaire-likelihood question and the conjoint's own attributes already hint at it, and the budget task explicitly describes a global tax pooled for LICs.

**6. Including Russia and Saudi Arabia, and population-weighted pooling, sit poorly with the framework.**
- *The concern.* The framework is about electoral supply. Russia and Saudi Arabia have no competitive elections. The conjoint was not fielded there, "vote was not asked" (line 270), the Russian instrument was curtailed and fielded months later by a different vendor, and Saudi non-nationals and people over 65 are badly under-represented (Table S7). Yet pooled estimates weight countries by population, which gives Russia 14% and Saudi Arabia 3% of the pooled weight.
- *Why it matters.* Saudi Arabia is consistently the most supportive country (for example 85% for the GCS), and expressing political attitudes may be constrained in both countries.
- *Request.* Report all pooled results for the nine democracies separately. Discuss whether "donor-country opinion" is a meaningful concept for Saudi Arabia, given the note in Question 38 that it "is considered a developed country." In Russia, Question 38's UNSC item was not asked to the control group (questionnaire footnote 10), so the share-supported outcome differs mechanically between arms. Russia should therefore be dropped from H5a or the outcome re-defined there.

**7. The exploratory evidence is presented as an explanation.**
- *The concern.* The abstract and conclusion state that low salience and underestimated support "help explain" political neglect. The evidence is descriptive: open-ended mentions and misperceived support. There is no test of whether correcting misperceptions changes attitudes or intended political action, and the preference–prominence gap itself is never measured, for example with manifesto data.
- *Request.* Either soften the language to "consistent with," or add evidence on the gap: manifesto or legislative salience measures, or a belief-correction treatment as in Andre et al. (2024). The motive and reparations results do not support the causal claim that a reparations framing "would not broaden support" (line 455); framing was not randomized. Similarly, "conditioning transfers on poverty-reduction programs makes government-to-government transfers acceptable" (lines 111 and 699) rests on non-experimental ratings (Figures S48–S49).

**8. Positioning, novelty and focus.**
- *Novelty.* The paper needs a precise statement of what is new relative to Fabre, Douenne and Mattauch (2025), including which questions are re-fielded. The claim that the conservative countries "had not been surveyed on most of these policies" should be checked against Cappelen et al.'s 40-country survey and Dechezleprêtre et al. (2025), which covers Japan and Poland.
- *Focus.* I recommend building the paper around (i) the conjoint, (ii) coverage and (iii) feasibility, and moving to the appendix the custom-redistribution task, moral circle, NCQG, sustainable-future scenario, and willingness-to-act items. The custom task has a 43% skip rate and anchoring problems, as the paper acknowledges.
- *Framework.* The framework would add value if it generated a quantitative prediction, for example mapping conjoint AMCEs by voter type into ΔV. As it stands it is a taxonomy.

## 4. Minor comments

1. **Line 237 vs. Table S10.** The text says 13% of respondents finish in under 6 minutes; Table S10, column 5, gives a mean of 0.087. Please reconcile, and state the denominator.
2. **Lines 386–387 vs. Figure 4.** The text says debt relief, 0.7% aid and UNSC expansion "all reach at least 70%"; UNSC is 69%.
3. **Line 528 vs. Figure 7 / S43.** The text says All-HICs acceptance runs "from 58% in Switzerland to 81% in Saudi Arabia"; Figure S43 shows 56% in Switzerland and 84% in Saudi Arabia (81% is Italy).
4. **Lines 467–470 vs. Figures S57–S59.** The text gives a "median" transfer of 5.4% of world income and a minimum income of $287/month. The appendix gives a median of 4.4% and $208, and a mean among satisfied respondents of 5.4% and $247; $287 appears nowhere. "48% worse off" compares with 46.4% in Figure S57. "56% who complete the task" seems to be the share *satisfied*. Footnote 17 of Appendix G ("Excluding the 39% of them …") is unclear.
5. **Coalition coverage.** Section VI.A gives Low = 33%. The US questionnaire (Question 31) says 40%, with "if the U.S. joined them, 40%," which looks like an error. Questions 33 and 34 give 71% and 72% for the same High coalition. The Figure 6 labels give ranges (25–33%, 64–72%). Please report the exact shares shown in each country.
6. **Cash-transfer and cost figures.** Footnote 10 says $35 and €20 per month; Appendix D.2 says $36 and $22. Question 26 shows a US NCS transfer of $115; Table S2 has $125. The Figure S36 net cost for the US is $88; Question 28 says $90.
7. **LIC population.** Footnote 2 gives 625 million people in LICs; Question 24 tells respondents 700 million.
8. **Line 242.** Item-order effects of "2 to 14 p.p." are described as small. A 14 p.p. effect (Table S15, column 2) is not small; please discuss which outcomes are affected.
9. **Line 653.** Using "70% found it unbiased" as reassurance leaves out that 22% perceived a left bias versus 8% a right bias (34% left in Switzerland; Figure S68). Please report this asymmetry.
10. **Figure 4 label.** "Developing countries providing $300 bn" should read "Developed."
11. **Figure S41.** The legend says "Intermediate ($200 bn)"; the questionnaire option is $300bn in grants.
12. **Figure S50.** The figure shows 70/30, while the text (line 430) reports 61% versus 26%. Please clarify that the figure excludes neutral answers.
13. **Line 270.** The reason the conjoint was not fielded in Russia and Saudi Arabia should be "no competitive elections," not "vote was not asked."
14. **Donation test.** State in the main text that it was not run in Russia and Saudi Arabia (Table S19).
15. **Standard errors.** Report weighted-design standard errors and margins of error. Footnote 3's ±1.8–4.5 p.p. ignores weighting and the fact that many outcomes use half-samples; Swiss control-group cells in Figure 4 rest on roughly 170 non-indifferent answers.
16. **Belief incentive.** A single $100 prize for the closest guess provides weak incentives and elicits something closer to a mode than a median; please discuss.
17. **Line 620.** Clarify how the 22 p.p. US underestimation is computed. Figure S36 shows that US respondents underestimate their own country by only 9 p.p.
18. **Line 95.** "I discuss threats to the exclusion restriction and show that warm glow could be rationalized only if …" overstates what a bound with a free parameter shows.
19. **Typos and references.** "Ansolabehere, Rodden and Jr" (should be Snyder Jr.). "Bridgetown Initiative, ." and "Global Nation, ." are bibliography formatting errors. The Blanchard and Tirole (2021) entry is incomplete. "much be chosen" and "interverted" appear in Appendix C. The Figure S15 back-link points to Section IV.C instead of IV.A.
20. **Table 1.** The "Pooled estimate" column for H3a reports the mean share of policies supported, not acceptance of each policy. For H4a, "61% to 70%" does not match the figures (62–69%).
21. **Fielding window.** The fielding coincided with major ODA cuts in several surveyed countries. Discussing this context, and comparing stated support for 0.7% with actual ODA/GNI, would give a useful revealed-preference benchmark.

## 5. Recommendation to the editor

**Recommendation: Major revision (revise and resubmit).**

**Confidential justification.** The topic is important and well suited to AEJ:EP. The multi-country, pre-registered data are valuable, and the conjoint evidence that a globally redistributive tax moves candidate choice is a real contribution. However:

- the headline acceptance claims depend on excluding a large indifferent share;
- several "confirmed" pre-registered hypotheses do not pass the pre-registered tests;
- the warm-glow/feasibility experiment's treatment describes the outcome policies, so it cannot isolate perceived likelihood;
- the coverage experiment confounds coverage with partner identity and question format;
- the exploratory correlational results are framed causally.

There are also a number of numerical inconsistencies between text and appendix. Overlap with the authors' or the literature's prior work (notably Fabre, Douenne and Mattauch 2025) needs a clearer delineation. A substantially refocused paper with transparent reporting of the pre-analysis plan, absolute-support measures, democracy-only pooled results, a domestic benchmark in the conjoint, and corrected claims could be publishable. If the authors cannot address the identification issues in Major Comments 3 and 4 (by reframing or new data), the remaining contribution may be too incremental for the journal.

**Scores:**
- Contribution: 6/10
- Execution/rigor: 5/10