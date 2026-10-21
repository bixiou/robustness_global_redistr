<!-- Referee report on support_global_redistr.tex (compiled as is) -->

# Referee Report

**Manuscript:** "Public Acceptance of International Redistribution in High-Income Countries"
**Journal:** American Economic Journal: Economic Policy

## 1. Summary of the paper

The paper reports a pre-registered online survey of 12,001 respondents in eleven high-income countries: the U.S., Japan, Russia, Saudi Arabia, and seven European countries. It documents stated acceptance of a wide range of international redistribution and climate policies. The headline is "majority acceptance in every country for nearly all global policies tested". The paper then asks why these policies have little prominence despite that acceptance, and proposes two explanations: low salience and pluralistic ignorance. The evidence comes from four sources:

- **Open-ended questions and a revenue-allocation task.** Global inequality rarely comes up unprompted. In the 5-item revenue split, the item funding low-income countries (LICs) gets about 17.5% of revenue.
- **Country-coverage experiments.** These vary the coalition implementing an international climate cap-and-dividend (ICS) and a millionaire wealth tax with 30% of revenue going to LICs. Acceptance falls by only 3–6 p.p. when coverage shrinks.
- **Tests for "warm glow".** A conjoint finds that an international millionaire tax raises a program's vote share by 5 p.p. A donation-lottery "moral substitute" treatment raises Global Climate Scheme (GCS) support by 3 p.p. An information ("realism") treatment raises the belief that transfers are likely by 7 p.p.; with that as first stage, an IV estimate implies an 18 p.p. causal effect of this belief on the share of policies supported.
- **Descriptive modules.** These cover radical proposals, moral circle, preferred transfer channels, the NCQG climate-finance goal, and an interactive "custom global redistribution" task, whose average implies transfers of about 5.4% of world income.

## 2. Overall assessment

The question matters for policy. International tax cooperation, climate finance, and multilateral development bank (MDB) reform are on the G20/UN agenda, and whether voters in rich countries would tolerate such transfers is a first-order political-feasibility question. The dataset is valuable. It covers countries rarely surveyed on this topic (Japan, Poland, Switzerland, Russia, Saudi Arabia), the questionnaire is fully documented, data and code are public, and the design includes several randomized components plus robustness appendices (attrition, balance, placebo, vote reweighting, extended sample, order effects). The paper is transparent about many of its limitations.

However, the manuscript is not yet an AEJ:EP paper in its current form, for four reasons:

1. **Incremental novelty.** Much of it extends the author's own recent multi-country work (Fabre et al. 2025, cited) and Cappelen et al. (2025b). The GCS, the global millionaire tax, the second-order beliefs, and the revenue split are explicitly replications.
2. **Weak identification of the central claims.** The "solve the puzzle" framing and the "no warm glow" conclusion rest on designs that cannot carry them. The realism IV has a serious exclusion-restriction problem, the moral-substitute test cannot discriminate between competing theories, and the low-salience → low-prominence link is asserted rather than tested.
3. **Metric-dependent headline numbers.** The headline levels depend on an "acceptance" metric that drops "Indifferent" respondents and on yes/no questions in which costs are inconsistently salient. Several internal patterns (Saudi support for a $101/month net cost, ICS > GCS, large anchoring and order effects) point to acquiescence or demand effects that the paper underplays.
4. **Too many modules and too many inconsistencies.** The paper is a collection of more than 15 modules without a unifying contribution, and there are many numerical inconsistencies between text, tables, and questionnaire.

A substantially refocused paper, built around the coverage experiments and a credible test of "expressive" versus "sincere" support, could fit the journal.

## 3. Major comments

**1. The exclusion restriction in the realism IV (Section 5.2, Table 1) is implausible.**
- **The concern.** The treatment text (Q36) describes, nearly item by item, the policies that make up the outcome (Q38): the IMO levy, 0.7% aid, the $300bn climate-finance goal, the loss-and-damage fund, the 15% minimum tax, the G20 billionaire tax, UN Security Council reform, and Bridgetown. The treatment therefore affects support through channels other than "belief that transfers are likely": elite and institutional endorsement cues, perceived social norms, information about policy content, and familiarity. In addition, only the treated arm receives the sentence promising incentivized comprehension questions, so attention and effort differ across arms. No such comprehension question on Q36 content appears in the questionnaire, which raises a deception or IRB issue. Footnote 48 adds that in Russia the Security Council item was not asked to the control group, so the outcome is defined differently across arms.
- **Fragility.** Without controls (Table S21), the 2SLS estimate is 0.141 (SE 0.088) and the reduced form is 0.011 (0.007): neither is significant. With controls, the reduced form (0.013, SE 0.007) is starred at p<0.05 even though t≈1.9.
- **Why it matters.** This experiment is the paper's main causal claim against warm glow, and a central element of the abstract.
- **Requests.**
  - Drop the IV interpretation, or test it partially. One option is to estimate effects separately on Q38 items mentioned versus not mentioned in Q36 (debt relief, the aviation levy, and arguably the 35% minimum tax were not mentioned).
  - Report whether the control specification was pre-registered.
  - Exclude Russia, or redefine the outcome there.
  - Explain the unfulfilled comprehension-incentive statement.
  - Reframe the result as an "information/endorsement effect".

**2. The warm-glow tests do not test warm glow as defined, and they are framed asymmetrically.**
- **The concern.** Warm glow, as the paper uses it, is the worry that support evaporates when implementation becomes real and costly. Neither experiment makes anything real or costly. The realism treatment shifts a 4-point belief by 7 p.p. and changes nothing about personal stakes. The donation lottery concerns reforestation (not global redistribution) and involves a tiny-probability prize, and a positive effect is equally predicted by consistency or foot-in-the-door mechanisms.
- **Why it matters.** A null or positive effect from these designs is weak evidence of sincerity. The paper also tends to read every result as favorable to sincerity.
- **Requests.**
  - Relabel the tests (e.g., "expressive/hypothetical support").
  - Report minimum detectable effects.
  - Ideally add a behavioral validation, e.g., a real donation to a global-poverty charity out of the bonus, or a tracked petition click.
  - Remove "solve this puzzle" from the abstract and introduction.

**3. The headline levels depend on the "acceptance" metric and on question format.**
- **The concern.** "Acceptance" excludes Indifferent responses (footnote 3). On absolute support (Figure S32), Japan shows a majority for only 1 of the 10 "plausible" policies (the billionaire tax, at 54%), and the U.S. for 3. Yes/no items (GCS, ICS, wealth tax) have no neutral option. Several patterns suggest acquiescence rather than considered preferences:
  - Saudi respondents support the GCS at 85% despite the largest stated net cost ($101/month; Figure S30, Table S2).
  - The ICS is accepted at 65–68%, versus 55% for the GCS, despite at least equal costs.
  - The median NCQG choice is $100bn in grants, yet 68% "accept" $300bn per year (Figure 10).
  - Item-order effects reach 14 p.p. (Table S15).
  - 22% of respondents perceive a left-wing bias, versus 8% a right-wing one (Figure S62).
- **Why it matters.** The abstract's "majority acceptance in every country" is not robust to the metric.
- **Requests.**
  - State absolute support alongside acceptance in the abstract, the introduction, and every headline claim.
  - Analyze acquiescence, for example with balanced or reversed-wording items, or straight-lining measures.
  - Discuss demand effects explicitly, possibly with a Mummolo-Peterson-style demand bound.
  - Confront the NCQG versus Q38 discrepancy directly rather than attributing it to "anchoring in existing agreements".

**4. The coverage experiments are informative but over-interpreted.**
- **The concern.** Every arm asks whether one's own country should *join* an already-formed coalition; there is no unilateral or "few others" arm. For European respondents, the "International" wealth-tax coalition still contains the main European partners, so the effective variation is small. In the wealth tax, 70% of revenue stays at home, so support largely reflects support for a domestic millionaire tax. The "High color" arm lowers acceptance by 6.6 p.p., more than the whole Low-to-High coverage gradient, which indicates that the salience of distributive effects matters more than coverage. The ICS–GCS gap is also left unexplained, and the GCS itself is already framed as "implemented as soon as 100 countries agree", with sanctions.
- **Why it matters.** The conclusion that "competitiveness or free riding are not decisive factors" (p. 24) does not follow.
- **Requests.**
  - Soften that conclusion.
  - Discuss the joining framing and power.
  - Report effects in a single unit (Table S16 is in p.p., Table S17 in shares).
  - Drop or recast the heterogeneity conditioned on the post-treatment comprehension question (p. 22–23), which is measured at the end of the survey.

**5. The conjoint cannot isolate the global component.** The "International tax on millionaires with 30% financing LICs" bundles a very popular domestic attribute (taxing millionaires) with the redistributive one. Without a domestic millionaire tax in the same pool, the +5 p.p. effect cannot be attributed to global redistribution. Also clarify how "Neither" (27%) enters the outcome, and what the AMCE means when abstention is allowed. Request: re-interpret the effect as an upper bound, and discuss magnitudes relative to the other attributes.

**6. Pluralistic ignorance is confounded with inflated stated support.** The benchmark for "actual support" is the survey's own yes/no GCS share, measured in an online panel that over-represents voters (Figure S66). If stated support is inflated (see comment 3), part of the "underestimation" may reflect accurate beliefs about real-world support. The paper also never links misperceptions to anything, for example through an information-correction treatment in the spirit of Andre et al. Request: discuss this confound and temper the claim that pluralistic ignorance "may be an important reason why global solidarity solutions are neglected".

**7. The salience and revenue-split evidence is weaker than presented.**
- **Revenue split.** Per-item allocation to global items is constant at 17.5% whatever the number of global items (p. 19). That points to a near-1/n allocation heuristic rather than revealed priorities, which undermines the comparison with the 66.8%-of-equal-split benchmark from Fabre et al. (2025). The Few menu also pits the global item against unattractive options (deficit reduction, income-tax cuts).
- **Open-ended coding.** The "manual benchmark" was coded by a single, non-blind coder (the author) on machine-translated text.
- **Requests.** Provide a second independent coder with inter-rater reliability. Interpret the allocation results in light of the heuristic.

**8. The custom redistribution task has large anchoring and selection problems.** 43% skip the task, and anchoring absorbs 36–57% of the distance between the two random default positions (footnote 45). The author's suggestion that the defaults were "drifting away from a shared preference" is unconvincing. The task also comes at the very end, after many pro-global prompts, and Russia is excluded. Requests:
- Report the transfer implied by each default.
- Report results separately by anchor.
- Present the "5.4% of world income" as a default-dependent figure, or move the module to the appendix.

**9. Sample, pooling, pre-registration, and inference need work.**
- **Saudi Arabia and Russia.** These are authoritarian contexts, with censorship in Russia and preference falsification risks in both. The samples are also poorly matched. In Saudi Arabia, 0% of the sample is 65+ (population 4%), 11% have below-upper-secondary education (31%), and only 24% are non-Saudi men (41%), so trimmed weights cannot correct (Table S7). Yet these countries anchor the cross-country rankings (Saudi Arabia is first) and make up 17% of the pooled "All".
- **Russia's questionnaire.** It was curtailed and fielded three months later.
- **Requests.**
  - Report the pooled results excluding RU and SA.
  - Provide a table mapping each pre-registered hypothesis and specification to the reported test, including the Russian deviations. The statement that "the study did not deviate" is hard to reconcile with footnote 48 and the curtailed questionnaire.
  - Address multiple-hypothesis testing (sharpened q-values) for the experimental families.
  - Add power calculations.

**10. Focus, length, and fit.** At 47 pages of main text and 167 pages overall, the paper reads as a survey report. Section 5 ("Sincerity") contains the conjoint, which concerns vote salience rather than sincerity. Section 6 lists modules (channels, moral circle, reparations, NCQG) with little connection to the stated puzzle. Request: restructure around one or two contributions (e.g., (i) robustness to coverage and realism, (ii) priority and salience), and move the rest to the appendix. Sharpen the novelty relative to Fabre et al. (2025), Cappelen et al. (2025b), Dechezleprêtre et al. (2025), and the conditional-cooperation literature. AEJ:EP readers will also want a clearer policy mapping: which specific instruments are politically feasible, and with what support margin under conservative metrics.

## 4. Minor comments

1. **p. 3, fn. 2.** "Would mechanically double" LIC income: at PPP, 1% of HIC GDP is +60%; only nominally is it more than double (+153%). Rephrase.
2. **p. 4–5, l. 98–101.** Security Council expansion is at 69% (Figure 10), not "at least 70%".
3. **p. 6, l. 152 vs. p. 34, l. 794.** "561 millionaires" vs. "584 millionaires". The wording also differs ("could actively participate" vs. "in favor"). Counting "sign a petition" as active participation is a stretch.
4. **p. 11, l. 292–294.** 9% + 13% − 5% = 17%, not 16%. Table S10 (col. 5) reports 8.7% completing in under 6 minutes, not 13%.
5. **p. 18, fn. 18.** "Lower than 33.4%" should read 13.4%. Also, 17.8/13.4 is a 33% increase, not 34%.
6. **p. 22, Figure 6, Q31–34.** The Low coverage is reported as "33%" (text), "25–33%" (figure), and "40%, and 40% if the U.S. joined" (Q31, where the with/without figures are identical). The High coverage is given as 71% in Q33 and 72% in Q34.
7. **p. 23, l. 551–552.** In Table S16, Switzerland's High-color effect is −11.7 (p<0.1), so it is not "of opposite sign"; only Spain's is (and Italy's and the UK's, which are not mentioned).
8. **p. 22, l. 539–541.** Japan's Low and Mid coefficients are not significant (Table S16); the "significantly increases" claim holds only for the U.S.
9. **p. 24, l. 576–577.** The text gives 4.8 and 1.4 p.p., versus 0.046 and −0.010 in Table S17.
10. **Figure 10 and Figure S32.** "Developing countries providing $300bn" should read "Developed".
11. **p. 30, l. 693.** Russia's acceptance of the higher global minimum tax is 50%, so it is not a majority; Japanese aviation is not the "only exception".
12. **p. 34, l. 803.** The quoted wording differs from Q52 ("Helping *those* in need… also true at the international level").
13. **p. 35, l. 818–820.** "As explained in Section 2": Section 2 does not discuss Global Nation (2023). "Replicate well" is also generous: Japan is 76 vs. 59 and the U.S. 44 vs. 55.
14. **p. 35, l. 818.** "60%" relative agreement vs. 59% in Figure 11.
15. **p. 7, l. 196; p. 35, l. 824.** Europe's universalists are at 50%, which is not a majority. The Saudi figure uses a different question (fn. 41), so the countries are not comparable.
16. **Q45/Q46.** The response options are listed as "Strongly oppose; Somewhat support; Strongly support; Somewhat oppose; Indifferent". Please confirm the order actually displayed.
17. **Q26 vs. Table S2.** The U.S. NCS transfer is $115 in one and $125 in the other.
18. **Q29.** Non-U.S. respondents are told that typical Americans lose $140/month, while Americans themselves were told $90 (Table S2: $88). Please explain.
19. **Q36 vs. Table S1.** The treatment says the IMO "adopted an international levy… that should partly finance LICs", while Table S1 describes a *draft* standard with revenue allocation still undecided. The information treatment overstates.
20. **Table S4.** The "All" urbanicity weighted shares (.52/.15/.16) neither match the population shares (.63/.15/.20) nor sum to one.
21. **Table S10.** Some coefficients are implausible (e.g., Poland +0.784 on a 0.087-mean outcome; Italy +640 minutes), and mean duration is 53.9 minutes against a median of 17. Check for collinearity with region dummies.
22. **Figure S66.** The axis runs to 2.7; redesign the figure. It also shows that non-voters are substantially under-represented, which deserves discussion in Section 2.
23. **p. 32, l. 728.** The 2035 goal "is being met with only $26bn in grants" conflates the 2022 figure with the 2035 goal. The NCQG GDP shares also differ across variants (0.14/0.15, 0.43/0.45, 0.86/0.9).
24. **Table 1.** Report the reduced form in the same table, and do not describe the 2SLS estimate as "causal" without qualification.
25. **Typos and wording.**
    - "low interests rates" (l. 100)
    - "I check… and finds" (l. 295)
    - "much be chosen" (Appendix B)
    - "double the price heating fuel" (Q44)
    - "five permanent member" (Q36)
    - "contries" (Table S1)
    - "insight on the topics vary" (A.2)
26. **Bibliography.** Malformed entries (". Bridgetown Initiative", ". Global Nation", "J. Frimer, Chan, & darloap"); Cappelen, Støstad & Tungodden (2025) has no venue or link; "Fabre (2025)" and "Fabre et al. (2025)" are easily confused.
27. **Anonymization.** It is inconsistent: the questionnaire says "[anonymized for review]" while the title page and acknowledgments (p. 48) identify the author.

## 5. Recommendation to the editor

**Recommendation: Major revision (revise and resubmit).**

**Confidential justification.** The question is policy-relevant and the eleven-country dataset, which includes rarely surveyed countries, is a genuine asset. The coverage experiments and the descriptive mapping of support are useful to the policy community. However, the paper's distinctive causal claims do not hold up:
- The "no warm glow" result rests on an IV with an implausible exclusion restriction (the treatment describes the outcome policies, and attention incentives differ by arm) and is not significant without controls.
- The moral-substitute test cannot discriminate between theories.
- The "prominence puzzle" is asserted to be "solved" without any test linking salience or misperceptions to political supply.
- The headline levels rely on an acceptance metric that drops Indifferent respondents, plus yes/no formats with clear signs of acquiescence or demand.
- Novelty relative to the author's own recent work is modest.
- There are many text–table–questionnaire inconsistencies.

I recommend R&R only on condition of a major refocus: tempered claims, absolute support reported throughout, a re-analysis or relabeling of the realism experiment, pooled results without RU and SA, a pre-registration audit, and a much shorter main text. If the author cannot provide a cleaner identification of the "sincerity" component, the paper may fit better in a field or general-interest outlet focused on public opinion, such as *Nature Human Behaviour*-type or political-science journals.

**Scores:** Contribution 5/10; Execution/rigor 4/10.