# AEJ:EP submission files

Current version (same structure as support_global_redistr.tex; AEA formatting + English editing):
- `support_global_redistr_AEJ.tex` (anonymized manuscript), `support_global_redistr_AEJ_appendix.tex` (anonymized Online Appendix = app.tex unchanged), `title_page_AEJ.tex`.
- Build (from papers/, outputs in build/, gitignored):
  ```
  export TEXINPUTS=.:build: BIBINPUTS=.:..: BSTINPUTS=.:..:
  for f in support_global_redistr_AEJ_appendix support_global_redistr_AEJ; do pdflatex -output-directory=build $f; done
  (cd build; bibtex support_global_redistr_AEJ_appendix; bibtex support_global_redistr_AEJ)
  # then the two pdflatex runs twice more (xr-hyper cross-references), and pdflatex -output-directory=build title_page_AEJ
  ```
- Abstract shortened to 100 words; previous abstract kept as a comment.

Earlier restructured draft (now in deprecated/): support_global_redistr_AEJEP*.tex, title_page_AEJEP.tex. Notes below refer to it.

# AEJ:EP version: files, build, and items to check

## Files (new; no existing .tex file was modified)
- `support_global_redistr_AEJEP.tex`: anonymized main manuscript (title + abstract on p. 1, as AEA double-blind review requires).
- `support_global_redistr_AEJEP_appendix.tex`: anonymized Online Appendix. Section A holds material moved out of the main text; the remaining sections are `app.tex`, input unchanged.
- `title_page_AEJEP.tex`: separate title page with the author, acknowledgments, IRB, OSF and AEA registry IDs, data link, and the disclosure of the unpaid treasurer role.
- `aejep_extra.bib`: six added references (Andreoni 1990; Conley, Hansen and Rossi 2012; Lindbeck and Weibull 1987; Holm 1979; de Quidt, Haushofer and Roth 2018; Mummolo and Peterson 2019). Note: `*.bib` is gitignored.
- `referee_reports_AEJEP/`: the two referee reports.

## Build (from `papers/`; output goes to `papers/build/`, which is gitignored)
Compile the appendix, then the main file, then run bibtex on both, then compile each twice more. The two documents cross-reference each other through `xr-hyper`. Run with `TEXINPUTS=.:build:`. The script used is reproduced below.

```
cd papers; export TEXINPUTS=.:build: BIBINPUTS=.: BSTINPUTS=.:
for f in support_global_redistr_AEJEP_appendix support_global_redistr_AEJEP; do pdflatex -output-directory=build $f; done
(cd build; BIBINPUTS=..: BSTINPUTS=..: bibtex support_global_redistr_AEJEP_appendix; BIBINPUTS=..: BSTINPUTS=..: bibtex support_global_redistr_AEJEP)
# then the two pdflatex runs twice more, and pdflatex title_page_AEJEP
```

## Numbers to verify before submission
1. Wealth tax coverage. The original text says the Global variant is +4.8 p.p. and the International variant −1.4 p.p. relative to the HIC variant. `tables/wealth_tax.tex` gives +4.6 and −1.0. The new version uses the table values.
2. Millionaires. The intro says "52% of the 561 millionaires"; Section 5.2 says "52% of the 584". The new version gives no count.
3. The footnote in Section 3.2 says the test is "that global spending is lower than 33.4%". This should probably read 13.4% (or 13.3%).
4. The comment on Trump voters (50.3% support for the millionaire tax with 30% to LICs) is left as a TODO comment in Section V.A rather than in the text.
5. Table 1 (pre-registered hypotheses): check the "by region" verdicts against the R output. In particular:
   - H3b: no GCS majority in the US and Russia (49%);
   - H3c by country;
   - H4a: the 61% lower bound is the High coalition minus 6.6 p.p. for High color;
   - H4b: how the pre-registration defines the benchmark for the four ICS scenarios.
6. The derived statistics come from the rounded coefficients and SEs in `tables/`:
   - confidence bounds;
   - Holm-adjusted p-values for H1b (Europe, Japan, US);
   - the 3.2 p.p. bound for the wealth tax;
   - the exclusion-restriction breakdown point δ* = 1.3 p.p.
   Recompute them from the data if possible.

## Submission checklist (AEA)
- Disclosure statement: a separate PDF. The old title page said "no competing interest", while the Science Advances cover letter discloses the Global Redistribution Advocates role. The two need to agree.
- AI use must be described in the submission form: ChatGPT-4.1 classification of open-ended answers, plus any AI help in editing.
- The AEA RCT Registry entry (AEARCTR-0017248) was registered on 2025-11-14, after fieldwork. The title page says so, and the OSF registration is cited as the ex-ante one.
- Check that the appendix figures (screenshots, maps) contain no identifying URLs.
