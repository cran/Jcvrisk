# Jcvrisk 0.1.2 (2025-04-20)

- Fixed a calculation error in the `suita_wECG()` and `suita_ECG()` function for estimated 10-year risk values.
  - Risk calculation about sex was corrected from [male=0, female =1] to [male=1, female =2].


# Jcvrisk 0.1.1 (2025-04-18)

- Fixed a calculation error in the `suita_wECG()` function for estimated 10-year risk values.
  - For individuals with SCORE = 30–35: the risk estimate was corrected from 14% to 15%.
  - For individuals with SCORE ≥ 36: the risk estimate was corrected from 25% to 26%.


# Jcvrisk 0.1.0 (2024-10-22)

* Initial CRAN submission.
