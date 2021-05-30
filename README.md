# Analysis with Splitted Plot Design: oats

This is the second mid-term project of Statisical Consulting, a course at Department of Statistics and Institue of Data Science, National Cheng Kung University, Taiwan. In this project, we followed Heiberger and Holland (2015) to demonstrate a complete analysis of `oats`, a classic dataset with the splitted plot design.

## Authors

- 廖傑恩 Jay Liao（RE6094028): re6094028@gs.ncku.edu.tw
- 陳溫茹（R26091040）
- 戴庭筠（R26091032）

## Files

- `oats_report.pdf`: the report of the analysis with interpretations in Chinese.
- `oats_report.Rmd`: the markdown file to produce `oats_report.pdf` with R codes.
- `oats.R`: the souce code of analysis.
- `./images` contains two residual plots which are included in `oats_report.pdf`.

## Usage

1. Install all required packages and access the dataset with `data(oats)` command. 

2. Run `oats.R` in R console.

If you want to re-build the report, knit `oats_report.Rmd` in RStudio. You may need to install `TeX` and other related packages first.

## References

1. Heiberger, R. M. & Burt Holland, B. H. (2015). Statistical Analysis and Data Display An Intermediate Course with Examples in R. Springer. 

2. Rutherford, A. (2001). Introducing ANOVA and ANCOVA: a GLM approach. Sage.

3. Lim, T. S., & Loh, W. Y. (1996). A comparison of tests of equality of variances. Computational Statistics & Data Analysis, 22(3), 287-301.

4. Dunnett, C. W. (1955). A multiple comparison procedure for comparing several treatments with a control. Journal of the American Statistical Association, 50(272), 1096-1121.

5. Bland, J. M., & Altman, D. G. (1995). Multiple significance tests: the Bonferroni method. Bmj, 310(6973), 170.
