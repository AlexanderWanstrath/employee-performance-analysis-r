# Employee Performance Analysis (R)

> **Academic group project**: FSAR, M.Sc. Business Analytics, Católica Lisbon School of Business & Economics - Fall 2025  

## Business case
A call center operator faces rising staff attrition and difficulty retaining top performers. Acting as consultants, we analysed historical data from one of its call centers to identify patterns in productivity and turnover and derive evidence-based recommendations for management. By design, the analysis relies on exploratory data analysis and hypothesis testing rather than regression models.

## Data
Five datasets from different administrative systems (productivity logs, HR records, wage data, worker surveys, end-of-period outcomes) covering 135 employees at weekly, monthly and person level. The raw data contained inconsistent date formats, mixed decimal separators, text-coded categories, duplicates and implausible values. The data was provided by the course and is not included in this repository.

## Approach
1. **Data cleaning:** standardised formats, parsed mixed date types, recoded categorical variables, removed duplicates, corrected implausible values, reconstructed missing wage components
2. **Data integration:** converted ISO weeks to months and merged all sources into one panel
3. **Exploratory analysis:** distributions, correlation matrices, scatter and box plots
4. **Hypothesis testing:** Welch t-tests, run at person level for employee characteristics to avoid counting individuals repeatedly
5. **Management presentation:** translated the findings into recommendations for the client's senior management

## Key findings
- Performance and call volume were significantly higher in weeks worked from home (p < 0.001)
- Employees who stayed had higher average performance than those who quit (p = 0.003)
- Promoted employees performed better on average, though the difference is only marginally significant (p = 0.045)
- Bonuses showed a weak positive correlation with performance (r = 0.25), while base and gross wage were unrelated to it
- Female employees showed higher average performance than male employees (p < 0.001)
- Education, commuting cost and exhaustion showed no significant relationship with performance or quitting

Results are descriptive and do not establish causal effects.

## Management recommendations
- **Promotion:** base promotions on clear performance metrics to retain top performers
- **Remote work:** maintain a flexible hybrid model and equip employees for productive remote work
- **Compensation:** link bonuses more closely to measurable KPIs
- **Retention:** offer clearer career paths, since top performers who left likely lacked advancement opportunities

## Tools
R · tidyverse · ggplot2 · lubridate · ISOweek · corrr · stargazer

## Files
- [`Statistic_Project_Trimester_1.md`](Statistic_Project_Trimester_1.md): full analysis with code, outputs and plots
- [`Statistic_Project_Trimester_1.Rmd`](Statistic_Project_Trimester_1.Rmd): source code
- [`SlideDeck_Analysis.pdf`](SlideDeck_Analysis.pdf): management presentation

*Code and presentation were revised after submission to fix several bugs, run person-level tests on one row per employee and align the slides with the corrected results.*
