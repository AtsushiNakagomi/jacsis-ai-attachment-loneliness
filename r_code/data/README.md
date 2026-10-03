# data/

This folder is a placeholder; no data are distributed in this repository.
Place the analytic CSV here as `jacsis_2wave2425.csv` (or point the environment
variable `JACSIS_2WAVE_2425_PATH` at it). For the attrition weights, also place the
file of all valid 2024 respondents as `jacsis_2024_all.csv` (or set
`JACSIS_2024_ALL_PATH`); it carries the same 2024 variables plus the respondent
identifier `Monitor_ID`, which is matched against the two-wave file to derive the
follow-up flag (a 0/1 column `followed_2025` is used directly if present). JACSIS is a
restricted-access survey.
