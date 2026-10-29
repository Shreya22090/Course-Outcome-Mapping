# CO – PO/PSO Mapping System (R Shiny + SQLite)

Faculty enter Course Outcomes (COs); the system compares them with the predefined
11 POs and 2 PSOs and generates the CO–PO/PSO mapping table (levels 3 / 2 / 1) with a
column-wise average, which can be edited, saved and exported to Excel.

## Run
1. Install R (>= 4.1) and optionally RStudio.
2. In this folder:  `Rscript install_packages.R`
3. Start the app:   `Rscript -e "shiny::runApp()"`   (or open app.R in RStudio -> Run App)

## Using it
1. **Course & COs** – enter course code/name/faculty, type COs (or import `sample_cos.csv`), Save.
2. **Mapping** – set thresholds, click *Generate*, edit any cell (1/2/3/blank), *Save mapping*, *Download Excel*.
3. **Similarity %** – shows the percentages behind each level.
4. **PO/PSO Library** – edit the wording of POs/PSOs for your department.

## Structure
| File | Role |
|---|---|
| `app.R` | Frontend (UI) + server logic |
| `R/engine.R` | Backend: text cleaning, TF-IDF, cosine similarity, Bloom-verb hints, level + average |
| `R/db.R` | SQLite data access |
| `R/outcomes.R` | Seed data: the POs and PSOs |
| `data/co_mapper.sqlite` | Created automatically on first run |

## Database
`outcomes(code, type, title, description, sort_order)` ·
`courses(id, code, name, faculty, created_at)` ·
`course_outcomes(id, course_id, co_code, description)` ·
`mappings(course_id, co_code, outcome_code, similarity, level)`

## Method
Text -> lowercase, stop-word removal, light stemming -> TF-IDF vectors -> cosine similarity (%)
-> +10 points if the CO's leading verb fits the PO/PSO (Bloom hint) -> scaling -> level:
>= 90 -> 3, 70–90 -> 2, below 70 -> 1, raw match below the minimum -> no mapping.
Raw text similarity is low, so scores are scaled (default: best match of each CO = 100%).
All thresholds are adjustable in the sidebar. The result is a suggestion; faculty review it.
