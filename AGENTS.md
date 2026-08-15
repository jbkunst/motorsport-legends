# Ferrari Historical Data Rules

These instructions apply to Ferrari model-history work in this repository.

## Historical Concepts

- Keep `source_year`, `history_year`, presentation, launch, production start, deliveries, competition season and production end distinct.
- Never infer a production interval solely from a Ferrari History Garage year.
- Each dataset row represents one atomic model. Normalize composite labels only when official evidence distinguishes the model identity; document example-level mechanical changes in notes.
- Do not create predecessor or successor links merely because models share a programme, market role, name, engine or styling theme.
- `main_family` describes the model's primary architecture and role. Do not move a model between families to make a lineage continuous.
- Use alternative relationships for legitimate cross-family continuity without overwriting the primary technical genealogy.

## Relationship Vocabulary

- `direct`: explicit commercial or generational replacement.
- `evolution`: substantial continuity of the same technical concept with an evolutionary update.
- `derivative`: parallel or later model based on a named base model.
- `spiritual_successor`: non-continuous conceptual inheritance supported by evidence.
- Blank: evidence is insufficient. A blank is preferable to an invented relationship.

`base_model` is independent from predecessor chronology. When both a road-car base and a programme predecessor matter, store the base as `derivative` and the programme connection as an alternative relationship.

## Source Priority

1. Ferrari official model pages and corporate releases.
2. Ferrari official historical material: History Garage and History Moments.
3. Ferrari official editorial history, including Magazine and Bloodlines articles.
4. Specialist automotive or motorsport sources.
5. Wikipedia only as a contrast or discovery aid.

Preserve source URLs by function rather than replacing one with another:

- `/auto/`: technical model identity and specifications.
- `/history/garage/`: Ferrari's curated chronological placement.
- `/history/moments/`: historical events, launches and explicit relationships.
- `/magazine/articles/` and Bloodlines: retrospective lineage and contextual evidence.

Record confidence, review status, review date and notes for manually curated facts. Flag genuinely unresolved interpretations with `human_decision_required`.

## History Garage

Maintain verified mappings in `data/cars/ferrari_manual/ferrari_history_correspondence.csv`.

Allowed `history_year_type` values are:

- `presentation`
- `launch`
- `production_start`
- `competition_season`
- `editorial_year`

Do not build or depend on a scraper when Ferrari or CloudFront blocks stable retrieval. Add mappings only after verifying the individual page or official Garage index. Leave `history_image_url` blank unless the asset URL itself has been verified and is sufficiently stable.

History correspondence approval is exact and manual. Do not use fuzzy matching. Pay particular attention to `340 America` / `342 America`, the `250` / `275` / `330` families, `312 P` / `312 PB`, `512 S` / `512 M` / `512 BB`, and `365 GTB4` / `365 GT4 BB`.

## Reproducible Workflow

1. Review `data/cars/ferrari_past_models.csv` as the upstream snapshot.
2. Edit manual facts in `data/cars/ferrari_manual/`. This folder contains production periods, genealogy evidence and History Garage correspondences.
3. Add explicit snapshot corrections or new models in `R/cars/99-ferrari-past-model-enrichment.R`.
4. Run:

   `Rscript --vanilla R/cars/99-ferrari-past-model-enrichment.R`

5. Use `data/cars/ferrari_past_models_enriched.csv` as the single downstream table. The other Ferrari CSV files are reproducible inputs, not competing outputs.

Never edit the enriched CSV as the sole or final source of a historical correction.
