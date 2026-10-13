# IEABank October 2026 — Admin GUI migration checklist

Source: To-do Item Bank 20261008 and student_v3_teacher_v2 workbook. Draft branch; no production change.

## Schema registry updates
- item_category -> topic (topic_id, category_name)
- item.category_id -> topic_id
- scale -> index; scale_version -> index_version; scale_item -> index_item
- admin.pop -> instrument_pop; add instrument_type
- item_admin.type -> response_type; replace four legacy publication fields with is_public_itembank, is_puf, is_version_published
- response_scheme add is_checkbox
- response_scheme_values value/label -> response_value/response_label; add checked_value/not_checked_value
- miss_scheme_values value/category -> miss_value/miss_description
- index_version contains index publication flags
- retain item_admin_pk and index_item_pk from workbook.

## Importer blockers
- app.R TABLE_SCHEMA, SHAPES and import staging SQL hardcode all legacy names and must migrate atomically.
- Validate workbook against FKs, unique keys, header types, and #N/A normalization; explicitly handle two item_admin missing response/miss references.
- Only replace the workbook's 11 target entities; preserve auth, profiles, stimuli, translations, item examples and parameter tables.
- Ensure delete / truncation order respects FKs and avoid dropping data in dependent tables.
- Rebuild grants and RLS based on new publication semantics before API is exposed.

## UI priorities
- Replace 'item categories' -> 'topics'; 'scales' -> 'derived variables'; 'variation' -> 'version'.
- Add input validation for naming conventions, document importer results; small tutorial low priority.
- Do not implement speculative scale_admin/index_admin table in this migration.

## Release gate
Back up data and schema; prepare migration + import in dry-run; check RLS, row counts, relationships, app smoke tests; deploy only after coordinated release.
