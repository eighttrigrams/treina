# Treina

A small training log. Add named **Trainings** (id, name, description); step into a
training to log its **Sessions** (date, notes).

Derived from `tracker` and hosted through the `plurama` umbrella at
`treina.eighttrigrams.net`. Namespace prefix `et.trn`.

## Ports (dev)

- `PORT` **3150** — the clojure server
- `SHADOW_PORT` **9805** — shadow-cljs

Set via `.envrc`.

## Run standalone (dev)

```bash
make start          # shadow-cljs watch + `clj -X:run` (DEV=true)
make stop
```

Then open http://localhost:3150. Dev uses `:dangerously-skip-logins? true`, so
no login is required and data is owned by the nil-owner user.

## Run via plurama (umbrella)

`plurama` mounts treina at `treina.localhost` (dev) / `treina.eighttrigrams.net`
(prod) — see `plurama/config.edn` and `plurama/config.prod.edn`. The umbrella
calls `et.trn.server/build-app` with treina's `:apps :treina` sub-config.

## Build

```bash
make build          # shadow-cljs release + uberjar
```

## Architecture

- `src/clj/et/trn` — ring/compojure backend, next.jdbc + honeysql over SQLite,
  ragtime migrations in `resources/migrations/net/et/trn`.
- `src/cljs/et/trn/ui` — reagent SPA (`core`, `state`, `views/trainings`,
  `views/sessions`).
- `resources/public/treina` — `index.html`, `styles.css`, `css/` (lila theme
  from tracker's `base.css`), shadow output under `js/`.

Entities live in parallel `db/<entity>.clj` + `server/<entity>_handler.clj`
pairs — copy that pair to add a new one.
