# Treina

A small training log. Add named **Trainings** (id, name, description); step into a
training to log its **Sessions** (date, notes, markdown).

Two views:

- **Trainings** — the list, most recently trained first. A card's header opens
  and closes it, the `▸` icon on a closed card jumps to its sessions, ⌥-clicking
  the title renames it in place, and the description (or the pencil, when there
  is none) opens the edit modal. Delete sits in the open card's footer.
- **All sessions** — read-only feed of every session, newest first, plus a
  calendar that highlights the days you trained (hovering a day names the
  training).
- **YouTube** — videos from the channels you subscribe to, newest arrival first,
  as cards: open one to watch it inline, flag it ★ *cool* or ★★ *supercool*,
  archive it onto the second shelf. Clicking a channel chip filters to that
  channel, ⇧-clicking filters it out (tracker's inbox gesture). Channels are
  managed on the *Channels* subpage.
- **Program** — one text document holding the training philosophy, rendered as a
  page. Click it to edit in the modal; every save keeps the previous state as a
  version, and the modal's *History* view steps through the changes as diffs.
  So what the program said at any point in time stays recoverable.

Notes are edited in a CodeMirror editor using an **IJKL** keyboard scheme
(⌘I/K/J/L to move, ⌥ for word/line steps, ⌃ for line ends, +⇧ to select) with
**⌘9 to save**.

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

## API

The HTTP API **describes itself**:

```bash
curl localhost:3150/api/describe
```

That returns every route with its `method`, `path` and docstring — the handler
docstrings *are* the documentation, so the listing can never drift from the code.
It's unauthenticated and read-only.

### Machine clients are read-only by default

A token from `et.trn.auth/create-machine-token` is marked `:machine? true`. Such
a caller may **read** anything, but its **writes are dropped** — logged and
answered with `{"dropped":true}` — unless recording mode is on:

```bash
curl localhost:3150/api/recording-mode          # {"recording":false}
curl -X POST localhost:3150/api/recording-mode/toggle
```

So an agent can discover and explore the API with no risk of mutating data, and
you opt into writes deliberately. Human/browser tokens are unaffected.

### Rate limiting

All requests pass a global sliding window: **180/min in production, 720/min in
dev**, overridable via `RATE_LIMIT_MAX_REQUESTS` / `RATE_LIMIT_WINDOW_SECONDS`.
Over the limit returns a bare `429`.

## Architecture

- `src/clj/et/trn` — ring/compojure backend, next.jdbc + honeysql over SQLite,
  ragtime migrations in `resources/migrations/net/et/trn`.
- `src/cljs/et/trn/ui` — reagent SPA (`core`, `state`, `views/trainings`,
  `views/sessions`).
- `resources/public/treina` — `index.html`, `styles.css`, `css/` (lila theme
  from tracker's `base.css`), shadow output under `js/`.

Entities live in parallel `db/<entity>.clj` + `server/<entity>_handler.clj`
pairs — copy that pair to add a new one.

### YouTube polling

Modelled on `rhizome`: each channel's public Atom feed
(`/feeds/videos.xml?channel_id=…`) is read on a timer — no API key, no quota.
That feed already carries every entry's title and publish date, so a video is
named the moment it appears; nothing is fetched later.

`et.trn.youtube.feed` does the talking (JDK HTTP client, `clojure.xml`) and
resolves a UC… id from whatever you paste — id, channel URL, `@handle` or a
video URL. For a channel page it reads the feed link / `externalId` rather than
the first `"channelId"` in the payload, which is often a *recommended* channel.

`et.trn.youtube.poll` runs the timer: every `YOUTUBE_POLL_MINUTES` (default 15,
`0` disables it) it walks every channel of every user and records unseen videos.
A failing feed is logged and skipped. `POST /api/youtube/poll` triggers a pass
immediately.

### Program history

`program` holds the current text, `program_history` the superseded ones keyed
`(program_id, version)`. A save archives the outgoing text first, so versions
only ever grow and never change afterwards. `GET /api/program/history` returns
them newest-first with the current text as the newest entry (`current: true`).
The diff viewer (`ui/components/diff.cljs`) compares consecutive entries with
CodeMirror's merge extension, unified or split. The model and the viewer follow
`rhizome`'s.
