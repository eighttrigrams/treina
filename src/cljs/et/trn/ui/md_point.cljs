(ns et.trn.ui.md-point
  "A point in the *rendering* answered as a point in the *text*: zoo's
  `et.zoo.ui.md-point`, as tracker carries it (`et.tr.ui.md-point`), kept in
  step with it and with cookbook's (`et.cb.ui.md-point`).

  Zoo and cookbook ask it about a click, to put a caret where the finger was.
  Treina, like tracker, asks it about the two ends of a selection, to copy the
  markdown that was selected (`ui.md-copy`). The question is the same one,
  asked twice.

  **The block is told rather than worked out.** `ui.markdown/render` draws each
  top-level block in a wrapper that says where in the text the block starts
  (`data-start`), and that offset is marked's own: the lexer's tokens carry
  their `raw`, and counting along them is the source position. Nothing here
  parses markdown.

  **Inside the block, the place is *aligned*, not parsed**, which is zoo's one
  real decision, kept whole. A rendering is its source with the markers taken
  out: `**bold**` renders `bold`, `- item` renders `item`, `[text](url)` renders
  `text`. So the rendered characters are the source's characters in order, with
  some missing: a subsequence. Walk both from the left and the source pointer
  lands where the point was.

  Whitespace-only text nodes outside a `<pre>` are kept out of the rendered
  side. marked writes `<ul>\\n<li>`, and that newline is layout, not a
  character from the source.

  A fenced block is aligned from its second line, so the fence's own letters
  (```clojure) cannot claim the code's first character.

  What the three copies add to zoo's: `column` passes over a rendered character
  the source does not have (see there), `block-offset` answers the offset that
  `block-point` turns into a line and a column, and `boundary-offset` reads a
  selection's boundary point, which may be an element and a child index (a
  triple-click leaves one) where a click is always a text node. Zoo's `at`,
  `caret` and `nearest` answer a click at a coordinate, which treina has no use
  for, so they were not brought over."
  (:require [clojure.string :as str]))

;; --- pure ----------------------------------------------------------------------

(defn column
  "Which index of `src` the rendered character at `offset` came from. zoo's, with
  one addition.

  Greedy from the left. The answer is where the source character that *produced*
  `rendered[offset]` stands, which is why the walk runs on past the last match until
  it finds that character. `offset` at the end of the rendering is the end of what was
  matched, before whatever markers close it. Never past the end of `src`, and a
  rendering that shares no characters with its source lands at the start.

  **The addition: a rendered character that occurs nowhere in the rest of the
  source is the renderer's, and is passed over.** zoo's walk takes every rendered
  character to be one of the source's. marked mostly obliges, but not always:
  `&copy;` renders `©`. The walk then ran off the end of the source looking for
  it, and every point after it in the block answered the block's end. Passing it
  over changes nothing where zoo's walk found a match, and answers where it
  could not: a point *on* such a character lands where the source pointer
  stands."
  [src rendered offset]
  (let [sn (count src) rn (count rendered)]
    (loop [si 0 ri 0]
      (cond
        (>= si sn) si
        (>= ri rn) si
        (not= (nth src si) (nth rendered ri))
        (if (neg? (.indexOf src (nth rendered ri) si))
          (if (>= ri offset) si (recur si (inc ri)))
          (recur (inc si) ri))
        (>= ri offset) si
        :else (recur (inc si) (inc ri))))))

(defn line-col
  "`{:line :col}`, both 0-based, of character `offset` in `text`."
  [text offset]
  (let [before (subs text 0 (max 0 (min offset (count text))))
        line (count (re-seq #"\n" before))
        nl (str/last-index-of before "\n")]
    {:line line :col (if nl (- (count before) nl 1) (count before))}))

(defn body-start
  "Where a block's aligned source begins: past the fence line for a fenced block,
  at the start for anything else."
  [{:keys [raw fenced?]}]
  (if fenced?
    (if-let [nl (str/index-of raw "\n")] (inc nl) (count raw))
    0))

(defn block-offset
  "The offset in the text of the rendered character at `offset` of `block`, where
  `rendered` is the block's rendered text as `rendered-text` reads it.

  `block-point` before it was split in two, so the offset is there for a caller
  that wants to slice the text rather than place a caret. `raw` is what is
  aligned against, so a caller may hand in a `raw` with characters blanked out
  (same length, so offsets hold), and `ui.md-copy` does: see `md-copy/masked`."
  [{:keys [start raw] :as block} rendered offset]
  (let [skip (body-start block)]
    (+ start skip (column (subs raw skip) rendered offset))))

(defn block-point
  "The point in `text` for the rendered character at `offset` of `block`.
  `text` is the same `\\n` text `markdown/blocks` was given."
  [text block rendered offset]
  (line-col text (block-offset block rendered offset)))

;; --- the DOM ------------------------------------------------------------------

(defn- skipped?
  "A text node that is not one of the author's characters (see the ns docstring)."
  [^js node]
  (and (str/blank? (.-nodeValue node))
       (nil? (some-> (.-parentElement node) (.closest "pre")))))

(defn text-nodes
  "The block's text nodes in document order, less the skipped ones."
  [^js el]
  (let [walker (.createTreeWalker js/document el 4)]   ; NodeFilter.SHOW_TEXT
    (loop [out []]
      (if-let [n (.nextNode walker)]
        (recur (if (skipped? n) out (conj out n)))
        out))))

(defn rendered-text
  "The block's rendered characters: what `column` aligns against."
  [el]
  (apply str (map #(.-nodeValue ^js %) (text-nodes el))))

(defn boundary-offset
  "How many of `el`'s rendered characters stand before the boundary point
  (`node`, `offset`), counted over the same nodes `rendered-text` reads.

  zoo's `text-before` asks the same of a click, which always lands in a text
  node. A selection's ends need not: a triple-click ends at an element and a
  child index, often the start of the next block. So this asks the DOM to
  compare, through a collapsed `Range` at the point, which is right for a text
  node and for an element alike. A text node that ends at or before the point
  counts whole. One that starts at or after it ends the count. The one the
  point is inside counts up to the point."
  [^js el ^js node offset]
  (let [at (doto (.createRange js/document) (.setStart node offset))]
    (loop [[^js n & more] (text-nodes el) acc 0]
      (cond
        (nil? n) acc
        (<= (.comparePoint at n (.-length n)) 0) (recur more (+ acc (.-length n)))
        (>= (.comparePoint at n 0) 0) acc
        :else (+ acc (if (identical? n node) offset 0))))))
