(ns et.trn.ui.md-copy
  "Copying rendered markdown puts its markdown on the clipboard: tracker's
  `et.tr.ui.md-copy`, brought to treina whole and kept in step with it. Treina
  renders markdown exactly as tracker does (a bare `marked`, task boxes as an
  `<input>`), so nothing below differs from tracker's but the names and this
  docstring.

  Select a stretch of rendered session notes, a training's description or the
  program, press Cmd+C (Ctrl+C off the Mac) or Option+C, and what lands on the
  clipboard is the markdown it was rendered from: `[text](url)` where the page
  showed a link, `**…**` where it showed bold, `- ` lines where it showed a
  list. As `text/plain` and nothing else, so every place it is pasted into gets
  the same thing.

  **How a selection becomes source.** Both ends of the selection go through
  `ui.md-point`, which is zoo's: the block wrapper says where its block starts in
  the text (`markdown/render`), and the place inside the block is found by
  aligning the rendered characters against the block's source as a subsequence.
  That gives two offsets, `a` and `b`, and the copy is the text between them,
  **widened to take in the markers of any construct cut at an edge**. The markers
  are read off marked's tokens, which carry their `raw`, so they are always the
  author's own: `__` stays `__`, `*` stays `*`.

  **The string md-point aligns against is what the rendering shows**, position
  for position (`analyse`): markup is `\\u0000`, an entity is its decoded
  character and then `\\u0000`, a tab marked turned into spaces is a space. Same
  length, so every offset holds. **And every copy is checked before it is
  written** (`round-trips?`): rendered as the app renders it, it has to show the
  text that was selected, whitespace aside, or the browser's own copy wins.

  **The rules**, which tracker's `md-copy-test` holds one by one, and which
  `test/browser/md-copy-checks.js` runs against this namespace in the page:

  1. Only a selection inside **one** rendered container (one notes field, one
     description) is taken, with focus outside any field. Anything else is the
     browser's, unchanged: two cards, markdown and the buttons around it.
  2. A selection of **all** of a container's text copies its source verbatim.
  3. An **inline construct cut at an edge** gets its own markers: the opening
     ones before the slice, outermost first, the closing ones after it,
     innermost first. `important` out of `**very important**` copies as
     `**important**`, and part of a link's text keeps its `(url)`. Whitespace
     at the cut goes outside the markers, and an entity or an escape is one
     thing to an edge.
  4. **Inside one block**, what comes out is inline markdown. A word from a
     heading or a list item comes without `## ` or `- `, and lines of code come
     plain, without the fence. A block selected **whole** brings its markup:
     `## Heading`, `- item`, `> quote`, the fence. **Raw HTML** is taken whole
     or not at all.
  5. **Across blocks**, the slice is verbatim, so every line keeps its markers,
     indentation and blank lines. The first line gets its block prefix back
     (`- `, `> `, `## `, `1. `), a fenced block cut at an edge gets its fence,
     and a setext heading its underline. The first line's indentation comes
     off, with every line after it that shares it (`dedent`). Code keeps its
     indentation.
  6. **Tables**: inside one cell, the cell's inline markdown. Across cells, the
     header and delimiter rows plus every row the selection touches, so it is
     still a table.

  **Option+C**, outside a field and with something selected, always copies:
  the markdown, or the browser's own copy when there is none (`on-key-down`)."
  (:require [clojure.string :as str]
            [et.trn.ui.markdown :as markdown]
            [et.trn.ui.md-point :as md-point]
            ["marked" :refer [marked Lexer]]))

;; --- alignment of a token's text in its raw ----------------------------------------

(defn- lines
  "`[start end]` of every line of `s`, the newline excluded."
  [s]
  (loop [i 0 out []]
    (let [nl (.indexOf s "\n" i)]
      (if (neg? nl)
        (conj out [i (count s)])
        (recur (inc nl) (conj out [i nl]))))))

(defn- greedy
  "md-point's walk, kept for every character instead of one: where each
  character of `text` stands in `raw`, from `from` on. A character that can no
  longer be placed stands at the end."
  [raw text from]
  (let [rn (count raw) tn (count text)]
    (loop [si from ti 0 out (transient [])]
      (cond
        (= ti tn) (persistent! out)
        (>= si rn) (persistent! (reduce conj! out (repeat (- tn ti) rn)))
        (= (.charAt raw si) (.charAt text ti)) (recur (inc si) (inc ti) (conj! out si))
        :else (recur (inc si) ti out)))))

(defn- tail-map
  "Where each character of `tline` stands in `rline`, when `tline` is `rline`'s
  tail, or nil. A tab in `rline` matches the run of spaces marked made of it
  (one to four): marked turns `- a\\tb` into the text `a b`, and an indenting
  tab into two spaces. Walked from the right, because what was taken off is
  at the start of the line."
  [rline tline]
  (loop [ri (dec (count rline)) ti (dec (count tline)) out ()]
    (cond
      (neg? ti) (vec out)
      (neg? ri) nil
      (= (.charAt rline ri) (.charAt tline ti)) (recur (dec ri) (dec ti) (conj out ri))
      (and (= "\t" (.charAt rline ri)) (= " " (.charAt tline ti)))
      (let [k (loop [k 0]
                (if (and (< k 4) (>= (- ti k) 0) (= " " (.charAt tline (- ti k))))
                  (recur (inc k))
                  k))]
        (recur (dec ri) (- ti k) (into out (repeat k ri))))
      :else nil)))

(defn align
  "Where each character of `text` stands in `raw`, for a token whose `text` is
  its `raw` with prefixes taken off: a list item without its marker and its
  continuation indent, a quote without its `>`s, a code block without its fence
  and indentation.

  Line by line first, each line of `text` being the tail of its line in `raw`
  (from `from-line` on), because what was taken off is always at the start of
  a line, a tab standing for the spaces marked made of it (`tail-map`). The
  greedy walk is the fallback for the cases that do not fit, and a little less
  exact."
  ([raw text] (align raw text 0))
  ([raw text from-line]
   (let [rl (lines raw) tl (lines text) tn (count text)]
     (or (when (<= (+ from-line (count tl)) (count rl))
           (loop [j 0 out []]
             (if (= j (count tl))
               out
               (let [[ts te] (tl j)
                     [rs re] (rl (+ from-line j))]
                 (when-let [tm (tail-map (subs raw rs re) (subs text ts te))]
                   (recur (inc j)
                          (cond-> (into out (map #(+ rs %)) tm)
                            (< te tn) (conj re))))))))
         (greedy raw text (first (nth rl from-line [0 0])))))))

;; --- what the rendering shows: entities ---------------------------------------------

(def ^:private named-entities
  "The named entities a description is likely to carry, for where there is no
  DOM to ask (the node suite). In the browser the DOM decodes any of them."
  {"amp" "&" "lt" "<" "gt" ">" "quot" "\"" "apos" "'" "nbsp" "\u00a0"
   "copy" "©" "reg" "®" "trade" "™" "mdash" "—" "ndash" "–" "hellip" "…"
   "laquo" "«" "raquo" "»" "lsquo" "‘" "rsquo" "’" "ldquo" "“" "rdquo" "”"
   "middot" "·" "bull" "•" "times" "×" "divide" "÷" "deg" "°" "euro" "€"
   "pound" "£" "larr" "←" "rarr" "→" "uarr" "↑" "darr" "↓" "check" "✓"})

(defonce ^:private decoder (atom nil))

(defn decode-entity
  "What the browser shows for the entity `s` (`&lt;`, `&#38;`, `&#x26;`), or nil
  when it shows `s` itself. A detached `<textarea>` decodes without running or
  fetching anything; without a DOM the numeric forms and `named-entities` do."
  [s]
  (let [via-dom (when (exists? js/document)
                  (let [^js t (or @decoder (reset! decoder (.createElement js/document "textarea")))]
                    (set! (.-innerHTML t) s)
                    (.-value t)))
        decoded (or via-dom
                    (try
                      (if-let [[_ hex] (re-find #"^&#[xX]([0-9a-fA-F]+);$" s)]
                        (js/String.fromCodePoint (js/parseInt hex 16))
                        (if-let [[_ dec] (re-find #"^&#([0-9]+);$" s)]
                          (js/String.fromCodePoint (js/parseInt dec 10))
                          (some->> (re-find #"^&([A-Za-z][A-Za-z0-9]*);$" s) second named-entities)))
                      (catch :default _ nil)))]
    (when (and decoded (not= decoded s) (<= (count decoded) (count s))) decoded)))

(defn- at
  "`m` read at `i`, and one past its last entry for `i` at its end."
  [m i]
  (cond
    (< i (count m)) (nth m i)
    (seq m) (inc (peek m))
    :else 0))

;; --- inline constructs -------------------------------------------------------------

(defn- inline-nodes
  "The inline tokens as `{:type :s :e}` in the coordinates of the leaf's raw,
  plus `:cs`/`:ce` (where the content runs, between the markers) and
  `:children` for the constructs that have markers. Inline tokens' raws add up
  to their parent's text, so counting along them is exact, from `base`."
  [tokens base]
  (loop [ts (some-> tokens array-seq) o base out []]
    (if-let [^js t (first ts)]
      (let [raw (or (.-raw t) "")
            text (or (.-text t) "")
            n (count raw)
            s o
            e (+ o n)
            node (case (.-type t)
                   ("strong" "em" "del")
                   (let [k (quot (- n (count text)) 2)]
                     {:type (.-type t) :s s :e e :cs (+ s k) :ce (- e k)
                      :children (inline-nodes (.-tokens t) (+ s k))})

                   "link"
                   (let [i (or (str/index-of raw text (if (str/starts-with? raw "[") 1 0)) 0)]
                     {:type "link" :s s :e e :cs (+ s i) :ce (+ s i (count text))
                      :children (inline-nodes (.-tokens t) (+ s i))})

                   "codespan"
                   (let [ticks (count (re-find #"^`+" raw))
                         i (str/index-of raw text ticks)]
                     (if i
                       {:type "codespan" :s s :e e :cs (+ s i) :ce (+ s i (count text))}
                       {:type "codespan" :s s :e e :cs (+ s ticks) :ce (max (+ s ticks) (- e ticks))}))

                   {:type (.-type t) :s s :e e})]
        (recur (rest ts) e (conj out node)))
      out)))

(def ^:private constructs #{"strong" "em" "del" "link" "codespan"})
(def ^:private atoms
  "Tokens that are one thing: an edge never cuts them, it takes them whole. An
  escape (`\\*`) has to keep its backslash or it stops being one."
  #{"escape" "image" "html"})

(defn- containing [nodes i]
  (some #(when (and (< (:s %) i) (< i (:e %))) %) nodes))

(defn- open-cuts
  "The constructs the start `la` cuts, as `[opening-markers la']`: their opening
  markers, outermost first, and the start moved off any marker it fell inside."
  [raw nodes la]
  (loop [nodes nodes opens []]
    (if-let [n (containing nodes la)]
      (cond
        (atoms (:type n)) [opens (:s n)]
        (not (constructs (:type n))) [opens la]
        (< la (:cs n)) [opens (:s n)]
        (>= la (:ce n)) [opens (:e n)]
        :else (recur (:children n) (conj opens (subs raw (:s n) (:cs n)))))
      [opens la])))

(defn- close-cuts
  "The constructs the end `lb` cuts, as `[closing-markers lb']`, innermost first."
  [raw nodes lb]
  (loop [nodes nodes closes ()]
    (if-let [n (containing nodes lb)]
      (cond
        (atoms (:type n)) [closes (:e n)]
        (not (constructs (:type n))) [closes lb]
        (> lb (:ce n)) [closes (:e n)]
        (<= lb (:cs n)) [closes (:s n)]
        :else (recur (:children n) (conj closes (subs raw (:ce n) (:e n)))))
      [closes lb])))

(defn- inline-markup
  "The positions of `nodes` that are markers and never render as text."
  [nodes]
  (mapcat (fn [{:keys [type s e cs ce children]}]
            (cond
              (constructs type) (concat (range s cs) (range ce e) (inline-markup children))
              (= "escape" type) [s]
              (#{"image" "html"} type) (range s e)
              :else nil))
          nodes))

(defn- text-ranges
  "`[s e]` of every plain text token under `nodes`: where an entity renders as
  the character it names (in a code span it renders as itself)."
  [nodes]
  (mapcat (fn [{:keys [type s e children]}]
            (if (= "text" type) [[s e]] (text-ranges children)))
          nodes))

(defn- entity-shows
  "`[position shown start end]` for the entities in `raw`'s text tokens: the decoded
  character at the entity's first position, `\\u0000` on the rest. The
  rendering has one `<` where the source has `&lt;`, and aligning `&lt;`
  against it sent the walk off to the next `<` in the block."
  [raw nodes]
  (mapcat (fn [[s e]]
            (let [re (js/RegExp. "&(?:#[0-9]{1,7}|#[xX][0-9a-fA-F]{1,6}|[A-Za-z][A-Za-z0-9]{1,31});" "g")
                  seg (subs raw s e)]
              (loop [out []]
                (if-let [^js m (.exec re seg)]
                  (let [ent (aget m 0)
                        p (+ s (.-index m))]
                    (recur (if-let [ch (decode-entity ent)]
                             (into out (map (fn [k] [(+ p k) (if (< k (count ch)) (nth ch k) "\u0000")
                                                     p (+ p (count ent))]))
                                   (range (count ent)))
                             out)))
                  out))))
          (text-ranges nodes)))

;; --- leaves: the blocks that hold text ----------------------------------------------

(defn- table-cells
  "Every cell as `{:row :c0 :c1 :inline}`, found along its line of the raw. Row 0
  is the header, which is line 0; body row `i` is line `i + 1`, the delimiter
  row being line 1."
  [raw ^js t]
  (let [ls (lines raw)
        rows (cons (.-header t) (array-seq (.-rows t)))]
    (vec
     (mapcat
      (fn [row-i cells]
        (let [[ls0 le] (nth ls (if (zero? row-i) 0 (inc row-i)) [0 0])]
          (loop [[^js c & more] (array-seq cells) cur ls0 out []]
            (if-not c
              out
              (let [text (or (.-text c) "")
                    i (let [i (str/index-of raw text cur)] (when (and i (<= (+ i (count text)) le)) i))
                    c0 (or i cur)
                    c1 (if i (+ i (count text)) cur)]
                (recur more c1 (conj out {:row row-i :c0 c0 :c1 c1
                                          :inline (when i (inline-nodes (.-tokens c) c0))})))))))
      (range) rows))))

(defn- html-tags
  "The positions of `raw` inside its tags, `<` to `>`: what a raw HTML block does
  not show."
  [raw]
  (loop [i 0 in? false out (transient [])]
    (if (>= i (count raw))
      (persistent! out)
      (let [ch (.charAt raw i)]
        (cond
          (and (not in?) (= "<" ch)) (recur (inc i) true (conj! out i))
          (and in? (= ">" ch)) (recur (inc i) false (conj! out i))
          in? (recur (inc i) true (conj! out i))
          :else (recur (inc i) false out))))))

(defn- leaf
  "A block that holds text, in the coordinates of its own `raw`: `:c0`/`:c1` say
  where its text runs, and `:pm` where each of its characters stands in the
  source."
  [^js t raw pm]
  (let [type (.-type t)
        text (or (.-text t) "")
        base {:type type :raw raw :pm pm}]
    (case type
      ("paragraph" "text")
      (assoc base :c0 0 :c1 (count text) :inline (inline-nodes (.-tokens t) 0))

      "heading"
      (let [after-hashes (count (re-find #"^ {0,3}#{1,6}[ \t]*" raw))
            i (or (str/index-of raw text after-hashes) (str/index-of raw text) 0)]
        (assoc base :c0 i :c1 (+ i (count text)) :inline (inline-nodes (.-tokens t) i)))

      "code"
      (let [fenced? (not= "indented" (.-codeBlockStyle t))
            m (align raw text (if fenced? 1 0))
            body (if fenced? (first (nth (lines raw) 1 [(count raw)])) 0)]
        (assoc base :fenced? fenced? :text text :code-map m
               :c0 (if (seq m) (first m) body) :c1 (if (seq m) (inc (peek m)) body)))

      "table"
      (assoc base :cells (table-cells raw t) :lines (lines raw))

      "html"
      ;; Its rendered text is what lies outside the tags; :c0/:c1 bound that,
      ;; so a selection of all of its words takes the block whole, tags and all.
      (let [shown (remove (set (html-tags raw)) (range (count raw)))
            text (remove #(re-find #"\s" (str (get raw %))) shown)]
        (assoc base :c0 (or (first text) 0) :c1 (if (seq text) (inc (last text)) 0)))

      nil)))

(defn- leaf-markup
  "The positions of a leaf's raw that never render as text."
  [{:keys [type raw c0 c1 inline code-map cells fenced?]}]
  (let [n (count raw)]
    (case type
      ("paragraph" "text" "heading")
      (concat (range 0 c0) (range c1 n) (inline-markup inline))

      "code"
      ;; Two newlines stay unmasked. The fence line's, because md-point finds
      ;; where a fenced block's code begins by the first newline of the raw it
      ;; is handed (`md-point/body-start`), and that raw is this masked one. And
      ;; the one before the closing fence, because marked ends a code block's
      ;; rendered text with a newline of its own, which is that one.
      (let [fence-nl (when fenced? (str/index-of raw "\n"))
            hit (cond-> (set code-map)
                  fence-nl (conj fence-nl)
                  (= "\n" (get raw c1)) (conj c1))]
        (remove hit (range n)))

      "html" (html-tags raw)

      "table"
      (let [hit (set (mapcat #(range (:c0 %) (:c1 %)) cells))]
        (concat (remove hit (range n)) (mapcat #(inline-markup (:inline %)) cells)))

      nil)))

(defn- leaf-shows
  "`[position shown]` for a leaf: its markup as `\\u0000`, then what its
  entities render as."
  [{:keys [raw inline cells] :as l}]
  (concat (map (fn [i] [i "\u0000"]) (leaf-markup l))
          (entity-shows raw (concat inline (mapcat :inline cells)))))

(defn- collect
  "The leaves under `tokens`, in document order, each with its `:pm`, and in
  `shown` what the rendering shows at each source position where that is not
  the source's own character (see `analyse`).

  `tokens`' raws add up to `coord`, and `pm` says where each character of
  `coord` stands in the source. Through a list item or a quote the coordinates
  change, because their children were lexed from their text with the prefixes
  off: `align` maps that text back into their raw, and `pm` is composed with
  it. A raw is taken as `coord`'s slice rather than the token's own, which
  marked 18 garbles in one case while keeping its length (see
  `markdown/blocks`)."
  [tokens coord pm shown spans]
  (loop [ts (some-> tokens array-seq) o 0 out []]
    (if-let [^js t (first ts)]
      (if (= "checkbox" (.-type t))
        ;; A task item's `[ ] ` is a token of its own, outside the item's text.
        (recur (rest ts) o out)
        (let [n (count (or (.-raw t) ""))
              raw (subs coord (min o (count coord)) (min (+ o n) (count coord)))
              tpm (fn [i] (pm (+ o i)))
              found (case (.-type t)
                      "list"
                      (collect (.-items t) raw tpm shown spans)

                      ("list_item" "blockquote")
                      (let [text (or (.-text t) "")
                            m (align raw text)
                            ;; A task item renders its box and then a space of
                            ;; the renderer's own, which is the source's space
                            ;; after `[ ]`: left unmasked so it has a match.
                            box-space (when (and (.-task t) (seq m)
                                                 (#{" " "\t"} (get raw (dec (first m)))))
                                        (dec (first m)))
                            hit (cond-> (set m) box-space (conj box-space))]
                        (doseq [i (range (count raw)) :when (not (hit i))]
                          (aset shown (tpm i) "\u0000"))
                        ;; A tab marked turned into spaces shows as a space.
                        (doseq [j (range (count text))
                                :let [r (nth m j nil)]
                                :when (and r (= " " (.charAt text j)) (= "\t" (.charAt raw r)))]
                          (aset shown (tpm r) " "))
                        (collect (.-tokens t) text (fn [i] (tpm (at m i))) shown spans))

                      (when-let [l (leaf t raw (mapv tpm (range (count raw))))]
                        (doseq [[i ch es ee] (leaf-shows l)]
                          (aset shown (nth (:pm l) i) ch)
                          ;; An entity is one thing to an edge, as an escape is.
                          (when (and es (= i es))
                            (.push spans [(nth (:pm l) es) (inc (nth (:pm l) (dec ee)))])))
                        [l]))]
          (recur (rest ts) (+ o n) (into out (remove #(empty? (:pm %))) found))))
      out)))

(defn- alignment-string
  "`src` as the rendering shows it, position for position: `shown`'s character
  where it has one, the source's own elsewhere. Same length, so every offset
  into it is an offset into `src`."
  [src shown]
  (apply str (map-indexed (fn [i c] (or (aget shown i) c)) src)))

(defn analyse
  "Everything a copy needs to know about `src`, lexed once:
  - `:leaves`, the blocks that hold text (see `leaf`), in document order
  - `:blocks`, the top-level blocks as `markdown/blocks` numbers them,
    `{:start :raw :fenced?}`, whose `:raw` is the *alignment string* md-point
    aligns against: what the rendering shows at each position. Markup (a link's
    tail, emphasis markers, list and quote prefixes, table pipes) is `\\u0000`;
    an entity is its decoded character and then `\\u0000`; a tab marked turned
    into spaces is a space. Same length as the source, so offsets hold.
  - `:src` itself.
  `inline?` for inline-only markdown (a title in tracker; treina has none yet)."
  [src inline?]
  (let [n (count src)
        shown (make-array n)
        spans #js []]
    (if inline?
      (let [l {:type "paragraph" :raw src :pm (vec (range n)) :c0 0 :c1 n
               :inline (inline-nodes (.lexInline Lexer src (.-defaults marked)) 0)}]
        (doseq [i (inline-markup (:inline l))] (aset shown i "\u0000"))
        (doseq [[i ch es ee] (entity-shows src (:inline l))]
          (aset shown i ch)
          (when (= i es) (.push spans [es ee])))
        {:src src :leaves [l] :entities (vec spans)
         :blocks [{:start 0 :raw (alignment-string src shown)}]})
      (let [tokens (.lexer marked src)
            leaves (collect tokens src identity shown spans)
            aligned (alignment-string src shown)
            blocks (loop [[^js t & more] (array-seq tokens) o 0 out []]
                     (if-not t
                       out
                       (let [k (count (or (.-raw t) ""))]
                         (recur more (+ o k)
                                (conj out {:start o
                                           :raw (subs aligned o (min n (+ o k)))
                                           :fenced? (and (= "code" (.-type t))
                                                         (not= "indented" (.-codeBlockStyle t)))})))))]
        {:src src :leaves leaves :blocks blocks :entities (vec spans)}))))

;; --- from two offsets to markdown -----------------------------------------------------

(defn- local-index
  "The first index of the leaf whose source position is at or after `pos`."
  [pm pos]
  (or (first (keep-indexed (fn [i p] (when (>= p pos) i)) pm)) (count pm)))

(defn- after
  "The source position just after the leaf's local index `k - 1`."
  [pm k]
  (if (pos? k) (inc (nth pm (dec k))) (first pm)))

(defn- line-start [src p]
  (if (pos? p) (inc (or (str/last-index-of src "\n" (dec p)) -1)) 0))

(defn- line-of [raw i]
  (count (re-seq #"\n" (subs raw 0 (min i (count raw))))))

(defn- trim-newlines [s] (str/replace s #"\n+$" ""))

(defn- lead-ws [line] (re-find #"^[ \t]*" line))

(defn- dedent
  "Take the first line's indentation off it, and off every line after it that
  starts with the same indentation, up to the first one that does not. A
  nested item copied on its own reads as an item; a selection from deep in a
  list out to its top level keeps its nested lines siblings, and its first line
  does not become an indented code block where it is pasted (four columns,
  which one tab is, would make it one). Lines after the run keep theirs.

  Never applied to code, whose indentation is its content (see `across` and
  `within`)."
  [s]
  (let [ls (str/split s #"\n" -1)
        lead (lead-ws (first ls))]
    (if (empty? lead)
      s
      (let [[run more] (split-with #(or (str/blank? %) (str/starts-with? % lead)) ls)]
        (str/join "\n" (concat (map #(if (str/blank? %) % (subs % (count lead))) run) more))))))

(defn- outside-markers
  "Whitespace at a cut edge goes outside the markers: `**does** ` and not
  `**does **`, which CommonMark reads as two literal stars. `opens` and `closes`
  are the markers the edges brought; `mid` is the selected source between them."
  [opens mid closes]
  (let [lead (when (seq opens) (re-find #"^\s+" mid))
        mid (subs mid (count lead))
        trail (when (seq closes) (re-find #"\s+$" mid))
        mid (subs mid 0 (- (count mid) (count trail)))]
    (str lead (apply str opens) mid (apply str closes) trail)))

(defn- start-side
  "Where a selection that starts in `leaf` at `a` and runs on past it starts in
  the source, and what has to go in front: the block prefix of the first line,
  and the opening markers of what the start cuts. nil when the start cuts into
  raw HTML, which is taken whole or not at all."
  [src {:keys [type raw pm c0 inline lines] :as _leaf} a]
  (let [la (local-index pm a)
        lead (fn [p] (subs src (line-start src (first pm)) p))]
    (case type
      "code" {:a (max a (at pm c0)) :prefix (lead (at pm c0))}
      "table" (let [row (line-of raw la)]
                (if (<= row 1)
                  {:a (line-start src (first pm)) :prefix "" :table? true}
                  {:a (nth pm (first (nth lines row)))
                   :prefix (lead (nth pm (first (nth lines 2))))
                   :table? true}))
      "html" (when (<= la c0) {:a (first pm) :prefix (lead (first pm))})
      (let [[opens la'] (open-cuts raw inline la)
            a' (at pm la')
            ;; Whitespace at the cut goes in front of the markers (`outside-markers`).
            ws (when (seq opens) (re-find #"^[ \t]+" (subs src a')))]
        {:a (+ a' (count ws))
         :prefix (str (lead (if (= "heading" type) (at pm c0) (first pm)))
                      ws
                      (apply str opens))}))))

(defn- end-side
  "Where a selection that ends in `leaf` at `b`, having started before it, ends in
  the source, and what has to follow: the closing markers of what the end cuts,
  a fence, a setext underline. nil when the end cuts into raw HTML."
  [src {:keys [type raw pm c1 inline lines] :as _leaf} b]
  (let [lb (inc (local-index pm (dec b)))]
    (case type
      "code" {:b (min b (after pm c1)) :suffix (trim-newlines (subs raw c1))}
      "table" (let [row (max 1 (line-of raw (dec lb)))]
                {:b (after pm (second (nth lines row))) :suffix "" :table? true})
      "html" (when (>= lb c1) {:b (after pm (count (str/trimr raw))) :suffix ""})
      (let [[closes lb'] (close-cuts raw inline lb)
            b' (after pm lb')
            ws (when (seq closes) (re-find #"[ \t]+$" (subs src (first pm) b')))]
        {:b (- b' (count ws))
         :suffix (str (apply str closes)
                      ws
                      (when (= "heading" type) (trim-newlines (subs raw c1))))}))))

(defn- across
  "Rule 5: from `a` in `start-leaf` to `b` in `end-leaf`, verbatim in between."
  [src start-leaf end-leaf a b]
  (let [start (start-side src start-leaf a)
        end (end-side src end-leaf b)]
    (when (and start end)
      (let [{a' :a :keys [prefix]} start
            {b' :b :keys [suffix]} end]
        {:md (cond-> (str prefix (subs src a' (max a' b')) suffix)
               (not= "code" (:type start-leaf)) dedent)
         :table? (boolean (or (:table? start) (:table? end)))}))))

(defn- cell-at [cells i]
  (some #(when (and (<= (:c0 %) i) (< i (:c1 %))) %) cells))

(defn- within
  "Rules 3, 4 and 6 inside one leaf, and raw HTML whole or not at all."
  [src {:keys [type raw pm c0 c1 inline cells text code-map] :as leaf} a b]
  (let [la (local-index pm a)
        lb (inc (local-index pm (dec b)))
        inline-slice (fn [nodes la lb]
                       (let [[opens la'] (open-cuts raw nodes la)
                             [closes lb'] (close-cuts raw nodes lb)]
                         {:md (outside-markers opens (subs raw la' (max la' lb')) closes)}))]
    (cond
      (= "table" type)
      (let [cell (cell-at cells la)]
        (if (and cell (identical? cell (cell-at cells (dec lb))))
          (inline-slice (:inline cell) la lb)
          (across src leaf leaf a b)))

      (and (<= la c0) (>= lb c1))
      {:md (cond-> (trim-newlines (subs src (line-start src (first pm)) (after pm (count raw))))
             (not= "code" type) dedent)}

      (= "html" type) nil

      (= "code" type)
      (let [i0 (or (first (keep-indexed #(when (>= %2 la) %1) code-map)) (count text))
            i1 (or (first (keep-indexed #(when (>= %2 lb) %1) code-map)) (count text))]
        {:md (subs text i0 (max i0 i1)) :plain? true})

      :else (inline-slice inline la lb))))

(defn between*
  "`between`, saying how it got there, for the round-trip check
  (`markdown-of-range`): `{:md s}`, plus `:table? true` when a table brought its
  header or completed a row (rule 6, the one widening that adds rendered text)
  and `:plain? true` when the copy is literal code rather than markdown. nil for
  nothing, or for a cut into raw HTML."
  [{:keys [src leaves entities]} a b]
  (when (< a b)
    (let [;; An edge inside an entity moves to its boundary: `&lt;` is one
          ;; character on the page, and a cut through it would copy `&l`.
          a (or (some (fn [[s e]] (when (< s a e) s)) entities) a)
          b (or (some (fn [[s e]] (when (< s b e) e)) entities) b)
          start-leaf (some #(when (< a (after (:pm %) (count (:pm %)))) %) leaves)
          end-leaf (last (filter #(> b (first (:pm %))) leaves))]
      (when (and start-leaf end-leaf)
        (let [a (max a (first (:pm start-leaf)))
              b (min b (after (:pm end-leaf) (count (:pm end-leaf))))]
          (when (< a b)
            (if (identical? start-leaf end-leaf)
              (within src start-leaf a b)
              (across src start-leaf end-leaf a b))))))))

(defn between
  "The markdown for the source between offsets `a` and `b` (exclusive) of an
  `analyse`d text, by the rules in the ns docstring, or nil for nothing."
  [analysis a b]
  (:md (between* analysis a b)))

;; --- the selection ---------------------------------------------------------------------

(defn- container-of [^js node]
  (some-> (if (= 1 (.-nodeType node)) node (.-parentElement node))
          (.closest "[data-md-src]")))

(defn- blank-between?
  "Whether nothing but whitespace stands between two boundary points."
  [^js sn so ^js en eo]
  (let [r (.createRange js/document)]
    (.setStart r sn so)
    (.setEnd r en eo)
    (str/blank? (.toString r))))

(defn- clip
  "The selection's range as `[container sn so en eo]` inside one container, or nil.

  A triple-click selects a paragraph by ending the range at the start of
  whatever follows it, which for the last paragraph of a description is outside
  the description. So an end (or start) outside the container still counts
  when only whitespace lies between it and the container."
  [^js r]
  (let [sn (.-startContainer r) so (.-startOffset r)
        en (.-endContainer r) eo (.-endOffset r)
        cs (container-of sn)
        ce (container-of en)]
    (cond
      (and cs (identical? cs ce)) [cs sn so en eo]
      (and cs (blank-between? cs (.-length (.-childNodes cs)) en eo))
      [cs sn so cs (.-length (.-childNodes cs))]
      (and ce (blank-between? sn so ce 0)) [ce ce 0 en eo]
      :else nil)))

(defn- block-els [^js container]
  (if (.hasAttribute container "data-start")
    [container]
    (vec (array-seq (.querySelectorAll container ".markdown-block[data-start]")))))

(defn- copy-of-range
  "What `between*` makes of a DOM range, with its container and the range's ends
  inside it (`:ends`) and whether the container is inline (`:inline?`), or nil
  when the range is not one this namespace takes (rule 1) or holds no text."
  [^js r]
  (when-let [[^js container sn so en eo] (clip r)]
    (let [src (or (.getAttribute container "data-md-src") "")
          inline? (.hasAttribute container "data-md-inline")
          els (block-els container)
          texts (mapv md-point/rendered-text els)
          start (first (keep-indexed (fn [i el]
                                       (let [off (md-point/boundary-offset el sn so)]
                                         (when (< off (count (texts i))) [i off])))
                                     els))
          end (last (keep-indexed (fn [i el]
                                    (let [off (md-point/boundary-offset el en eo)]
                                      (when (pos? off) [i off])))
                                  els))
          texty (keep-indexed (fn [i t] (when (seq t) i)) texts)
          found (when (and start end (<= (first start) (first end)))
                  (if (and (= start [(first texty) 0])
                           (= end [(last texty) (count (texts (last texty)))]))
                    {:md src}
                    (let [{:keys [blocks] :as analysis} (analyse src inline?)
                          block-of (fn [i]
                                     (let [s (js/parseInt (.getAttribute (els i) "data-start") 10)]
                                       (some #(when (= s (:start %)) %) blocks)))
                          [si soff] start
                          [ei eoff] end
                          sb (block-of si)
                          eb (block-of ei)]
                      (when (and sb eb)
                        (between* analysis
                                  (md-point/block-offset sb (texts si) soff)
                                  (inc (md-point/block-offset eb (texts ei) (dec eoff))))))))]
      (when found
        (assoc found :inline? inline? :ends [container sn so en eo])))))

(defn- squash [s] (str/replace (or s "") #"\s+" ""))

(defn round-trips?
  "Whether a copy renders as what was selected: the check that lets a wrong
  mapping fall back to the browser's copy instead of putting wrong text on the
  clipboard. `rendered` is the copy's markdown as the app renders it, as text;
  `selected` is the selection's text. Whitespace does not count, since the two
  lay it out differently.

  Equal, with two exceptions that are the design's own: a literal code copy
  (`:plain?`) is its own text, and a table that brought its header row or
  completed a row (`:table?`, rule 6) has to *contain* the selection. Anywhere
  else containment would let an overshoot through. Both empty is a match: a
  selection inside a cookbook diagram, which is taken whole, has no author text
  on either side. A blank copy is refused before this (`selection-markdown`)."
  [{:keys [md table? plain?]} rendered selected]
  (let [got (squash (if plain? md rendered))
        want (squash selected)]
    (if table? (str/includes? got want) (= got want))))

(defn- selection-text
  "The text a range covers, as `round-trips?` compares it, read off the live
  text nodes of `container` that the range touches. Two of cookbook's
  renderings are left out (no-ops elsewhere): a diagram block, whose picture is
  not the author's text and which is taken whole by design, and a task box
  (`span.task-box`), a marker as a bullet is. The copy's side leaves out the
  same (`markdown/rendered-text`). Live nodes and not a cloned fragment,
  because a range inside a diagram clones without the diagram's wrapper, and
  the wrapper is what says it is one."
  [^js container ^js sn so ^js en eo]
  (let [r (doto (.createRange js/document) (.setStart sn so) (.setEnd en eo))
        w (.createTreeWalker js/document container 4)]   ; NodeFilter.SHOW_TEXT
    (loop [out ""]
      (if-let [^js n (.nextNode w)]
        (recur (if (and (.intersectsNode r n)
                        (not (some-> (.-parentElement n) (.closest ".markdown-block-diagram, .task-box"))))
                 (let [v (.-nodeValue n)]
                   (str out (subs v (if (identical? n sn) so 0) (if (identical? n en) eo (count v)))))
                 out))
        out))))

(defn markdown-of-range
  "The markdown for a DOM range, or nil to leave the copy to the browser: when
  the range is not one this namespace takes (rule 1), holds no text, or makes a
  copy that does not render as what was selected (`round-trips?`). That last
  is the mapping saying it is unsure, and then the native copy wins."
  [^js r]
  (when-let [{:keys [md plain? inline?] [container sn so en eo] :ends :as copy} (copy-of-range r)]
    (when (round-trips? copy
                        (when-not plain? (markdown/rendered-text md inline?))
                        (selection-text container sn so en eo))
      md)))

(defn- typing?
  "Focus in something that edits text: an input, a textarea, or a CodeMirror,
  which is a contenteditable. The question tracker's `core/typing?` asks, and the
  answer that keeps Option+C typing a ç there."
  []
  (when-let [el (.-activeElement js/document)]
    (or (= "INPUT" (.-tagName el))
        (= "TEXTAREA" (.-tagName el))
        (.-isContentEditable el))))

(defn selection-markdown
  "The markdown for the page's current selection, or nil to leave it alone.
  Never a blank string: whatever went wrong to produce one, the browser's own
  copy is better than an emptied clipboard."
  []
  (when-not (typing?)
    (let [sel (.getSelection js/window)]
      (when (and sel (= 1 (.-rangeCount sel)) (not (.-isCollapsed sel)))
        (let [md (markdown-of-range (.getRangeAt sel 0))]
          (when-not (str/blank? md) md))))))

;; --- the two keys ------------------------------------------------------------------------

(defn- on-copy
  "Cmd+C, Ctrl+C, and the browser's own Copy: a `copy` event. Setting the data and
  preventing the default is what replaces the browser's copy, and leaving out
  `text/html` is what makes every paste target take the markdown. A copy some
  other handler has already taken is left to it."
  [^js e]
  (when-not (.-defaultPrevented e)
    (when-let [md (selection-markdown)]
      (.setData (.-clipboardData e) "text/plain" md)
      (.preventDefault e))))

(defn- on-key-down
  "Option+C (Alt+C). By `code`, since on a Mac the `key` is `ç`, and only with
  nothing else held.

  **Outside a field, with something selected, Option+C always copies**: the
  markdown when there is markdown to copy, and otherwise the browser's own Copy
  (`execCommand`), which runs `on-copy`, which declines, and so copies natively.
  The human's words make it a copy key (\"option+c should also work!\"), and a
  key that silently left the clipboard as it was would read as the copy
  failing. A `writeText` that is refused (no focus, no permission) falls back
  the same way. In a field the key is never taken, so a ç still types."
  [^js e]
  (when (and (.-altKey e) (= "KeyC" (.-code e))
             (not (.-metaKey e)) (not (.-ctrlKey e)) (not (.-shiftKey e))
             (not (typing?)))
    (let [sel (.getSelection js/window)]
      (when (and sel (pos? (.-rangeCount sel)) (not (.-isCollapsed sel)))
        (.preventDefault e)
        (let [md (try (selection-markdown) (catch :default _ nil))
              native! (fn [] (.execCommand js/document "copy"))
              ^js clipboard (.-clipboard js/navigator)]
          (if (and md clipboard)
            (-> (.writeText clipboard md) (.catch (fn [_] (native!))))
            (native!)))))))

;; The listeners are stable functions over the current definitions, so a hot
;; reload swaps the behaviour without stacking a second listener.
(defonce ^:private copy-listener (fn [e] (on-copy e)))
(defonce ^:private key-listener (fn [e] (on-key-down e)))

(defn install!
  "Listen for the copy and for Option+C, once, on the document. The keydown
  listener is the document's own, as in tracker: treina has no global
  shortcut handler (the modals' own claim Cmd+9 and Escape only), and the
  key belongs to every page with rendered markdown on it."
  []
  (.removeEventListener js/document "copy" copy-listener)
  (.addEventListener js/document "copy" copy-listener)
  (.removeEventListener js/document "keydown" key-listener)
  (.addEventListener js/document "keydown" key-listener))
