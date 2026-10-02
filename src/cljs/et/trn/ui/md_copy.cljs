(ns et.trn.ui.md-copy
  "Copying rendered markdown puts its markdown on the clipboard: tracker's
  `et.tr.ui.md-copy`, brought to treina whole and kept in step with it. Treina
  renders markdown exactly as tracker does (a bare `marked`, task boxes as an
  `<input>`), so nothing below differs from tracker's but the names.

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

  **Before aligning, the markup is blanked.** The walk is greedy, and a link's
  tail is where that goes wrong on ordinary text: `see [docs](https://x.com).`
  renders `see docs.`, and the final `.` matches the `.` of `x.com`. So every
  character the tokens say is markup (a link's `[` and `](url)`, an image, the
  markers of emphasis, code and headings, list and quote prefixes, table pipes)
  is replaced by `\\u0000` in the string handed to `column`. Same length, so
  every offset holds.

  **The rules**, which tracker's `md-copy-test` holds one by one, and which
  `test/browser/md-copy-checks.js` runs against this namespace in the page:

  1. Only a selection inside **one** rendered container (one notes field, one
     description) is taken, with focus outside any field. Anything else is the
     browser's, unchanged: two cards, markdown and the buttons around it.
  2. A selection of **all** of a container's text copies its source verbatim.
  3. An **inline construct cut at an edge** gets its own markers: the opening
     ones before the slice, outermost first, the closing ones after it,
     innermost first. `important` out of `**very important**` copies as
     `**important**`, and part of a link's text keeps its `(url)`.
  4. **Inside one block**, what comes out is inline markdown. A word from a
     heading or a list item comes without `## ` or `- `, and lines of code come
     plain, without the fence. A block selected **whole** brings its markup:
     `## Heading`, `- item`, `> quote`, the fence.
  5. **Across blocks**, the slice is verbatim, so every line keeps its markers,
     indentation and blank lines. The first line gets its block prefix back
     (`- `, `> `, `## `, `1. `), a fenced block cut at an edge gets its fence,
     and a setext heading its underline. When every line is indented at least
     as far as the first, that common indentation is taken off.
  6. **Tables**: inside one cell, the cell's inline markdown. Across cells, the
     header and delimiter rows plus every row the selection touches, so it is
     still a table."
  (:require [clojure.string :as str]
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

(defn align
  "Where each character of `text` stands in `raw`, for a token whose `text` is
  its `raw` with prefixes taken off: a list item without its marker and its
  continuation indent, a quote without its `>`s, a code block without its fence
  and indentation.

  Line by line first, each line of `text` being the tail of its line in `raw`
  (from `from-line` on), because what was taken off is always at the start of
  a line. The greedy walk is the fallback for the cases that do not fit (a tab
  marked expanded), and a little less exact."
  ([raw text] (align raw text 0))
  ([raw text from-line]
   (let [rl (lines raw) tl (lines text) tn (count text)]
     (or (when (<= (+ from-line (count tl)) (count rl))
           (loop [j 0 out []]
             (if (= j (count tl))
               out
               (let [[ts te] (tl j)
                     [rs re] (rl (+ from-line j))]
                 (when (str/ends-with? (subs raw rs re) (subs text ts te))
                   (recur (inc j)
                          (cond-> (into out (range (- re (- te ts)) re))
                            (< te tn) (conj re))))))))
         (greedy raw text (first (nth rl from-line [0 0])))))))

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
      (assoc base :c0 0 :c1 (count (str/trimr raw)))

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

      "table"
      (let [hit (set (mapcat #(range (:c0 %) (:c1 %)) cells))]
        (concat (remove hit (range n)) (mapcat #(inline-markup (:inline %)) cells)))

      nil)))

(defn- collect
  "The leaves under `tokens`, in document order, each with its `:pm`, and the
  container markup (list markers, quote prefixes) marked in `mask`.

  `tokens`' raws add up to `coord`, and `pm` says where each character of
  `coord` stands in the source. Through a list item or a quote the coordinates
  change, because their children were lexed from their text with the prefixes
  off: `align` maps that text back into their raw, and `pm` is composed with
  it. A raw is taken as `coord`'s slice rather than the token's own, which
  marked 18 garbles in one case while keeping its length (see
  `markdown/blocks`)."
  [tokens coord pm mask]
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
                      (collect (.-items t) raw tpm mask)

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
                          (aset mask (tpm i) true))
                        (collect (.-tokens t) text (fn [i] (tpm (at m i))) mask))

                      (when-let [l (leaf t raw (mapv tpm (range (count raw))))]
                        (doseq [i (leaf-markup l)]
                          (aset mask (nth (:pm l) i) true))
                        [l]))]
          (recur (rest ts) (+ o n) (into out (remove #(empty? (:pm %))) found))))
      out)))

(defn analyse
  "Everything a copy needs to know about `src`, lexed once:
  - `:leaves`, the blocks that hold text (see `leaf`), in document order
  - `:blocks`, the top-level blocks as `markdown/blocks` numbers them,
    `{:start :raw :fenced?}`, whose `:raw` is the *masked* one md-point aligns
  - `:src` itself.
  `inline?` for inline-only markdown (a title in tracker; treina has none yet)."
  [src inline?]
  (let [n (count src)
        mask (make-array n)]
    (if inline?
      (let [l {:type "paragraph" :raw src :pm (vec (range n)) :c0 0 :c1 n
               :inline (inline-nodes (.lexInline Lexer src (.-defaults marked)) 0)}]
        (doseq [i (inline-markup (:inline l))] (aset mask i true))
        {:src src :leaves [l]
         :blocks [{:start 0 :raw (apply str (map-indexed #(if (aget mask %1) "\u0000" %2) src))}]})
      (let [tokens (.lexer marked src)
            leaves (collect tokens src identity mask)
            masked (apply str (map-indexed #(if (aget mask %1) "\u0000" %2) src))
            blocks (loop [[^js t & more] (array-seq tokens) o 0 out []]
                     (if-not t
                       out
                       (let [k (count (or (.-raw t) ""))]
                         (recur more (+ o k)
                                (conj out {:start o
                                           :raw (subs masked o (min n (+ o k)))
                                           :fenced? (and (= "code" (.-type t))
                                                         (not= "indented" (.-codeBlockStyle t)))})))))]
        {:src src :leaves leaves :blocks blocks}))))

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

(defn- dedent
  "Take off the indentation the first line has, when every non-blank line has
  at least as much. A nested item copied on its own reads as an item."
  [s]
  (let [ind (count (re-find #"^[ \t]*" s))
        ls (str/split s #"\n" -1)]
    (if (and (pos? ind)
             (every? #(or (str/blank? %) (>= (count (re-find #"^[ \t]*" %)) ind)) ls))
      (str/join "\n" (map #(if (str/blank? %) % (subs % ind)) ls))
      s)))

(defn- start-side
  "Where a selection that starts in `leaf` at `a` and runs on past it starts in
  the source, and what has to go in front: the block prefix of the first line,
  and the opening markers of what the start cuts."
  [src {:keys [type raw pm c0 inline lines] :as _leaf} a]
  (let [la (local-index pm a)
        lead (fn [p] (subs src (line-start src (first pm)) p))]
    (case type
      "code" {:a (max a (at pm c0)) :prefix (lead (at pm c0))}
      "table" (let [row (line-of raw la)]
                (if (<= row 1)
                  {:a (line-start src (first pm)) :prefix ""}
                  {:a (nth pm (first (nth lines row)))
                   :prefix (lead (nth pm (first (nth lines 2))))}))
      (let [[opens la'] (open-cuts raw inline la)]
        {:a (at pm la')
         :prefix (str (lead (if (= "heading" type) (at pm c0) (first pm)))
                      (apply str opens))}))))

(defn- end-side
  "Where a selection that ends in `leaf` at `b`, having started before it, ends in
  the source, and what has to follow: the closing markers of what the end cuts,
  a fence, a setext underline."
  [{:keys [type raw pm c1 inline lines] :as _leaf} b]
  (let [lb (inc (local-index pm (dec b)))]
    (case type
      "code" {:b (min b (after pm c1)) :suffix (trim-newlines (subs raw c1))}
      "table" (let [row (max 1 (line-of raw (dec lb)))]
                {:b (after pm (second (nth lines row))) :suffix ""})
      (let [[closes lb'] (close-cuts raw inline lb)]
        {:b (after pm lb')
         :suffix (str (apply str closes)
                      (when (= "heading" type) (trim-newlines (subs raw c1))))}))))

(defn- across
  "Rule 5: from `a` in `la-leaf` to `b` in `lb-leaf`, verbatim in between."
  [src start-leaf end-leaf a b]
  (let [{a' :a :keys [prefix]} (start-side src start-leaf a)
        {b' :b :keys [suffix]} (end-side end-leaf b)]
    (dedent (str prefix (subs src a' (max a' b')) suffix))))

(defn- cell-at [cells i]
  (some #(when (and (<= (:c0 %) i) (< i (:c1 %))) %) cells))

(defn- within
  "Rules 3, 4 and 6 inside one leaf."
  [src {:keys [type raw pm c0 c1 inline cells text code-map] :as leaf} a b]
  (let [la (local-index pm a)
        lb (inc (local-index pm (dec b)))
        inline-slice (fn [nodes la lb]
                       (let [[opens la'] (open-cuts raw nodes la)
                             [closes lb'] (close-cuts raw nodes lb)]
                         (str (apply str opens) (subs raw la' (max la' lb')) (apply str closes))))]
    (cond
      (= "table" type)
      (let [cell (cell-at cells la)]
        (if (and cell (identical? cell (cell-at cells (dec lb))))
          (inline-slice (:inline cell) la lb)
          (across src leaf leaf a b)))

      (and (<= la c0) (>= lb c1))
      (dedent (trim-newlines (subs src (line-start src (first pm)) (after pm (count raw)))))

      (= "code" type)
      (let [i0 (or (first (keep-indexed #(when (>= %2 la) %1) code-map)) (count text))
            i1 (or (first (keep-indexed #(when (>= %2 lb) %1) code-map)) (count text))]
        (subs text i0 (max i0 i1)))

      :else (inline-slice inline la lb))))

(defn between
  "The markdown for the source between offsets `a` and `b` (exclusive) of an
  `analyse`d text, by the rules in the ns docstring, or nil for nothing."
  [{:keys [src leaves]} a b]
  (when (< a b)
    (let [start-leaf (some #(when (< a (after (:pm %) (count (:pm %)))) %) leaves)
          end-leaf (last (filter #(> b (first (:pm %))) leaves))]
      (when (and start-leaf end-leaf)
        (let [a (max a (first (:pm start-leaf)))
              b (min b (after (:pm end-leaf) (count (:pm end-leaf))))]
          (when (< a b)
            (if (identical? start-leaf end-leaf)
              (within src start-leaf a b)
              (across src start-leaf end-leaf a b))))))))

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

(defn markdown-of-range
  "The markdown for a DOM range, or nil when the range is not one this
  namespace takes (rule 1) or holds no text."
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
          texty (keep-indexed (fn [i t] (when (seq t) i)) texts)]
      (when (and start end (<= (first start) (first end)))
        (if (and (= start [(first texty) 0])
                 (= end [(last texty) (count (texts (last texty)))]))
          src
          (let [{:keys [blocks] :as analysis} (analyse src inline?)
                block-of (fn [i]
                           (let [s (js/parseInt (.getAttribute (els i) "data-start") 10)]
                             (some #(when (= s (:start %)) %) blocks)))
                [si soff] start
                [ei eoff] end
                sb (block-of si)
                eb (block-of ei)]
            (when (and sb eb)
              (between analysis
                       (md-point/block-offset sb (texts si) soff)
                       (inc (md-point/block-offset eb (texts ei) (dec eoff)))))))))))

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
  `text/html` is what makes every paste target take the markdown."
  [^js e]
  (when-let [md (selection-markdown)]
    (.setData (.-clipboardData e) "text/plain" md)
    (.preventDefault e)))

(defn- on-key-down
  "Option+C (Alt+C). By `code`, since on a Mac the `key` is `ç`. Only with
  nothing else held, only outside a field, and only when there is markdown to
  copy, so everywhere else the key is exactly what it was."
  [^js e]
  (when (and (.-altKey e) (= "KeyC" (.-code e))
             (not (.-metaKey e)) (not (.-ctrlKey e)) (not (.-shiftKey e)))
    (when-let [md (selection-markdown)]
      (.preventDefault e)
      ;; Where the async clipboard is missing (an insecure origin), the
      ;; browser's own Copy command goes through `on-copy`, which writes the
      ;; same markdown.
      (if-let [^js clipboard (.-clipboard js/navigator)]
        (.writeText clipboard md)
        (.execCommand js/document "copy")))))

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
