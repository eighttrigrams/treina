(ns et.trn.ui.markdown
  "Everything the user writes — session notes, training descriptions, the
  program — is markdown, rendered through marked. `markdown.css` keeps headings
  at body size so a rendered block sits inside a card without shouting."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            ["marked" :refer [marked]]))

(defn lf
  "The text with its line breaks as `\\n`, as marked's lexer makes them. So an
  offset computed against marked's tokens means the same thing in the text."
  [text]
  (str/replace (or text "") #"\r\n?" "\n"))

(defn blocks
  "The text as its top-level blocks, **each told where in the text it starts**:
  `{:line :start :raw :type :html}`, plus `:fenced?` for a fenced code block,
  both offsets into `(lf text)`. Tracker's `ui.markdown/blocks`, which is
  cookbook's, which is zoo's idea: it is what lets a copy out of the rendering
  be answered with its markdown (`ui.md-copy`).

  Where a block starts is marked's own answer: the lengths of the top-level
  tokens' `raw`s add up to the text. `:raw` is the text's own slice rather than
  the token's, because marked 18 garbles the raw of a quote nested in a quote
  with a lazy line after it while keeping its length. Blank lines and link
  definitions draw nothing and are left out."
  [text]
  (let [text (lf text)]
    (loop [ts (array-seq (.lexer marked text)) offset 0 line 0 out []]
      (if-let [^js t (first ts)]
        (let [n (count (or (.-raw t) ""))
              raw (subs text (min offset (count text)) (min (+ offset n) (count text)))
              html (.parser marked #js [t])
              block (cond-> {:line line :start offset :raw raw :type (.-type t) :html html}
                      (and (= "code" (.-type t)) (not= "indented" (.-codeBlockStyle t)))
                      (assoc :fenced? true))]
          (recur (rest ts) (+ offset n) (+ line (count (re-seq #"\n" raw)))
                 (cond-> out (not (str/blank? html)) (conj block))))
        out))))

(defn render
  "Drawn one top-level block per wrapper, each saying where in the text it
  starts (`data-start`, `data-line`), in a container that carries the text
  (`data-md-src`): that is what a copy reads (`ui.md-copy`). The wrappers'
  contents, joined, are byte for byte what `(marked text)` was, and they have
  no padding or border, so margins collapse across them as between siblings;
  `markdown.css` restores the two rules that notice position."
  [text]
  (let [text (lf text)]
    (into [:div.markdown-content.markdown-blocks {:data-md-src text}]
          (map-indexed
           (fn [i {:keys [line start html]}]
             ^{:key i} [:div.markdown-block
                        {:data-line line :data-start start
                         :dangerouslySetInnerHTML (r/unsafe-html html)}]))
          (blocks text))))
