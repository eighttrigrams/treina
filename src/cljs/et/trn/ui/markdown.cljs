(ns et.trn.ui.markdown
  "Everything the user writes — session notes, training descriptions, the
  program — is markdown, rendered through marked. `markdown.css` keeps headings
  at body size so a rendered block sits inside a card without shouting."
  (:require [reagent.core :as r]
            ["marked" :refer [marked]]))

(defn render [text]
  [:div.markdown-content
   {:dangerouslySetInnerHTML (r/unsafe-html (marked (or text "")))}])
