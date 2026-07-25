(ns et.trn.ui.components.diff
  "Read-only diff between two versions of a text, following rhizome's approach:
  CodeMirror's merge extension, either side-by-side or unified."
  (:require [reagent.core :as r]
            ["@codemirror/merge" :refer [MergeView unifiedMergeView]]
            ["@codemirror/state" :refer [EditorState]]
            ["@codemirror/view" :refer [EditorView]]))

(def ^:private diff-theme
  (.theme EditorView
          #js {"&" #js {:backgroundColor "transparent"
                        :color "var(--text-primary)"
                        :fontFamily "inherit"
                        :fontSize "0.88em"}
               ".cm-scroller" #js {:fontFamily "inherit" :lineHeight "1.55"}
               ".cm-content" #js {:fontFamily "inherit"}
               ".cm-gutters" #js {:display "none"}}))

(defn- read-only-extensions []
  [diff-theme
   (.-lineWrapping EditorView)
   (.of (.-editable EditorView) false)
   (.of (.-readOnly EditorState) true)])

(defn- mount! [el older newer unified?]
  (if unified?
    (EditorView. #js {:doc newer
                      :extensions (into-array (conj (read-only-extensions)
                                                    (unifiedMergeView #js {:original older
                                                                           :mergeControls false})))
                      :parent el})
    (MergeView. #js {:a #js {:doc older :extensions (into-array (read-only-extensions))}
                     :b #js {:doc newer :extensions (into-array (read-only-extensions))}
                     :parent el})))

(defn diff-view
  "Mount fresh per (older, newer, unified?) — give it a matching :key upstream,
  since the merge view has no reconfigure path we need here."
  [_older _newer _unified?]
  (let [view (atom nil)]
    (fn [older newer unified?]
      [:div.diff-view
       {:ref (fn [el]
               (if el
                 (reset! view (mount! el (or older "") (or newer "") unified?))
                 (when-let [v @view] (.destroy v) (reset! view nil))))}])))

(defn collapse-identical
  "Versions worth diffing, newest first: consecutive entries with identical text
  collapse to their newest one."
  [versions]
  (->> versions
       (partition-by :text)
       (mapv first)))

(defn diff-browser
  "Step through a text's versions, newest pair first. `versions` is newest-first
  and its head is the current text."
  [_versions]
  (let [idx (r/atom 0)
        unified? (r/atom true)]
    (fn [versions]
      (let [revisions (collapse-identical versions)
            max-idx (max 0 (- (count revisions) 2))
            i (min @idx max-idx)
            newer (nth revisions i nil)
            older (nth revisions (inc i) nil)
            label (fn [{:keys [version created_at]}]
                    (str "v" version (when created_at (str " · " created_at))))]
        [:div.diff-browser
         [:div.diff-controls
          [:button.icon-btn {:disabled (>= i max-idx)
                             :title "Older change"
                             :on-click #(swap! idx (fn [n] (min max-idx (inc n))))} "‹"]
          [:button.icon-btn {:disabled (<= i 0)
                             :title "Newer change"
                             :on-click #(swap! idx (fn [n] (max 0 (dec n))))} "›"]
          [:span.diff-label
           (if older
             (str (label older) "  →  " (label newer) (when (zero? i) "  (current)"))
             "Only one version so far")]
          [:button.secondary.diff-mode {:on-click #(swap! unified? not)}
           (if @unified? "Split" "Unified")]]
         (if older
           ^{:key (str i "-" @unified?)}
           [diff-view (:text older) (:text newer) @unified?]
           [:p.empty "Save an edit and the change will show up here."])]))))
