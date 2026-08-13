(ns et.trn.ui.codemirror
  "Treina's single keyboard scheme for text editing — tracker's \"alternative\"
  scheme, here as the only one (there is no toggle and no plain-textarea mode).

  Navigation is IJKL instead of the arrow keys: ⌘I/K up/down, ⌘J/L left/right,
  with ⌥ for word/line steps, ⌃ for line ends, and +⇧ to select. ⌘9 saves (wired
  at the view level, not here).

  The scheme itself is not in this file any more. It was a 47-chord table and
  eleven hand-written commands, near-identical to the copies tracker and rhizome
  each held; all three now call `@eighttrigrams/kw-codemirror`, the library in the
  keyboard-wizardry repo that also holds Daniel's VSCode and Obsidian keymaps of
  the same scheme. Same 47 chords, with two differences on purpose: ⌃J and ⌃L are
  the markdown \"sentence\" motions rather than line start and end (the block, or
  just the line when the one above ended in a hard break), ⌃⇧J and ⌃⇧L select as
  far as those move, and the four ⌥ motions move by *form* inside a fenced
  ```clojure block.

  What stays here is treina's own: the theme, the placeholder, and reporting
  changes back to the caller."
  (:require ["@codemirror/state" :refer [EditorState]]
            ["@codemirror/view" :refer [EditorView placeholder]]
            ["@codemirror/commands" :as commands]
            ["@eighttrigrams/kw-codemirror" :as ijkl]))

(defn create-editor [element {:keys [doc on-change placeholder-text]}]
  (let [doc (or doc "")
        minimal-theme
          (.theme EditorView
                  #js {"&" #js {:backgroundColor "var(--glass-bg)"
                                :border "1px solid rgba(0, 0, 0, 0.1)"
                                :borderRadius "10px"
                                :fontFamily "inherit"
                                :fontSize "0.92em"
                                :height "100%"
                                :color "var(--text-primary)"}
                       "&.cm-focused" #js {:outline "none"
                                           :borderColor "var(--accent)"
                                           :boxShadow "0 0 0 3px rgba(124, 58, 237, 0.18)"}
                       ".cm-scroller" #js {:overflow "auto"
                                           :fontFamily "inherit"
                                           :lineHeight "1.55"}
                       ".cm-content" #js {:padding "10px 14px"
                                          :fontFamily "inherit"
                                          :caretColor "var(--text-primary)"}
                       ".cm-line" #js {:padding "0"}
                       ".cm-gutters" #js {:display "none"}
                       ".cm-activeLine" #js {:backgroundColor "transparent"}
                       ".cm-activeLineGutter" #js {:display "none"}
                       ".cm-cursor" #js {:borderLeftColor "var(--text-primary)"}
                       ".cm-placeholder" #js {:color "var(--text-secondary)"}})
        line-wrapping (.-lineWrapping EditorView)
        update-listener (.of (.-updateListener EditorView) (fn [^js update]
                                                            (when (.-docChanged update)
                                                              (when on-change
                                                                (on-change (.. update -state -doc toString))))))
        extensions (cond-> #js [minimal-theme line-wrapping update-listener]
                     (seq placeholder-text) (doto (.push (placeholder placeholder-text))))
        state (.create EditorState #js {:doc doc :extensions extensions})
        view (new EditorView #js {:state state :parent element})]
    ;; The scheme. install puts a capture-phase keydown listener on the view's own
    ;; element, so these chords win before CodeMirror's keymaps — which is what the
    ;; listener this replaces did, one level further out on `element`.
    (ijkl/install view commands)
    view))

(defn get-editor-value [view]
  (when view (.. view -state -doc toString)))

(defn set-editor-value [view value]
  (when view
    (let [transaction
            (.update (.-state view)
                     #js {:changes #js {:from 0 :to (.. view -state -doc -length) :insert value}})]
      (.dispatch view transaction))))
