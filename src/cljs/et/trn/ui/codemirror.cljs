(ns et.trn.ui.codemirror
  "Treina's single keyboard scheme for text editing — tracker's \"alternative\"
  scheme, here as the only one (there is no toggle and no plain-textarea mode).

  Navigation is IJKL instead of the arrow keys: ⌘I/K up/down, ⌘J/L left/right,
  with ⌥ for word/line steps, ⌃ for line ends, and +⇧ to select. See key-commands
  for the full map. ⌘9 saves (wired at the view level, not here)."
  (:require ["@codemirror/state" :refer [EditorState]]
            ["@codemirror/view" :refer [EditorView placeholder]]
            ["@codemirror/commands" :as commands]))

(def key-commands
  {#{"KeyJ" #{:meta}} commands/cursorCharLeft
   #{"KeyL" #{:meta}} commands/cursorCharRight
   #{"KeyI" #{:meta}} commands/cursorLineUp
   #{"KeyK" #{:meta}} commands/cursorLineDown
   #{"KeyJ" #{:alt}} commands/cursorGroupLeft
   #{"KeyL" #{:alt}} commands/cursorGroupRight
   #{"KeyI" #{:alt}} commands/cursorLineUp
   #{"KeyK" #{:alt}} commands/cursorLineDown
   #{"KeyJ" #{:ctrl}} commands/cursorLineStart
   #{"KeyL" #{:ctrl}} commands/cursorLineEnd
   #{"KeyJ" #{:meta :shift}} commands/selectCharLeft
   #{"KeyL" #{:meta :shift}} commands/selectCharRight
   #{"KeyI" #{:meta :shift}} commands/selectLineUp
   #{"KeyK" #{:meta :shift}} commands/selectLineDown
   #{"KeyJ" #{:alt :shift}} commands/selectGroupLeft
   #{"KeyL" #{:alt :shift}} commands/selectGroupRight
   #{"KeyI" #{:alt :shift}} commands/selectLineUp
   #{"KeyK" #{:alt :shift}} commands/selectLineDown
   #{"KeyJ" #{:ctrl :shift}} commands/selectLineStart
   #{"KeyL" #{:ctrl :shift}} commands/selectLineEnd
   #{"Equal" #{:alt}} commands/deleteGroupForward
   #{"Equal" #{:meta}} commands/deleteCharForward
   #{"Backspace" #{:ctrl}} commands/deleteToLineStart
   #{"Equal" #{:ctrl}} commands/deleteToLineEnd
   #{"Equal" #{:ctrl :meta}} commands/deleteLine
   #{"Enter" #{:shift}} :custom-new-line-below
   #{"Enter" #{:meta}} :custom-new-line-above
   #{"KeyI" #{:ctrl :meta}} commands/moveLineUp
   #{"KeyK" #{:ctrl :meta}} commands/moveLineDown
   #{"KeyL" #{:ctrl :meta}} commands/indentMore
   #{"KeyJ" #{:ctrl :meta}} commands/indentLess
   #{"KeyP" #{:alt :meta}} commands/cursorPageUp
   #{"Semicolon" #{:alt :meta}} commands/cursorPageDown
   #{"KeyI" #{:alt :meta :shift}} :custom-scroll-down
   #{"KeyK" #{:alt :meta :shift}} :custom-scroll-up
   #{"KeyI" #{:alt :meta}} :custom-cursor-viewport-up
   #{"KeyK" #{:alt :meta}} :custom-cursor-viewport-down
   #{"KeyP" #{:ctrl :alt :meta}} commands/cursorDocStart
   #{"Semicolon" #{:ctrl :alt :meta}} commands/cursorDocEnd
   #{"Semicolon" #{:meta}} :custom-center-caret
   #{"Semicolon" #{:ctrl :meta}} :custom-center-line
   #{"KeyA" #{:alt}} commands/selectAll
   #{"Backquote" #{:alt}} commands/undo
   #{"Backquote" #{:shift}} commands/redo
   #{"KeyC" #{:alt}} :custom-copy
   #{"KeyV" #{:alt}} :custom-paste
   #{"KeyX" #{:alt}} :custom-cut})

(defn- custom-copy [view]
  (let [selection (.. view -state -selection -main)]
    (when-not (= (.-from selection) (.-to selection))
      (let [text (.. view -state -doc (slice (.-from selection) (.-to selection)))]
        (.writeText js/navigator.clipboard text)))))

(defn- custom-paste [view]
  (.then (.readText js/navigator.clipboard)
         (fn [text]
           (let [selection (.. view -state -selection -main)
                 transaction (.update (.-state view)
                                      #js {:changes #js {:from (.-from selection)
                                                         :to (.-to selection)
                                                         :insert text}})]
             (.dispatch view transaction)))))

(defn- custom-cut [view]
  (let [selection (.. view -state -selection -main)]
    (when-not (= (.-from selection) (.-to selection))
      (let [text (.. view -state -doc (slice (.-from selection) (.-to selection)))]
        (.writeText js/navigator.clipboard text)
        (let [transaction (.update (.-state view)
                                   #js {:changes #js {:from (.-from selection)
                                                      :to (.-to selection)
                                                      :insert ""}})]
          (.dispatch view transaction))))))

(defn- custom-new-line-below [view]
  (let [state (.-state view)
        cursor (.. state -selection -main -head)
        doc (.-doc state)
        line-info (.lineAt ^js doc cursor)
        line-end (.-to line-info)
        transaction (.update state
                             #js {:changes #js {:from line-end :to line-end :insert "\n"}
                                  :selection #js {:anchor (inc line-end) :head (inc line-end)}})]
    (.dispatch view transaction)))

(defn- custom-new-line-above [view]
  (let [state (.-state view)
        cursor (.. state -selection -main -head)
        doc (.-doc state)
        line-info (.lineAt ^js doc cursor)
        line-start (.-from line-info)
        transaction (.update state
                             #js {:changes #js {:from line-start :to line-start :insert "\n"}
                                  :selection #js {:anchor line-start :head line-start}})]
    (.dispatch view transaction)))

(defn- custom-scroll-up [view]
  (let [scroll-dom ^js (.-scrollDOM view)]
    (set! (.-scrollTop scroll-dom) (- (.-scrollTop scroll-dom) 20))))

(defn- custom-scroll-down [view]
  (let [scroll-dom ^js (.-scrollDOM view)]
    (set! (.-scrollTop scroll-dom) (+ (.-scrollTop scroll-dom) 20))))

(defn- custom-cursor-viewport-up [view]
  (let [scroll-dom ^js (.-scrollDOM view)]
    (commands/cursorLineUp view)
    (set! (.-scrollTop scroll-dom) (- (.-scrollTop scroll-dom) 20))))

(defn- custom-cursor-viewport-down [view]
  (let [scroll-dom ^js (.-scrollDOM view)]
    (commands/cursorLineDown view)
    (set! (.-scrollTop scroll-dom) (+ (.-scrollTop scroll-dom) 20))))

(defn- custom-center-caret [view]
  (try
    (let [state (.-state view)
          doc (.-doc state)
          scroll-dom ^js (.-scrollDOM view)
          viewport-height (.-clientHeight scroll-dom)
          scroll-top (.-scrollTop scroll-dom)
          viewport-middle-y (+ scroll-top (/ viewport-height 2))
          total-lines (.-lines doc)
          middle-line-num
            (loop [line-num 1]
              (if (>= line-num total-lines)
                total-lines
                (let [line-obj (.line ^js doc line-num)
                      line-start-pos (.-from line-obj)
                      coords ^js (.coordsAtPos ^js view line-start-pos)]
                  (if coords
                    (let [line-y (+ (.-top coords) scroll-top)]
                      (if (>= line-y viewport-middle-y) line-num (recur (inc line-num))))
                    (recur (inc line-num))))))
          middle-line-obj (.line ^js doc middle-line-num)
          middle-line-pos (.-from middle-line-obj)
          transaction
            (.update state #js {:selection #js {:anchor middle-line-pos :head middle-line-pos}})]
      (.dispatch view transaction))
    (catch :default _e nil)))

(defn- custom-center-line [view]
  (try
    (let [state (.-state view)
          selection (.-selection state)
          main-selection (.-main selection)
          cursor-pos (.-head main-selection)
          doc (.-doc state)
          line-info (.lineAt ^js doc cursor-pos)
          line-start (.-from line-info)
          coords ^js (.coordsAtPos ^js view line-start)
          scroll-dom ^js (.-scrollDOM view)
          scroll-top (.-scrollTop scroll-dom)
          viewport-height (.-clientHeight scroll-dom)]
      (when coords
        (let [line-top (.-top coords)
              absolute-line-top (+ line-top scroll-top)
              target-scroll (- absolute-line-top (/ viewport-height 2))]
          (.scrollTo scroll-dom #js {:top (max 0 target-scroll) :behavior "smooth"}))))
    (catch :default _e nil)))

(defn- get-modifiers [e]
  (let [modifiers #{}]
    (cond-> modifiers
      (.-altKey e) (conj :alt)
      (.-metaKey e) (conj :meta)
      (.-ctrlKey e) (conj :ctrl)
      (.-shiftKey e) (conj :shift))))

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
    (.addEventListener
      element
      "keydown"
      (fn [e]
        (let [code (.-code e)
              modifiers (get-modifiers e)
              key #{code modifiers}
              command (key-commands key)]
          (when command
            (.preventDefault e)
            (.stopPropagation e)
            (cond (= command :custom-copy) (custom-copy view)
                  (= command :custom-paste) (custom-paste view)
                  (= command :custom-cut) (custom-cut view)
                  (= command :custom-new-line-below) (custom-new-line-below view)
                  (= command :custom-new-line-above) (custom-new-line-above view)
                  (= command :custom-scroll-up) (custom-scroll-up view)
                  (= command :custom-scroll-down) (custom-scroll-down view)
                  (= command :custom-cursor-viewport-up) (custom-cursor-viewport-up view)
                  (= command :custom-cursor-viewport-down) (custom-cursor-viewport-down view)
                  (= command :custom-center-caret) (custom-center-caret view)
                  (= command :custom-center-line) (custom-center-line view)
                  (fn? command) (command view)
                  :else nil))))
      true)
    view))

(defn get-editor-value [view]
  (when view (.. view -state -doc toString)))

(defn set-editor-value [view value]
  (when view
    (let [transaction
            (.update (.-state view)
                     #js {:changes #js {:from 0 :to (.. view -state -doc -length) :insert value}})]
      (.dispatch view transaction))))
