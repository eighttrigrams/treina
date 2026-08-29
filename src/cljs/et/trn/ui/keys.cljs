(ns et.trn.ui.keys
  "⌘9, the app's one save chord.

  Every form that commits something answers to it: the session editors, both
  modals, the trainings and roster add-forms, the inline rename, the YouTube
  inputs, the login. The single-line fields keep saving on Enter too — that
  never stopped being the obvious thing to press in a one-line input. ⌘9 is what
  works *everywhere*, including inside a CodeMirror doc, where Enter has to stay
  a newline (see et.trn.ui.codemirror for the wider IJKL scheme this belongs
  to).

  The live search field on the trainings page has no chord: it filters as you
  type, so there is nothing to commit.")

(defn save-key?
  "Reads `code` rather than `key`, so the chord is the physical 9 regardless of
  what the layout prints there."
  [e]
  (and (.-metaKey e) (= "Digit9" (.-code e))))

(defn on-save
  "An :on-key-down that runs `save` on ⌘9. Put it on the block that wraps a
  form, not on each field: the keystroke bubbles up out of a plain input and out
  of CodeMirror alike, so one handler covers everything inside. Scoping it to
  the block — rather than listening on the document — is also what keeps two
  editors open at once from both saving on one keypress."
  [save]
  (fn [e]
    (when (save-key? e)
      (.preventDefault e)
      (save))))
