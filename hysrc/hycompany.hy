(import
  builtins

  hy hy.compiler hy.macros
  [hy.lex.parser [hy-symbol-unmangle hy-symbol-mangle]]
  [hy.core.shadow [*]] [hy.core.language [*]]  ; To complete the namespace
  )


;; * Formatting

(defn --HYCOMPANY-split-prefix [prefix]
  "Split prefix on last dot accessor, returning an obj, attr pair."
  (setv components
        (.split prefix "."))

  [(->> components butlast (.join "."))
   (->> components last)])

(defn --HYCOMPANY-obj-string [obj]
  "Return obj if a string, otherwise its name."
  (if (isinstance obj str) obj obj.--name--))

;; * Candidates
;; ** Extraction

(defn --HYCOMPANY-obj-candidates [obj]
  "Try to retrieve unmangled attrs list for given (python) obj."
  (try
    (->> obj
       builtins.eval
       dir
       (map hy-symbol-unmangle)
       list)
    (except [e Exception]
      [])))

(defn --HYCOMPANY-get-macros []
  "Extract macro names from all namespaces and compile-table symbols."
  (->> hy.macros.-hy-macros
     (.values)
     (map dict.keys)
     (chain hy.compiler.-compile-table)
     flatten
     (map (comp hy-symbol-unmangle --HYCOMPANY-obj-string))
     distinct
     list))

(defn --HYCOMPANY-get-globals []
  "Extract unmangled globals."
  (->> (globals)
     (.keys)
     (map hy-symbol-unmangle)
     list))

(defn --HYCOMPANY-all-candidates []
  "All global and macro candidates."
  (->> (--HYCOMPANY-get-globals)
     (chain (--HYCOMPANY-get-macros))
     flatten
     distinct
     list))

;; ** Pipeline

(defn --HYCOMPANY-candidates [obj]
  "Return candidates for possibly None obj."
  (if obj
      (--HYCOMPANY-obj-candidates obj)
      (--HYCOMPANY-all-candidates)))

(defn --HYCOMPANY-trim-candidates [candidates attr]
  "Limit list of candidates to those starting with attr."
  (->> candidates
     (filter (fn [x] (.startswith x attr)))
     list))

(defn --HYCOMPANY-format-candidates [candidates obj]
  "Modify candidates for full prefix rather, not just the attr completions."
  (if obj
      (->> candidates
         (map (fn [x] (+ obj "." x)))
         list)
      candidates))

;; ** Driver

(defn --HYCOMPANY [prefix]
  "Extract candidates for a given prefix."
  (setv [obj attr]
        (--HYCOMPANY-split-prefix prefix))

  (-> obj
     --HYCOMPANY-candidates
     (--HYCOMPANY-trim-candidates attr)
     (--HYCOMPANY-format-candidates obj)))

;; * Annotations

(defn --HYANNOTATE-search-builtins [text]
  (setv text (hy-symbol-mangle text))
  (try
    (do (setv obj (builtins.eval text))
        (setv obj-name obj.--class--.--name--)
        (cond [(in obj-name ["function" "builtin_function_or_method"])
               "def"]
              [(= obj-name "type")
               "class"]
              [(= obj-name "module")
               "module"]
              [True "instance"]))
    (except [e Exception]
      None)))

(defn --HYANNOTATE-search-compiler [text]
  (in text hy.compiler.-compile-table))

(defn --HYANNOTATE-search-shadows [text]
  (->> hy.core.shadow
     dir
     (map hy-symbol-unmangle)
     (in text)))

(defn --HYANNOTATE-search-macros [text]
  (setv text (hy-symbol-mangle text))
  (for [macro-dict (.values hy.macros.-hy-macros)]
    (when (in text macro-dict)
      (return (get macro-dict text))))
  None)

(defn --HYANNOTATE [x]
  ;; only builtins format on per-case basis
  (setv annotation (--HYANNOTATE-search-builtins x))
  (when (and (not annotation) (--HYANNOTATE-search-shadows x))
    (setv annotation "shadowed"))
  (when (and (not annotation) (--HYANNOTATE-search-compiler x))
    (setv annotation "compiler"))
  (when (and (not annotation) (--HYANNOTATE-search-macros x))
    (setv annotation "macro"))

  (if annotation
      (.format "<{} {}>" annotation x)
      ""))
