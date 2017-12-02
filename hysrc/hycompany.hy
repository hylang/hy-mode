(require [hysrc.macros [*]])
(import [hysrc.macros [*]])
(require [hy.extra.anaphoric [*]])

(import
  builtins

  hy hy.compiler hy.macros
  [hy.lex.parser [hy-symbol-unmangle hy-symbol-mangle]]
  [hy.core.shadow [*]] [hy.core.language [*]]  ; To complete the namespace
  )

(defn --HYCOMPANY-split-prefix [prefix]
  "Split prefix on last dot accessor."
  (-> prefix
     (.split ".")
     ((juxt (fn->> butlast (.join "."))
            (fn->> last)))))

(defn --HYCOMPANY-obj-candidates [obj]
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
     (map --HYCOMPANY-get-name)
     (map hy-symbol-unmangle)
     distinct
     list))

(defn --HYCOMPANY-get-global-candidates []
  (->> (globals)
     (.keys)
     (map hy-symbol-unmangle)
     (chain (--HYCOMPANY-get-macros))
     flatten
     distinct
     list))

(defn --HYCOMPANY-get-name [x]
  "Return the candidate name for x."
  (if (isinstance x str)
      x
      x.--name--))

(defn --HYCOMPANY-trim-candidates [candidates attr]
  "Limit list of candidates to those starting with attr."
  (list (filter (fn [cand] (.startswith cand attr)) candidates)))

(defn --HYCOMPANY [prefix]
  (setv [obj attr]
        (--HYCOMPANY-split-prefix prefix))

  (if obj
      (setv candidates (--HYCOMPANY-obj-candidates obj))
      (setv candidates (--HYCOMPANY-get-global-candidates)))

  (setv choices (--HYCOMPANY-trim-candidates candidates attr))

  (if obj
      (list (map (fn [x] (+ obj "." x))
                 choices))
      choices))

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
