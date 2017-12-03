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

(defn --HYANNOTATE-class-annotation [klass]
  "Return annotation given a name of a class."
  (cond [(in klass ["function" "builtin_function_or_method"])
         "def"]
        [(= klass "type")
         "class"]
        [(= klass "module")
         "module"]
        [True
         "instance"]))

(defn --HYANNOTATE-annotate-builtin [candidate]
  "Try to extract annotation searching builtins."
  (try
    (-> candidate
       hy-symbol-mangle
       builtins.eval
       (. --class--)
       (. --name--)
       --HYANNOTATE-class-annotation)
    (except [e Exception]
      None)))

(defn --HYANNOTATE-compiler? [candidate]
  "Is candidate a compile table construct?"
  (in candidate hy.compiler.-compile-table))

(defn --HYANNOTATE-shadow? [candidate]
  "Is candidate a shadowed operator?"
  (in (hy-symbol-mangle candidate) (dir hy.core.shadow)))

(defn --HYANNOTATE-macro? [candidate]
  "Is candidate a macro?"
  (in (hy-symbol-mangle candidate) (get hy.macros.-hy-macros None)))

(defn --HYANNOTATE-format-annotation [annotation candidate]
  "Format an annotation for company display."
  (if annotation
      (.format "<{} {}>" annotation x)
      ""))

(defn --HYANNOTATE [candidate]
  "Return annotation for a candidate."
  (-> (cond [(--HYANNOTATE-annotate-builtin candidate)]
           [(--HYANNOTATE-shadow? candidate)
            "shadowed"]
           [(--HYANNOTATE-compiler? candidate)
            "compiler"]
           [(--HYANNOTATE-macro? candidate)
            "macro"])
     (--HYANNOTATE-format-annotation candidate)))
