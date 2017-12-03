(import
  builtins inspect

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

;; * Hydoc
;; ** Argspec Extraction

(defn --HYDOC-args [argspec]
  "Extract standard positional arguments from argspec."
  (if (and argspec.args argspec.defaults)
      (-> argspec.defaults
         len
         (drop-last argspec.args)
         list)
      argspec.args))

(defn --HYDOC-defaults [argspec]
  "Extract &optional arguments from argspec."
  (if (and argspec.args argspec.defaults)
      (-> (--HYDOC-args)
         len
         (drop argspec.args)
         list)
      argspec.defaults))

(defn --HYDOC-kwonlyargs [argspec]
  "Extract :keyword arguments without a default from argspec."
  (if (and argspec.kwonlyargs argspec.kwonlydefaults)
      (->> argspec.kwonlyargs
         (remove (fn [x] (in x (.keys argspec.kwonlydefaults))))
         list)
      argspec.kwonlyargs))

(defn --HYDOC-kwonlydefaults [argspec]
  "Extract :keyword arguments with their default from argspec."
  (if (and argspec.kwonlyargs argspec.kwonlydefaults)
      (->> argspec.kwonlydefaults
         (.items)
         (*map (fn [k v] (.format "[{} {}]" k v)))
         list)
      argspec.kwonlydefaults))

(defn --HYDOC-kwargs [argspec]
  (-> argspec
     ((juxt --HYDOC-kwonly-args --HYDOC-kwonlydefaults))
     flatten
     list))

;; ** Format Argspec

(defn --HYDOC-acc-formatted-argspec [formatted-argspec [args arg-opener]]
  "Accumulator for adding formatted argspec components."
  (+ formatted-argspec
     (if formatted-argspec " " "")
     (when args
       (+ (if arg-opener (+ "&" arg-opener " ") "")
          (sum args)))))

(defn --HYDOC-format-argspec [argspec]
  "Lispy version of format argspec covering all defun kwords."
  (setv [args defaults kwargs]
        ((juxt --HYDOC-args --HYDOC-defaults --HYDOC-kwargs) argspec))

  (reduce --HYDOC-acc-formatted-argspec
          [[args None]
           [defaults "optional"]
           [[argspec.varargs] "rest"]
           [[argspec.varkw] "kwargs"]
           [kwargs "kwonly"]]))

;; ** Format Eldoc String

(defn --HYDOC-format-eldoc-string [obj-name f &optional full]
  "Format an obj name for callable f."
  (if f.--doc--
      (.format "{obj}: ({args}){docs}"
               :obj (.replace obj-name "_" "-")
               :args (--HYDOC-format-argspec (inspect.getfullargspec f))
               :docs (if full
                         (->> f.--doc-- (.splitlines) (.join "\n") (+ "\n"))
                         (+ " - " (->> f.--doc-- (.splitlines) first))))
      (.format "{obj}: ({args})"
               :obj (.replace obj-name "_" "-")
               :args (--HYDOC-format-argspec (inspect.getfullargspec f)))))

(defn --HYDOC-python-eldoc [obj &optional full]
  "Build eldoc string for python obj or string.

Not all defuns can be argspeced - eg. C defuns."
  (try
    (do (when (isinstance obj str)
          (setv obj (.eval builtins obj (globals))))
        (setv full-doc (.getdoc inspect obj))
        (setv doc full-doc)
        (try
          (setv doc (--HYDOC-format-eldoc-string obj.--name-- obj
                                                :full full))
          (except [e TypeError]
            (setv doc (->> doc (.splitlines) first (+ "builtin: ")))
            (when full
              (setv doc (+ doc "\n"
                           (->> full-doc (.splitlines) rest (.join ""))))))))
    (except [e Exception]
      (setv doc "")))
  doc)

(defn --HYDOC-macro-eldoc [obj &optional full]
  "Get eldoc string for a macro."
  (try
    (do (setv obj (.replace obj "-" "_"))
        (setv macros (get -hy-macros None))

        (when (in obj macros)
          (--HYDOC-format-eldoc-string obj (get macros obj) :full full)))
    (except [e Exception] "")))

(defn --HYDOC [obj &optional full]
  "Get eldoc string for any obj."
  (setv doc (--HYDOC-python-eldoc obj :full full))
  (unless doc (setv doc (--HYDOC-macro-eldoc obj :full full)))
  doc)
