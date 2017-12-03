(import [jedhy.jedhy
         [--HYCOMPANY-split-prefix
          --HYCOMPANY-obj-candidates
          --HYCOMPANY-get-macros
          --HYCOMPANY-get-globals
          --HYCOMPANY-trim-candidates
          --HYCOMPANY

          --HYANNOTATE-annotate-builtin
          --HYANNOTATE-compiler?
          --HYANNOTATE-macro?
          --HYANNOTATE
          ]])

;; * Asserts

(defn assert= [x y]
  (assert (= x y)))

(defn assert-in [x y]
  (assert (in x y)))

(defn assert-not-in [x y]
  (assert (not (in x y))))

(defn assert-all= [x y]
  (assert (->> (zip-longest x y) (*map =) all)))

(defn assert-all-in [x y]
  (assert (->> x (map (fn [z] (in z y))) all)))

;; * Tests
;; ** Formatting

(defn test-split-prefix []
  (assert-all= (--HYCOMPANY-split-prefix "ob")
               ["" "ob"])
  (assert-all= (--HYCOMPANY-split-prefix "ob.j")
               ["ob" "j"])
  (assert-all= (--HYCOMPANY-split-prefix "ob.j.attr")
               ["ob.j" "attr"])

  (assert-all= (--HYCOMPANY-split-prefix "ob.j.")
               ["ob.j" ""])
  (assert-all= (--HYCOMPANY-split-prefix "")
               ["" ""]))

;; ** Candidates
;; *** Objects

(defn test-candidates-built-in-func []
  (assert-in "--call--"
             (--HYCOMPANY-obj-candidates "print"))
  (assert-in "--call--"
             (--HYCOMPANY-obj-candidates "str.format")))

(defn test-candidates-class []
  (assert-in "format"
             (--HYCOMPANY-obj-candidates "str")))

(defn test-candidates-module []
  (assert-in "eval"
             (--HYCOMPANY-obj-candidates "builtins")))

;; *** Macros

(defn test-candidates-macros []
  (assert-all-in ["+=" "->" "HySet" "for"]
                 (--HYCOMPANY-get-macros)))

(defn test-candidates-compile-table []
  (assert-all-in ["yield" "require" "import" "list-comp"]
                 (--HYCOMPANY-get-macros)))

;; *** Globals

(defn test-candidates-globals []
  (assert-all-in ["first" "in" "+" "even?"]
                 (--HYCOMPANY-get-globals)))

;; ** Pipeline

(defn test-candidates-trimmed []
  (setv candidates
        (--HYCOMPANY-get-globals))

  (assert-all-in ["first" "even?"] candidates)

  (setv trimmed-candidates
        (--HYCOMPANY-trim-candidates candidates "fi"))

  (assert-in "first" trimmed-candidates)
  (assert-not-in "even?" trimmed-candidates))

(defn test-candidates-formatted []
  (assert-in "builtins"
             (--HYCOMPANY "built"))
  (assert-in "builtins.eval"
             (--HYCOMPANY "builtins.e"))
  (assert-in "builtins.eval.--call--"
             (--HYCOMPANY "builtins.eval.")))
