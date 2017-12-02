(import [hysrc.hycompany
         [--HYCOMPANY-split-prefix

          ]])

;; * Asserts

(defn assert= [x y]
  (assert (= x y)))

(defn assert-all= [x y]
  (assert (->> (zip-longest x y) (*map =) all)))

;; * Tests

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
