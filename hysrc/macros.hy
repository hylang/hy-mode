(require [hy.extra.anaphoric [*]])


(defmacro/g! fn-> [&rest forms]
  "Thread first an anonymous function."
  `(fn [~g!x] (-> ~g!x ~@forms)))

(defmacro/g! fn->> [&rest forms]
  "Thread last an anonymous function."
  `(fn [~g!x] (->> ~g!x ~@forms)))
