(ns semantic-csv.impl.core
  "This namespace consists of implementation details for the main API"
  (:require [clojure.string :as s]
            [clojure-csv.core :as csv]))


(defn mappify-row
  "Translates a single row of values into a map of `colname -> val`, given colnames in `header`."
  [header row]
  (into {} (map vector header row)))


(defn apply-kwargs
  "Utility that takes a function f, any number of regular args, and a final kw-args argument which will be
  splatted in as a final argument"
  [f & args]
  (apply
    (apply partial
           f
           (butlast args))
    (apply concat (last args))))


(defn stringify-keyword
  "Leaves strings alone. Turns keywords into the stringified version of the keyword, sans the initial `:`
  character. On anything else, calls str."
  [x]
  (cond
    (string? x)   x
    (keyword? x)  (->> x str (drop 1) (apply str))
    :else         (str x)))


(defn row-val-caster
  "Returns a function that casts casts a single row value based on specified casting function and
  optionally excpetion handler"
  [cast-fns exception-handler]
  (fn [row col]
    (let [cast-fn (if (map? cast-fns) (cast-fns col) cast-fns)]
      (try
        (update-in row [col] cast-fn)
        (catch Exception e
          (update-in row [col] (partial exception-handler col)))))))


(defn cast-row
  "Format the values of row with the given function. This gives us some flexbility with respect to formatting
  both vectors and maps in similar fashion."
  [cast-fns row & {:keys [only exception-handler]}]
  (let [cols (cond
               ; If only is specified, just use that
               only
                 (flatten [only])
               ; If cast-fns is a map, use those keys
               (map? cast-fns)
                 (keys cast-fns)
               ; Then assume cast-fns is single fn, and fork on row type
               (map? row)
                 (keys row)
               :else
                 (range (count row)))]
    (reduce (row-val-caster cast-fns exception-handler)
            (if (seq? row)
              (vec row)
              row)
            cols)))


(def not-blank?
  "Check if value is a non-blank string."
  (every-pred string? (complement s/blank?)))


(defn write-rows
  "Serialize `rows` with `clojure-csv.core/write-csv`, write them to `file` and return it."
  [file writer-opts rows]
  (.write file (apply-kwargs csv/write-csv rows writer-opts))
  file)


;; The following is ripped off from prismatic/plumbing:

(defmacro ?>>
  "Conditional double-arrow operation (->> nums (?>> inc-all? (map inc)))"
  [do-it? & args]
  `(if ~do-it?
     (->> ~(last args) ~@(butlast args))
     ~(last args)))

;; We include it here in lieue of depending on the full library due to dependency conflicts with other
;; libraries.


