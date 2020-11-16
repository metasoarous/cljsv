(ns semantic-csv.impl.core
  "This namespace consists of implementation details for the main API"
  #?(:cljs (:require-macros semantic-csv.impl.core))
  (:require [clojure.string :as s]))


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
        (catch #?(:clj Exception :cljs js/Object) e
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


;; The following is for copying

(defn- subset-map [m ks]
  (into {} (map (fn [k] [k (get m k)]) ks)))

#?(:clj
   (defmacro clone-var
     "Clone the var pointed to by fsym into current ns such that arglists, name and doc metadata are preserned."
     [fsym]
     (let [v (resolve fsym)
           m (subset-map (meta v) [:arglists :name :doc])
           m (update m :arglists (fn [arglists] (list 'quote arglists)))]
       `(def ~(vary-meta (:name m) (constantly m)) ~fsym))))


;; i'm not sure if the above will work for self-compiling cljs; below is some work on this, but it may just not be possible
;; May have to just catch that case and do a best effort clone without getting all the metadata

;(defn- qualified-name
;  [m]
;  (symbol (str (:ns m) "/" (:name m))))
;
;(defmacro clone
;  "Clone a "
;  [fvar]
;  (let [;v (resolve fsym)
;        m' (subset-map (meta fvar) [:arglists :name :doc :ns])
;        m (subset-map (meta fvar) [:arglists :name :doc])
;        m (update m :arglists (fn [arglists] (list 'quote arglists)))]
;    (println m)
;    `(def ~(vary-meta (:name m) (constantly m)) ~(qualified-name m'))))

(comment
  (ns whatnot)

  (defn stuff
    "Shit and yeah"
    [x y]
    :yeah)

  (ns semantic-csv.impl.core)

  (clone-var whatnot/stuff)
  (meta #'stuff)

  :end)


(defn str->long
  "Translate to long from string."
  [^String v] (-> v clojure.string/trim Long/parseLong long))


(defn str->double
  "Translate to double from string."
  [^String v] (-> v clojure.string/trim Double/parseDouble double))


(defn str->rational
  "Translate to rational from string."
  [^String v]
  (if-let [result (re-find #"^\d+\/\d+$" v)]
    (-> v
        (clojure.string/split #"\/")
        (#(/ (-> % first str->long)
             (-> % second str->long))))
    nil ;; TODO Should this fall back to using str->double?
    ))


(defn- test-sniff-fn [f value]
  (try (f value)
       (catch Exception e nil)))


(defn- try-cast-map
  ([^String value cast-map]
   (try-cast-map value nil cast-map))
  ([^String value previous-type cast-map]
   (let [hierarchy (if previous-type
                     ;; dropping here keeps us from going from :integer to :decimal to :integer.
                     ;; If we promote to :decimal, we should stay there.
                     (drop-while #(not= % previous-type) (:hierarchy cast-map))
                     (:hierarchy cast-map))]
     (loop [cast-type (first hierarchy)
            todo-casters (rest hierarchy)]
       (cond
         (test-sniff-fn (get cast-map cast-type) value) cast-type
         (empty? todo-casters) (if previous-type :string nil)
         :else (recur (first todo-casters) (rest todo-casters)))))))


(defn sniff-value
  "Takes an input value, a previous-try path, and a sniff map.
  The function will return the path representation (i.e. [:numeric :decimal]) of the type best suited for casting.
  The [:string] vector is used to denote a column that we know is a string for certain.  This will be seen when a
  column has gone from say, [:numeric :integer] and then tested again, without success. For instance, one value
  may be '1010', and the next is '101Z'. [:string] is used to mark the column for these cases."
  [^String value previous-try cast-rules-map]
  (if (or (= previous-try [:string]) (empty? value))  ;; if the value is nil or an empty string, then return the previous try, else sniff.
    previous-try
      (if-let [[cast-class cast-type] previous-try] ;; pull the class and type out of the previous try
        ;; if we have a previous try, we can use the class to start testing only that class
        (let [cast-map (get cast-rules-map cast-class)]
          (if-let [result (try-cast-map value cast-type cast-map)]
            (if (= :string result)
              [result]
              [cast-class result])))
        ;; there was no previous try.  Cycle through all the classes
        (loop [[cast-class cast-map] (first cast-rules-map)
               todo-class-maps (rest cast-rules-map)]
          ;; if the result is non-nil, return a vector.
          (if-let [result (try-cast-map value cast-map)]
            [cast-class result]
            (if (empty? todo-class-maps)
              [:string] ;; if nothing has passed, we can mark this column a string.
              (recur (first todo-class-maps) (rest todo-class-maps))))))))


(defn sniff-vector
  "Takes an input vector, a prevous-results vector, and a cast-rules-map.
  The function will return a vector of paths for each index, representing the best suited cast operation per column."
  [row-vec previous-results cast-rules-map]
  (let [prev-res (if (empty? previous-results)
                   (into [] (repeat (count row-vec) nil))
                   previous-results)]
    (mapv #(sniff-value %1 %2 cast-rules-map)
          row-vec
          prev-res)))


(defn sniff-map
  "Takes an input map, a prevous-results map, and a cast-rules-map.
  The function will return a map of paths for each index, representing the best suited cast operation per column."
  [row-map previous-results cast-rules-map]
  (reduce-kv
   #(assoc %1 %2 (sniff-value %3 (get %1 %2) cast-rules-map))
   previous-results
   row-map))


(defn sniff-test
  "Takes an input data set, with either maps or vectors.  It then tests each rows of the passed collection,
  resulting in a single collection (based on the row level collection), providing the best suited cast operation per column.
  Note: The data passed in should be a subset of the overall data set, not including a header row."
  [data cast-rules-map]
  (let [test-fn (if (map? (first data))
                  sniff-map
                  sniff-vector)
        init-coll (if (map? (first data))
                    {}
                    [])]
    (reduce
     #(test-fn %2 %1 cast-rules-map)
     init-coll
     data)))


;; The following is ripped off from prismatic/plumbing:

(defmacro ?>>
  "Conditional double-arrow operation (->> nums (?>> inc-all? (map inc)))"
  [do-it? & args]
  `(if ~do-it?
     (->> ~(last args) ~@(butlast args))
     ~(last args)))

;; We include it here in lieue of depending on the full library due to dependency conflicts with other
;; libraries.


