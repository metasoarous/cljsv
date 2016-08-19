;; # Higher level CSV parsing/processing functionality
;;
;; The two most popular CSV parsing libraries for Clojure presently - `clojure/data.csv` and `clojure-csv` -
;; concern themselves only with the _syntax_ of CSV;
;; They take CSV text, transform it into a collection of vectors of string values, and that's it.
;; Semantic CSV takes the next step by giving you tools for addressing the _semantics_ of your data, helping
;; you put it in a form that better reflects what it represents.
;;
;; ## Features
;;
;; * Absorb header row as a vector of column names, and return remaining rows as maps of `column-name -> row-val`
;; * Write from a collection of maps, given a header
;; * Apply casting/formatting functions by column name, while reading or writing
;; * Numeric casting function helpers
;; * Remove commented out lines (by default, those starting with `#`)
;; * Compatible with any CSV parsing library returning/writing a sequence of row vectors
;; * (SOON) A "sniffer" that reads in N lines, and uses them to guess column types
;;
;; ## Structure
;;
;; Semantic CSV is structured around a number of composable processing functions for transforming data as it
;; comes out of or goes into a CSV file.
;; This leaves room for you to use whatever parsing/formatting tools you like, reflecting a nice decoupling
;; of grammar and semantics.
;; However, a couple of convenience functions are also provided which wrap these individual steps
;; in an opinionated but customizable manner, helping you move quickly while prototyping or working at the
;; REPL.
;;
;; <br/>


(ns semantic-csv.core
  "# Core API namespace"
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure-csv.core :as csv]
            [semantic-csv.impl.core :as impl :refer [?>>]]))


;; To start, require this namespace, `clojure.java.io`, and your favorite CSV parser (e.g.,
;; [clojure-csv](https://github.com/davidsantiago/clojure-csv) or 
;; [clojure/data.csv](https://github.com/clojure/data.csv); we'll mostly be using the former).
;; 
;;     (require '[semantic-csv.core :refer :all]
;;              '[clojure-csv.core :as csv]
;;              '[clojure.java.io :as io])
;;
;; Now let's take a tour through some of the processing functions we have available, starting with the input
;; processing functions.
;;
;; <br/>


;; # Input processing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Note that all of these processing functions leave the rows collection as the final argument.
;; This is to make these functions interoperable with other standard collection processing functions (`map`,
;; `filter`, `take`, etc.) in the context of the `->>` threading macro.
;;
;; Let's start with what may be the most basic and frequently needed function:


;; <br/>
;; ## mappify

(defn mappify
  "Takes a sequence of row vectors, as commonly produced by csv parsing libraries, and returns a sequence of
  maps. By default, the first row vector will be interpreted as a header, and used as the keys for the maps.
  However, this and other behaviour are customizable via an optional `opts` map with the following options:
 
  * `:keyify` - bool; specify whether header/column names should be turned into keywords (default: `true`).
  * `:header` - specify the header to use for map keys, preventing first row of data from being consumed as header.
  * `:structs` - bool; use structs instead of hash-maps or array-maps, for performance boost (default: `false`)."
  ([rows]
   (mappify {} rows))
  ([{:keys [keyify header structs] :or {keyify true} :as opts}
    rows]
   (let [consume-header (not header)
         header (if header
                  header
                  (first rows))
         header (if keyify (mapv keyword header) header)
         map-fn (if structs
                  (let [s (apply create-struct header)]
                    (partial apply struct s))
                  (partial impl/mappify-row header))]
     (map map-fn
          (if consume-header
            (rest rows)
            rows)))))

;; Here's an example to whet our whistle:
;;
;;     => (with-open [in-file (io/reader "test/test.csv")]
;;          (->>
;;            (csv/parse-csv in-file)
;;            mappify
;;            doall))
;;
;;     ({:this "# some comment lines..."}
;;      {:this "1", :that "2", :more "stuff"}
;;      {:this "2", :that "3", :more "other yeah"})
;;
;; <br/>
;;
;; Note that `"# some comment lines..."` was really intended to be left out as a comment.
;; We can address this with the following function:


;; <br/>
;; ## remove-comments

(defn remove-comments
  "Removes rows which start with a comment character (by default, `#`). Operates by checking whether
  the first item of every row in the collection matches a comment pattern. Also removes empty lines.
  Options include:
  
  * `:comment-re` - Specify a custom regular expression for determining which lines are commented out.
  * `:comment-char` - Checks for lines lines starting with this char.
  
  Note: this function only works with rows that are vectors, and so should always be used before mappify."
  ([rows]
   (remove-comments {:comment-re #"^\#"} rows))
  ([{:keys [comment-re comment-char]} rows]
   (let [commented? (if comment-char
                      #(= comment-char (first %))
                      (partial re-find comment-re))]
     (remove
       (fn [row]
         (let [x (first row)]
           (when x
             (commented? x))))
       rows))))

;; Let's see this in action with the same data we looked at in the last example:
;;
;;     => (with-open [in-file (io/reader "test/test.csv")]
;;          (->>
;;            (csv/parse-csv in-file)
;;            remove-comments
;;            mappify
;;            doall))
;;
;;     ({:this "1", :that "2", :more "stuff"}
;;      {:this "2", :that "3", :more "other yeah"})
;;
;; Much better :-)
;;
;; <br/>
;; Next, let's observe that `:this` and `:that` point to strings, while they should really be pointing to
;; numeric values.
;; This can be addressed with the following function:


;; <br/>
;; ## cast-with

(defn cast-with
  "Casts the vals of each row according to `cast-fns`, which must either be a map of
  `column-name -> casting-fn` or a single casting function to be applied towards all columns.
  Additionally, an `opts` map can be used to specify:
  
  * `:except-first` - Leaves the first row unaltered; useful for preserving header row.
  * `:exception-handler` - If cast-fn raises an exception, this function will be called with args
    `colname, value`, and the result used as the parse value.
  * `:only` - Only cast the specified column(s); can be either a single column name, or a vector of them."
  ([cast-fns rows]
   (cast-with cast-fns {} rows))
  ([cast-fns {:keys [except-first exception-handler only] :as opts} rows]
   (->> rows
        (?>> except-first (drop 1))
        (map #(impl/cast-row cast-fns % :only only :exception-handler exception-handler))
        (?>> except-first (cons (first rows))))))

;; Let's try casting a numeric column using this function:
;;
;;     => (with-open [in-file (io/reader "test/test.csv")]
;;          (->>
;;            (csv/parse-csv in-file)
;;            remove-comments
;;            mappify
;;            (cast-with {:this #(Integer/parseInt %)})
;;            doall))
;;
;;     ({:this 1, :that "2", :more "stuff"}
;;      {:this 2, :that "3", :more "other yeah"})
;;
;; Alternatively, if we want to cast multiple columns using a single function, we can do so
;; by passing a single casting function as the first argument.
;; 
;;     => (with-open [in-file (io/reader "test/test.csv")]
;;          (->>
;;            (csv/parse-csv in-file)
;;            remove-comments
;;            mappify
;;            (cast-with #(Integer/parseInt %) {:only [:this :that]})
;;            doall))
;;
;;     ({:this 1, :that 2, :more "stuff"}
;;      {:this 2, :that 3, :more "other yeah"})
;;
;; Note that this function handles either map or vector rows.
;; In particular, if you’ve imported data without consuming a header (by either not using mappify or
;; by passing `:header false` to `process` or `slurp-csv` below), then the columns can be keyed by their
;; zero-based index. 
;; For instance, `(cast-with {0 #(Integer/parseInt %) 1 #(Double/parseDouble %)} rows)`
;; will parse the first column as integers and the second as doubles.
;;
;; <br/>
;; It's worth pointing out this function isn't strictly an _input_ processing function, but could be
;; used for intermediate or output preparation processing.
;; Similarly, the following macro provides functionality which could be useful at multiple points of a
;; pipeline.


;; <br />
;; ## except-first

(defmacro except-first
  "Takes any number of forms and a final `data` argument. Threads the data through the forms, as though
  with `->>`, except that the first item in `data` remains unaltered. This is intended to operate within
  the context of an _actual_ `->>` threading macro for processing, where you might want to leave a header
  column unmodified by your processing functions."
  [& forms-and-data]
  (let [data (last forms-and-data)
        forms (butlast forms-and-data)]
    `((fn [rows#]
        (let [first-row# (first rows#)
              rest-rows# (rest rows#)]
          (cons first-row# (->> rest-rows# ~@forms))))
        ~data)))

;; This macro generalizes the `:except-first` option of the `cast-with` function for arbitrary processing,
;; operating on every row _except_ for the first.
;; For example:
;;
;;     => (->> [["a" "b" "c"] [1 2 3] [4 5 6]]
;;             (except-first (cast-with inc)
;;                           (cast-with #(/ % 2))))
;;     (["a" "b" "c"] [1 3/2 2] [5/2 3 7/2])
;;
;; This could be useful if you know you want to do some processing on all non-header rows, but don't really
;; need to know which columns are which to do this, and still want to keep the header row.
;;
;; <br/>
;; Hopefully you see the value of these functions being separate, composable units.
;; However, sometimes this modularity isn't necessary, and you want something quick and dirty to
;; get the job done, particularly at the REPL.
;; To meet this need, the following convenience functions are provided:


;; <br/>
;; ## process

(defn process
  "This function wraps together the most frequently used input processing capabilities,
  controlled by an `opts` hash with opinionated defaults:

  * `:mappify` - bool; transform rows from vectors into maps using `mappify`.
  * `:keyify` - bool; specify whether header/column names should be turned into keywords (default: `true`).
  * `:header` - specify header to be used in mappify; as per `mappify`, first row will not be consumed as header
  * `:structs` - bool; use structs instead of array-maps or hash-maps in mappify.
  * `:remove-comments` - bool; remove comment lines, as specified by `:comment-re` or `:comment-char`. Also
     removes empty lines. Defaults to `true`.
  * `:comment-re` - specify a regular expression to use for commenting out lines.
  * `:comment-char` - specify a comment character to use for filtering out comments; overrides comment-re.
  * `:cast-fns` - optional map of `colname | index -> cast-fn`; row maps will have the values as output by the
     assigned `cast-fn`.
  * `:cast-exception-handler` - If cast-fn raises an exception, this function will be called with args
    `colname, value`, and the result used as the parse value.
  * `:cast-only` - Only cast the specified column(s); can be either a single column name, or a vector of them."
  ([{:keys [mappify keify header remove-comments comment-re comment-char structs cast-fns cast-exception-handler cast-only]
     :or   {mappify         true
            keify           true
            remove-comments true
            comment-re      #"^\#"}
     :as opts}
    rows]
   (->> rows
        (?>> remove-comments (semantic-csv.core/remove-comments {:comment-re comment-re :comment-char comment-char}))
        (?>> mappify (semantic-csv.core/mappify {:keify keify :header header :structs structs}))
        (?>> cast-fns (cast-with cast-fns {:exception-handler cast-exception-handler :only cast-only}))))
  ; Use all defaults
  ([rows]
   (process {} rows)))

;; Using this function, the code we've been building above is reduced to the following:
;;
;;     (with-open [in-file (io/reader "test/test.csv")]
;;       (doall
;;         (process {:cast-fns {:this #(Integer/parseInt %)}}
;;                  (csv/parse-csv in-file))))


;; <br/>
;; ## parse-and-process

(defn parse-and-process
  "This is a convenience function for reading a csv file using `clojure/data.csv` and passing it through `process`
  with the given set of options (specified _last_ as kw_args, in contrast with our other processing functions).
  Note that `:parser-opts` can be specified and will be passed along to `clojure-csv/parse-csv`"
  [csv-readable & {:keys [parser-opts]
                   :or   {parser-opts {}}
                   :as   opts}]
  (let [rest-options (dissoc opts :parser-opts)]
    (process
      rest-options
      (impl/apply-kwargs csv/parse-csv csv-readable parser-opts))))

;;     (with-open [in-file (io/reader "test/test.csv")]
;;       (doall
;;         (parse-and-process in-file
;;                            :cast-fns {:this #(Integer/parseInt %)})))


;; <br/>
;; ## slurp-csv

(defn slurp-csv
  "This convenience function let's you `parse-and-process` csv data given a csv filename. Note that it is _not_
  lazy, and must read in all data so the file handle can be closed."
  [csv-filename & {:as opts}]
  (let [rest-options (dissoc opts :parser-opts)]
    (with-open [in-file (io/reader csv-filename)]
      (doall
        (impl/apply-kwargs parse-and-process in-file opts)))))

;; For the ultimate in _programmer_ laziness:
;;
;;     (slurp-csv "test/test.csv"
;;                :cast-fns {:this #(Integer/parseInt %)})



;; <br/>
;; # Some casting functions for your convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These functions can be imported and used in your `:cast-fns` specification.
;; They focus on handling some of the mess of dealing with numeric casting.

;; ## ->int

(defn ->int
  "Translate to int from string or other numeric. If string represents a non integer value,
  it will be rounded down to the nearest int.

  An opts map can be specified as the first arguments with the following options:
  * `:nil-fill` - return this when input is empty/nil."
  ([x]
   (->int {} x))
  ([{:keys [nil-fill]} x]
   (cond
     (impl/not-blank? x) (-> x s/trim Double/parseDouble int)
     (number? x) (int x)
     :else nil-fill)))

;; ## ->long

(defn ->long
  "Translate to long from string or other numeric. If string represents a non integer value,
  will be rounded down to the nearest long.

  An opts map can be specified as the first arguments with the following options:
  * `:nil-fill` - return this when input is empty/nil."
  ([x]
   (->long {} x))
  ([{:keys [nil-fill]} x]
   (cond
     (impl/not-blank? x) (-> x s/trim Double/parseDouble long)
     (number? x) (long x)
     :else nil-fill)))

;; ## ->float

(defn ->float
  "Translate to float from string or other numeric.

  An opts map can be specified as the first arguments with the following options:
  * `:nil-fill` - return this when input is empty/nil."
  ([x]
   (->float {} x))
  ([{:keys [nil-fill]} x]
   (cond
     (impl/not-blank? x) (-> x s/trim Float/parseFloat)
     (number? x) (float x)
     :else nil-fill)))

;; ## ->double

(defn ->double
  "Translate to double from string or other numeric.

  An opts map can be specified as the first arguments with the following options:
  * `:nil-fill` - return this when input is empty/nil."
  ([x]
   (->double {} x))
  ([{:keys [nil-fill]} x]
   (cond
     (impl/not-blank? x) (-> x s/trim Double/parseDouble)
     (number? x) (double x)
     :else nil-fill)))

;; ## ->boolean

(defn ->boolean
  "Translate to boolean from string or other numeric.

  An opts map can be specified as the first arguments with the following options:
  * `:nil-fill` - return this when input is empty/nil."
  ([x]
   (->boolean {} x))
  ([{:keys [nil-fill]} x]
   (cond
     (string? x) (case (-> x s/trim s/lower-case)
                   ("true" "yes" "t") true
                   ("false" "no" "f") false
                   "" nil-fill)
     (number? x) (not (zero? x))
     (nil? x) nil-fill
     :else (boolean x))))


;;     (slurp-csv "test/test.csv"
;;                :cast-fns {:this ->int})

;; Note these functions place a higher emphasis on flexibility and convenience than performance, as you can
;; likely see from their implementations.
;; If maximum performance is a concern for you, and your data is fairly regular, you may be able to get away with
;; less robust functions, which shouldn't be hard to implement yourself.
;; For most cases though, the performance of those provided here should be fine.


;; <br/>
;; # Output processing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; As with the input processing functions, the output processing functions are designed to be small,
;; composable pieces which help you push your data through to a third party writer.
;; And as with the input processing functions, higher level, opinionated, but configurable functions
;; are offered which automate some of this for you.
;;
;; We've already looked at `cast-with`, which can be useful in output as well as input
;; processing.
;; Another important function we'll need is one that takes a sequence of maps and turns it into a sequence
;; of vectors since this is what most of our csv writing/formatting libraries will want.


;; <br/>
;; ## vectorize

(defn vectorize
  "Take a sequence of maps, and transform them into a sequence of vectors. Options:

  * `:header` - The header to be used. If not specified, this defaults to `(-> rows first keys)`. Only
    values corresponding to the specified header will be included in the output, and will be included in the
    order corresponding to this argument.
  * `:prepend-header` - Defaults to true, and controls whether the `:header` vector should be prepended
    to the output sequence.
  * `:format-header` - If specified, this function will be called on each element of the `:header` vector, and
    the result prepended to the output sequence. The default behaviour is to leave strings alone but stringify
    keyword names such that the `:` is removed from their string representation. Passing a falsey value will
    leave the header unaltered in the output."
  ([rows]
   (vectorize {} rows))
  ([{:keys [header prepend-header format-header]
     :or {prepend-header true format-header impl/stringify-keyword}}
    rows]
   ;; Grab the specified header, or the keys from the first row. We'll
   ;; use these to `get` the appropriate values for each row.
   (let [header     (or header (-> rows first keys))
         ;; This will be the formatted version we prepend if desired
         out-header (if format-header (mapv format-header header) header)]
     (->> rows
          (map
            (fn [row] (mapv (partial get row) header)))
          (?>> prepend-header (cons out-header))))))


;; Let's see this in action:
;;
;;     => (let [data [{:this "a" :that "b"}
;;                    {:this "x" :that "y"}]]
;;          (vectorize data))
;;     (["this" "that"]
;;      ["a" "b"]
;;      ["x" "y"])
;;
;; With some options:
;;
;;     => (let [data [{:this "a" :that "b"}
;;                    {:this "x" :that "y"}]]
;;          (vectorize {:header [:that :this]
;;                      :prepend-header false}
;;                     data))
;;     (["b" "a"]
;;      ["y" "x"])


;; <br/>
;; ## batch

(defn batch
  "Takes sequence of items and returns a sequence of batches of items from the original
  sequence, at most `n` long."
  [n rows]
  (partition n n [] rows))

;; This function can be useful when working with `clojure-csv` when writing lazily.
;; The `clojure-csv.core/write-csv` function does not actually write to a file, but just formats the data you
;; pass in as a CSV string.
;; If you're working with a lot of data, calling this function would build a single massive string of the
;; results, and likely crash.
;; To write _lazily_, you have to take some number of rows, write them, and repeat till you're done.
;; Our `batch` function helps by giving you a lazy sequence of batches of `n` rows at a time, letting you pass
;; _that_ through to something that writes off the chunks lazily.

;; <br/>
;; And as promised, a function for doing all the dirty work for you:


;; <br/>
;; ## spit-csv

(defn spit-csv
  "Convenience function for spitting out CSV data to a file using `clojure-csv`.

  * `file` - Can be either a filename string, or a file handle.
  * `opts` - Optional hash of settings.
  * `rows` - Can be a sequence of either maps or vectors; if the former, vectorize will be
      called on the input with `:header` argument specifiable through `opts`.

  The Options hash can have the following mappings:

  * `:batch-size` - How many rows to format and write at a time?
  * `:cast-fns` - Formatter(s) to be run on row values. As with `cast-with` function, can be either a map
     of `column-name -> cast-fn`, or a single function to be applied to all values. Note that `str` is called
     on all values just before writing regardless of `:cast-fns`.
  * `:writer-opts` - Options hash to be passed along to `clojure-csv.core/write-csv`.
  * `:header` - Header to be passed along to `vectorize`, if necessary.
  * `:prepend-header` - Should the header be prepended to the rows written if `vectorize` is called?"
  ([file rows]
   (spit-csv file {} rows))
  ([file
    {:keys [batch-size cast-fns writer-opts header prepend-header]
     :or   {batch-size 20 prepend-header true}
     :as   opts}
    rows]
   (if (string? file)
     (with-open [file-handle (io/writer file)]
       (spit-csv file-handle opts rows))
     ; Else assume we already have a file handle
     (let [vectorize? (-> rows first map?)]
       (reduce (fn [[w prepend-header] batch]
                 [(->> batch
                       (?>> cast-fns (cast-with cast-fns))
                       (?>> vectorize?
                            (vectorize {:header header
                                        :prepend-header prepend-header}))
                       ; For safe measure
                       (cast-with str)
                       (impl/write-rows w writer-opts))
                  false])
               [file prepend-header]
               (batch batch-size rows))))))


;; Note that since we use `clojure-csv` here, we offer a `:batch-size` option that lets you format and write small
;; batches of rows out at a time, to avoid constructing a massive string representation of all the data in the
;; case of bigger data sets.


;; <br/>
;; # One last example showing everything together
;;
;; Let's see how Semantic CSV works in the context of a little data pipeline.
;; We're going to thread data in, transform into maps, run some computations for each row and assoc in,
;; then write the modified data out to a file, all lazily.
;;
;; First let's show this with `clojure/data.csv`, which I find a little easier to use for writing:
;;
;;     (require '[clojure.data.csv :as cd-csv])
;;
;;     (with-open [in-file (io/reader "test/test.csv")
;;                 out-file (io/writer "test-out.csv")]
;;       (->>
;;         (cd-csv/read-csv in-file)
;;         remove-comments
;;         mappify
;;         (cast-with {:this ->int :that ->float})
;;         ;; Do your processing...
;;         (map
;;           (fn [row]
;;             (assoc row :jazz (* (:this row)
;;                                 (:that row)))))
;;         vectorize
;;         (cd-csv/write-csv out-file)))
;;
;; Now let's see what this looks like with `clojure-csv`:
;;
;; As mentioned above, `clojure-csv` doesn't handle file writing for you, just formatting.
;; So, to maintain laziness, you'll have to add a couple steps to the end.
;; Additionally, it doesn't accept row items with anything that isn't a string, in contrast with
;; `clojure/data.csv`, so we'll have to account for this as well.
;;
;;     (with-open [in-file (io/reader "test/test.csv")
;;                 out-file (io/writer "test-out.csv")]
;;       (->>
;;         (csv/parse-csv in-file)
;;         ...
;;         (cast-with str)
;;         (batch 1)
;;         (map csv/write-csv)
;;         (reduce
;;           (fn [w row]
;;             (.write w row)
;;             w)
;;           out-file)))
;;
;; <br/>
;; And there you have it.
;; A simple, composable, and easy to use library for taking you the extra mile with CSV processing in
;; Clojure.



;; <br/>
;; # That's it for the core API
;;
;; Hope you find this library useful.
;; If you have questions or comments please either [submit an issue](https://github.com/metasoarous/semantic-csv/issues)
;; or join us in the [dedicated chat room](https://gitter.im/metasoarous/semantic-csv).


