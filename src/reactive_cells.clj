(ns reactive-cells
  "Clojure implementations of ValueCell and FormulaCell as presented in the
   book.

   These are very similar in spirit to the Hoplon Javelin library, and it's nice
   to see how easy these primitives are to implement from scratch.

   This can also be implemented very simply by using Clojure atoms and
   `add-watch`. See the comment at the bottom. We are doing it the hard way as
   an exercise in how we might implement something like this in another language
   that doesn't have such affordances in the standard library.")

(defmulti cell-value :cell-type)

(defmulti cell-add-watcher!
  (fn [{:keys [cell-type]} _watch-fn] cell-type))

(defn cell
  [initial-value]
  {:cell-type :input
   :value     (atom initial-value)
   :watchers  (atom [])})

(defmethod cell-value :input
  [{:keys [value]}]
  @value)

(defn cell-update!
  [{:keys [cell-type value watchers]} f]
  {:pre [(= :input cell-type)]}
  ;; This isn't idiomatic Clojure. Usually, we would use `swap!`. But we're
  ;; going "lower level" in order to illustrate how we might do this in a
  ;; language that doesn't have `swap!`.
  (let [old-value @value
        new-value (f old-value)]
    (when (not= old-value new-value)
      (reset! value new-value)
      (run! #(% new-value) @watchers))
    new-value))

(defmethod cell-add-watcher! :input
  [{:keys [watchers]} f]
  (swap! watchers concat [f])
  :ok)

(defn formula-cell
  [input-cell f]
  (let [cell (cell (f (cell-value input-cell)))]
    (cell-add-watcher!
      input-cell
      (fn [new-input-value]
        (cell-update!
          cell
          (fn [_current-formula-value]
            (f new-input-value)))))
    {:cell-type :formula
     :cell      cell}))

(defmethod cell-value :formula
  [{:keys [cell]}]
  (cell-value cell))

(defmethod cell-add-watcher! :formula
  [{:keys [cell]} f]
  (cell-add-watcher! cell f))

(comment
  (def c (cell 42))
  (cell-value c)
  (cell-update! c inc)
  (cell-add-watcher! c #(println "Cell value updated:" %))

  (def d (formula-cell c #(* % 2)))
  (cell-value d)
  (cell-add-watcher! d #(println "Formula cell value updated:" %)))

;; Much simpler way to do this using the Clojure standard library (atoms and
;; `add-watch`)
(comment
  (defn formula-cell
    [input-cell f]
    (let [cell (atom (f @input-cell))]
      (add-watch input-cell (gensym) (fn [_ _ _ new-value]
                                       (reset! cell (f new-value))))
      cell))


  (def c (atom 42))
  (deref c)
  (swap! c inc)
  (add-watch c (gensym) (fn [_ _ _ new-value]
                          (println "Cell value updated:" new-value)))

  (def d (formula-cell c #(* % 2)))
  (deref d)
  (add-watch d (gensym) (fn [_ _ _ new-value]
                          (println "Formula cell value updated:" new-value))))
