(ns pizza
  (:import [java.util.concurrent LinkedBlockingQueue]))

;; Lightweight logging because I didn't feel like bringing in a logging library
;; and configuring it
(defonce log-queue
  (let [queue (LinkedBlockingQueue.)]
    (future
      (while true
        (binding [*out* *err*]
          (apply println "[LOG]" (.take queue)))))
    queue))

(defn log
  [& xs]
  (.put log-queue xs))

(defn make-dough
  []
  (Thread/sleep 10000)
  (log "Made dough.")
  (gensym "dough"))

(defn grate-cheese
  []
  (Thread/sleep 3000)
  (log "Grated cheese.")
  (gensym "cheese"))

(defn make-sauce
  []
  (Thread/sleep 7500)
  (gensym "sauce"))

(defn roll-out
  [dough]
  (Thread/sleep 2000)
  (log "Rolled out dough.")
  ;; Minimal pizza object
  {:dough dough})

(defn spread-sauce
  [pizza sauce]
  (Thread/sleep 1000)
  (log "Spread sauce.")
  (assoc pizza :sauce sauce))

(defn spread-cheese
  [pizza cheese]
  (Thread/sleep 1000)
  (log "Spread cheese.")
  (assoc pizza :cheese cheese))

(defn bake
  [pizza]
  (log "Put pizza in oven.")
  (Thread/sleep 60000)
  (log "Took pizza out of oven.")
  (assoc pizza :baked? true))

(defn make-pizza-v1
  "Make pizza synchronously."
  []
  (-> (roll-out (make-dough))
      (spread-sauce (make-sauce))
      (spread-cheese (grate-cheese))
      bake))

(defn make-pizza-v2
  "First prepare the ingredients (one actor per ingredient), then one actor
   finishes the assembly."
  []
  (let [dough  (future (make-dough))
        sauce  (future (make-sauce))
        cheese (grate-cheese)]
    (-> (roll-out @dough)
        (spread-sauce @sauce)
        (spread-cheese cheese)
        bake)))

(comment
  ;; Elapsed time: 84506.408808 msecs
  (time (make-pizza-v1))
  ;; Elapsed time: 74004.934794 msecs
  (time (make-pizza-v2)))
