(ns coordinating-timelines
  (:import [java.util.concurrent LinkedBlockingQueue]))

(def cart
  (atom {:items []}))

(def total
  (atom 0.00))

(defn cost-ajax
  [{:keys [items]}]
  ;; Simulate the time the HTTP call might take
  (Thread/sleep (rand-int 2000))
  ;; In reality, the price data would come from the backend and be returned to
  ;; us in the HTTP response. But for demo purposes, we'll assume here that the
  ;; prices on the items on the frontend are the same as what the backend would
  ;; return.
  (reduce + (map :price items)))

(defn shipping-ajax
  [{:keys [items]}]
  ;; Simulate the time the HTTP call might take
  (Thread/sleep (rand-int 2000))
  ;; See comment above.
  (let [cost (reduce + (map :price items))]
    (* 0.1 cost)))

;; NOTE: See v2 in `sharing-resources` for the answer to the question of why we
;; are using a queue here. The code below is copied and modified from v2.

(defonce update-total-queue-v3
  (let [queue (LinkedBlockingQueue.)]
    (future
      (while true
        (let [cart'       (.take queue)
              ;; Here, we simulate a local variable, e.g. `let total = 0;` Both
              ;; of the AJAX calls below update it in parallel.
              local-total (atom 0)]
          ;; In an attempt to optimize for performance, we calculate the cost
          ;; and shipping in parallel and add each to the local total variable.
          (future
            (let [cost (cost-ajax cart')]
              (swap! local-total + cost)))
          (future
            (let [shipping (shipping-ajax cart')]
              (swap! local-total + shipping)
              ;; Timing bug: We're assuming here that the second AJAX call will
              ;; complete after the first, and proceeding to update the DOM
              ;; (represented here by the global `total` atom)
              (reset! total @local-total))))))
    queue))

(defn add-item-to-cart-v3
  "Prone to another race condition where the DOM update could happen before the
   cost AJAX call has updated the local total. The result is that the total is
   written to the DOM as just the shipping, instead of cost + shipping"
  [item]
  (let [cart' (swap! cart update :items concat [item])]
    (.put update-total-queue-v3 cart')))

(defonce update-total-queue-v4
  (let [queue (LinkedBlockingQueue.)]
    (future
      (while true
        (let [cart'       (.take queue)
              ;; Here, we simulate a local variable, e.g. `let total = 0;` Both
              ;; of the AJAX calls below update it in parallel.
              local-total (atom 0)
              ;; The number of tasks remaining.
              remaining   (atom 2)
              ;; A function that each task calls when it's done.
              done!       (fn []
                            (swap! remaining dec)
                            ;; Only update the global total (which represents
                            ;; the DOM) once all tasks have completed.
                            (when (= 0 @remaining)
                              (reset! total @local-total)))]
          ;; In an attempt to optimize for performance, we calculate the cost
          ;; and shipping in parallel and add each to the local total variable.
          (future
            (let [cost (cost-ajax cart')]
              (swap! local-total + cost)
              (done!)))
          (future
            (let [shipping (shipping-ajax cart')]
              (swap! local-total + shipping)
              (done!))))))
    queue))

(defn add-item-to-cart-v4
  "Here we eliminate the race condition by waiting until both the cost and
   shipping numbers come back before we add them to the total."
  [item]
  (let [cart' (swap! cart update :items concat [item])]
    (.put update-total-queue-v4 cart')))

(comment
  cart
  total

  (defn- test-add-item
    [add-item-fn]
    (reset! total 0.00)
    (reset! cart {:items [{:price 1.99}
                          {:price 2.50}
                          {:price 3.00}
                          {:price 10.99}]})
    (add-item-fn {:price 5.00})
    (add-item-fn {:price 500.00}))

  ;; Sometimes when you add two items in rapid succession like this, the total
  ;; ends up being written as $575.83 (desirable), and sometimes, the total ends
  ;; up being written as a lesser amount that is just the shipping
  ;; (undesirable).
  (do
    (test-add-item add-item-to-cart-v3)
    (Thread/sleep 7000)
    @total)

  ;; We can solve this problem by coordinating the cost and shipping AJAX calls
  ;; to ensure that we wait until both have completed before we update the DOM.
  ;;
  ;; FIXME: This still isn't working. There must be a bug in my implementation,
  ;; but you get the idea!
  ;;
  ;; This code is also a bit silly because in idiomatic Clojure, we would just
  ;; deref the futures and call it a day.
  (do
    (test-add-item add-item-to-cart-v4)
    (Thread/sleep 7000)
    @total))
