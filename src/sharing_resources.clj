(ns sharing-resources
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

(defn add-item-to-cart-v1
  "Prone to a race condition where the DOM update is supposed to be \"last
   writer wins\", but that might not necessarily be what happens."
  [item]
  (future
    (swap! cart update :items concat [item])
    (let [cost     (cost-ajax @cart)
          shipping (shipping-ajax @cart)]
      (reset! total (+ cost shipping)))))

(defonce update-total-queue
  (let [queue (LinkedBlockingQueue.)]
    (future
      (while true
        (let [cart'    (.take queue)
              cost     (cost-ajax cart')
              shipping (shipping-ajax cart')]
          (reset! total (+ cost shipping)))))
    queue))

(defn add-item-to-cart-v2
  "Here we eliminate the race condition by queuing the work to be done, in the
   order we want."
  [item]
  (let [cart' (swap! cart update :items concat [item])]
    (.put update-total-queue cart')))

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
  ;; up being written an $75.83 (undesirable). This is because we cannot control
  ;; the order in which the final writes (e.g. to the DOM) happen.
  (do
    (test-add-item add-item-to-cart-v1)
    (Thread/sleep 7000)
    @total)

  ;; We can solve this problem by replacing work with adding to a queue. This
  ;; way, we can control the order of the work to be done.
  (do
    (test-add-item add-item-to-cart-v2)
    (Thread/sleep 7000)
    @total))
