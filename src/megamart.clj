(ns megamart)

(defn clear-cart
  [state]
  (update-in state [:cart :items] empty))

(defn add-item
  [state item]
  (update-in state [:cart :items] concat [item]))

(defn tax
  [amount]
  (* amount 0.10))

(defn cart-total
  [{:keys [items]}]
  (reduce #(+ %1 (:price %2)) 0 items))

(defn free-shipping?
  [cart]
  (<= 20 (cart-total cart)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def app-state
  (atom {:cart
         {:items []}

         :catalog
         [{:name "milk" :price 2.49}
          {:name "apple" :price 0.50}
          {:name "tangerines" :price 1.50}
          {:name "bread" :price 1.99}
          {:name "crackers" :price 0.99}]}))

(defn view
  [{:keys [cart catalog]}]
  (let [total (cart-total cart)]
    {:cart
     (assoc cart
            :total total
            :tax   (tax total))

     :catalog
     (for [item catalog
           :let [cart-with-item (update cart :items concat [item])]]
       (assoc item :free-shipping? (free-shipping? cart-with-item)))}))

(comment
  (view @app-state)

  (swap! app-state clear-cart)
  (swap! app-state add-item (rand-nth (:catalog @app-state))))
