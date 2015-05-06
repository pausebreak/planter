(ns ^:figwheel-always planter.core
  (:require [cljs.core.async :refer [chan close! <!]]
            [quiescent.core :as q]
            [quiescent.dom :refer [header h1 div img button]])
  (:require-macros
    [cljs.core.async.macros :as m :refer [go]]))

;;plant maker/Jeans/Seeds/Field of plants,Dirt, and Seeds/Hello_World.  

(enable-console-print!)

(defonce state (atom {:plots [{:id 0 :stage "dirt"} 
                              {:id 1 :stage "dirt"} 
                              {:id 2 :stage "dirt"} 
                              {:id 3 :stage "dirt"}]
                      :pocket 1
                      :win? false}))
;WIN! "mon" ;;/;;

(defn console
  "Helper for the println debug crowd"
  [& s]
  (. js/console (log (apply pr-str s))))

(defn timeout [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn- did-win?
  [seeds]
  (>= seeds 20))

(defn- next-stage
  [s]
  (cond
    (= s "dirt")
      "planted"
    (= s "planted")
      "growing"
    (= s "growing")
      "seeds"
    (= s "seeds")
      "dirt"))

(q/defcomponent pocket
  "A pocket for seeds"
  [spec]
  (div {:className "pants"}
    (header {} "Jeans")
    (div {:className "pocket"}
      (header {} "Seeds")
      spec)))

(defn- grow-plot
  [p]
  (go
    (<! (timeout 3600))
    (swap! state #(update-in % [:plots (:id p) :stage] next-stage))
    (<! (timeout 3600))
    (swap! state #(update-in % [:plots (:id p) :stage] next-stage))))

(defn- plot-click 
  [p]
  (let [id (:id p)
        next-state (next-stage (:stage p))]
    (when (or (not (= (:stage p) "dirt"))
              (> (:pocket @state) 0))
      (swap! state #(update-in % [:plots id :stage] next-stage))
      (cond 
        (= next-state "dirt")
          (swap! state #(update-in % [:pocket] + 2))
        (= next-state "planted")
          (do
            (swap! state #(update-in % [:pocket] dec))
            (grow-plot p))))))

(q/defcomponent plot
  "A seedable plot"
  [p]
  (let [cls-name (str "plot " (:stage p))]
    (div {:className cls-name :onClick (fn [] (plot-click p))}
      (:stage p))))

(q/defcomponent land
  "A grid of plots"
  [props]
  (div {:className "land"}
    (header {} "Field of Plants, Dirt and Seeds")
      (apply div {:className "field"}
        (map plot (:plots props)))))

(defn- playagain
  [& _]
  (let [ns (-> @state
               (assoc-in [:pocket] 1)
               (assoc-in [:win?] false))]
    (reset! state ns)))

(q/defcomponent winner
  "What shows when you win"
  [_]
  (div {:className "win"}
    (div {}
      (img {:src "winner.jpg"})
      (button {:className "playagain" :onClick playagain} "Play Again"))))

(q/defcomponent farm
  "The overall farm"
  [props]
  (div {} 
    (h1 {} "Plant Maker")
    (if (:win? props)
      (winner)
      (div {}
        (pocket (:pocket props))
        (land props)))))

(defn render!
  [state]
  (q/render (farm state)
            (.getElementById js/document "app")))

(add-watch state :main 
  (fn [k r os ns] 
    (console ns)
    (render! ns)
    (when (and (did-win? (:pocket ns))
               (not (:win? ns)))
      (swap! state #(assoc-in % [:win?] true)))))

;(swap! state #(assoc-in % [:pocket] 1))
;(swap! state #(assoc-in % [:win?] false))
;(playagain)
(render! @state)
