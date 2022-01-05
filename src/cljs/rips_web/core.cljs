(ns rips-web.core
  (:require
   [quil.core :as q :include-macros true]
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [rips-web.events :as events]
   [rips-web.routes :as routes]
   [rips-web.views :as views]
   [rips-web.config :as config]))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

;(defn ^:dev/after-load resize-listeners []
;  (.addEventListener js/window "resize" 
;   (fn [event]
;     (let [w (.-innerWidth js/window)
;           h (.-innerHeight js/window)]
;       (q/with-sketch (q/get-sketch-by-id "canvas_wrapper")
;         (q/background 100))))))

(defn init []
  (routes/app-routes)
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
  ;(resize-listeners))
