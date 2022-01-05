(ns rips-web.events
  (:require
   [re-frame.core :as re-frame]
   [rips-web.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::set-active-panel
 (fn-traced [db [_ active-panel]]
   (assoc db :active-panel active-panel)))

(re-frame/reg-event-db
  ::set-name
  (fn-traced [db [_ new-name]]
     (assoc db :name new-name)))
