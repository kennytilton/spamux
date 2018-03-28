(ns tiltontec.webmx.example.spamux.core
  (:require [clojure.string :as str]
            [tiltontec.util.core :refer [pln]]
            [tiltontec.cell.base :refer [ia-type]]
            [tiltontec.cell.core :refer-macros [cF+ cF cFonce] :refer [cI]]
            [tiltontec.cell.observer :refer-macros [fn-obs]]

            [tiltontec.model.core
             :refer [matrix mx-par <mget <mget mset!> mset!> mxi-find mxu-find-name
                     mxu-find-type]
             :as md]

            [tiltontec.xhr
             :refer [make-xhr send-xhr send-unparsed-xhr xhr-send xhr-await xhr-status
                     xhr-status-key xhr-resolved xhr-error xhr-error? xhrfo synaptic-xhr synaptic-xhr-unparsed
                     xhr-selection xhr-to-map xhr-name-to-map xhr-response]]

            [tiltontec.webmx.gen :refer [evt-tag target-value]
             :refer-macros [h1 h4 h5 input div span button p b]]
            [cljs.pprint :as pp]))

(declare start-button show-stats)

(defn matrix-build! []
  ;;;;(pline/pipe-go)
  (md/make ::spamux
    :mx-dom (cFonce (md/with-par me
                      (let [mtx me]
                        (assert mtx)
                        [(div {:style "margin:36px"}
                           (h1 "Hello, SpamUX!")
                           (start-button)
                           (show-stats))])))))

(defn start-button []
  (button
    {:onclick #(let [me (do (evt-tag %))]
                 (mset!> me :job-key
                   (case (<mget me :job-key)
                     nil :start
                     :start :stop
                     :stop :start)))

     :style   (cF (case (<mget me :job-key)
                    :stop "background:green"
                    :start "background:red"
                    "background:green"))

     :content (cF (or (when-let [xhr (<mget me :ae)]
                        (when-let [r (xhr-response xhr)]
                          (if (= 200 (:status r))
                            (:body r)
                            "<b>Something happened.</b>")))
                    "<h2>Start</h2>"))}
    {:job-key (cI nil)
     :ae      (cF+ [:obs (fn-obs
                           (println :ae-obs new old))]
                (when-let [job (<mget me :job-key)]
                  (println :sending!!!-start job)
                  (send-xhr (case job
                              :start "/start"
                              :stop "/stop"))))}))

(defn show-stats []
  (div
    (button
      {:name "stats-button"
       :onclick #(let [me (do (evt-tag %))]
                   (mset!> me :job-key :get-stats))
       :content "<b>See da Stats</b>"}

      {:job-key (cI nil :ephemeral? true)

       :xhr (cF+ [:obs (fn-obs
                         (println :xhr-obs new old))]
              (when-let [job (<mget me :job-key)]
                (println :sending!!!start job)
                (send-xhr :get-stats "/batchstats" {:accept :json})))

       :stats (cF (when-let [xhr (<mget me :xhr)]
                    (when-let [r (xhr-response xhr)]
                      (println
                        (if (= 200 (:status r))
                        (:body r)
                        {:oops r}))
                      (if (= 200 (:status r))
                        (:body r)
                        {:oops (:status r)}))))})


    (div
      (for [[k v] (<mget (md/mxu-find-name me "stats-button") :stats)]
        (p (b (str k " = " v)))))))



