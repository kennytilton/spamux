(ns spamux.util
  (:require

    [tiltontec.cell.base :refer [ia-type unbound]]
    [tiltontec.cell.core :refer-macros [cF+ cF cFonce] :refer [cI]]
    [tiltontec.cell.observer :refer-macros [fn-obs]]
    [tiltontec.cell.integrity
     :refer-macros [with-cc with-integrity]
     :refer []]
    [tiltontec.cell.synapse
     :refer-macros [with-synapse]
     :refer []]

    [tiltontec.model.core
     :refer [matrix mx-par <mget <mget mset!> mset!> mswap!>
             fasc mxi-find mxu-find-name mxu-find-type mxu-find-id
             fmo fmov]
     :as md]
    [tiltontec.xhr
     :refer [make-xhr send-xhr send-unparsed-xhr xhr-send xhr-await xhr-status
             xhr-status-key xhr-resolved xhr-error xhr-error? xhrfo synaptic-xhr synaptic-xhr-unparsed
             xhr-selection xhr-to-map xhr-name-to-map xhr-response]]
    [clojure.string :as str]))

(defn pln [& args]
  (locking *out*
    (println (str/join " " args))))

(defn xpln [& args])

(defn now []
  (.getTime (js/Date.)))

(defn eko [key value]
  (pln :eko!!! key value)
  value)

;;; --- view  -----------------------------------------

(def hbox {:display "flex"
           :flex-direction "row"})

;;; --- xhr bound ---------------------------------------

(defn xhr?-response
  "Tolerates nil XHR assuming will arrive later in data flow"
  [xhr]
  (when xhr
    (xhr-response xhr)))

(defn xhr?-ok-body [xhr]
  (when-let [r (xhr?-response xhr)]
    (when (= 200 (:status r))
      (:body r))))

(defn if-bound [x]
  (when (not= x unbound) x))

(defn mx-find-matrix [mx]
  (mxu-find-type mx :spamux.core/spamux))

(defn syn-xhr-ok-body [me id uri]
  (when-let [r (xhr-response
                 (with-synapse (id)
                   (send-xhr uri)))]
    (when (= 200 (:status r))
      (:body r))))

;;; --- utils -------------------------------------------

(defn xhr-poller
  ([id uri re-poll-test re-poll-after]
   (xhr-poller id uri re-poll-test re-poll-after (constantly true)))
  ;; todo GC these
  ([id uri re-poll-test re-poll-after new-poller-when]
   (with-synapse (id)
     (when-let [final-uri (if (string? uri)
                            uri (uri))]
       (md/make ::poller
         :re-poll (cI 0)
         :re-poll-test re-poll-test
         :xhr (cF (when (<mget me :re-poll)
                    (send-xhr :get-running final-uri)))
         :response (cF+ [:obs (fn-obs
                                (when ((:re-poll-test @me) new)
                                  (js/setTimeout
                                    #(with-cc
                                       (mswap!> me :re-poll inc)) re-poll-after)))]
                     (if-let [body (xhr?-ok-body (<mget me :xhr))]
                       (do
                         (merge {:when (now)} body))
                       (if-bound cache))))))))