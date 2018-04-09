(ns spamux.progress-viewer
  (:require [clojure.string :as str]
            [tiltontec.util.core :refer [pln]]
            [tiltontec.cell.base :refer [ia-type unbound]]
            [tiltontec.cell.core :refer-macros [cF+ cF cFonce] :refer [cI]]
            [tiltontec.cell.observer :refer-macros [fn-obs]]
            [tiltontec.cell.integrity
             :refer-macros [with-cc with-integrity]
             :refer []]
            [tiltontec.cell.synapse
             :refer-macros [with-synapse]]

            [tiltontec.model.core
             :refer [matrix mx-par <mget <mget mset!> mset!> mswap!>
                     mxi-find mxu-find-name mxu-find-type mxu-find-id
                     fmo fmov]
             :as md]

            [tiltontec.xhr
             :refer [make-xhr send-xhr send-unparsed-xhr xhr-send xhr-await xhr-status
                     xhr-status-key xhr-resolved xhr-error xhr-error? xhrfo synaptic-xhr synaptic-xhr-unparsed
                     xhr-selection xhr-to-map xhr-name-to-map xhr-response]]

            [tiltontec.webmx.gen :refer [evt-tag target-value]
             :refer-macros [h1 h2 h3 h4 h5 input div span button p b a li ul
                            select option label]]
            [tiltontec.webmx.html :refer [mxu-find-tag]]
            [tiltontec.webmx.widget :refer [tag-checkbox]]

            [cljs.pprint :as pp]

            [spamux.job :refer [mtx-job mtx-job-id mtx-job-type]]
            [spamux.util :refer [if-bound xhr-poller mx-find-matrix]]))

(declare json-view stats-displayer)

(defn job-status-view [title]
  (div {:style "opacity:0;min-width:144px"
        :class (cF (when (<mget me :value) "fazer"))}

    {:value (cF (when-let [job (mtx-job me)]
                  (when-let [s (<mget job :status)]
                    (str/capitalize (:status s)))))}

    (b title)
    (p {:content (cF (<mget (mx-par me) :value))
        :style   "font-size:1em"})))

(declare rejected-score-display)
(def standard-stat-style
  {:min-width     "72px"
   :margin        "2px"
   :padding-right "2px"
   :text-align    "right"
   :font-weight   "bold"
   :background    "white"})

(defn stats-displayer []
  (div {:style "opacity:0;margin-left:36px"
        :class (cF (when (and
                           (<mget (md/mxu-find-name me "watch-progress") :on?)
                           (when-let [job (mtx-job me)]
                             (= :clean (:job-type @job))
                             (<mget job :job-id)))
                     "fazer"))}
    {:name  "stat-group"

     ;; the stats derived state might reasonably go right on the job object,
     ;; but so far it is not needed unless the user wants to watch the
     ;; running stats, so why do all the heavy lifting? Let us see if
     ;; we end up needing stats for automatic job control, such as aborting
     ;; jobs if the failure rate gets too high.
     :stats (cF (let [displayer me]
                  (when-let [poller (xhr-poller :poll-running
                                      (fn []
                                        (when-let [job-id (and
                                                            (<mget (fmo displayer "watch-progress") :on?)
                                                            (mtx-job-id displayer))]
                                          (str "runningstats?job-id=" job-id)))
                                      (fn [response]

                                        (not= "complete"
                                          (:status response)))
                                      100)]
                    (<mget poller :response))))}
    [(b "Running Stats")
     (p)
     (when-let [job-id (mtx-job-id me)]
       (for [[lbl stat-key fmtr] (cons
                                   ["Duration" :run-duration
                                    (fn [val]
                                      (pp/cl-format nil "~,3f" (/ val 1000.0)))]
                                   (case (mtx-job-type me)
                                     :clean
                                     [
                                      ["Sent" :sent-ct]
                                      ["Dup Email" :rejected-dup-addr]
                                      ["High score" :rejected-score]
                                      ["High mean" :rejected-overall-mean]
                                      ["Span mean" :rejected-span-mean]]

                                     :build
                                     [["Built" :built-ct]]))]
         (div {:style {:display        "flex",
                       :flex-direction "row"}} {}
           (span {:style   {:min-width "96px"
                            :margin    "2px"}
                  :content (str lbl ": ")})
           (case stat-key
             :rejected-score (rejected-score-display)

             (span {:style   standard-stat-style
                    :content (let [f (or fmtr str)]
                               (cF (when-let [ss (fmo me "stat-group")]
                                     (when-let [stats (<mget ss :stats)]
                                       (f (stat-key stats))))))})))))]))

(defn rejected-score-display []
  ;; We want operators to stop batches if the reject rate is too high.
  ;;
  ;; Your Mission (extra credit): Change the formula to display a reject *ratio* of
  ;; high score rejections to emails sent, and change the packground to pink if it exceeds
  ;; something like 40%.
  ;;
  (span {:style standard-stat-style
         :content (cF (str (:rejected-score (<mget (fmo me "stat-group") :stats))))}))

(def spam-format "~{<p :style 'background:#FCC;min-width:250px;max-width:250px'>~a</p>~}")

(defn fails-displayer []
  (div {:style "opacity:0;margin-left:36px"
        :class (cF (when (and
                           (<mget (fmo me "sample-fails") :on?)
                           (mtx-job-id me)
                           (= :clean (mtx-job-type me)))
                     "fazer"))}
    {:name  "fails-group"
     :fails (cF (let [src (mxu-find-name me "stat-group")]
                  (assert src)
                  (let [fails (:fails (<mget src :stats))]
                    (or fails (if-bound cache)))))}
    (b "Fails")
    (div {:style   "background:#fdd"
          :content (cF (pp/cl-format nil spam-format
                         (map #(with-out-str (pp/pprint %))
                           (<mget (fmo me "fails-group") :fails))))})))
