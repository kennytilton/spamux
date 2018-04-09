(ns spamux.job-control
  (:require [clojure.string :as str]
            [tiltontec.util.core :refer [pln now]]
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

            [tiltontec.webmx.gen :refer [evt-tag target-value]
             :refer-macros [h1 h2 h3 h4 h5 input div span button p b a li ul
                            select option label]]
            [tiltontec.webmx.html :refer [mxu-find-tag]]
            [tiltontec.webmx.widget :refer [tag-checkbox]]

            [cemerick.url :refer (url url-encode)]
            [cljs.pprint :as pp]
            [spamux.util
             :refer [syn-xhr-ok-body if-bound mx-find-matrix xhr?-ok-body xhr?-response hbox]]
            [spamux.job :refer [mtx-job make-xhr-job mtx-job-running? mtx-job-id
                                job-start-button]]))

;;; --- builder interface --------------------------

(declare build-email-file-button)

(defn builder-panel []
  (div {:style {:padding      "9px"
                :min-width    "350px"
                :border       "solid"
                :border-width "1px"
                :border-color "gray"}}
    (p (b "1. Build a new file to be cleaned."))

    (label {:for   "email-k-count"
            :style "margin-right:6px"}
      "Email count in thousands:")
    (input
      {:id          "email-k-count"
       :type        "number"
       :style       "text-align:right"
       :placeholder "Number of K emails"
       :oninput     #(mset!> (evt-tag %) :value (target-value %))
       }
      {:value (cI "")})
    (p)
    (label {:for    "projected-size"
            :style  "margin-right:6px"

            ;; Your Mission part B: change this to be a formulaic cell returning true
            ;; if the email volume is blank
            :hidden (cF (empty? (<mget (fmo me "email-k-count") :value)))}
      "Projected File Size:")

    ;; Start Here:
    ;;   First, a glossary for formulas:
    ;;      - cI is short for "[make] an input cell, one to which we can assign
    ;;        to (in event handlers, mostly). You won't need to code these just yet,
    ;;        but just in case you see one and are curious, that's the deal.
    ;;
    ;;      - cF is short for "[make] cell formulaic". These are re-run automatically
    ;;        when any of their dependencies change.
    ;;
    ;;      - inside formulas, the anaphoric variable 'me' is like 'self' or 'this' in other languages
    ;;
    ;;      - 'fmo' is a family (fm) tree searcher, by name or id, for some other (o) matrix object
    ;;        It gets used a lot, so we keep the name short.
    ;;
    ;;      - (<mget X P) establishes a dependency of a formulaic cell on the property P of
    ;;        object X. You can also use this as a generic accessor outside formulas. Anythin X that
    ;;        is being accessed this way is an atom (CLJS) or ref (CLJ) containing a map.
    ;;
    ;;   Now look around for the string "Your Mission" and try to implement what is suggested.
    ;;   The "part X" bit suggests a good order.
    ;;
    ;;   Please file an issue if you get stuck and I will improve the instructions until you
    ;;   get unstuck.

    (span
      ;; For any tag/function such as 'span', the optional first map is for HTML attributes
      ;; destined for the DOM.
      {
       :id      "projected-size"

       ;; Your Mission, part A: change this to compute a tenth of the volume if the volume is
       ;; not blank and make that the content, adding an "MB" suffix.
       :content (cF (let [emv$ (<mget (fmo me "email-k-count") :value)]
                      (pp/cl-format nil "For ~a thousand emails?" emv$)))

       ;; Your Mission part C (find B at line 66): have the background switch to pink #fcc" if the file size
       ;; is more than 100MB. Do not worry about DRY (ie, duplicating calculations) just yet.
       :style   "background:#cfc"
       }

      ;; The optional second map parameter to a tag is for non-HTML attributes. We use
      ;; it for custom state we may find handy to pre-compute.
      {
       ;; Your Mission part D: compute here the number of megabytes of file size we project, then
       ;; go back to use this value instead of doing their own calculations. We offer an example.
       :email-mb-size       (cF (* 6 9))
       :hh-guide-answer-ok? (cF 42 (= <mget me :email-mb-size))
       })

    (p (build-email-file-button))))

(defn build-email-file-button []
  ;; todo: have job-start-button take a map of self-ducmenting options
  (job-start-button :build
    (fn [me]
      (empty? (fmov me "email-k-count")))
    (fn [me]
      (make-xhr-job {
                     :job-type :build
                     :uri      (pp/cl-format nil "start?job-type=build&k-count=~a"
                                 (let [fw (fmo me "email-k-count")]
                                   (assert fw)
                                   (<mget fw :value)))}))))

(declare raw-file-menu)

(defn cleaner-panel []
  (div {:style {:min-width    "500px"
                :margin-left  "24px"
                :padding      "9px"
                :border       "solid"
                :border-width "1px"
                :border-color "gray"}}

    (div
      (p "<b>2. Clean a file.</b>")
      (raw-file-menu))

    (div {:style "display:flex; flex-direction:row"}
      (span "Job options: ")
      (tag-checkbox me "output?"
        "Generate output?" false
        {:name  "output?"
         :style "background:white;padding:6px"})

      (tag-checkbox me "log-fails?"
        "Log fails?" false
        {:name  "log-fails"
         :style "background:white;padding:6px"})

      (tag-checkbox me "sample-fail-p"
        "Sample fails?" true
        {:name  "sample-fails"
         :style "background:white;padding:6px"})

      (tag-checkbox me "watch-progress"
        "Watch progress" true
        {:name  "watch-progress"
         :style "background:white;padding:6px"}))

    (job-start-button :clean
      (fn [me]
        (let [raw-file (fmov me "email-file-raw")]
          (or (nil? raw-file)
            (= raw-file "<none>"))))
      (fn [me]
        (make-xhr-job {:job-type :clean
                       :uri      (pp/cl-format nil
                                   "start?job-type=clean&filename=~a&outputp=~a&logfail=~a"
                                   (fmov me "email-file-raw")
                                   (fmov me "output?" :on?)
                                   (fmov me "log-fails?" :on?))})))))

(defn raw-file-menu []
  (div {:class "pure-u-1 pure-u-md-1-3"
        :style "margin-bottom:18px"}

    (label {:for   "email-file-raw"
            :style "margin-right:6px"}
      "File to clean:")

    (select {:id       "email-file-raw"
             :class    "pure-input-1-2"
             :style    "background:white"
             :onchange #(mset!> (evt-tag %) :value (target-value %))}
      {:value   (cI nil)
       :options (cF (let [menu me
                          xhr (with-synapse (:getraws)
                                (when (or (when-let [job (mtx-job menu)]
                                            (and (= :build (:job-type @job))
                                                 (= "complete" (:status (<mget job :status)))))
                                        (= cache unbound))  ;; force initial load
                                  (send-xhr :get-raws "rawfiles")))]
                      (or (xhr?-ok-body xhr)
                        (if-bound cache))))}

      [(option {:enabled  "false"
                :selected true
                :value    "<none>"}
         "Pick a file, any file.")
       (map (fn [s]
              (option {:selected false} s))
         (<mget me :options))])))

