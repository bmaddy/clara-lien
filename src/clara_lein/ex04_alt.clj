(ns clara-lein.ex04-alt
  (:require [clara.rules :as c :refer [defrule defquery insert! retract!]]
            [clara.rules.accumulators :as acc]
            [clara.tools.fact-graph :refer [session->fact-graph]]
            [clara.tools.inspect :refer [inspect]]))

(defrecord Datum [name attr value])
(defrecord Value [attr value])
(defrecord Guess [name attr value])
(defrecord Person [name age birthday])

(def facts [(->Value :age 7)
            (->Value :age 8)
            (->Value :age 9)
            (->Value :birthday :january)
            (->Value :birthday :april)
            (->Value :birthday :september)
            ;; (->Value :name "Arnold")
            ;; (->Value :name "Eric")
            ;; (->Value :name "Peter")
            ])

(defrule generate-combinations
  [?f <- Value (= attr ?a) (= value ?v)]
  =>
  (insert! (->Guess "Arnold" ?a ?v)
           (->Guess "Eric" ?a ?v)
           (->Guess "Peter" ?a ?v)))

(defrule find-solution
  [Guess (= name "Arnold") (= attr :age) (= value ?a-age)]
  [Guess (= name "Eric")   (= attr :age) (= value ?e-age)]
  [Guess (= name "Peter")  (= attr :age) (= value ?p-age)]
  [Guess (= name "Arnold") (= attr :birthday) (= value ?a-birthday)]
  [Guess (= name "Eric")   (= attr :birthday) (= value ?e-birthday)]
  [Guess (= name "Peter")  (= attr :birthday) (= value ?p-birthday)]
  [:test (= ?a-birthday :september)]
  [:test (= ?e-age 7)]
  [:test (= ?p-age 8)]
  [:test (= ?p-birthday :april)]
  [:test (= 3 (count (hash-set ?a-age ?e-age ?p-age)))]
  [:test (= 3 (count (hash-set ?a-birthday ?e-birthday ?p-birthday)))]
  =>
  #_(println {:a [?a-age ?a-birthday]
            :e [?e-age ?e-birthday]
            :p [ ?p-age ?p-birthday]})
  (insert! (->Person "Arnold" ?a-age ?a-birthday)
           (->Person "Eric"   ?e-age ?e-birthday)
           (->Person "Peter"  ?p-age ?p-birthday)))

(defquery test-query
  []
  [?f <- Person]
  )

(comment
  (-> (c/mk-session)
      (c/insert-all facts)
      (c/fire-rules)
      (c/query test-query))

  )
