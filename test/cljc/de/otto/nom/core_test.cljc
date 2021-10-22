(ns de.otto.nom.core-test
  (:require [de.otto.nom.core :as nom]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(defn random-id []
  (str (rand-int 1073741824)))

;;; Example code.  Tests may use this to show the features by passing different
;;; data into it.

;; This graph implementation is not complete at all: no delete functions, no
;; checks against overwriting, no id index on edges…

(defn make-graph []
  {:nodes {}
   :edges {}})

(defn graph-add-node [graph id]
  (assoc-in graph [:nodes id] {:id id}))

(defn graph-add-edge [graph from to]
  (assoc-in graph
            [:edges {:from-id (:id from)
                     :to-id (:id to)}]
            {:id (random-id)
             :from-id (:id from)
             :to-id (:id to)}))

(defn graph-node [graph id]
  (get-in graph [:nodes id]))

(defn graph-nodes-edge [graph from to]
  (get-in graph [:edges {:from-id (:id from)
                         :to-id (:id to)}]))

;; Some less naïve wrappers that can fail

(defn existing-node [graph id]
  (or (graph-node graph id)
      (nom/fail :missing-node)))

(defn absent-edge [graph from to]
  (if (graph-nodes-edge graph from to)
    (nom/fail :edge-exists)
    true))

;;; Tests

;; fail

(deftest fail-should-create-an-anomaly-vector
  (testing "When constructing an anomaly with fail"
    (let [r (nom/fail :i-don't-like-it)]
      (testing "Then the result is a simple vector"
        (is (= [::nom/anomaly :i-don't-like-it]
               r))))))

(deftest fail-should-append-data-as-a-map
  (testing "When giving more data to `fail`"
    (let [r (nom/fail :i-don't-like-it :aha 'aha :uhu 'uhu)]
      (testing "Then the data are appended as a map"
        (is (= [::nom/anomaly :i-don't-like-it {:aha 'aha
                                                :uhu 'uhu}]
               r))))))

(deftest fail-should-equivalently-accept-keyword-args-or-a-map
  (is (= [::nom/anomaly :nonono {:foo 1 :bar 2}]
         (nom/fail :nonono :foo 1 :bar 2)
         (nom/fail :nonono {:foo 1 :bar 2}))))

;; selectors

(deftest kind-should-return-the-type-of-anomaly
  (is (= :kind
         (nom/kind [::nom/anomaly :kind {:data "some data"}]))))

(deftest kind-should-return-the-mandatory-categorization-of-an-anomaly
  (is (= :foo
         (nom/kind [::nom/anomaly :foo]))))

(deftest kind-should-return-nil-for-non-anomalies
  (is (nil? (nom/kind [::something-else :foo]))))

(deftest payload-should-return-the-additional-data-in-an-anomaly
  (let [data {:some "data" :within "anomaly"}]
    (is (= data
           (nom/payload [::nom/anomaly :some-kind data])))))

(deftest payload-should-return-the-additional-data-of-an-anomaly
  (is (= {:foo 1 :bar 2}
         (nom/payload [::nom/anomaly :foo {:foo 1 :bar 2}]))))

(deftest payload-should-return-the-empty-map-when-no-additional-data
  (is (= {} (nom/payload [::nom/anomaly :foo]))))

(deftest payload-should-return-nil-for-non-anomalies
  (is (nil? (nom/payload [::moep :what :nonono]))))

;; try-nom

(deftest try-nom-should-catch-exceptions
  (testing "When an exception is thrown in a try-nom body"
    (let [r (nom/try-nom
              (nom/let-nom [x (/ 3 5)
                            y (nom/fail :nope) ; no short-circuit in let-nom
                            z (/ 3 0)]
                (nom/nom + x y z)))]
      (testing "Then the exception is caught and wrapped in an anomaly"
        (is (nom/anomaly? r))
        (is (= :thrown (nom/kind r)))
        (is (= ArithmeticException (class (:exception (nom/payload r)))))))))

;; nom*

(deftest nom*-should-fail-if-any-arg-is-anomal
  (testing "Given an anomaly and a function"
    (let [arg (nom/fail :incorrect)
          f (fn [a] (/ a 0))]           ; would throw if called
      (testing "When f is called through nom*"
        (let [r (nom/nom* f arg)]
          (testing "Then the result should be the passed-in anomaly without
                   calling f"
            (is (= r arg))))))))

(deftest nom*-should-go-through-if-every-arg-is-ok
  (testing "Given a homal value and a function"
    (let [arg 5
          f (fn [a] (/ a 2))]
      (testing "When f is called through nom*"
        (let [r (nom/nom* f arg)]
          (testing "Then the result should be the result of calling f"
            (is (= r (/ 5 2)))))))))

;; nom

(deftest nom-should-fail-if-any-arg-is-anomal
  (testing "Given an anomaly and a function"
    (let [arg (nom/fail :incorrect)
          f (fn [a] (/ a 0))]           ; would throw if called
      (testing "When f is called through nom"
        (let [r (nom/nom f arg)]
          (testing "Then the result should be the passed-in anomaly without
                   calling f"
            (is (= r arg))))))))

(deftest nom-should-go-through-if-every-arg-is-ok
  (testing "Given a homal value and a function"
    (let [arg 5
          f (fn [a] (/ a 2))]
      (testing "When f is called through nom"
        (let [r (nom/nom f arg)]
          (testing "Then the result should be the result of calling f"
            (is (= r (/ 5 2)))))))))

;; with-nom

(deftest with-nom-should-fail-if-checking-an-anomaly
  (testing "Given an anomaly"
    (let [a (nom/fail :incorrect)]
      (testing "When with-nom checks it"
        (let [r (nom/with-nom [1 nil 2 a 3]
                  (/ :kek 0))]
          (testing "Then the result should be the anomaly without executing the
                    body"
            (is (= r a))))))))

(deftest with-nom-should-short-circuit-on-any-anomaly
  (testing "When with-nom encounters a form that evaluates to an anomaly"
    (let [r (nom/with-nom [3
                           nil
                           (nom/fail :incorrect)
                           (/ :kek 0)] ; would throw
              :whatever)]
      (testing "Then neither the rest of the forms nor the body is evaluated and
                the anomaly returned."
        (is (= (nom/fail :incorrect)
               r))))))

;; let-nom

(deftest let-nom-should-propagate-anomalies
  (testing "Given empty graph data"
    (let [graph (make-graph)]
      (testing "When attempting to get a node from it and do something with it"
        (let [r (nom/let-nom [node (existing-node graph "some-id") ; fails
                              enriched (assoc node :foo 5)]
                  (nom/nom :foo enriched))]
          (testing "Then the result should be the propagated anomaly"
            (is (nom/anomaly? r))
            (is (= :missing-node (second r)))))))))

(deftest let-nom-should-short-circuit-on-guards
  (testing "Given a graph with two nodes and an edge between them"
    (let [graph (-> (make-graph)
                    (graph-add-node "n1")
                    (graph-add-node "n2")
                    (graph-add-edge {:id "n1"} {:id "n2"}))]
      (testing "When attempting to add the edge again while checking with
                let-nom"
        (let [r (nom/let-nom [from (existing-node graph "n1")
                              to (existing-node graph "n2")
                              _ (absent-edge graph from to)]
                  (graph-add-edge graph from to))]
          (testing "Then the result should be an anomaly showing the source of
                    error"
            (is (nom/anomaly? r))
            (is (= :edge-exists (second r)))))))))

(deftest let-nom-should-short-circuit-on-successive-guards
  (testing "Given an empty graph"
    (let [graph (make-graph)]
      (testing "When doing something with successive guards"
        (let [r (nom/let-nom [x (existing-node graph "whatever")
                              _ x
                              _ (/ x 0)]
                  :nothing)]
          (testing "Then each guard should short-circuit immediately"
            (is (nom/anomaly? r))
            (is (= :missing-node (second r)))))))))

(deftest let-nom-should-short-circuit-with-vector-guards
  (testing "Given an empty graph"
    (let [graph (make-graph)]
      (testing "When doing something with a guard for a vector literal"
        (let [r (nom/let-nom [x (existing-node graph "whatever")
                              _ [x (/ x 0)]]
                  :nothing)]
          (testing "Then the guard should short-circuit immediately"
            (is (nom/anomaly? r))
            (is (= :missing-node (second r)))))))))

(deftest let-nom-should-continue-on-successful-guards
  (testing "Given a graph with only two nodes"
    (let [graph (-> (make-graph)
                    (graph-add-node "n1")
                    (graph-add-node "n2"))]
      (testing "When adding the edge while checking with let-nom"
        (let [r (nom/let-nom [from (existing-node graph "n1")
                              to (existing-node graph "n2")
                              _ (absent-edge graph from to)
                              new-graph (graph-add-edge graph from to)]
                  (nom/nom graph-add-node new-graph "n3"))]
          (testing "Then the calculation should go on yielding the new graph"
            (is (= {:id "n3"} (graph-node r "n3")))))))))

(deftest let-nom-should-work-with-map-destructuring
  (testing "Given a function that returns a map or an anomaly"
    (let [f (fn [a]
              (if (zero? a)
                (nom/fail :div-by-zero)
                {:p (* 2 a)
                 :q (/ 2 a)}))]
      (testing "When calling it with something that triggers the anomaly
                and then destructuring the result"
        (let [r (nom/let-nom [{pogo :p
                               :keys [q]
                               :as loco
                               :or {q 3}}
                              (f 5)]
                  [pogo q loco])]
          (testing "Then the destructured bindings should all receive the
                    anomaly"
            (is (= [10 (/ 2 5) {:p 10 :q (/ 2 5)}] ; Cljs has no ratio values
                   r))))))))

(deftest let-nom-should-propagate-anomalies-through-map-destructuring
  (testing "Given a function that returns a map or an anomaly"
    (let [f (fn [a]
              (if (zero? a)
                (nom/fail :div-by-zero)
                {:p (* 2 a)
                 :q (/ 2 a)}))]
      (testing "When calling it with something that triggers the anomaly
                and then destructuring the result"
        (let [r (nom/let-nom [{pogo :p
                               :keys [q]
                               :as loco
                               :or {q 3}}
                              (f 0)]
                  [pogo q loco])]
          (testing "Then the destructured bindings should all receive the
                    anomaly"
            (is (every? #(= [::nom/anomaly :div-by-zero] %)
                        r))))))))

(deftest let-nom-should-work-with-vector-destructuring
  (testing "Given a function that returns a vector or an anomaly"
    (let [f (fn [a]
              (if (zero? a)
                (nom/fail :div-by-zero)
                [(* 2 a) (/ 2 a)]))]
      (testing "When calling it with something that triggers the anomaly
                and then destructuring the result"
        (let [r (nom/let-nom [[p q
                               :as loco]
                              (f 5)]
                  [p q loco])]
          (testing "Then the destructured bindings should all receive the
                    anomaly"
            (is (= [10 (/ 2 5) [10 (/ 2 5)]]
                   r))))))))

(deftest let-nom-should-propagate-anomalies-through-vector-destructuring
  (testing "Given a function that returns a vector or an anomaly"
    (let [f (fn [a]
              (if (zero? a)
                (nom/fail :div-by-zero)
                [(* 2 a) (/ 2 a)]))]
      (testing "When calling it with something that triggers the anomaly
                and then destructuring the result"
        (let [r (nom/let-nom [[p q
                               :as loco]
                              (f 0)]
                  [p q loco])]
          (testing "Then the destructured bindings should all receive the
                    anomaly"
            (is (every? #(= [::nom/anomaly :div-by-zero] %)
                        r))))))))

;; let-nom>

(deftest let-nom>-should-work-like-normal-let-if-no-anomaly
  (testing "When doing homal things"
    (let [r (nom/let-nom> [x (* 3 2)
                           y (assoc {} :foo (/ x 5))
                           {:keys [foo]} (assoc y :x x)]
              [x y foo])]
      (testing "Then the result should be just like in let"
        (is (= [6 {:foo (/ 6 5)} (/ 6 5)]  ; not using ratio literals, for
                                           ; migration to cljc
               r))))))

(deftest let-nom>-should-short-circuit-on-any-anomaly
  (testing "Given an empty graph"
    (let [graph (make-graph)]
      (testing "When trying to get a node from it"
        (let [r (nom/let-nom> [node (existing-node graph "whatever")
                               dbz  (/ 5 0)] ; would throw
                  :something)]
          (testing "Then the anomaly short-circuits, so that later bindings are
                    not executed, even if they do not depend on an anomaly"
            (is (= [::nom/anomaly :missing-node]
                   r))))))))

;; nom->

(deftest nom->-should-work-like-->-in-the-absence-of-anomalies
  (testing "When there are no anomalies during threading with nom->"
    (let [r (nom/nom-> 3
                       (/ 2)
                       /
                       (* 5))
          check (-> 3
                    (/ 2)
                    /
                    (* 5))]
      (testing "Then the result should be the same as with ->"
        (is (= check r))))))

(deftest nom->-should-short-circuit-on-anomalies
  (testing "When there is an anomaly during threading"
    (let [f #(if (zero? %)
               (nom/fail :div-by-zero)
               %)
          r (nom/nom-> 3
                       (- 3)
                       f
                       /)]              ; this would throw if executed
      (testing "Then the result should short-circuit to the anomaly"
        (is (= [::nom/anomaly :div-by-zero]
               r))))))

;; nom->>

(deftest nom->>-should-work-like-->-in-the-absence-of-anomalies
  (testing "When there are no anomalies during threading with nom->"
    (let [r (nom/nom->> 3
                        (/ 2)
                        /
                        (* 5))
          check (->> 3
                     (/ 2)
                     /
                     (* 5))]
      (testing "Then the result should be the same as with ->"
        (is (= check r))))))

(deftest nom->>-should-short-circuit-on-anomalies
  (testing "When there is an anomaly during threading"
    (let [f #(if (zero? %)
               (nom/fail :div-by-zero)
               %)
          r (nom/nom-> 3
                       (- 3)
                       f
                       /)]              ; this would throw if executed
      (testing "Then the result should short-circuit to the anomaly"
        (is (= [::nom/anomaly :div-by-zero]
               r))))))

;; default

(deftest with-default-should-pass-on-success
  (testing "When the result is not an anomaly"
    (let [r 3
          a (atom 0)]
      (testing "Then with-default should pass it through"
        (is (= 3
               (nom/with-default [b r]
                 (swap! a inc)
                 (str "ah well, " b ", nevermind")))))
      (testing "And the body should not be evaluated"
        (is (zero? @a))))))

(deftest with-default-should-evaluate-body-on-anomaly
  (testing "When the result is an anomaly"
    (let [r (nom/fail :something :foo 5)
          a (atom 0)]
      (testing "Then with-default should evaluate the body for a return value"
        (is (= "ah well, 5, nevermind"
               (nom/with-default [[_ _ {:keys [foo]}] r]
                 (swap! a inc)
                 (str "ah well, " foo ", nevermind")))))
      (testing "And the body should have been evaluated"
        (is (= 1 @a))))))
