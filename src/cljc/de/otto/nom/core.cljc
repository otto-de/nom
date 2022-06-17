(ns de.otto.nom.core
  (:require [clojure.string :as str]))

;;; Util

(defn cl-find-if [pred coll & {:keys [from-end start end key]
                               :or {from-end false
                                    start 0
                                    end nil
                                    key identity}}]
  (loop [[x & xs :as coll] (cond->> coll
                             start (drop start)
                             end (take (- end (or start 0)))
                             from-end reverse)]
    (cond (empty? coll) nil
          (pred (key x)) x
          :else (recur xs))))

;;; Anomaly Structure

(defn fail
  "Construct a new anomaly with the given `category`.  Accepts either keyword
  arguments or a map with further data.

  Example:
  ```
  (= [::anomaly :nonono {:foo 1 :bar 2}]
              (fail :nonono :foo 1 :bar 2)
              (fail :nonono {:foo 1 :bar 2}))
  ```"
  [category & more]
  ;; This map conversion can be inlined into the lambda list in Clojure 1.11
  (let [more-map (cond (map? (first more)) (first more)
                       (keyword? (first more)) (apply hash-map more))]
    (cond-> [::anomaly category]
      more-map (conj more-map))))

(defn anomaly?
  "Predicate for anomalies."
  [a]
  (and (vector? a)
       (= ::anomaly (first a))))

(defn kind
  "The kind of an anomaly."
  [a]
  (or (and (anomaly? a) (second a))
      nil))

(defn payload
  "The payload of an anomaly."
  [a]
  (when (anomaly? a)
    (get a 2 {})))

;;; Wrap Exceptions

(defmacro try-nom
  {:style/indent 0}
  [& body]
  `(try ~@body
        (catch Exception e#
          (fail :thrown :exception e#))))

;;; Extensible to other anomalies

(defmulti abominable?
  "Is the argument something that should be treated as an anomaly?"
  (fn [x]
    (cl-find-if #(% x)
                (keys (methods abominable?)))))

(defmethod abominable? :default [_]
  false)

(defmethod abominable? anomaly? [_]
  true)

(defmulti adapt
  "Convert the argument to a nom/anomaly."
  (fn [x]
    (cl-find-if #(% x)
                (keys (methods adapt)))))

(defmethod adapt :default [x]
  (fail :default :thing x))

(defmethod adapt anomaly? [x]
  x)

;;; Extend to other anomalies

(defn cognitect-anomaly? [a]
  (and (map? a)
       (some? (:cognitect.anomalies/category a))))

(defmethod abominable? cognitect-anomaly? [_]
  true)

(defmethod adapt cognitect-anomaly? [x]
  (fail (:cognitect.anomalies/category x)
        (dissoc x :cognitect.anomalies/category)))

;;; Lifting

(defn some-abomination
  "Returns an anomaly from x if it is an anomaly or abominable, else nil."
  [x]
  (when (abominable? x)
    (adapt x)))

(defn nom*
  "Takes a function `f` and arguments `args` for it.  Checks all `args`.  If any
  of them is an anomaly, returns that.  Otherwise, returns the result of
  applying `f` on `args`."
  [f & args]
  (if-let [anomaly (cl-find-if abominable? args)]
    (adapt anomaly)
    (let [r (apply f args)]
      (cond-> r
        (abominable? r) adapt))))

(defmacro nom
  "Macro version of `nom*`.  Checks all the values given in the form.  If any
  value is an anomaly, returns that anomaly.  Otherwise, evaluate the rest of
  the form (without the `nom`).

  Example: `(nom foo bar)` — if any of nom, foo, or bar is an anomaly, returns
  that; otherwise calls and returns `(foo bar)`.

  Because this is a macro that expands to a conditional and the wrapped form,
  arities are checked normally by compiler and runtime."
  [& forms]
  (let [syms (repeatedly (count forms) #(gensym "nom-"))]
    `(let [~@(mapcat vector syms forms)]
       (if-let [anomaly# (cl-find-if abominable? [~@syms])]
         (adapt anomaly#)
         (let [r# (~@syms)]
           (cond-> r#
             (abominable? r#) adapt))))))

(defmacro with-nom
  "Takes a vector of forms to check and a body.  If any of the checked forms
  returns an anomaly, the rest of the forms and the body are not executed and
  the anomaly returned."
  {:style/indent 1}
  [vs & body]
  `(if-let [anomaly# (or ~@(map (fn [v]
                                  `(some-abomination ~v))
                                vs))]
     (adapt anomaly#)
     (do ~@body)))

;;; Threading

(defn- nomify-first [arg [op & args]]
  `(nom ~op ~arg ~@args))

(defn- nomify-last [arg [op & args]]
  `(nom ~op ~@args ~arg))

(defn- expand-nom [form forms nomify]
  (reduce (fn [acc next]
            (if (seq? next)
              (nomify acc next)
              `(nom ~next ~acc)))
          (if (seq? form)
            `(nom ~@form)
            form)
          forms))

(defmacro nom->
  "Like `some->`, but instead of `nil` short-circuits on any anomaly."
  [form & forms]
  (expand-nom form forms nomify-first))

(defmacro nom->>
  "Like `some->>`, but instead of `nil` short-circuits on any anomaly."
  [form & forms]
  (expand-nom form forms nomify-last))

;;; Binding

(defn- underscore-symbol? [s]
  (and (symbol? s)
       (str/starts-with? (name s) "_")))

(defn- ensure-vec [x]
  (if (vector? x)
    x
    [x]))

(defn- destructuring-syms [form]
  (cond (symbol? form) [form]
        (vector? form) (mapcat destructuring-syms form)
        (map? form) (mapcat (fn [[k v]]
                              (cond (symbol? k) [k]
                                    (vector? k) (mapcat destructuring-syms k)
                                    (map? k) (destructuring-syms k)
                                    (= k :as) [v]
                                    (= (name k) "keys") (mapv #(symbol nil (name %))
                                                              v)))
                            form)))

(defn- expand-nom-binding [[left right]]
  (let [nom-right (if (seq? right)
                    `(nom ~@right)
                    right)]
    (if (symbol? left)
      [left nom-right]
      (let [dsyms (vec (destructuring-syms left))
            single (gensym "nom-single-")]
        [single nom-right
         dsyms `(if (abominable? ~single)
                  (repeat (adapt ~single))
                  (let [~left ~single]
                    ~dsyms))]))))

(defn- expand-let-nom-group [body group]
  (if (underscore-symbol? (ffirst group))
    `(with-nom ~(reduce (fn [noms form]
                          (into noms (-> form second ensure-vec)))
                        []
                        group)
       ~body)
    `(let [~@(mapcat expand-nom-binding group)]
       ~body)))

(defmacro let-nom
  "Like a `let`, but each binding is made anomaly-propagating with `nom`.  Any
  binding that assigns to a single symbol whose name starts with an underscore
  is treated as a guard.  If the form in a guard is a vector, its constituent
  forms are checked one-by-one as in `with-nom`.  If a guard finds an anomaly,
  the bindings short-circuit at that point, so no further binding nor the body
  are executed, and the value of the entire `let-nom` form is that anomaly.
  This means that if you want to recover from an anomaly in the body, you should
  not use guards."
  {:style/indent 1}
  [bindings & body]
  (let [binding-pairs (partition 2 bindings)
        grouped (partition-by (comp underscore-symbol? first)
                              binding-pairs)]
    (reduce expand-let-nom-group
            `(do ~@body)
            (reverse grouped))))

(defmacro let-nom>
  "Like `let-nom`, but any binding of an anomaly short-circuits, so that no
  further binding nor the body are executed, and the value of the entire
  `let-nom>` form is that anomaly.  This means that you cannot recover in the
  body from anomalies in the bindings."
  {:style/indent 1}
  [bindings & body]
  (if (seq bindings)
    (let [[k v & more] bindings]
      `(let [result# ~v]
         (with-nom [result#]
           (let [~k result#]
             (let-nom> [~@more]
               ~@body)))))
    `(do ~@body)))

;;; Recovering

(defmacro with-default
  "Evaluates `form` and returns the result, unless it is an anomaly, in which case
  the anomaly is bound (possibly destructured) to `v`, the body evaluated, and
  that returned."
  [[v form] & body]
  `(let [r# ~form]
     (if-let [~v (some-abomination r#)]
       (do ~@body)
       r#)))
