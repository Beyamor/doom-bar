(ns bar.system.macros
  (:refer-clojure :exclude [do]))

(defn group
  [forms]
  (loop [forms forms, groups []]
    (let [[group forms]
          (cond
            (= '<- (second forms))
            [{:type    :binding
              :binding (first forms)
              :value   (nth forms 2)}
             (drop 3 forms)]

            (= :let (first forms))
            [{:type     :let
              :bindings (second forms)}
             (drop 2 forms)]

            (= :when (first forms))
            [{:type       :when
              :condition  (second forms) }
             (drop 2 forms)]

            :else
            [{:type   :normal
              :value  (first forms)}
             (drop 1 forms)])
          groups (conj groups group)]
      (if (empty? forms)
        groups
        (recur forms groups)))))

(defn build-form
  [body form]
  (case (:type form)
    :normal
    `(bar.system/bind ~(:value form)
                      (fn [~(gensym)]
                        ~body))
    :binding
    `(bar.system/bind ~(:value form)
                      (fn [~(:binding form)]
                        ~body))

    :let
    `(fn [state#]
       (let ~(:bindings form)
         (~body state#)))

    :when
    `(fn [state#]
       (let [handler# (if ~(:condition form)
                       ~body
                       (bar.system/return nil))]
         (handler# state#)))))

(defn build
  [[base & more-forms]]
  (reduce build-form (:value base) more-forms))

(defmacro do
  [& forms]
  (-> forms
      group
      reverse
      build))
