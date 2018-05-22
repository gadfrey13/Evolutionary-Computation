(ns fungp.pj4B
  (:use fungp.core)
  (:use fungp.util)
  (:use clojure.pprint))

(def criteria 0.01)

(defn exp [x] (* x x))


(def sample-functions
  '[[+ 2]
    [- 2]
    [* 2]
    [fungp.util/abs 1]
    [inc 1]
    [dec 1]])

(def sample-parameters
    ['x 'y 'w 'z])

(def number-literals
  (map float (range 10)))

(def in-list1 '(0.40 0.20 0.35 0.15 0.15 0.06 0.12 0.10 0.06 0.20))
(def in-list2 '(0.40 0.20 0.35 0.15 0.15 0.06 0.06 0.10 0.06 0.20))
(def in-list3 '(0.190 0.240 0.035 0.045 0.035 0.018 0.018 0.007 0.004 0.050))
(def in-list4 '(0.190 0.240 0.035 0.120 0.095 0.123 0.067 0.028 0.024 0.100))
(def out-list '(0.6313 0.3630 0.1000 0.1400 0.1000 0.0490 0.1000 0.0233 0.0110 0.1590))

(defn sample-fitness;;;function name
  [tree];;;Parameter
  (try
    (let [f (compile-tree tree sample-parameters);;;store the value 
          results (map f in-list1 in-list2 in-list3 in-list4)];;;
      (let [r (reduce + (map off-by-sq out-list results))]
         (if (< r criteria) 0 r)))
    (catch Exception e (println e) (println tree))))

(defn sample-report
  [tree fitness]
  (pprint tree)
  (println (str "Error:\t" fitness "\n"))
  (flush))

(defn test-regression2
  [n1 n2]
  (println "\nfungp :: Functional Genetic Programming in Clojure")
  (println "Mike Vollmer, 2012")
  (println (str "Test inputs: " (vec in-list1)))
  (println (str "Test inputs: " (vec in-list2)))
  (println (str "Test inputs: " (vec in-list3)))
  (println (str "Test inputs: " (vec in-list4)))
  (println (str "Test outputs: " (vec out-list)))
  (println (str "Max generations: " (* n1 n2)))
  (println)
  (let [options {:iterations n1
                 :migrations n2
                 :num-islands 9
                 :population-size 40
                 :tournament-size 5
                 :mutation-probability 0.3
                 :max-depth 10
                 :terminals sample-parameters
                 :numbers number-literals
                 :fitness sample-fitness
                 :functions sample-functions
                 :report sample-report}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
      (sample-report tree score))))