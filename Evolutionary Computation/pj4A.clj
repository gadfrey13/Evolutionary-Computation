(ns fungp.pj4A
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
    [fungp.util/sin 1]
    [fungp.util/cos 1]
    [exp 1]
    [inc 1]
    [dec 1]])

(def sample-parameters
    ['x 'y])

(def number-literals
  (map float (range 10)))

(def in-list1 '(-3  -2.8  -2.6  -2.4  -2.2  -2  -1.8  -1.6  -1.4  -1.2  -1  -0.8  -0.6  -0.4  -0.2  3.88578  0.2  0.4  0.6  0.8  1  1.2  1.4  1.6  1.8  2  2.2  2.4  2.6  2.8  3))     ;;; x value
(def in-list2 '(-1  -0.9  -0.8  -0.7  -0.6  -0.5  -0.4  -0.3  -0.2  -0.1  -1.38778  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1  1.1  1.2  1.3  1.4  1.5  1.6  1.7  1.8  1.9  2))
(def out-list '(1.412118485  1.809902259  1.098951486  -0.009641883  -0.631868757  -0.506802495  0.061751406  0.639355436  0.965211521  1.001458348  0.841470985  0.607195441  0.392274233  0.249318207  0.199989334  0.25  0.399989334  0.649318207  0.992274233  1.407195441  1.841470985  2.201458348  2.365211521  2.239355436  1.861751406  1.493197505  1.568131243  2.390358117  3.698951486  4.609902259  4.412118485))

(defn sample-fitness;;;function name
  [tree];;;Parameter
  (try
    (let [f (compile-tree tree sample-parameters);;;store the value 
          results (map f in-list1 in-list2)];;;
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