(ns comp348-a3.core
  (:gen-class))
(use 'clojure.java.io)
(require '[clojure.core.reducers :as r])
(require '[clojure.string :as str])
(def name-of-file "eclipsefile.txt")

(defn write-to-file [sequence]
  "This function is used to overwrite the file using any sequence.
  Since the parameter is a sequence, the file is written element by element."
  (with-open [wrtr (writer name-of-file)]
    (doseq [element sequence] (.write wrtr element))))

(defn menu
  []
  (println "=== Eclipse History Encyclopedia ===")
  (println)
  (println "1. View Eclipse Events")
  (println "2. Add New Eclipse Event")
  (println "3. Modify Eclipse Event")
  (println "4. Search for Eclipse Events")
  (println "5. Exit")
  (println)
  (println "Enter your choice (1-5): "))

(defn case-1-function []
  (def myvec (str/split (slurp name-of-file) #"\r\n\r\n"))
  (println "Total events found: " (count myvec))
  (doseq [n myvec]
    #_=> (println n)
    #_=> (println "------------------------------------------"))
  )

(defn case-2-function []
  "After the user has input all the relevant information,
  the information is used to create a vector (neweclipse).
  This new vector is added to the set (updatedset) to make sure that the eclipse event is not repeated.
  With mysetwithspaces and flattenedset, I create a vector that does not have nested sequences
  so that I can use it in write-to-file"
  (println "Enter date: ")
  (def date (read-line))
  (println "\nEnter location: ")
  (def location (read-line))
  (println "\nEnter type: ")
  (def type-of-eclipse (read-line))
  (println "\nEnter significance: ")
  (def significance (read-line))
  (println "Event added successfully.")
  (def myvec (str/split (slurp name-of-file) #"\r\n\r\n"))
  (def myset (set (for [x myvec :let [y (str/split x #"\r\n")]] y)))
  (def neweclipse [(format "Date: %s" date) (format "Location: %s" location) (format "Type: %s" type-of-eclipse) (format "Significance: %s" significance)])
  (def updatedset (conj myset neweclipse))
  (def mysetwithspaces (zipmap (for [vecs1 updatedset :let [lines (zipmap vecs1 (repeat "\r\n"))]] (into [] lines)) (repeat ["\r\n"])))
  (def flattenedset (into [] (r/flatten mysetwithspaces)))
  (write-to-file flattenedset)
  )

(defn case-3-function []
  "myset is transformed into a vector (myvec2) so that an index can be extracted from it.
  The information input by the user is used to create a vector (updatedeclipse).
  The vector is then used to replace the corresponding that has to be replaced using index.
  Again, using mysetwithspaces and flattenedset, a vector is created that can be used to overwrite the file"
  (def myvec (str/split (slurp name-of-file) #"\r\n\r\n"))
  (def myset (set (for [x myvec :let [y (str/split x #"\r\n")]] y)))
  (def myvec2 (into [] myset))
  (doseq [n myvec2] (println "index: " (+ (.indexOf myvec2 n) 1))(doseq [m n] (println m))(println "------------------------------------------------------------------------------------------------------------------"))
  (println "Enter the index of the event you want to modify: ")
  (def stringindex (read-line))
  (def int-index (Integer/parseInt stringindex))
  (def index (- int-index 1))
  (println "\nEnter updated date: ")
  (def updated (read-line))
  (println "\nEnter updated location: ")
  (def updlocation (read-line))
  (println "\nEnter updated type: ")
  (def updtype-of-eclipse (read-line))
  (println "\nEnter updated significance: ")
  (def updsignificance (read-line))
  (println "\nEvent modified successfully.")
  (def updatedeclipse [(format "Date: %s" updated) (format "Location: %s" updlocation) (format "Type: %s" updtype-of-eclipse) (format "Significance: %s" updsignificance)])
  (def updatedvec2 (assoc myvec2 index updatedeclipse))
  (def mysetwithspaces1 (zipmap (for [vecs2 updatedvec2 :let [lines (zipmap vecs2 (repeat "\r\n"))]] (into [] lines)) (repeat ["\r\n"])))
  (def flattenedset1 (into [](r/flatten mysetwithspaces1)))
  (write-to-file flattenedset1)
  )

(defn case-4-function []
  "Using the variable query,
   a vector (relevantvec) is created in which myset is analyzed
   and only the relevant vectors are retained and printed"
  (println "Enter search type (date/location): ")
  (def myvec (str/split (slurp name-of-file) #"\r\n\r\n"))
  (def myset (set (for [x myvec :let [y (str/split x #"\r\n")]] y)))
  (read-line)
  (println "Enter search query: ")
  (def query (read-line))
  (println "Search results: ")
  (def relevantvec (for [internalvecs myset :let [elements internalvecs] :when (str/includes? elements query)] elements))
  (println )
  (if (= (count relevantvec) 0)
    (println "There are no results matching your query.")
    (doseq [vecs relevantvec] (doseq [elements vecs] (println elements)) (println "------------------------------------------------------------------------------------------------------------------")))
  )

(defn -main  [& args]
  (def bool 1)
   (while (> bool 0)
     (menu)
     (def choice (read-line))
     (cond
       (= choice "1") (case-1-function)
       (= choice "2") (case-2-function)
       (= choice "3") (case-3-function)
       (= choice "4") (case-4-function)
       (= choice "5") (def bool 0)
       )))
