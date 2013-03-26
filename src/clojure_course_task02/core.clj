(ns clojure-course-task02.core
  (:import java.io.File
           java.util.ArrayList
           java.util.Arrays
           java.util.regex.Pattern)
  (:gen-class))

(defn create-sniffer [pattern]
  (fn [file-name]
    (re-matches (Pattern/compile pattern) file-name)))
 
(defn path-wanderer-recur [sniffer path & multi-threaded]
  (let [file-path (File. path)
        list-files (Arrays/asList (.listFiles file-path))]
    (loop [i 0 
           acc-list (list)]
      (if (= i (.size list-files))
        acc-list
        (let [file-or-dir (.get list-files i)]
          (if (.isDirectory file-or-dir)
            (if multi-threaded
              (recur (inc i) 
                     (concat acc-list 
                             @(future (path-wanderer-recur 
                                       sniffer (.getPath file-or-dir) true))))
              (recur (inc i) 
                     (concat acc-list 
                             (path-wanderer-recur 
                              sniffer (.getPath file-or-dir)))))
            (recur (inc i) 
                   (if (sniffer (.getName file-or-dir)) 
                     (conj acc-list (.getName file-or-dir))
                     acc-list))))))))

(def acc-list (agent (list)))

(defn path-wanderer-pmap [sniffer file-path]
  (if (.isDirectory file-path)
    (dorun (pmap #(path-wanderer-pmap sniffer %) 
                 (Arrays/asList (.listFiles file-path))))
    (if (sniffer (.getName file-path))
      (send acc-list conj (.getName file-path)))))

(comment
  "Speed tests."

  (def test-path "./")
  (def sniffer (create-sniffer ".clj"))
  (time (path-wanderer-recur sniffer test-path))
  (time (path-wanderer-recur sniffer test-path true))
  (time (do (def acc-list (agent (list)))
            (dorun (path-wanderer-pmap sniffer (File. test-path)))
            @acc-list))
)

(defn find-files [file-name path]
  "TODO: Implement searching for a file using his name as a regexp."
  (def sniffer (create-sniffer file-name))
  (def acc-list (agent (list)))
  (path-wanderer-pmap sniffer (File. path))
  @acc-list)

(defn usage []
  (println "Usage: $ run.sh file_name path"))

(defn -main [file-name path]
  (if (or (nil? file-name)
          (nil? path))
    (usage)
    (do
      (println "Searching for " file-name " in " path "...")
      (dorun (map println (find-files file-name path))))))

(comment
  (find-files "^core.+" "./")
)
