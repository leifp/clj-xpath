(ns clj-xpath.bench.core
  (:use [clojure.test]
        [clj-xpath.core :as xp
         :only [$x $x:tag $x:text $x:attrs $x:node $x:tag? $x:text? $x:tag+
                $x:text+ xp:compile tag xml->doc *xpath-compiler*
                *namespace-aware* nscontext xmlnsmap-from-root-node
                with-namespace-context abs-path]]
        [criterium.core :only [quick-bench]]))

(def benchmarks (atom []))

(defmacro defbench [name & body]
  `(do
     (defn ~name []
       (println "Benchmarking:" '~name)
       (quick-bench (do ~@body)))
     (swap! benchmarks conj ~name)))

(def xml-fixtures {:simple (tag :top-tag "this is a foo")
                   :attrs  (tag [:top-tag :name "bobby tables"]
                             "drop tables")
                   :nested (tag :top-tag
                             (tag :inner-tag
                               (tag :more-inner "inner tag body")))
                   :namespaces (slurp "fixtures/namespace1.xml")})

(defbench bench-$x-top-tag
  (= :top-tag
     ($x:tag "/*" (:simple xml-fixtures))))

(defbench bench-$x-get-body
  (= "this is a foo"
     ($x:text "/*" (:simple xml-fixtures))))

(defbench bench-$x-get-attrs
  (= "bobby tables"
     (:name
      ($x:attrs "/*" (:attrs xml-fixtures)))))

(defbench bench-$x-node
  (= "top-tag"
     (.getNodeName ($x:node "/*" (:simple xml-fixtures)))))

(defbench bench-$x-on-result
  (= :more-inner
     ($x:tag "./*"
             ($x:node "/top-tag/*" (:nested xml-fixtures)))))

(let [expr (xp:compile "/*")
      doc  (xml->doc (:simple xml-fixtures))]
  (defbench $x-should-support-precompiled-xpath-expressions
    (= :top-tag
       ($x:tag expr doc))))

(defbench bench-namespace
  (.setNamespaceContext
   *xpath-compiler*
   (nscontext {"atom" "http://www.w3.org/2005/Atom"}))
  (binding [*namespace-aware* true]
    (= "BookingCollection" ($x:text "//atom:title" (:namespaces xml-fixtures)))))

(defbench bench-with-ns-context-macro
  (with-namespace-context
      (xmlnsmap-from-root-node (:namespaces xml-fixtures))
      (= "BookingCollection" ($x:text "//atom:title" (:namespaces xml-fixtures)))))

(def labels-xml "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<labels>
  <label added=\"2003-06-20\">
    <quote>
      <emph>Midwinter Spring</emph> is its own season&#8230;
    </quote>
    <name>Thomas Eliot</name>
    <address>
      <street>3 Prufrock Lane</street>
      <city>Stamford</city>
      <state>CT</state>
    </address>
  </label>
  <label added=\"2003-06-10\">
    <name>Ezra Pound</name>
    <address>
      <street>45 Usura Place</street>
      <city>Hailey</city>
      <state>ID</state>
    </address>
  </label>
</labels>")

;; The document soap1.xml uses 3 namepsaces.  The 3rd is implicit / blank in the SOAP Body element.
(defbench bench-with-blank-ns
  (let [xml (slurp "fixtures/soap1.xml")]
    (xp/with-namespace-awareness
      (let [doc (xp/xml->doc xml)]
        (xp/set-namespace-context! (xp/xmlnsmap-from-document doc))
        (= :OTA_HotelAvailRQ (xp/$x:tag "/soapenv:Envelope/soapenv:Body/:OTA_HotelAvailRQ" doc))))))

(defn run-all []
  (doseq [b @benchmarks]
    (b)))

(defn -main [& args]
  (let [s (with-out-str (run-all))]
    (spit "bench.txt" s)))
