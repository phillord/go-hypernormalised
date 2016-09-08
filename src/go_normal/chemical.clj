(ns go-normal.chemical
  (:require
   [tawny.owl :refer :all]))

(defontology chemical
  :iri "http://www.russet.org.uk/ontolink/go-normal/chemical"
  )

(defclass Chemical)

(defclass DNA
  :super Chemical)
