;; #+begin_src clojure
(ns go-normal.go-normal
  (:require
   [clojure.string :as str]
   [go-normal.chemical :as che]
   [tawny.owl :refer :all]
   [tawny.pattern :as p]))


(defontology geneontology
  :iri "http://www.russet.org.uk/ontolink/go-normal")
;; #+end_src


;; Top Level
(defclass nProcess)
(defclass RealizableEntity)
(defclass Activity)
(defoproperty hasFunction
  :range RealizableEntity)



(defn stem [name]
  (if (some
       identity
       (map
        #(str/ends-with? name %)
        ["e"]))
    (str/join (drop-last name))
    name))

;; Copy from owl.clj!
(defn- var-get-maybe
  "Given a var return it's value, given a value return the value."
  [var-maybe]
  (if (var? var-maybe)
    (var-get var-maybe)
    var-maybe))

(defmontfn subactivity
  [o activity-name comment
   [super-activity super-process super-realizable]]
  (let [realizable
        (p/p owl-class o
             (str
              "To" activity-name)
             :super (var-get-maybe super-realizable)
             :comment (str "The RealizableEntity for: "
                           comment))]
      (p/pattern-annotator
       o
       (list
        (p/p owl-class o
             (str
              (stem activity-name)
              "ingActivity")
             :super Activity
             :equivalent
             (owl-some hasFunction realizable))
        (p/p owl-class o
             (str
              (stem activity-name) "ing")
             :super (var-get-maybe super-process)
             :comment (str "The Activity for: "
                           comment))
        realizable))))

(defmacro defsubactivity
  [activity-name comment super-pair]
  (tawny.pattern/pattern-generator
   'subactivity
   (list (name activity-name)
         comment super-pair)))

(defmontfn activity
  [o activity-name comment]
  (subactivity activity-name comment
              [Activity nProcess RealizableEntity]))


(defmacro defactivity
  [activity-name comment]
  `(def ~activity-name
     ~(tawny.pattern/pattern-generator
       'activity
       (list (name activity-name)
           comment))))


(defactivity Bind
  "To interact tightly with another entity, longer than
   transiently, such that separating the entity requires significant energy.
   ToBind functions are often transitive; A has a function ToBind B, then vice
   versa is also true.")

(defoproperty hasLigand
  :range che/Chemical)

(refine Binding
        :super (owl-some hasLigand che/Chemical))

(defactivity Catalyse
  "To reduce the activation energy of a reaction, enabling
   it to go faster.")


(defoproperty hasSubstrate
  :range che/Chemical)

(refine Catalysing
        :super (owl-some hasSubstrate che/Chemical))


(defactivity Store
  "To contain a substance for later use.")

(defactivity Diffuse
  "To spread outward from a single point as a result of Brownian
   motion.")

(defactivity Transport
  "To enable the movement of an entity in a directed manner.")

;; this is for structural molcular activity
(defactivity IntegrityMaintain
  "To keep the same structure, shape or organisation despite
   physical forces, either in compression or in extension.")

(defactivity Protect
  "To prevent an event occuring to this or another entity.")

(defactivity Modulate
  "To alter the strength or quantity of some other realisable entity.")

(defactivity Transduce
  "To change energy from one form to another.")

;; TODO pattern does not currently support superclasses
(defsubactivity Regulate
  "To modulate in a directed manner, as part of a feedback loop."
  Modulate)

(defclass Mark
  "To bind between this entity X, and another entity Y, so that
   a third entity Z can also be bound, and thereby interact with Y."
  Bind)

;; This is a bit imperfect, since the evaluation is very order dependent. It
;; should work, though, so long as only defactivity is used to define children
;; of nActivity or RealizableEntity.
(apply
 as-disjoint
 (direct-subclasses nProcess))

(apply
 as-disjoint
 (direct-subclasses RealizableEntity))
;; * Realizable Entities

;; This section represents the realisable entities. At the moment, I am choose
;; not to distinguish between function, role and dispositional entities, because
;; it's not clear how to make this distinction.

;; #+begin_src clojure

;; #+end_src

(defoproperty realisedIn
  :domain RealizableEntity
  :range Activity)


(defn fun-in [realizable process]
  (owl-some
   hasFunction
   (owl-and
    realizable
    (owl-some
     realisedIn process))))


(defn on-substrate [chemical]
  (owl-some hasSubstrate chemical))


;; * Catalytic Activity
(defmacro defcatalyst [name substrate & rest]
  `(defclass ~name
     :super (fun-in
             ToCatalyse
             (on-substrate ~substrate))
     ~@rest))

(defcatalyst RecombinaseActivity
  che/DNA
  :comment
  "Catalysis of the identification and base-pairing of homologous sequences
  between single-stranded DNA and double-stranded DNA.")


(defn on-ligand [chemical]
  (owl-some hasLigand chemical))


(defmacro defbinding [name substrate]
  `(defclass ~name
     :super (fun-in
             ToBind
             (on-ligand ~substrate))))

(defbinding DNABindingActivity
  che/DNA)
