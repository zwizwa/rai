#lang racket/base


;; LV2 uses TTL files in Turtle RDF format to encode a nesting of
;; lists, dictionaries and atomic values.
;;
;; http://wikitravel.org/en/Wikitravel:Turtle_RDF
;;
;; The basic format is simple:
;; <subject> <relationship> <object>
;;
;; The first two are URLs pointing to an XML document describing the
;; meaning and attributes.  URLs can be shortened using a prefix
;; method where symbols represent URL prefixes.
;;
;; What makes the format hard is the arcane ways of making a file
;; "human readable" through context-sensitive concatenation.
;;
;; ';' reuses <subject>
;; ',' reuses <subject> <relationship>
;;
;; '[' ']' defines an anonymous object, which can appear in object
;; position, and contains a one or more <relationship> <object>
;; clauses.



;; Practically, the main distinction is between these two forms:
;;   <rel> <obj1> , <obj2> , <obj3>
;;   <rel> [ <rel1> <obj1> ; <rel2> <obj2> ]
;;
;; At the LV2 level this seems to be used to define a (top <rel>) list
;; of objects, or as a structured object.  The first is shorthand for:
;;   <rel> <obj1>
;;   <rel> <obj2>
;;   <rel> <obj3>
;;
;; The second is actually <rel> <obj> where <obj> is an anonymous
;; object defined by [ <rel1> <obj1> ; <rel2> <obj2> ... ]
;;
;; To map this to a simple scheme coding we can use scheme lists and
;; scheme assoc lists to represent lists and structs.  This requires
;; lists and dictionaries to be distinguished.  Since all relations
;; are represented as symbols, the following should do the trick:

;; A structure object is an atom-indexed assoc list.
(define (struct-obj? x)
  (and (list? x)
       (not (null? x))
       (pair? (car x))
       (atom? (caar x))))

(define (atom? v)
  (or (string? v)
      (number? v)
      (symbol? v)))

(define (url? v)
  (and (string? v)
       (eq? #\< (string-ref v 0))))

(define (rdf-atom v)
  (if (url? v) v (format "~s" v)))

(define (wrong-type v)
  (error 'wrong-type (format "~s" v)))

(define (rdf-value v)
  (cond
   ((atom? v)       (rdf-atom v))
   ((struct-obj? v) (rdf-dict v))
   ((list? v)       (rdf-list v))
   (else            (wrong-type v))))

(define (rdf-pair p)
  (let ((tag   (car p))
        (value (cdr p)))
    (string-append
     (rdf-atom tag)
     " "
     (rdf-value value))))

(define (rdf-composite lst format-el left sep right)
  (apply
   string-append
   `(,left
     " " ,(format-el (car lst))
     ,@(for/list ((el (cdr lst)))
         (string-append " " sep " " (format-el el)))
     ,right)))

(define (rdf-dict d) (rdf-composite d rdf-pair  "[" ";\n" "]\n"))
(define (rdf-list l) (rdf-composite l rdf-value ""  "," ""))
(define (rdf-global name v)
  (string-append
   (rdf-atom name) "\n"
   (rdf-composite v rdf-pair "" ";\n" ".\n")))

(define (test)
  (display
   (rdf-global "<http://zwizwa.be/lv2/test>"
               `((a lv2:Plugin lv2:AmplifierPlugin)
                 (doap:name "Simple Amplifier")
                 (doap:licence "<http://opensource.org/licenses/isc>")
                 (lv2:optionalFeature lv2:hardRTCapable)
                 (lv2:port
                  (a lv2:InputPort lv2:ControlPort)
                  (lv2:project "<http://lv2plug.in/ns/lv2>")
                  (lv2:index 0)
                  (lv2:symbol "gain")
                  (lv2:name "Gain")
                  (lv2:default 0.0)
                  (lv2:minimum -90.0)
                  (lv2:maximum 24.0)
                  (lv2:scalePoint
                   ((rdfs:label "+5") (rdf:value 5.0))
                   ((rdfs:label  "0") (rdf:value 0.0))))
                 (lv2:port
                  (a lv2:AudioPort lv2:InputPort)
                  (lv2:index 1)
                  (lv2:symbol "in")
                  (lv2:name "In"))
                 (lv2:port
                  (a lv2:AudioPort lv2:OutputPort)
                  (lv2:index 2)
                  (lv2:symbol "out")
                  (lv2:name "Out"))))))
                   
                   
  
