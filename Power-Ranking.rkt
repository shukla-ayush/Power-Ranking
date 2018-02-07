;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)
(check-location "08" "q2.rkt")

(provide
 tie
 defeated
 defeated?
 outranks
 outranked-by
 power-ranking)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;

;; Competitor:

;;;;;;;;;;;;;;;;

;; A competitor can be represented as any string.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;

;; Tie:

;;;;;;;;;;;;;;;;;


;; REPRESENTATION:
;; a Tie is represented as:
;; (make-tiestruct comp1-t comp2-t)

;; INTERPRETATION:
;; comp1-t: String, represents the name of first competitor
;; comp2-t: Stirng, represents the name of second competitor

;; IMPLEMENTATION:
(define-struct tiestruct (comp1-t comp2-t))

;; CONSTRUCTOR TEMPLATE:
;; (make-tiestruct String String)

;; OBSERVER TEMPLATE:
;; tiestruct-fn: Tie -> ??
#|
(define (tiestruct-fn t)
 ...
 (tiestruct-comp1-t t)
 (tiestruct-comp2-t t)
)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;

;; Defeat:

;;;;;;;;;;;;;;;;;


;; REPRESENTATION:
;; a Defeat is represented as:
;; (make-defeatstruct comp1-d comp2-d)

;; INTERPRETATION:
;; comp1-d: String, represents the name of first competitor
;; comp2-d: Stirng, represents the name of second competitor

;; IMPLEMENTATION:
(define-struct defeatstruct (comp1-d comp2-d))

;; CONSTRUCTOR TEMPLATE:
;; (make-tiestruct String String)

;; OBSERVER TEMPLATE:
;; defeatstruct-fn: Defeat -> ??
#|
(define (defeatstruct-fn d)
 ...
 (defeatstruct-comp1-d d)
 (defeatstruct-comp2-d d)
)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;

;; Outcome:

;;;;;;;;;;;;;;;;;


;; REPRESENTATION:
;; an Outcome is represented as:
;; -- a Tie
;; -- a Defeat

;; OBSERVER TEMPLATE:
;; outcome-fn: Outcome -> ??
#|
(
cond
((tiestruct? o)...)
((defeatstruct? o)...)
)
|#

;;;;;;;;;;;;;;;;;

;; OutcomeList:

;;;;;;;;;;;;;;;;;


;; An OutcomeList is represented as a list of outcome.

;; Constructor Template and Interpretation:
;; empty                    --- the empty list
;; (cons o ol)
;;   WHERE:
;;    o is an outcome       --- the first outcome
;;    ol is the list        --- rest of the outcomes except the first

;; Observer Template:
;; outcomelist-fn : OutcomeList -> ??
#|(define (outcome-fn lst)
  |(cond
    [(empty? lst)...]
    [ else (... (first lst)
                (outcomelist-fn (rest lst)))]))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;

;; StringList:

;;;;;;;;;;;;;;;;;

;; A StringList is represented as a list of competitors (strings).

;; Constructor Template and Interpretation:
;; empty                    --- the empty list
;; (cons s sl)
;;   WHERE:
;;    s is a competitor     --- the first competitor string
;;    sl is the list        --- rest of the competitors string except the first

;; Observer Template:
;; stringlist-fn : StringList -> ??
#|(define (stringlist-fn lst)
  |(cond
    [(empty? lst)...]
    [ else (... (first lst)
                (stirnglist-fn (rest lst)))]))
|#

;; The CompetitorList, TemporaryList, Outranklist, TraversedList are all
;; StringList.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;

;; Competitorstats:

;;;;;;;;;;;;;;;;;;;;

;; REPRESENTATION:
;; a competitorstats is represented as:
;; (make-competitorstats name outrnked-by outrnk nonlosing)

;; INTERPRETATION:
;; name: String, represents the Competitor
;; outrnked-by: NonNegInteger, represents the list of competitors that outrank
;;              given competitor
;; outrnked-by: NonNegInteger, represents the list of competitors that are being
;;              outranked-by the given competitor
;; nonlosing: PosReal, represnts the number of outcomes is which a competitor is
;;            winner or engaged in a tie, divided by the total number of
;;            outcomes in which the competitor is mentioned.

;; IMPLEMENTATION:
(define-struct competitorstats (name outrnked-by outrnk nonlosing))

;; CONSTRUCTOR TEMPLATE:
;; (make-competitorstats String NonNegInteger NonNegInteger PosReal)

;; OBSERVER TEMPLATE:
;; competitorstats-fn: competitorstats -> ??
#|
(define (competitorstats-fn r)
 ...
 (competitorstats-name r)
 (competitorstats-outrnked-by r)
 (competitorstats-outrnk r)
 (competitorstats-nonlosing r)
)
|#

;;;;;;;;;;;;;;;;;;;;;;;

;; Competitorstatslist:

;;;;;;;;;;;;;;;;;;;;;;;


;; A competitorstatslist is represented as a list of competitorstats.

;; Constructor Template and Interpretation:
;; empty                      --- the empty list
;; (cons r rl)
;;   WHERE:
;;    r is a competitorstats   --- the first competitorstats
;;    rl is the list           --- rest of the competitorstats except the first

;; Observer Template:
;; competitorstatslist-fn : competitorstatslist -> ??
#|(define (competitorstatslist-fn lst)
  |(cond
    [(empty? lst)...]
    [ else (... (first lst)
                (competitorstatslist-fn (rest lst)))]))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TIE:

;; tie : Competitor Competitor -> Tie
;; GIVEN: the names of two competitors
;; RETURNS: an indication that the two competitors have
;;     engaged in a contest, and the outcome was a tie
;; EXAMPLE: (tie "A" "B")
;;          => (make-tiestruct "A" "B")
;; DESIGN STRATEGY: Use Constructor template of tiestruct.

(define (tie comp1 comp2)
  (make-tiestruct comp1 comp2))

;; TESTS:
(begin-for-test
  (check-equal? (tie "A" "B")
                (make-tiestruct "A" "B")
                "correct assigning of values to structure"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFEATED:

;; defeated : Competitor Competitor -> Defeat
;; GIVEN: the names of two competitors
;; RETURNS: an indication that the two competitors have
;;     engaged in a contest, with the first competitor
;;     defeating the second
;; EXAMPLE: (defeat "A" "B")
;;          => (make-defeatstruct "A" "B")
;; DEISGN STRATEGY: Use Constructor template of defeatstruct.

(define (defeated comp1 comp2)
  (make-defeatstruct comp1 comp2))

;; TESTS:
(begin-for-test
  (check-equal? (defeated "A" "B")
                (make-defeatstruct "A" "B")
                "correct assigning of values to structure"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFEATED?:

;; defeated? : Competitor Competitor OutcomeList -> Boolean
;; GIVEN: the names of two competitors and a list of outcomes
;; RETURNS: true if and only if one or more of the outcomes indicates
;;     the first competitor has defeated or tied the second
;; EXAMPLES:
;;     (defeated? "A" "B" (list (defeated "A" "B") (tie "B" "C")))
;;  => true
;;
;;     (defeated? "A" "C" (list (defeated "A" "B") (tie "B" "C")))
;;  => false
;;
;;     (defeated? "B" "A" (list (defeated "A" "B") (tie "B" "C")))
;;  => false
;;
;;     (defeated? "B" "C" (list (defeated "A" "B") (tie "B" "C")))
;;  => true
;;
;;     (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
;;  => true
;; DESIGN STRATEGY: Divide into cases of Competitor Strings.


(define (defeated? c1 c2 outcomelist)
  (if (equal? c1 c2)
      true
      (defeated?-helper c1 c2 outcomelist)))

;; TESTS:
(begin-for-test
  (check-equal? (defeated? "B" "C" (list (defeated "A" "B") (tie "B" "C")))
                true
                 "correctly identified")
  (check-equal? (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
                true
                 "correctly identified")
  (check-equal? (defeated? "B" "A" (list (defeated "A" "B") (tie "B" "C")))
                false
                 "correctly identified")
  (check-equal? (defeated? "A" "C" (list (defeated "A" "B") (tie "B" "C")))
                false
                 "correctly identified")
  (check-equal? (defeated? "A" "B" (list (defeated "A" "B") (tie "B" "C")))
                true
                 "correctly identified")
  (check-equal? (defeated? "A" "A" (list (defeated "A" "A") (tie "B" "A")))
                true
                 "correctly identified"))

           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; defeated?-helper : Competitor Competitor OutcomeList -> Boolean
;; GIVEN: the names of two competitors and a list of outcomes
;; RETURNS: true if and only if one or more of the outcomes indicates
;;     the first competitor has defeated or tied the second.
;; WHERE:   Competitor's name are distinct.
;; EXAMPLES: (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
;;            => true
;; DESIGN STRATEGY: Divide into cases of Outcomes in Outcomelist.

(define (defeated?-helper c1 c2 outcomelist)
  (cond
    [(or
      (empty? outcomelist)
      (equal? (first outcomelist) (defeated c2 c1)))
     false]
    
    [(or
      (equal? (first outcomelist) (defeated c1 c2))
      (equal? (first outcomelist) (tie c1 c2))
      (equal? (first outcomelist) (tie c2 c1)))
     true]
    
    [else
     (defeated? c1 c2 (rest outcomelist))]))

;; TESTS:
(begin-for-test
  (check-equal? (defeated?-helper
                  "A"
                  "B"
                  (list (defeated "A" "B")
                        (tie "A" "B")))
                true
                "correctly identified"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OUTRANKS:

;; outranks : Competitor OutcomeList -> CompetitorList
;; GIVEN: the name of a competitor and a list of outcomes
;; RETURNS: a list of the competitors outranked by the given
;;     competitor, in alphabetical order
;; NOTE: it is possible for a competitor to outrank itself
;; EXAMPLES:
;;     (outranks "A" (list (defeated "A" "B") (tie "B" "C")))
;;  => (list "B" "C")
;;
;;     (outranks "B" (list (defeated "A" "B") (defeated "B" "A")))
;;  => (list "A" "B")
;;
;;     (outranks "C" (list (defeated "A" "B") (tie "B" "C")))
;;  => (list "B" "C")

;; DESIGN STRATEGY: Call a more general function.

(define (outranks c outcomelist)
  (sort (outrank-helper outcomelist (list c) '() '() 1) string<?))

;; TESTS:
(begin-for-test
  (check-equal? (outranks "A" (list (defeated "A" "B") (tie "B" "C")))
                (list "B" "C")
                "correct set of competitors")
  (check-equal? (outranks "B" (list (defeated "A" "B") (defeated "B" "A")))
                (list "A" "B")
                "correct set of competitors")
  (check-equal? (outranks "C" (list (defeated "A" "B") (tie "B" "C")))
                (list "B" "C")
                "correct set of competitors"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OUTRANKED-BY:

;; outranked-by : Competitor OutcomeList -> CompetitorList
;; GIVEN: the name of a competitor and a list of outcomes
;; RETURNS: a list of the competitors that outrank the given
;;     competitor, in alphabetical order
;; NOTE: it is possible for a competitor to outrank itself
;; EXAMPLES:
;;     (outranked-by "A" (list (defeated "A" "B") (tie "B" "C")))
;;  => (list)
;;
;;     (outranked-by "B" (list (defeated "A" "B") (defeated "B" "A")))
;;  => (list "A" "B")
;;
;;     (outranked-by "C" (list (defeated "A" "B") (tie "B" "C")))
;;  => (list "A" "B" "C")
;; DESIGN STRATEGY: Call a more general function.

(define (outranked-by c outcomelist)
  (sort (outrank-helper outcomelist (list c) '() '() 2) string<?))

;; TESTS:
(begin-for-test
  (check-equal? (outranked-by "A" (list (defeated "A" "B") (tie "B" "C")))
                (list)
                "correct set of competitors")
  (check-equal? (outranked-by "B" (list (defeated "A" "B") (defeated "B" "A")))
                (list "A" "B")
                "correct set of competitors")
  (check-equal? (outranked-by "C" (list (defeated "A" "B") (tie "B" "C")))
                (list "A" "B" "C")
                "correct set of competitors"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; outrank-helper:
;; OutcomeList TemporaryList OutrankList TraversedList -> OutrankList
;; GIVEN: OutcomeList TemporaryList OutrankList TraversedList
;; WHERE: TemporaryList holds the competitors which are to be checked for
;;        outranking or being outranked-by till this definition; OutrankList
;;        holds the competitors that are being outranked-by or outrank the given
;;        competitor till this definition; TraversedList holds the competitors
;;        that have been checked for outranking or being outranked-by.
;; RETURNS: an OutrankList that contains the competitors that have been
;;          outranked by the given competitor or the competitors that
;;          outrank the given competitor.
;; EXAMPLES: (outrank-helper
;;               (list (defeated "A" "B") (tie "B" "C"))
;;               (list "A" "B" "C")
;;               (list "B")
;;               (list "A"))
;;           => (list "C" "B")

;; DESIGN STRATEGY: Divide into cases of TemporaryList.
;; HALTING MEASURES: (length of the list, of competitors that will
;;                   traversed to see who they outrank, or who they are
;;                   being outranked-by) - (length of the TraversedList)

(define (outrank-helper
         outcomelist temporarylist outranklist traversedlist case)
  (cond
    [(empty? temporarylist) outranklist]
    [else (outrank-helper outcomelist
                          (temporary-list-update
                           outcomelist temporarylist traversedlist case)
                          (outrank-list-update
                           outcomelist temporarylist outranklist case)
                          (traversed-list-update temporarylist traversedlist)
                          case)]))

;; TESTS:
(begin-for-test
  (check-equal? (outrank-helper
                 (list (defeated "A" "B") (tie "B" "C"))
                 (list "A" "B" "C")
                 (list "B")
                 (list "A")
                 1)
                (list "C" "B")
                "correct set of competitors"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; temporary-list-update: OutcomeList TemporaryList TraversedList -> Temporary
;; GIVEN: OutcomeList TemporaryList TraversedList
;; WHERE: TemporaryList holds the competitors which are to be checked for
;;        outranking or being outranked-by till this definition;
;;        TraversedList holds the competitors that have been checked for
;;        outranking or being outranked-by.
;; RETURNS: a Temporary list that holds the competitors that are to be checked
;;          if they outrank any competitors; or to hold the competitors
;;          that are to checked if they are outranked by any.
;; EXAMPLES: (temporary-list-update
;;               (list (defeated "A" "B") (tie "B" "C") (defeated "A" "D"))
;;               (list "A" "B" "D")
;;               (list "A"))
;;            => (list "B" "D")

;; DESIGN STRATEGY: Combine Simpler functions.

(define (temporary-list-update outcomelist temporarylist traversedlist case)
  (remove-duplicates
   (append (rest temporarylist)
           (list-after-competitors-check
            (outrank-find
             (first temporarylist)
             outcomelist case)
            (remove-duplicates
             (cons (first temporarylist)
                   traversedlist))
            case))))

;; TESTS:
(begin-for-test
  (check-equal? (temporary-list-update
                 (list (defeated "A" "B") (tie "B" "C") (defeated "A" "D"))
                 (list "A" "B" "D")
                 (list "A")
                 1)
                (list "B" "D")
                "correct updation of temporarylist"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; outrank-list-update: OutcomeList TemporaryList OutrankList -> OutrankList
;; GIVEN: OutcomeList TemporaryList OutrankList
;; WHERE: TemporaryList holds the competitors which are to be checked for
;;        outranking or being outranked-by till this definition; OutrankList
;;        holds the competitors that are being outranked-by or outrank the given
;;        competitor till this definition. 
;; RETURNS: the OutrankList, after adding the competitors that have been
;;          outranked by the given competitor directly or indirectly; or
;;          the OutrankList after adding the competitors that have outranked
;;          the given competitor directly or indirectly.
;; EXAMPLES: (outrank-list-update
;;               (list (defeated "A" "B") (tie "B" "C") (defeated "A" "D"))
;;               (list "A" "B" "D")
;;               (list "B"))
;;           => (list "D" "B")
;; DESIGN STRATEGY: Combine Simpler functions.

(define (outrank-list-update outcomelist temporarylist outranklist case)
  (remove-duplicates
   (append (outrank-find
            (first temporarylist) outcomelist case)
           outranklist)))

;; TESTS:
(begin-for-test
  (check-equal? (outrank-list-update
                 (list (defeated "A" "B") (tie "B" "C") (defeated "A" "D"))
                 (list "A" "B" "D")
                 (list "B")
                 1)
                (list "D" "B")
                "correct updation of outranklist"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; traversed-list-update: TemporaryList TraversedList -> TraversedList
;; GIVEN: TemporaryList TraversedList
;; WHERE: TemporaryList holds the competitors which are to be checked for
;;        outranking or being outranked-by till this definition;
;;        TraversedList holds the competitors that have been checked for
;;        outranking or being outranked-by.
;; RETURNS: a TraversedList, which holds the competitors that have been
;;          checked for the competitors they outrank; or a TraversedList
;;          which holds the competitors that have been checked for being
;;          outranked.
;; EXAMPLES: (traversed-list-update
;;               (list "A" "B" "C")
;;               (list "A"))
;;            => (list "A")

;; DESIGN STRATEGY: Use constructor template on TraversedList. 

(define (traversed-list-update temporarylist traversedlist)
  (remove-duplicates
   (cons (first temporarylist)
         traversedlist)))

(begin-for-test
  (check-equal? (traversed-list-update
                 (list "A" "B" "C")
                 (list "A"))
                (list "A")
                "correct updation of traversed list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-after-competitors-check:
;; CompetitorList CompetitorList -> CompetitorList
;; GIVEN: CompetitorList CompetitorList
;; RETURNS: a CompetitorList after checking if the competitor,to be
;;          added to the temporary list (to check competitors it outranks
;;          or has been outranked by), already has been checked, if yes
;;          it does not add the competitor to the TemporaryList.
;; EXAMPLES: (list-after-competitors-check
;;               (list 1 2 3 )
;;               (list 2 3))
;;           => (list 1)
;; DESIGN STRATEGY: Recur on the (rest lst1) and (rest lst2). 

(define (list-after-competitors-check lst1 lst2 case)
  (cond
    [(empty? lst1) '()]
    [(empty? lst2) '()]
    [else
     (if (member (first lst1) lst2)
         (list-after-competitors-check (rest lst1) lst2 case)
         (cons
          (first lst1)
          (list-after-competitors-check (rest lst1) lst2 case)))]))

;; TESTS:
(begin-for-test
  (check-equal? (list-after-competitors-check
                 (list 1 2 3 )
                 (list 2 3)
                 1)
                (list 1)
                "correct element(s) removed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; outrank-find: Competitor OutcomeList -> CompetitorList
;; GIVEN: a Competitor and an OutcomeList.
;; RETURNS: a CompetitorList after finding the competitors that the given
;;          Competitor outranks directly; or the CompetitorList after
;;          finding the competitors that the given competitor is being
;;          directly outranked by.
;; EXAMPLES: (outrank-find
;;               "A"
;;               (list (defeated "A" "B") (tie "B" "C") (defeated "A" "D")))
;;           => (list "B" "D") 
;; DESIGN STRATEGY: Divide into cases of case (outranks(1) or outranked-by(2)).

(define (outrank-find c outcomelist case)
  (cond
    [(equal? case 1)
     (append
      (outranks-in-defeat c outcomelist)
      (outrank-in-first-tie c outcomelist)
      (outrank-in-second-tie c outcomelist))]

    [(equal? case 2)
     (append
      (outranked-by-in-defeat c outcomelist)
      (outrank-in-first-tie c outcomelist)
      (outrank-in-second-tie c outcomelist))]))

;; TESTS:
(begin-for-test
  (check-equal? (outrank-find
                 "A"
                 (list (defeated "A" "B") (tie "B" "C") (defeated "A" "D"))
                 1)
                (list "B" "D")
                "correct set of outranked competitors"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; outranks-in-defeat: Competitor OutcomList -> CompetitorList
;; GIVEN: a Competitor and an OutcomeList.
;; RETURNS: a CompetitorList after finding the competitors that the given
;;          Competitor outranks directly by defeating.
;; EXAMPLES: (outranks-in-defeat
;;               "A"
;;               (list (defeated "A" "B") (tie "B" "C") (defeated "A" "D")))
;;           => (list "B" "D") 
;; DESIGN STRATEGY: Use HOF map and filter.

(define (outranks-in-defeat c outcomelist)
  (map defeatstruct-comp2-d
       (filter
        ;; Competitor OutcomeList -> CompetitorList
        ;; RETURNS: true if competitor is equal to 1st competitor
        ;;          in defeatstruct. 
        (lambda (n) (equal? c (defeatstruct-comp1-d n)))
        (filter defeatstruct? outcomelist))))

;; TESTS:
(begin-for-test
  (check-equal? (outranks-in-defeat
                 "A"
                 (list (defeated "A" "B") (defeated "A" "C")))
                (list "B" "C")
                "correct set of outranked competitors"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; outranked-by-in-defeat: Competitor OutcomeList -> CompetitorList
;; GIVEN: a Competitor and an OutcomeList.
;; RETURNS: a CompetitorList after finding the competitors that the given
;;          Competitor is being outranked by directly by defeating.
;; EXAMPLES: (outranked-by-in-defeat
;;               "A"
;;               (list (defeated "A" "B") (tie "B" "C") (defeated "A" "D")))
;;           => (list "B" "D") 
;; DESIGN STRATEGY: Use HOF map and filter.

(define (outranked-by-in-defeat c outcomelist)
  (map defeatstruct-comp1-d
       (filter
        ;; Competitor OutcomeList -> CompetitorList
        ;; RETURNS: true if competitor is equal to 2nd competitor
        ;;          in defeatstruct. 
        (lambda (n) (equal? c (defeatstruct-comp2-d n)))
        (filter defeatstruct? outcomelist))))

;; TESTS:
(begin-for-test
  (check-equal? (outranked-by-in-defeat
                 "B"
                 (list (defeated "A" "B") (defeated "C" "B")))
                (list "A" "C")
                "correct list of competitors thaht outrank given competitor"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; outrank-in-first-tie: Competitor OutcomeList -> CompetitorList
;; GIVEN: a Competitor and an OutcomeList.
;; RETURNS: a CompetitorList after finding the competitors that the given
;;          Competitor outranks directly by drawing, if competitor is in
;;          position 1 in the tiestruct.
;; EXAMPLES: (outrank-in-first-tie
;;               "A"
;;               (list (tie "A" "B") (tie "A" "C")))
;;           => (list "B" "C")
;; DESIGN STRATEGY: Use HOF filter and map. 

(define (outrank-in-first-tie c outcomelist)
  (map tiestruct-comp2-t 
       (filter
        ;; RETURNS: true if competitor is equal to 1st competitor
        ;;          in tiestruct. 
        (lambda (n) (equal? c (tiestruct-comp1-t n)))
        (filter tiestruct? outcomelist))))

;; TESTS:
(begin-for-test
  (check-equal? (outrank-in-first-tie
                 "A"
                 (list (tie "A" "B") (tie "A" "C")))
                (list "B" "C")
                "correct list of competitors tied with competitor at
                 1st position"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; outrank-in-second-tie: Competitor OutcomeList -> CompetitorList
;; GIVEN: a Competitor and an OutcomeList.
;; RETURNS: a CompetitorList after finding the competitors that the given
;;          Competitor outranks directly by drawing, if competitor is in
;;          position 2 in the tiestruct.
;; EXAMPLES: (outrank-in-second-tie
;;               "B"
;;               (list (tie "A" "B") (tie "C" "B")))
;;           => (list "A" "C")
;; DESIGN STRATEGY: Use HOF filter and map.

(define (outrank-in-second-tie c outcomelist)
  (map tiestruct-comp1-t
       (filter
        ;; RETURNS: true if competitor is equal to 2nd competitor
        ;;          in tiestruct.          
        (lambda (n) (equal? c (tiestruct-comp2-t n)))
        (filter tiestruct? outcomelist))))

;; TESTS:
(begin-for-test
  (check-equal? (outrank-in-second-tie
                 "C"
                 (list (tie "A" "C") (tie "B" "C")))
                (list "A" "B")
                "correct list of competitors tied with competitor at
                 2nd position"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REMOVE-DUPLICATES:

;; remove-duplicates: StringList -> StringList
;; GIVEN: a StringList
;; RETURNS: a StringList, with no duplicate elements
;; EXAMPLES: (remove-duplicates
;;               (list "a" "a" "b"))
;;           => (list "a" "b")

;; DESIGN STRATEGY: Divide into cases of StringList elements.

(define (remove-duplicates l)
  (cond
    [(empty? l) '()]
    [(not (member (first l)
                  (remove-duplicates (rest l))))
     (cons (first l)
           (remove-duplicates (rest l)))]
    [else
     (remove-duplicates (rest l))]))

;; TESTS:
(begin-for-test
  (check-equal? (remove-duplicates
                 (list "a" "a" "b"))
                (list "a" "b")
                "correct removal of duplicate elements"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; power-ranking : OutcomeList -> CompetitorList
;; GIVEN: a list of outcomes
;; RETURNS: a list of all competitors mentioned by one or more
;;     of the outcomes, without repetitions, with competitor A
;;     coming before competitor B in the list if and only if
;;     the power-ranking of A is higher than the power ranking
;;     0of B.
;; EXAMPLE:
;;     (power-ranking
;;      (list (defeated "A" "D")
;;            (defeated "A" "E")
;;            (defeated "C" "B")
;;            (defeated "C" "F")
;;            (tie "D" "B")
;;            (defeated "F" "E")))
;;  => (list "C"   ; outranked by 0, outranks 4
;;           "A"   ; outranked by 0, outranks 3
;;           "F"   ; outranked by 1
;;           "E"   ; outranked by 3
;;           "B"   ; outranked by 4, outranks 2, 50%
;;           "D")  ; outranked by 4, outranks 2, 50%

;; DESIGN STRATEGY: Use HOF map on OutcomeList.

(define (power-ranking outcomelist)
  (map
   competitorstats-name
   (ranking (assign-stats-to-competitors outcomelist))))

;; TESTS:
(begin-for-test
  (check-equal? (power-ranking
                 (list (defeated "A" "D")
                       (defeated "A" "E")
                       (defeated "C" "B")
                       (defeated "C" "F")
                       (tie "D" "B")
                       (defeated "F" "E")))
                (list "C" "A" "F" "E" "B" "D")
                "Correct power ranking of the competitors"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ranking: competitorstatslist -> competitorstatslist
;; GIVEN: a competitorstatslist
;; RETURNS: a competitorstatslist after sorting them in order of highest to
;;          lowest rank.
;; EXAMPLES: (ranking
;;              (list
;;               (make-competitorstats "A" 5 0 0)
;;               (make-competitorstats "B" 4 0 0)))
;;          => (list
;;               (make-competitorstats "B" 4 0 0)
;;               (make-competitorstats "A" 5 0 0))

;; DESIGN STRATEGY: Divide into cases of competitorstatslist

(define (ranking l)
  (cond
    [(empty? l) '()]
    [else
     (ranks-sort (first l) (ranking (rest l)))]))

;; TESTS:
(begin-for-test
  (check-equal? (ranking
                (list
                 (make-competitorstats "A" 5 0 0)
                 (make-competitorstats "B" 4 0 0)))
                (list
                 (make-competitorstats "B" 4 0 0)
                 (make-competitorstats "A" 5 0 0))
                "correct ranking order"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ranks-sort: competitorstats competitorstatslist -> competitorstatslist
;; GIVEN: a competitorstats and competitorstatslist
;; RETURNS: a competitorstatslist, after sorting the competitorstatslist as per
;;          the conditions of outranked-by, outranks, non-losing percentage,
;;          or name of competitor.
;; EXAMPLES: (ranks-sort
;;               (make-competitorstats "A" 3 0 0)
;;               (list
;;                (make-competitorstats "B" 2 0 0)
;;                (make-competitorstats "C" 4 0 0)))
;;            => (list (make-competitorstats "B" 2 0 0)
;;                    (make-competitorstats "A" 3 0 0)
;;                    (make-competitorstats "C" 4 0 0))

;; DESIGN STRATEGY: Divide into cases of competitorstatslist.

(define (ranks-sort n l)
  (cond
     [(empty? l) (list n)]
     [(higher-ranking? n l)
      (cons n l)]
     [else
      (cons (first l) (ranks-sort n (rest l)))]))

;; TESTS:
(begin-for-test
  (check-equal? (ranks-sort
                 (make-competitorstats "A" 3 0 0)
                 (list
                  (make-competitorstats "B" 2 0 0)
                  (make-competitorstats "C" 4 0 0)))
                (list (make-competitorstats "B" 2 0 0)
                      (make-competitorstats "A" 3 0 0)
                      (make-competitorstats "C" 4 0 0))
                "correct sorting of the CompetitorstatsList"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; higher-ranking: Rankfield competitorstatslist -> Boolean
;; GIVEN: a Rankfield and a competitorstatslist
;; RETURNS: a boolean value, true if given rankfield is higher ranked than
;;          the rankfield from the list based on condition of outranked-by,
;;          otherwise false.
;; EXAMPLES: (higher-ranking?
;;               (make-competitorstats "A" 3 0 0)
;;               (list
;;                (make-competitorstats "B" 4 0 0)))
;;            => true
;; DEISGN STRATEGY: Divide into cases of competitorstatslist.

(define (higher-ranking? n l)
  (cond
    [(< (competitorstats-outrnked-by n)
        (competitorstats-outrnked-by (first l)))
     true]
    [(= (competitorstats-outrnked-by n)
        (competitorstats-outrnked-by (first l)))
     (higher-rank-outrank? n l)]
    [else
     false]))

;; TESTS:
(begin-for-test
  (check-equal? (higher-ranking?
                 (make-competitorstats "A" 3 0 0)
                 (list
                  (make-competitorstats "B" 4 0 0)))
                true
                "given competitor is higher ranked than 1st
                 competitor of list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; higher-rank-outrank?: Rankfield competitorstatslist -> Boolean
;; GIVEN: a Rankfield and a competitorstatslist
;; RETURNS: a Boolean, true if given rankfield outranks more
;;          competitors than rankfield from list, otherwise false.
;; EXAMPLES: (higher-rank-outrank?
;;               (make-competitorstats "A" 3 4 0)
;;               (list
;;                (make-competitorstats "B" 3 2 0)))
;;            => true
;; DEISGN-STRATEGY: Divide into cases of competitorstatslist.

(define (higher-rank-outrank? n l)
  (cond
    [(> (competitorstats-outrnk n) (competitorstats-outrnk (first l)))
     true]
    [(= (competitorstats-outrnk n) (competitorstats-outrnk (first l)))
     (higher-rank-nonlosingper? n l)]
    [else
     false]))

;; TESTS:
(begin-for-test
  (check-equal? (higher-rank-outrank?
                 (make-competitorstats "A" 3 4 0)
                 (list
                  (make-competitorstats "B" 3 2 0)))
                true
                "given competitor is higher ranked than 1st
                 competitor of list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; higher-rank-nonlosingper?: Rankfield competitorstatslist -> Boolean
;; GIVEN: a Rankfield and a competitorstatslist
;; RETURNS: a Boolean, true if given rankfield has higher nonlosing
;;          percentage than rankfield from list, otherwise false.
;; EXAMPLES: (higher-rank-nonlosingper?
;;               (make-competitorstats "A" 3 4 0.4)
;;               (list
;;                (make-competitorstats "B" 3 2 0.5)))
;;            => false
;; DESIGN-STRATEGY: Divide into cases of competitorstatslist.

(define (higher-rank-nonlosingper? n l)
  (cond
       [(> (competitorstats-nonlosing n) (competitorstats-nonlosing (first l)))
        true]
       [(= (competitorstats-nonlosing n) (competitorstats-nonlosing (first l)))
        (higher-rank-string n l)]
       [else
        false]))

;; TESTS:
(begin-for-test
  (check-equal? (higher-rank-nonlosingper?
                 (make-competitorstats "A" 3 4 0.4)
                 (list
                  (make-competitorstats "B" 3 2 0.5)))
                false
                "given competitor is hlower ranked than 1st
                 competitor of list")
  
  (check-equal? (higher-rank-nonlosingper?
                 (make-competitorstats "A" 3 4 0.5)
                 (list
                  (make-competitorstats "B" 3 2 0.4)))
                true
                "given competitor is higher ranked than 1st
                 competitor of list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; higher-rank-string?: Rankfield competitorstatslist -> Boolean
;; GIVEN: a Rankfield and a competitorstatslist
;; RETURNS: a Boolean, true if given rankfield is alphabetically
;;          higher ranked than rankfield from list, otherwise false.
;; EXAMPLES: (higher-rank-string
;;               (make-competitorstats "A" 2 0 0)
;;               (list
;;                (make-competitorstats "B" 2 0 0)))
;;            => true
;; DESIGN-STRATEGY: Divide into cases of competitorstatslist.

(define (higher-rank-string n l)
  (cond
    [(string<? (competitorstats-name n) (competitorstats-name (first l)))
     true]
    [else
     false]))

;; TESTS:
(begin-for-test
  (check-equal? (higher-rank-string
                 (make-competitorstats "A" 2 0 0)
                 (list
                  (make-competitorstats "B" 2 0 0)))
                true
                "given competitor is higher ranked than 1st
                 competitor of list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assign-stats-to-competitors: OutcomeList -> competitorstatslist
;; GIVEN: an OutcomeList
;; RETURNS: a competitorstatslist, with fields of rankfield struct set for
;;          every competitor mentioned in the outcomelist.
;; EXAMPLES: (assign-stats-to-competitors
;;               (list
;;                (defeated "A" "B")
;;                (tie "B" "C")))
;;           => (list
;;               (make-competitorstats "A" 0 2 1)
;;               (make-competitorstats "B" 3 2 0.5)
;;               (make-competitorstats "C" 3 2 1))
;; DESIGN-STRATEGY: Use HOF map.

(define (assign-stats-to-competitors outcomelist)
  (map
   ;; OutcomeList -> competitorstatslist
   ;; RETURNS: a competitorstatslist after adding field values
   ;;          for every competitor.
   (lambda (n) (make-competitorstats
                n
                (length (outranked-by n outcomelist))
                (length (outranks n outcomelist))
                (non-losing-calculate n outcomelist)))
   (list-of-competitors outcomelist)))

;; TESTS:
(begin-for-test
  (check-equal? (assign-stats-to-competitors
                 (list
                  (defeated "A" "B")
                  (tie "B" "C")))
                (list
                 (make-competitorstats "A" 0 2 1)
                 (make-competitorstats "B" 3 2 0.5)
                 (make-competitorstats "C" 3 2 1))
                "correct stats assigned to elements of list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-of-competitors: OutcomeList -> CompetitorList
;; GIVEN: an OutcomeList
;; RETURNS: a CompetitorList that includes all the competitors
;;          that are mentioned in the outcomes.
;; EXAMPLES: (list-of-competitors
;;               (list
;;                (defeated "A" "B")
;;                (tie "B" "C")
;;                (defeated "A" "D")))
;;            => (list "A" "B" "D" "C")
;; DESIGN STRATEGY: Combine simpler functions.

(define (list-of-competitors outcomelist)
  (remove-duplicates (append
                     (append (list-of-defeat-first outcomelist)
                             (list-of-tie-first outcomelist))
                     (append (list-of-defeat-second outcomelist)
                             (list-of-tie-second outcomelist)))))

;; TESTS:
(begin-for-test
  (check-equal? (list-of-competitors
                 (list
                  (defeated "A" "B")
                  (tie "B" "C")
                  (defeated "A" "D")))
                (list "A" "B" "D" "C")
                "correct list of competitors"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-of-defeat-first: OutcomeList -> CompetitorList
;; GIVEN: an OutcomeList
;; RETURNS: a CompetitorList, after adding 1st Competitor
;;          of a defeatstruct.
;; EXAMPLES: (list-of-defeat-first
;;               (list (defeated "A" "B")
;;                     (tie "B" "C")))
;;           => (list "A")
;; DESIGN-STRATEGY: Use HOF map.

(define (list-of-defeat-first outcomelist)
  (map
   defeatstruct-comp1-d
   (filter defeatstruct? outcomelist)))

;; TESTS:
(begin-for-test
  (check-equal? (list-of-defeat-first
                 (list (defeated "A" "B")
                       (tie "B" "C")))
                (list "A")
                "correct list after taking out 1st element of defeatstruct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-of-tie-first: OutcomeList -> CompetitorList
;; GIVEN: an OutcomeList
;; RETURNS: a CompetitorList, after adding 1st Competitor
;;          of a tiestruct.
;; EXAMPLES: (list-of-tie-first
;;               (list (defeated "A" "B")
;;                     (tie "B" "C")))
;;            => (list "B")
;; DESIGN-STRATEGY: Use HOF map.

(define (list-of-tie-first outcomelist)
  (map
   tiestruct-comp1-t
   (filter tiestruct? outcomelist)))

;; TESTS:
(begin-for-test
  (check-equal? (list-of-tie-first
                 (list (defeated "A" "B")
                       (tie "B" "C")))
                (list "B")
                "correct list after taking out 1st element of tiestruct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-of-defeat-second: OutcomeList -> CompetitorList
;; GIVEN: an OutcomeList
;; RETURNS: a CompetitorList, after adding 2nd Competitor
;;          of a defeatstruct.
;; EXAMPLES: (list-of-defeat-first
;;               (list (defeated "A" "B")
;;                     (tie "B" "C")))
;;           => (list "B")
;; DESIGN-STRATEGY: Use HOF map.

(define (list-of-defeat-second outcomelist)
  (map
   defeatstruct-comp2-d
   (filter defeatstruct? outcomelist)))

;; TESTS:
(begin-for-test
  (check-equal? (list-of-defeat-second
                 (list (defeated "A" "B")
                       (tie "B" "C")))
                 (list "B")
                 "correct list after taking out 2nd element of defeatstruct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-of-tie-second: OutcomeList -> CompetitorList
;; GIVEN: an OutcomeList
;; RETURNS: a CompetitorList, after adding 2nd Competitor
;;          of a tiestruct.
;; EXAMPLES: (list-of-defeat-first
;;               (list (defeated "A" "B")
;;                     (tie "B" "C")))
;;           => (list "C")
;; DESIGN-STRATEGY: Use HOF map.

(define (list-of-tie-second outcomelist)
  (map
   tiestruct-comp2-t
   (filter tiestruct? outcomelist)))

;; TESTS:
(begin-for-test
  (check-equal? (list-of-tie-second
                 (list (defeated "A" "B")
                       (tie "B" "C")))
                (list "C")
                "correct list after taking out 2nd element of tiestruct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; non-losing-calculate: Competitor OutcomeList -> Real
;; GIVEN: a Competitor and Outcomelist
;; RETURNS: the non losing percentage of the given competitor.
;; EXAMPLES: (non-losing-calculate
;;               "A"
;;               (list
;;                (defeated "A" "B")
;;                (defeated "C" "A")))
;;            => 0.5
;; DESIGN STRATEGY: Combine simpler functions.

(define (non-losing-calculate n outcomelist)
  (/ (length (outrank-find n outcomelist 1))
     (length (append (outrank-find n outcomelist 1)
                     (outranked-by-in-defeat n outcomelist)))))

;; TESTS:
(begin-for-test
  (check-equal? (non-losing-calculate
                 "A"
                 (list
                  (defeated "A" "B")
                  (defeated "C" "A")))
                0.5
                "correct non losing percentage calculated"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
