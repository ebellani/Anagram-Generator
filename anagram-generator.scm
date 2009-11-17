#lang scheme

(require test-engine/scheme-tests)

;; read-all : input-port -> (listof string)
;; just everything from the dictionary
(define (read-all the-dictionary)
  (local [(define (read-accumulative accumulator)
            (let ([readed (read the-dictionary)])
              (cond
                [(empty? the-dictionary) accumulator]
                [(eof-object? readed) accumulator]
                [else (read-accumulative
                       (append accumulator
                               (list (symbol->string readed))))])))]
    (read-accumulative empty)))


;; (define MEDIUM-DICTIONARY (read-all (open-input-file "dictionaries/medium.txt")))

;; something to test with
;; replace with (read-all (open-input-file some-file)) 
;; like the file above if you want to make some 'real' tests
(define DEFAULT-DICTIONARY (list "abash" "aura" "bar" "barb" "bee" "beg"
                                 "blush" "bog" "bogus" "bough" "bow" "brew"
                                 "brow" "brush" "bug" "bugs" "bus" "but" "egg"
                                 "ego" "erg" "ghost" "go" "goes" "gorge"
                                 "gosh" "grew" "grow" "grub" "gush" "he"
                                 "her" "here" "hew" "hog" "hose" "how" "hub"
                                 "hug" "owe" "rub" "sew" "she" "shrub"
                                 "shrug" "sub" "surge" "swore" "web" "wee"
                                 "were" "whore" "whose" "woe" "wore" "worse"))


;; build-lexical-analysis : string -> hash
;; calculates the letter frequency for a given word
;; uses the letter as key and the frequency as value.
;; Disconsiders whitespace
(define (build-lexical-analysis word0)
  (local [(define (build-it word the-analysis)
            (cond
              [(empty? word) the-analysis]
              [(or (char-whitespace? (first word))
                   (hash-has-key? the-analysis
                                  (first word)))
               (build-it (rest word)
                         the-analysis)]
              [else
               (build-it
                (rest word)
                (hash-set the-analysis
                          (first word)
                          (count (λ (the-char)
                                   (equal? (first word)
                                           the-char))
                                 word)))]))]
    (build-it (string->list word0) #hash())))


(check-expect (build-lexical-analysis "")
              #hash())

(check-expect (build-lexical-analysis "hellol")
              #hash((#\e . 1)
                    (#\h . 1)
                    (#\l . 3)
                    (#\o . 1)))

(check-expect (build-lexical-analysis "hello world")
              #hash((#\d . 1)
                    (#\e . 1)
                    (#\h . 1)
                    (#\l . 3)
                    (#\o . 2)
                    (#\r . 1)
                    (#\w . 1)))



;; is-contained? : hash hash -> boolean
;; checks if a hash contains all the keys of another 
;; I've used for here to get to know it. Frankly, I prefer
;; recursion so TODO: refactor this to use normal recursion.
(define (is-contained? given-word origin-word)
  (let ([contains? true])
    (begin (for ((key (in-hash-keys given-word))
                 #:when (or (not (hash-has-key? origin-word key))
                            (and (hash-has-key? origin-word key)
                                 (> (hash-ref given-word key)
                                    (hash-ref origin-word key)))))
             (set! contains? false))
           contains?)))


(check-expect (is-contained? (build-lexical-analysis "hell")
                             (build-lexical-analysis "hellol"))
              true)

(check-expect (is-contained? (build-lexical-analysis "hella")
                             (build-lexical-analysis "hellol"))
              false)

(check-expect (is-contained? (build-lexical-analysis "hellll")
                             (build-lexical-analysis "hellol"))
              false)

(check-expect (is-contained? (build-lexical-analysis "go")
                             (build-lexical-analysis "ghost"))
              true)




;; subtract : hash hash -> hash or false
;; checks if a given word is contained
;; in the original, which means that an the given
;; word could be part of an anagram. Returns
;; the original word minus the contents of the given word.
;; Returns false if the given word is not contained in the original.
(define (subtract origin-word given-word)
  (local [;; remove-key-or-value : hash symbol -> hash
          ;; checks if there is something left in the value
          ;; for the key, if not remove the key/pair.
          (define (remove-key-or-value new-word key)
            (let ([value (- (hash-ref new-word key)
                            (hash-ref given-word key))])
              (cond
                [(zero? value)
                 (hash-remove new-word key)]
                [else (hash-set new-word key value)])))
          
          ;; subtract-acc : hash number -> hash
          (define (subtract-acc new-word index)
            (cond
              [(false? index) new-word]
              [else
               (let ([next-index (hash-iterate-next given-word index)]
                     [key (hash-iterate-key given-word index)])
                 (cond
                   [(false? index) false]
                   [(false? next-index) ;;last item
                    (remove-key-or-value new-word key)]
                   [else
                    (subtract-acc (remove-key-or-value new-word key)
                                  next-index)]))]))]
    (cond
      [(not (is-contained? given-word origin-word)) false]
      [else
       (subtract-acc origin-word
                     (hash-iterate-first given-word))])))


(check-expect (subtract (build-lexical-analysis "hellol")
                        (build-lexical-analysis ""))
              (build-lexical-analysis "hellol"))

(check-expect (subtract (build-lexical-analysis "hellol")
                        (build-lexical-analysis "hella"))
              false)

(check-expect (subtract (build-lexical-analysis "funky monkey")
                        (build-lexical-analysis "not so funky monkey"))
              false)

(check-expect (subtract (build-lexical-analysis "hellol")
                        (build-lexical-analysis "hell"))
              (build-lexical-analysis "ol"))

(check-expect (subtract (build-lexical-analysis "funky monkey")
                        (build-lexical-analysis "monkey"))
              (build-lexical-analysis "funky"))

(check-expect (subtract (build-lexical-analysis "monkey")
                        (build-lexical-analysis "monkey"))
              (build-lexical-analysis ""))


;; build-lexicon : hash (listof string) -> (listof string)
;; clean a dictionary to contain only those words that are 
;; subsets of the original word
(define (build-lexicon given-word dictionary0)
  (local [(define (build-lexicon-acc dictionary lexicon)
            (cond
              [(empty? dictionary) lexicon]
              [else
               (let ([current-word-lexical-analysis
                      (build-lexical-analysis (first dictionary))])
                 (cond
                   [(is-contained? current-word-lexical-analysis
                                   given-word)
                    (build-lexicon-acc (rest dictionary)
                                       (append lexicon
                                               (list (first dictionary))))]
                   [else
                    (build-lexicon-acc (rest dictionary)
                                       lexicon)]))]))]
    (build-lexicon-acc dictionary0 empty)))


(check-expect (build-lexicon (build-lexical-analysis "swore web")
                             DEFAULT-DICTIONARY)
              '("bee" "bow" "brew" "brow"
                      "owe" "sew" "swore" "web"
                      "wee" "were" "woe" "wore" "worse"))

(check-expect (build-lexicon (build-lexical-analysis "ghost")
                             DEFAULT-DICTIONARY)
              '("ghost" "go" "gosh" "hog"))


(check-expect (build-lexicon (build-lexical-analysis "st")
                             '("g" "ho" "st" "host"))
              '("st"))


;; analyse-list : (listof string) -> hash
;; build a lexical analysis of an entire lexicon
(define (analyse-list los)
  (build-lexical-analysis
   (foldr string-append
          ""
          los)))

;; generate-all-anagrams : string (listof string) -> (listof (listof string))
;; generates all the anagrams for a given word that are 
;; found in the lexicon. Returns empty if no 
;; anagram is possible
(define (generate-all-anagrams given-word dictionary)
  (local [;; hash-empty? : hash or boolean -> boolean
          ;; checks if a hash is empty. For more readable code.
          (define (hash-empty? a-hash)
            (cond
              [(false? a-hash) false]
              [else (false? (hash-iterate-first a-hash))]))
          
          ;; find-anagram (listof string) (listof string) -> (listof string) or false
          ;; discover if the the anagram accumulator matches the initial word, 
          ;; because if so, it is an anagram. 
          ;; Returns false if it is not and the lexicon is empty, because
          ;; there is no more ways to search, but continue the search if it is not empty.
          (define (find-anagram a-lexicon anagram-acc)
            (let ([word-result (subtract (build-lexical-analysis given-word)
                                         (analyse-list anagram-acc))])
              (cond
                [(false? word-result) false]
                [(not (hash-empty? word-result))
                 (cond
                   [(empty? a-lexicon) false]
                   [else 
                    (find-anagrams (build-lexicon word-result
                                                  a-lexicon)
                                   anagram-acc)])] ;; chamar outra func com o prox elemento
                [else anagram-acc])));; we have found an anagram
          
          ;; find-anagrams : (listof string) (listof string) -> (listof (listof string)) or false
          ;; Checks for the existence of anagrams for the anagram accumulator
          ;; within the lexicon given.
          ;; Uses the find-anagram for each entry of the new lexicon trying to find a 
          ;; valid anagram. If one is not found return false an trackbacks
          (define (find-anagrams a-lexicon anagram-acc)
            (cond
              [(empty? a-lexicon) false]
              [(hash-empty? (subtract
                             (build-lexical-analysis given-word)
                             (build-lexical-analysis (first anagram-acc))))
               (list (first anagram-acc))]
              [else
               (let ([possible-anagram
                      (find-anagram a-lexicon
                                    (append anagram-acc
                                            (list (first a-lexicon))))])
                 (cond
                   [(false? possible-anagram)
                    (find-anagrams (rest a-lexicon)
                                   anagram-acc)]
                   [else
                    (sort possible-anagram string<?)]))]))]
    
    (remove-duplicates
     (filter-map (λ (dictionary-entry)
                   (find-anagrams dictionary
                                  (list dictionary-entry)))
                 (build-lexicon (build-lexical-analysis given-word)
                                dictionary)))))



(check-expect (generate-all-anagrams "ghost"
                                     '("ghost" "g" "ho" "host" "st"))
              '(("ghost")
                ("g" "ho" "st")
                ("g" "host")))



(check-expect (generate-all-anagrams "web"
                                     DEFAULT-DICTIONARY)
              '(("web")))


(check-expect (generate-all-anagrams "bow"
                                     DEFAULT-DICTIONARY)
              '(("bow")))

(check-expect (generate-all-anagrams ""
                                     DEFAULT-DICTIONARY)
              '())



(check-expect (generate-all-anagrams "swore"
                                     DEFAULT-DICTIONARY)
              '(("swore")
                ("worse")))



(check-expect (generate-all-anagrams "swore web"
                                     DEFAULT-DICTIONARY)
              '(("swore" "web")
                ("web" "worse")))

(test)



