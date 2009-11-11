#lang scheme

(require test-engine/scheme-tests)
(require scheme/system)

(define SMALL-DICTIONARY (open-input-file "small.txt"))


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
            (let ([next-index (hash-iterate-next given-word 
                                                 index)])
              (cond
                [(false? next-index) ;;last item
                 (remove-key-or-value new-word
                                      (hash-iterate-key given-word
                                                        index))]
                [else
                 (subtract-acc
                  (remove-key-or-value new-word
                                       (hash-iterate-key given-word
                                                         index))
                  next-index)])))]
    (cond
      [(not (is-contained? given-word origin-word)) false]
      [else
       (subtract-acc origin-word
                     (hash-iterate-first given-word))])))


(check-expect (subtract (build-lexical-analysis "hellol")
                        (build-lexical-analysis "hell"))
              (build-lexical-analysis "ol"))

(check-expect (subtract (build-lexical-analysis "funky monkey")
                        (build-lexical-analysis "monkey"))
              (build-lexical-analysis "funky"))

(check-expect (subtract (build-lexical-analysis "monkey")
                        (build-lexical-analysis "monkey"))
              (build-lexical-analysis ""))


;; read-all : input-port -> (listof string)
;; just everything from the dictionary
(define (read-all the-dictionary)
  (local [(define (read-accumulative accumulator)
            (let ([readed (read the-dictionary)])
              (cond
                [(empty? the-dictionary) accumulator]
                [(eof-object? readed) accumulator]
                [else (read-accumulative (append accumulator
                                                 (list readed)))])))]
    (read-accumulative empty)))


;; always ship this with at least small dictionary
(check-expect (read-all SMALL-DICTIONARY)
              '(abash aura bar barb bee beg blush bog bogus bough bow brew brow brush bug bugs bus but egg ego erg ghost go goes gorge gosh grew grow grub gush he her here hew hog hose how hub hug owe rub sew she shrub shrug sub surge swore web wee were whore whose woe wore worse))

(test)



