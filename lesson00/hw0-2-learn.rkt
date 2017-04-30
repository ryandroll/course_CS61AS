#lang racket

(require berkeley)
(provide (all-defined-out))

#|

Write a procedure indef-article that takes in a word as its only argument and returns a sentence.
See examples below for how indef-article should work.
Remember that the indefinite article for anything that starts with a consonant is "a",
and the indefinite article for anything that starts with a vowel is "an". You can ignore any edge cases.

-> (indef-article 'beetle)
'(a beetle)
-> (indef-article 'apple)
'(an apple)

|#

(define (indef-article word)
  (if (member? (first word) 'aeiou)
      (sentence 'an word)
      (sentence 'a word)
  )
)
