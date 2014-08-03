(ns mastermind.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Welcome. Please start a repl session, and call (init) to start."))

;; Start that game with (init) function. Assuming the player knows the rules. As will be explained by the output of the (init), guess must be provided as (guess '(x y)), where x is the number of exact matches, and y the number of non-exact matches.

;; We first define the available colors

(def color-list
  '[red, green, orange, yellow, blue, purple])

;; Define the current-guess. I use an atom, because (a) I have only managed to get halfway through my first clojure book - and atom seems like the best way of doing this, and (b) I understand so far that states need to be handled with care in clojure. Haven't completely understood state yet, so this may not be the best way of doing this. 

(def current-guess
  (atom '()))

;; We define a 'possibles' list to keep track of all the possible items 
;; all-possibles here is the static one - used to re-init, and needed for testing (see bottom)

(def all-possibles
  (atom (for [a color-list
              b color-list
              c color-list
              d color-list]
          (vector a b c d))))

(def possibles
  (atom '()))

;; Let's start the game!
;; We set possibles to all the possible values, and init with [red red green green] - because Wikipedia says so!

(defn init []
  (do 
    (reset! possibles (deref all-possibles))
    (println "game on!")
    (println "Please provide the hint in the form - (guess '(x y)), where x is the number of exact matches, and y the number of non-exact matches")
    (reset! current-guess '[red red green green])))

;; This is the checker function for the exact matches. Quite simple. 

(defn compare-exact [code test]
  (count (filter true? (map = code test))))

;; This is the checker function for the non-exact matches. Not so simple. A bit of google helps. http://stackoverflow.com/questions/18152062/common-elements-in-two-lists-with-duplicates. Modified one of the easier methods to make it even simpler. 

(defn compare-approx [code test]
  (apply + (vals (merge-with min 
                           (select-keys (frequencies code) test)
                           (select-keys (frequencies test) code)))))

;; Function to compare a code with a test

(defn compare-code [code test]
  (list (compare-exact code test)
        (- (compare-approx code test) (compare-exact code test))))

;; Function to filter the current set of possible by a test, and a given result

(defn filter-color [possibles test result]
  (filter #(= result
              (compare-code % test))
          possibles))

;; Finally - the guessing function. I am unable to prove mathematically why this finishes in <= 10 moves, but it does seem to for all the tests I ran (see function "test-all" below. So I also produce the Knuth algorithm, as that is mathematically proven to solve in <= 5 moves. Well -  I mean proven by Knuth, obviously. For that, see the other file. 

(defn guess [feedback]
  (if (= feedback '(4 0)) (println "I have read your mind. Final guess" (deref current-guess))
      (do
        (reset! possibles (filter-color (deref possibles)
                                        (deref current-guess)
                                        feedback))
        (cond (= (count (deref possibles)) 1) (println "I have read your mind. Final guess" (deref possibles))
              (= (count (deref possibles)) 0) "Looks like YOU made a mistake - puny human! Therefore you lose! Mistakes will not be tolerated. Just kidding - start again with (init). Intermediate states are not stored, so everything resets to 0 - sorry."
              :else (reset! current-guess (first (deref possibles)))))))

;; Testing - As I couldn't confirm mathematically that it works for all codes in <= 10 moves, I prove it empirically. 

(defn test-number-of-guesses [code tries]
  (cond (= (deref current-guess) code) tries
        (= (count (deref possibles)) 1) tries
        :else (do
                (guess (compare-code code (deref current-guess)))
                (recur code (inc tries)))))

;; Need to run init before each test - otherwise it won't work. Used "when" because non-nil/false is true. Also - please remove/comment all output strings (line numbers 40/41/77) before running - otherwise could crash. Maximum depth found = 8. Therefore this solution works.

(defn test-all []
  (reduce max (for [x (deref all-possibles) :when (init)]
                (test-number-of-guesses x 1))))
