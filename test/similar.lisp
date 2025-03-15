(cl:in-package #:spell.test)

(fiveam:in-suite :spell)

(test map-similar.entry
  "Smoke test for the `map-similar' function with \"entry results\"."
  (flet ((expected-entry (spelling class distance)
           (let ((word (find-if (a:of-type class) (spell:english-lookup spelling))))
             (list spelling word distance)))
         (collect (string threshold &rest args)
           (let ((results '()))
             (flet ((collect (spelling entry distance)
                      (push (list spelling entry distance) results)))
               (apply #'spell:map-similar
                      #'collect string spell::*english-dictionary* threshold
                      args))
             results)))
    (is (set-equal/entry (list (expected-entry "pitchfork" 'spell::verb 1)
                               (expected-entry "pitchfork" 'spell::noun 1))
                         (collect "pitchfort" 1)))
    (is (set-equal/entry (list (expected-entry "pitchfork" 'spell::verb 0)
                               (expected-entry "pitchforks" 'spell::verb 1)
                               (expected-entry "pitchfork" 'spell::noun 0)
                               (expected-entry "pitchforks" 'spell::noun 1))
                         (collect "pitchfork" 1)))
    ;; The following query string is specifically chosen to trigger a
    ;; somewhat uncommon code path in `map-similar'.
    (is (set-equal/entry (list (expected-entry "willful" 'spell::adjective 0)
                               (expected-entry "wilful"  'spell::adjective 1))
                         (collect "willful" 1)))))

(test map-similar.spelling
  "Smoke test for the `map-similar' function with \"spelling results\"."
  (flet ((collect (string threshold &rest args)
           (let ((results '()))
             (flet ((collect (spelling distance)
                      (push (list spelling distance) results)))
               (apply #'spell:map-similar
                      #'collect string spell::*english-dictionary* threshold
                      :group-by :spelling args))
             results)))
    (is (set-equal/equal '(("pitchfork" 1))
                         (collect "pitchfort" 1)))
    (is (set-equal/equal '(("pitchfork" 0) ("pitchforks" 1))
                         (collect "pitchfork" 1)))
    ;; The following query string is specifically chosen to trigger a
    ;; somewhat uncommon code path in `%map-similar'.
    (is (set-equal/equal '(("banded" 0) ("bandied" 1) ("banted" 1)
                           ("banned" 1) ("banked" 1) ("banged" 1)
                           ("barded" 1) ("branded" 1) ("bonded" 1)
                           ("sanded" 1) ("landed" 1) ("handed" 1))
                         (collect "banded" 1)))))
