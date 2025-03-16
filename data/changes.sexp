(:changes
 (:release "0.3" nil
  (:item
   (:paragraph
    "A" "new" "protocol" "enables" "clients" "to" "find" "dictionary" "entries"
    "that" "are" "similar" "to" "a" "given" "string" "or" "corrections" "for"
    "a" "given" "misspelled" "word" ".")
   (:paragraph
    "The" "following" "new" "functions" "provide" "increasingly" "abstract"
    "functionality" "for" "enumerating" "similar" "words" "and" "corrections:"
    (:symbol "spell:map-similar") "," (:symbol "spell:map-corrections") "and"
    (:symbol "spell:corrections") ".")
   (:paragraph
    "For" "convenience" "," "the" "function"
    (:symbol "spell:english-corrections") "automatically" "uses" "the" "English"
    "dictionary" "and" "considers" "the" "appropriate" "case" "variants" "of"
    "the" "supplied" "string" "."))
  (:item
   (:paragraph
    "Documentation" "is" "now" "available" "in" "the" (:tt "documentation")
    "directory" "."))
  (:item
   (:paragraph
    "The" "new" "function" (:symbol "map-entries") "calls" "a" "supplied"
    "function" "for" "each" "entry" "in" "a" "given" "dictionary" ".")))
 (:release "0.2" "2025-01-05"
  (:item
   (:paragraph
    "The" (:tt "README.org") "file" "now" "uses" "the" "org-mode" "format"
    "." "The" "examples" "have" "been" "updated" "."))
  (:item
   (:paragraph
    "The" (:tt "spell/simple") "system" "is" "now" "an" "alias" "for" "the"
    (:tt "spell") "system" "since" "the" "improved" "memory" "footprint"
    "makes" "the" "former" "unnecessary" "."))
  (:item
   (:paragraph
    "Compilation" "and" "loading" "times" "as" "well" "as" "memory" "footprint"
    "have" "been" "significantly" "reduced" "."))
  (:item
   (:paragraph
    "Dictionaries" "can" "now" "be" "built" "from" "multiple" "source" "files"
    "." "Additional" "words" "are" "now" "loaded" "from"
    (:tt "data/english-additions.txt") ".")))
 (:release "0.1" "2024-12-20"
  (:item
   (:paragraph
    "Initial" "release" "with" "basic" "lookup" "for" "the" "English"
    "dictionary" "but" "slow" "compilation" "and" "loading" "and" "a" "big"
    "memory" "footprint" "."))))
