#lang racket/base


(require racket/contract/base)


(provide
 (struct-out word)
 (contract-out
  [split-words (-> string? (listof word?))]
  [lowercase-alphanumeric-ascii-string? (-> any/c boolean?)]
  [camel-case-ascii-string? (-> any/c boolean?)]
  [snake-case-ascii-string? (-> any/c boolean?)]
  [kebab-case-ascii-string? (-> any/c boolean?)]
  [upper-camel-case
   (-> (or/c string? (sequence/c word?)) (and/c camel-case-ascii-string? immutable?))]
  [lower-camel-case
   (-> (or/c string? (sequence/c word?)) (and/c camel-case-ascii-string? immutable?))]
  [upper-snake-case
   (-> (or/c string? (sequence/c word?)) (and/c snake-case-ascii-string? immutable?))]
  [screaming-snake-case
   (-> (or/c string? (sequence/c word?)) (and/c snake-case-ascii-string? immutable?))]
  [lower-snake-case
   (-> (or/c string? (sequence/c word?)) (and/c snake-case-ascii-string? immutable?))]
  [upper-kebab-case
   (-> (or/c string? (sequence/c word?)) (and/c kebab-case-ascii-string? immutable?))]
  [screaming-kebab-case
   (-> (or/c string? (sequence/c word?)) (and/c kebab-case-ascii-string? immutable?))]
  [lower-kebab-case
   (-> (or/c string? (sequence/c word?)) (and/c kebab-case-ascii-string? immutable?))]
  [camel-case-words (-> camel-case-ascii-string? (listof word?))]
  [snake-case-words (-> snake-case-ascii-string? (listof word?))]
  [kebab-case-words (-> kebab-case-ascii-string? (listof word?))]))


(require racket/match
         racket/sequence
         racket/string)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (lowercase-alphanumeric-ascii-string? v)
  (match v
    [(pregexp #px"^[[:lower:][:digit:]]*$") #true]
    [_ #false]))


(define (camel-case-ascii-string? v)
  (match v
    [(pregexp #px"^([[:alpha:]][[:alnum:]]*)?$") #true]
    [_ #false]))


(define (snake-case-ascii-string? v)
  (match v
    [(pregexp #px"^([[:alpha:]_][[:alnum:]_]*)?$") #true]
    [_ #false]))


(define (kebab-case-ascii-string? v)
  (match v
    [(pregexp #px"^([[:alpha:]-][[:alnum:]-]*)?$") #true]
    [_ #false]))


(define contract-guard (struct-guard/c (sequence/c lowercase-alphanumeric-ascii-string?)))

(struct word (parts)
  #:transparent
  #:guard
  (λ (parts name)
    (for/list ([part (contract-guard parts name)])
      (string->immutable-string part))))


(define (split-words text)
  (for/list ([w (in-list (string-split text))])
       (let* ([w (string-replace w #px"-" " ")]
              [w (string-replace w #px"_" " ")]
              [w (string-replace w #px"[^[:alnum:][:space:]]" "")]
              [w
               (let loop ([w w])
                 (match w
                   [(pregexp #px"(^.*[[:alnum:]][[:lower:]])([[:upper:]].*$)"
                             (list _ before after))
                    (loop (string-append before " " after))]
                   [_ w]))]
              [w (string-downcase w)])
         (word (string-split w)))))


(define (split-words-if-string text)
  (if (string? text) (split-words text) (sequence->list text)))


(define (word-upper-camel-case w)
  (string-join (map string-titlecase (word-parts w)) ""))


(define (word-lower-camel-case w)
  (match-define (cons first-part rest-parts) (word-parts w))
  (string-join (cons (string-downcase first-part) (map string-titlecase rest-parts)) ""))


(define (word-lowercase w)
  (string-downcase (string-join (word-parts w) "")))


(define (word-screaming-case w)
  (string-upcase (string-join (word-parts w) "")))


(module+ test
  (test-case "split-words"
    (check-equal?
     (split-words "XML HTTP request")
     (list (word (list "xml")) (word (list "http")) (word (list "request"))))
    (check-equal?
     (split-words "new customer ID")
     (list (word (list "new")) (word (list "customer")) (word (list "id"))))
    (check-equal?
     (split-words "inner stopwatch")
     (list (word (list "inner")) (word (list "stopwatch"))))
    (check-equal?
     (split-words "supports IPv6 on iOS?")
     (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
    (check-equal?
     (split-words "YouTube importer")
     (list (word (list "you" "tube")) (word (list "importer"))))
    (check-equal?
     (split-words "check nonempty")
     (list (word (list "check")) (word (list "nonempty"))))
    (check-equal?
     (split-words "check non-empty")
     (list (word (list "check")) (word (list "non" "empty"))))
    (check-equal?
     (split-words "many   spaces")
     (list (word (list "many")) (word (list "spaces"))))
    (check-equal?
     (split-words-if-string (vector (word (list "you" "tube")) (word (list "importer"))))
     (list (word (list "you" "tube")) (word (list "importer"))))))


(define (upper-camel-case text)
  (string->immutable-string
   (string-join (map word-upper-camel-case (split-words-if-string text)) "")))


(define (lower-camel-case text)
  (match-define (cons first-word rest-words) (split-words-if-string text))
  (define word-strings
    (cons (word-lower-camel-case first-word) (map word-upper-camel-case rest-words)))
  (string->immutable-string (string-join word-strings "")))


(define (upper-snake-case text)
  (string->immutable-string
   (string-join (map word-upper-camel-case (split-words-if-string text)) "_")))

(define (screaming-snake-case text)
  (string->immutable-string
   (string-join (map word-screaming-case (split-words-if-string text)) "_")))


(define (lower-snake-case text)
  (string->immutable-string
   (string-join (map word-lowercase (split-words-if-string text)) "_")))


(define (upper-kebab-case text)
  (string->immutable-string
   (string-join (map word-upper-camel-case (split-words-if-string text)) "-")))


(define (screaming-kebab-case text)
  (string->immutable-string (string-join (map word-screaming-case (split-words-if-string text)) "-")))


(define (lower-kebab-case text)
  (string->immutable-string (string-join (map word-lowercase (split-words-if-string text)) "-")))


(module+ test
  (test-case "upper-camel-case"
    (check-equal? (upper-camel-case "XML HTTP request") "XmlHttpRequest")
    (check-equal? (upper-camel-case "new customer ID") "NewCustomerId")
    (check-equal? (upper-camel-case "inner stopwatch") "InnerStopwatch")
    (check-equal? (upper-camel-case "supports IPv6 on iOS?") "SupportsIpv6OnIos")
    (check-equal? (upper-camel-case "YouTube importer") "YouTubeImporter")
    (check-equal? (upper-camel-case "check nonempty") "CheckNonempty")
    (check-equal? (upper-camel-case "check non-empty") "CheckNonEmpty")
    (check-equal? (upper-camel-case "many   spaces") "ManySpaces"))

  (test-case "lower-camel-case"
    (check-equal? (lower-camel-case "XML HTTP request") "xmlHttpRequest")
    (check-equal? (lower-camel-case "new customer ID") "newCustomerId")
    (check-equal? (lower-camel-case "inner stopwatch") "innerStopwatch")
    (check-equal? (lower-camel-case "supports IPv6 on iOS?") "supportsIpv6OnIos")
    (check-equal? (lower-camel-case "YouTube importer") "youTubeImporter")
    (check-equal? (lower-camel-case "check nonempty") "checkNonempty")
    (check-equal? (lower-camel-case "check non-empty") "checkNonEmpty")
    (check-equal? (lower-camel-case "many   spaces") "manySpaces"))

  (test-case "upper-snake-case"
    (check-equal? (upper-snake-case "XML HTTP request") "Xml_Http_Request")
    (check-equal? (upper-snake-case "new customer ID") "New_Customer_Id")
    (check-equal? (upper-snake-case "inner stopwatch") "Inner_Stopwatch")
    (check-equal? (upper-snake-case "supports IPv6 on iOS?") "Supports_Ipv6_On_Ios")
    (check-equal? (upper-snake-case "YouTube importer") "YouTube_Importer")
    (check-equal? (upper-snake-case "check nonempty") "Check_Nonempty")
    (check-equal? (upper-snake-case "check non-empty") "Check_NonEmpty")
    (check-equal? (upper-snake-case "many   spaces") "Many_Spaces"))

  (test-case "screaming-snake-case"
    (check-equal? (screaming-snake-case "XML HTTP request") "XML_HTTP_REQUEST")
    (check-equal? (screaming-snake-case "new customer ID") "NEW_CUSTOMER_ID")
    (check-equal? (screaming-snake-case "inner stopwatch") "INNER_STOPWATCH")
    (check-equal? (screaming-snake-case "supports IPv6 on iOS?") "SUPPORTS_IPV6_ON_IOS")
    (check-equal? (screaming-snake-case "YouTube importer") "YOUTUBE_IMPORTER")
    (check-equal? (screaming-snake-case "check nonempty") "CHECK_NONEMPTY")
    (check-equal? (screaming-snake-case "check non-empty") "CHECK_NONEMPTY")
    (check-equal? (screaming-snake-case "many   spaces") "MANY_SPACES"))

  (test-case "lower-snake-case"
    (check-equal? (lower-snake-case "XML HTTP request") "xml_http_request")
    (check-equal? (lower-snake-case "new customer ID") "new_customer_id")
    (check-equal? (lower-snake-case "inner stopwatch") "inner_stopwatch")
    (check-equal? (lower-snake-case "supports IPv6 on iOS?") "supports_ipv6_on_ios")
    (check-equal? (lower-snake-case "YouTube importer") "youtube_importer")
    (check-equal? (lower-snake-case "check nonempty") "check_nonempty")
    (check-equal? (lower-snake-case "check non-empty") "check_nonempty")
    (check-equal? (lower-snake-case "many   spaces") "many_spaces"))

  (test-case "upper-kebab-case"
    (check-equal? (upper-kebab-case "XML HTTP request") "Xml-Http-Request")
    (check-equal? (upper-kebab-case "new customer ID") "New-Customer-Id")
    (check-equal? (upper-kebab-case "inner stopwatch") "Inner-Stopwatch")
    (check-equal? (upper-kebab-case "supports IPv6 on iOS?") "Supports-Ipv6-On-Ios")
    (check-equal? (upper-kebab-case "YouTube importer") "YouTube-Importer")
    (check-equal? (upper-kebab-case "check nonempty") "Check-Nonempty")
    (check-equal? (upper-kebab-case "check non-empty") "Check-NonEmpty")
    (check-equal? (upper-kebab-case "many   spaces") "Many-Spaces"))

  (test-case "screaming-kebab-case"
    (check-equal? (screaming-kebab-case "XML HTTP request") "XML-HTTP-REQUEST")
    (check-equal? (screaming-kebab-case "new customer ID") "NEW-CUSTOMER-ID")
    (check-equal? (screaming-kebab-case "inner stopwatch") "INNER-STOPWATCH")
    (check-equal? (screaming-kebab-case "supports IPv6 on iOS?") "SUPPORTS-IPV6-ON-IOS")
    (check-equal? (screaming-kebab-case "YouTube importer") "YOUTUBE-IMPORTER")
    (check-equal? (screaming-kebab-case "check nonempty") "CHECK-NONEMPTY")
    (check-equal? (screaming-kebab-case "check non-empty") "CHECK-NONEMPTY")
    (check-equal? (screaming-kebab-case "many   spaces") "MANY-SPACES"))

  (test-case "lower-kebab-case"
    (check-equal? (lower-kebab-case "XML HTTP request") "xml-http-request")
    (check-equal? (lower-kebab-case "new customer ID") "new-customer-id")
    (check-equal? (lower-kebab-case "inner stopwatch") "inner-stopwatch")
    (check-equal? (lower-kebab-case "supports IPv6 on iOS?") "supports-ipv6-on-ios")
    (check-equal? (lower-kebab-case "YouTube importer") "youtube-importer")
    (check-equal? (lower-kebab-case "check nonempty") "check-nonempty")
    (check-equal? (lower-kebab-case "check non-empty") "check-nonempty")
    (check-equal? (lower-kebab-case "many   spaces") "many-spaces")))


(define (camel-case-words name)
  (for/list ([p (in-list (camel-case-parts name))])
    (word (list p))))


(define (camel-case-parts name)
  (match name
    [(pregexp #px"(^[[:alnum:]][[:lower:][:digit:]]+)([[:upper:]].*$)" (list _ first-part rest-name))
     (cons (string-downcase first-part) (camel-case-parts rest-name))]
    [_ (list (string-downcase name))]))


(define (snake-case-words name)
  (map (λ (w) (word (camel-case-parts w))) (string-split name "_")))


(define (kebab-case-words name)
  (map (λ (w) (word (camel-case-parts w))) (string-split name "-")))


(module+ test
  (test-case "camel-case-words"
    (test-case "upper camel case"
      (check-equal?
       (camel-case-words "XmlHttpRequest")
       (list (word (list "xml")) (word (list "http")) (word (list "request"))))
      (check-equal?
       (camel-case-words "NewCustomerId")
       (list (word (list "new")) (word (list "customer")) (word (list "id"))))
      (check-equal?
       (camel-case-words "InnerStopwatch")
       (list (word (list "inner")) (word (list "stopwatch"))))
      (check-equal?
       (camel-case-words "SupportsIpv6OnIos")
       (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
      (check-equal?
       (camel-case-words "YouTubeImporter")
       (list (word (list "you")) (word (list "tube")) (word (list "importer"))))
      (check-equal?
       (camel-case-words "CheckNonempty")
       (list (word (list "check")) (word (list "nonempty"))))
      (check-equal?
       (camel-case-words "CheckNonEmpty")
       (list (word (list "check")) (word (list "non")) (word (list "empty")))))

    (test-case "lower camel case"
      (check-equal?
       (camel-case-words "xmlHttpRequest")
       (list (word (list "xml")) (word (list "http")) (word (list "request"))))
      (check-equal?
       (camel-case-words "newCustomerId")
       (list (word (list "new")) (word (list "customer")) (word (list "id"))))
      (check-equal?
       (camel-case-words "innerStopwatch")
       (list (word (list "inner")) (word (list "stopwatch"))))
      (check-equal?
       (camel-case-words "supportsIpv6OnIos")
       (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
      (check-equal?
       (camel-case-words "youTubeImporter")
       (list (word (list "you")) (word (list "tube")) (word (list "importer"))))
      (check-equal?
       (camel-case-words "checkNonempty")
       (list (word (list "check")) (word (list "nonempty"))))
      (check-equal?
       (camel-case-words "checkNonEmpty")
       (list (word (list "check")) (word (list "non")) (word (list "empty"))))))

  (test-case "snake-case-words"
    (test-case "upper snake case"
      (check-equal?
       (snake-case-words "Xml_Http_Request")
       (list (word (list "xml")) (word (list "http")) (word (list "request"))))
      (check-equal?
       (snake-case-words "New_Customer_Id")
       (list (word (list "new")) (word (list "customer")) (word (list "id"))))
      (check-equal?
       (snake-case-words "Inner_Stopwatch")
       (list (word (list "inner")) (word (list "stopwatch"))))
      (check-equal?
       (snake-case-words "Supports_Ipv6_On_Ios")
       (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
      (check-equal?
       (snake-case-words "YouTube_Importer")
       (list (word (list "you" "tube")) (word (list "importer"))))
      (check-equal?
       (snake-case-words "Check_Nonempty")
       (list (word (list "check")) (word (list "nonempty"))))
      (check-equal?
       (snake-case-words "Check_NonEmpty")
       (list (word (list "check")) (word (list "non" "empty")))))

    (test-case "lower snake case"
      (check-equal?
       (snake-case-words "xml_http_request")
       (list (word (list "xml")) (word (list "http")) (word (list "request"))))
      (check-equal?
       (snake-case-words "new_customer_id")
       (list (word (list "new")) (word (list "customer")) (word (list "id"))))
      (check-equal?
       (snake-case-words "inner_stopwatch")
       (list (word (list "inner")) (word (list "stopwatch"))))
      (check-equal?
       (snake-case-words "supports_ipv6_on_ios")
       (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
      (check-equal?
       (snake-case-words "youtube_importer")
       (list (word (list "youtube")) (word (list "importer"))))
      (check-equal?
       (snake-case-words "check_nonempty")
       (list (word (list "check")) (word (list "nonempty")))))

    (test-case "screaming snake case"
      (check-equal?
       (snake-case-words "XML_HTTP_REQUEST")
       (list (word (list "xml")) (word (list "http")) (word (list "request"))))
      (check-equal?
       (snake-case-words "NEW_CUSTOMER_ID")
       (list (word (list "new")) (word (list "customer")) (word (list "id"))))
      (check-equal?
       (snake-case-words "INNER_STOPWATCH")
       (list (word (list "inner")) (word (list "stopwatch"))))
      (check-equal?
       (snake-case-words "SUPPORTS_IPV6_ON_IOS")
       (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
      (check-equal?
       (snake-case-words "YOUTUBE_IMPORTER")
       (list (word (list "youtube")) (word (list "importer"))))
      (check-equal?
       (snake-case-words "CHECK_NONEMPTY")
       (list (word (list "check")) (word (list "nonempty"))))))

  (test-case "kebab-case-words"
    (test-case "upper kebab case"
      (check-equal?
       (kebab-case-words "Xml-Http-Request")
       (list (word (list "xml")) (word (list "http")) (word (list "request"))))
      (check-equal?
       (kebab-case-words "New-Customer-Id")
       (list (word (list "new")) (word (list "customer")) (word (list "id"))))
      (check-equal?
       (kebab-case-words "Inner-Stopwatch")
       (list (word (list "inner")) (word (list "stopwatch"))))
      (check-equal?
       (kebab-case-words "Supports-Ipv6-On-Ios")
       (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
      (check-equal?
       (kebab-case-words "YouTube-Importer")
       (list (word (list "you" "tube")) (word (list "importer"))))
      (check-equal?
       (kebab-case-words "Check-Nonempty")
       (list (word (list "check")) (word (list "nonempty"))))
      (check-equal?
       (kebab-case-words "Check-NonEmpty")
       (list (word (list "check")) (word (list "non" "empty")))))

    (test-case "lower kebab case"
      (check-equal?
       (kebab-case-words "xml-http-request")
       (list (word (list "xml")) (word (list "http")) (word (list "request"))))
      (check-equal?
       (kebab-case-words "new-customer-id")
       (list (word (list "new")) (word (list "customer")) (word (list "id"))))
      (check-equal?
       (kebab-case-words "inner-stopwatch")
       (list (word (list "inner")) (word (list "stopwatch"))))
      (check-equal?
       (kebab-case-words "supports-ipv6-on-ios")
       (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
      (check-equal?
       (kebab-case-words "youtube-importer")
       (list (word (list "youtube")) (word (list "importer"))))
      (check-equal?
       (kebab-case-words "check-nonempty")
       (list (word (list "check")) (word (list "nonempty")))))

    (test-case "screaming kebab case"
      (check-equal?
       (kebab-case-words "XML-HTTP-REQUEST")
       (list (word (list "xml")) (word (list "http")) (word (list "request"))))
      (check-equal?
       (kebab-case-words "NEW-CUSTOMER-ID")
       (list (word (list "new")) (word (list "customer")) (word (list "id"))))
      (check-equal?
       (kebab-case-words "INNER-STOPWATCH")
       (list (word (list "inner")) (word (list "stopwatch"))))
      (check-equal?
       (kebab-case-words "SUPPORTS-IPV6-ON-IOS")
       (list (word (list "supports")) (word (list "ipv6")) (word (list "on")) (word (list "ios"))))
      (check-equal?
       (kebab-case-words "YOUTUBE-IMPORTER")
       (list (word (list "youtube")) (word (list "importer"))))
      (check-equal?
       (kebab-case-words "CHECK-NONEMPTY")
       (list (word (list "check")) (word (list "nonempty")))))))
