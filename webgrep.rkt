#lang at-exp racket

(require net/url
         racket/contract/base
         racket/contract/region
         racket/format
         racket/function
         racket/hash
         racket/list
         racket/match
         racket/math
         racket/port
         racket/promise
         racket/set
         racket/string)

(define (url->content! url-string)
  (with-handlers ([url-exception? (thunk* "")]
                  [exn:fail? (thunk* "")])
    (call/input-url (string->url url-string)
                    get-pure-port
                    port->string)))

(struct page (url depth content-matches children) #:transparent)

;; url? (listof regexp?) regexp? -> (promise/c page?)
(define (launch-page-search! url depth content-pats url-pat)
  (delay/thread
   (define content (url->content! url))
   (define content-matches (flatten
                            (map (λ (pat) (regexp-match* pat content))
                                 content-pats)))
   (define children (urls-matching url-pat content #:base url))
   (page url depth content-matches children)))

(define (urls-matching pat content #:base base-url)
  (define url-pat
    (pregexp
     #<<HERE
a +href *= *["']([^#][^ "]*)["']
HERE
     ))
  (define all-urls (regexp-match* url-pat content #:match-select cadr))
  (define all-urls/normalized
    (map (normalize-relative-url base-url) all-urls))
  (remove-duplicates
   (filter (λ (url) (regexp-match? pat url)) all-urls/normalized)))

(module+ test
  (require ruinit)
  (test-begin
    #:name urls-matching
    (ignore (define content
              #<<HERE
returning parenthesized sub-patterns.  It can be given as a &lsquo;selector&rsquo;
function which chooses an item from a list, or it can choose a list of
items.  For example, you can use <span class="RktSym"><a href="pairs.html#%28def._%28%28quote._~23~25kernel%29._cdr%29%29" class="RktValLink" data-pltdoc="x">cdr</a></span> to get a list of lists
of parenthesized sub-patterns matches, or <span class="RktSym"><a href="values.html#%28def._%28%28quote._~23~25kernel%29._values%29%29" class="RktValLink" data-pltdoc="x">values</a></span> (as an
identity function) to get the full matches as well.  (Note that the
selector must choose an element of its input list or a list of
elements, but it must not inspect its input as they can be either a
<a href="/foo.html">foo</a>
list of strings or a list of position pairs.  Furthermore, the
selector must be consistent in its choice(s).)</p><p><div class="SIntrapara">Examples:</div><div class="SIntrapara"><blockquote class="SCodeFlow"><table cellspacing="0" cellpadding="0" class="RktBlk"><tr><td><span class="stt">&gt; </span><span class="RktPn">(</span><span class="RktSym"><a href="#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-match%2A%29%29" class="RktValLink" data-pltdoc="x">regexp-match*</a></span><span class="hspace">&nbsp;</span><span class="RktVal">#rx"x(.)"</span><span class="hspace">&nbsp;</span><span class="RktVal">"12x4x6"</span><span class="hspace">&nbsp;</span><span class="RktPn">#:match-select</span><span class="hspace">&nbsp;</span><span class="RktSym"><a href="pairs.html#%28def._%28%28quote._~23~25kernel%29._cadr%29%29" class="RktValLink" data-pltdoc="x">cadr</a></span><span class="RktPn">)</span></td></tr><tr><td><p><span class="RktRes">'("4" "6")</span></p></td></tr><tr><td><span class="stt">&gt; </span><span class="RktPn">(</span><span class="RktSym"><a href="#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-match%2A%29%29" class="RktValLink" data-pltdoc="x">regexp-match*</a></span><span class="hspace">&nbsp;</span><span class="RktVal">#rx"x(.)"</span><span class="hspace">&nbsp;</span><span class="RktVal">"12x4x6"</span><span class="hspace">&nbsp;</span><span class="RktPn">#:match-select</span><span class="hspace">&nbsp;</span><span class="RktSym"><a href="values.html#%28def._%28%28quote._~23~25kernel%29._values%29%29" class="RktValLink" data-pltdoc="x">values</a></span><span class="RktPn">)</span></td></tr><tr><td><p><span class="RktRes">'(("x4" "4") ("x6" "6"))</span></p></td></tr></table></blockquote></div></p><p>In addition, specifying <span class="RktVar">gap-select</span> as a non-<span class="RktVal">#f</span> value
will make the result an interleaved list of the matches as well as the
separators between them matches, starting and ending with a separator.
HERE
              ))
    (test-match (urls-matching #px".*" content #:base "base")
                (list-no-order
                 "base/foo.html"
                 "pairs.html#%28def._%28%28quote._~23~25kernel%29._cdr%29%29"
                 ;; No internal page links!
                 ;; "#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-match%2A%29%29"
                 "pairs.html#%28def._%28%28quote._~23~25kernel%29._cadr%29%29"
                 ;; "#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-match%2A%29%29"
                 "values.html#%28def._%28%28quote._~23~25kernel%29._values%29%29"))
    (test-match (urls-matching #px"value" content #:base "base")
                (list-no-order "values.html#%28def._%28%28quote._~23~25kernel%29._values%29%29"))))


(define ((normalize-relative-url base-url) url)
  (if (or (string-prefix? url "/")
          (not (regexp-match? #px"http(s)?://" url)))
      (url->string (combine-url/relative (string->url base-url) url))
      url))

;; results? := (hash/c url? (listof string?))
;; childmap? := (hash/c url? (listof url?))
;; todo-list? := (listof todo?)

(struct todo (url depth) #:transparent)

;; (listof (promise/c page?))
;; ->
;; (values todo-list?
;;         childmap?
;;         results?)
(define (collect-search-results finished-links
                                #:print-progress? [print-progress? #f])
  (define-values {todos childmap results}
    (for/lists {todos childmap results}
               ([link finished-links])
      (match-define (page url depth content-matches children) (force link))
      (match* {print-progress? (length content-matches)}
        [{'url (not 0)} (displayln url)]
        [{'all (not 0)} (displayln
                         @~a{
                             @url
                             ==================================================
                             @~v[content-matches]

                             })]
        [{_ _} (void)])
      (values (map (λ (child) (todo child (add1 depth))) children)
              (hash url children)
              (hash url content-matches))))
  (values (flatten todos)
          (apply hash-union childmap
                 #:combine (λ (a b) a))
          (apply hash-union results
                 #:combine (λ (a b) (remove-duplicates
                                     (append a b))))))

(module+ test
  (require (for-syntax racket/base))
  (define-test-syntax (test-search-results promises
                                           todos-pat
                                           children-pat
                                           results-pat)
    #'(let-values ([{todos children results} (collect-search-results promises)])
        (and/test (test-match todos todos-pat)
                  (test-match children children-pat)
                  (test-match results results-pat))))
  (test-begin
    #:name collect-search-results
    (test-search-results (list (delay (page "u" 1 '() '())))
                         '()
                         (== (hash "u" '()))
                         (== (hash "u" '())))
    (test-search-results (list (delay (page "u" 1 '("foo") '())))
                         '()
                         (== (hash "u" '()))
                         (== (hash "u" '("foo"))))
    (test-search-results (list (delay (page "u" 1 '("foo") '("a"))))
                         (list (todo "a" 2))
                         (== (hash "u" '("a")))
                         (== (hash "u" '("foo"))))
    (test-search-results (list (delay (page "u" 1 '("foo") '("a" "b"))))
                         (list (todo "a" 2) (todo "b" 2))
                         (== (hash "u" '("a" "b")))
                         (== (hash "u" '("foo"))))))


(define url-string? string?)

(define ((hash-has-key/c k) h)
  (hash-has-key? h k))

(define ((todo-url-not-in/c set) t)
  (not (member (todo-url t) set)))

;; url? natural? (hash/c url? any/c) -> (listof todo?)
;; Produce a list of the unseen children of URL
(define/contract (unseen-children url depth urls-seen)
  (->i ([url url-string? ]
        [depth natural?]
        [urls-seen (url) (and/c (hash/c url-string? (listof url-string?))
                                (hash-has-key/c url))])
       [result (url urls-seen)
               (listof (and/c todo?
                              (todo-url-not-in/c (hash-keys urls-seen))))])

  (define children/all (hash-ref urls-seen url))
  (define seen-urls (hash-keys urls-seen))
  (define children/unseen (set-subtract children/all seen-urls))
  (map (λ (unseen-child) (todo unseen-child (add1 depth)))
       children/unseen))

(module+ test
  (test-begin
    #:name unseen-children
    (test-match (unseen-children "a" 2 (hash "a" '("b" "c")
                                             "b" '("d" "e")))
                (list-no-order (todo "c" 3)))
    (test-match (unseen-children "b" 2 (hash "a" '("b" "c")
                                             "b" '("d" "e")))
                (list-no-order (todo "d" 3) (todo "e" 3)))
    (test-match (unseen-children "z" 2 (hash "a" '("b" "c")
                                             "b" '("d" "e")
                                             "z" '("a" "b")))
                '())))

;; url? (listof regexp?) (listof regexp?) natural? #:thread-limit natural?
;; ->
;; results?
;;
;; Return a mapping from url to content matches
(define (find-matches-in! base-url content-pats url-pat depth
                          #:thread-limit [thread-limit 50]
                          #:print-progress? [print-progress? #f])
  (let loop ([links-todo (list (todo base-url 0))]
             [links-waiting empty]
             [urls->children (hash)]
             [search-results (hash)]
             [searched (set)])
    (cond [(or (>= (length links-waiting) thread-limit)
               (and (empty? links-todo)
                    (not (empty? links-waiting))))
           (apply sync links-waiting)
           (define-values {links-still-waiting links-done}
             (partition promise-running? links-waiting))
           (define-values {new-todos new-urls->children new-matches}
             (collect-search-results links-done
                                     #:print-progress? print-progress?))
           (loop (append new-todos links-todo)
                 links-still-waiting
                 (hash-union urls->children new-urls->children
                             #:combine (λ (a b) a))
                 (hash-union search-results new-matches
                             #:combine (λ (a b) (remove-duplicates
                                                 (append a b))))
                 searched)]
          [else
           (match links-todo
             ['() search-results]
             [(cons (todo url url-depth) todos-rest)
              #:when (hash-has-key? urls->children url)
              (define unseen-links-todo
                (unseen-children url url-depth urls->children))
              (loop (append unseen-links-todo (rest links-todo))
                    links-waiting
                    urls->children
                    search-results
                    searched)]
             [(cons (todo url url-depth) todos-rest)
              #:when (set-member? searched url)
              (loop todos-rest
                    links-waiting
                    urls->children
                    search-results
                    searched)]
             [(cons (todo url url-depth) todos-rest)
              (loop todos-rest
                    (if (<= url-depth depth)
                        (cons (launch-page-search! url url-depth
                                                   content-pats url-pat)
                              links-waiting)
                        links-waiting)
                    urls->children
                    search-results
                    (set-add searched url))])])))

(module+ main
  (require rscript/cmdline)
  (match-define (cons flags args)
    (command-line/declarative
     #:multi
     [("-c" "--content-pat")
      'content-pats
      "Patterns to find in page contents."
      #:collect ["regexp" cons empty]]
     #:once-each
     [("-u" "--url-pat")
      'url-pat
      ("Pattern of links to find in page contents."
       "Default: any link")
      #:collect ["regexp" take-latest ".*"]]
     [("-r" "--root")
      'root-url
      "Url at which to root search."
      #:mandatory
      #:collect ["url" take-latest #f]]
     [("-d" "--depth")
      'depth
      ("Depth of linked pages to traverse."
       "Default: 2")
      #:collect ["natural" take-latest "2"]]
     [("-l" "--links-only")
      'links-only
      ("Only show links of pages with matching content,"
       "not content matches as well.")
      #:record]
     [("-j" "--thread-limit")
      'thread-limit
      ("Maximum number of threads to search in parallel."
       "Default: 50")
      #:collect ["natural" take-latest "50"]]))
  (file-stream-buffer-mode (current-output-port) 'line)
  (define full-line-content-pats
    (map (λ (pat) @~a{(?m:^.*@|pat|.*$)})
         (hash-ref flags 'content-pats)))
  (void
   (find-matches-in!
    (hash-ref flags 'root-url)
    full-line-content-pats
    (pregexp (hash-ref flags 'url-pat))
    (string->number (hash-ref flags 'depth))
    #:thread-limit (string->number (hash-ref flags 'thread-limit))
    #:print-progress? (if (hash-ref flags 'links-only)
                          'url
                          'all))))
