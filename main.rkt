#lang racket

(require (rename-in json [write-json racket-write-json])
         racket/cmdline
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         net/url
         net/uri-codec)

(define max-codepoint #x10FFFF)
(define columns 256)
(define rows (ceiling (/ (+ max-codepoint 1) columns)))
(define ucd-base-url "https://www.unicode.org/Public/UCD/latest/ucd/")
(define ucd-files '("Blocks.txt" "Scripts.txt" "UnicodeData.txt"))

(define (truthy-env? name)
  (match (getenv name)
    [(or "1" "true" "yes" "on") #t]
    [_ #f]))

(define planes
  (for/list ([plane (in-range 17)])
    (hash 'number plane
          'start (* plane #x10000)
          'end (+ (* plane #x10000) #xFFFF)
          'name (match plane
                  [0 "Basic Multilingual Plane"]
                  [1 "Supplementary Multilingual Plane"]
                  [2 "Supplementary Ideographic Plane"]
                  [3 "Tertiary Ideographic Plane"]
                  [14 "Supplementary Special-purpose Plane"]
                  [15 "Supplementary Private Use Area-A"]
                  [16 "Supplementary Private Use Area-B"]
                  [_ (format "Plane ~a" plane)]))))

(define ranges
  (list
   (hash 'start #x000000 'end #x00001F 'kind "control" 'label "C0 controls")
   (hash 'start #x00007F 'end #x00009F 'kind "control" 'label "C1 controls")
   (hash 'start #x00D800 'end #x00DFFF 'kind "surrogate" 'label "Surrogates")
   (hash 'start #x00E000 'end #x00F8FF 'kind "private" 'label "Private Use Area")
   (hash 'start #x0F0000 'end #x0FFFFD 'kind "private" 'label "Supplementary Private Use Area-A")
   (hash 'start #x100000 'end #x10FFFD 'kind "private" 'label "Supplementary Private Use Area-B")))

(define seed-blocks
  (list
   (hash 'start #x0000 'end #x007F 'name "Basic Latin")
   (hash 'start #x0080 'end #x00FF 'name "Latin-1 Supplement")
   (hash 'start #x0100 'end #x017F 'name "Latin Extended-A")
   (hash 'start #x0370 'end #x03FF 'name "Greek and Coptic")
   (hash 'start #x0400 'end #x04FF 'name "Cyrillic")
   (hash 'start #x0590 'end #x05FF 'name "Hebrew")
   (hash 'start #x0600 'end #x06FF 'name "Arabic")
   (hash 'start #x0900 'end #x097F 'name "Devanagari")
   (hash 'start #x3040 'end #x309F 'name "Hiragana")
   (hash 'start #x30A0 'end #x30FF 'name "Katakana")
   (hash 'start #x4E00 'end #x9FFF 'name "CJK Unified Ideographs")
   (hash 'start #xAC00 'end #xD7AF 'name "Hangul Syllables")
   (hash 'start #x1F300 'end #x1FAFF 'name "Emoji and Symbols")))

(define (ensure-output!)
  (make-directory* "data/ucd")
  (make-directory* "public/data")
  (make-directory* "public/fonts"))

(define (write-json-file path value)
  (call-with-output-file path
    #:exists 'replace
    (lambda (out)
      (racket-write-json value out))))

(define (strip-comment line)
  (string-trim (regexp-replace #px"#.*$" line "")))

(define (hex->number text)
  (string->number text 16))

(define (parse-range text)
  (match (string-split (string-trim text) "..")
    [(list one) (define cp (hex->number one)) (values cp cp)]
    [(list start end) (values (hex->number start) (hex->number end))]
    [_ (error 'parse-range "bad range: ~a" text)]))

(define (parse-ranged-file path key-name)
  (call-with-input-file path
    (lambda (in)
      (for/list ([line (in-lines in)]
                 #:do [(define clean (strip-comment line))]
                 #:unless (string=? clean ""))
        (match (map string-trim (string-split clean ";"))
          [(list range label)
           (define-values (start end) (parse-range range))
           (hash 'start start 'end end key-name label)]
          [_ (error 'parse-ranged-file "bad line in ~a: ~a" path line)])))))

(define (unicode-range-name raw)
  (regexp-replace #px", First>$" raw ">"))

(define (parse-unicode-data path)
  (define ranges '())
  (define pending #f)
  (call-with-input-file path
    (lambda (in)
      (for ([line (in-lines in)])
        (define fields (string-split line ";"))
        (match fields
          [(list cp-text name category _ ...)
           (define cp (hex->number cp-text))
           (cond
             [(regexp-match? #px", First>$" name)
              (set! pending (list cp (unicode-range-name name) category))]
             [(and pending (regexp-match? #px", Last>$" name))
              (match-define (list start range-name range-category) pending)
              (set! ranges (cons (hash 'start start
                                       'end cp
                                       'name range-name
                                       'category range-category)
                                 ranges))
              (set! pending #f)]
             [else
              (set! ranges (cons (hash 'start cp
                                       'end cp
                                       'name name
                                       'category category)
                                 ranges))])]
          [_ (error 'parse-unicode-data "bad line: ~a" line)]))))
  (reverse ranges))

(define (ucd-path file)
  (build-path "data" "ucd" file))

(define (have-ucd?)
  (for/and ([file (in-list ucd-files)])
    (file-exists? (ucd-path file))))

(define (fetch-file file)
  (define target (ucd-path file))
  (define url (string->url (string-append ucd-base-url file)))
  (printf "fetching ~a\n" url)
  (define in (get-pure-port url))
  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file target
        #:exists 'replace
        (lambda (out)
          (copy-port in out))))
    (lambda ()
      (close-input-port in))))

(define (fetch)
  (ensure-output!)
  (for ([file (in-list ucd-files)])
    (fetch-file file)))

(define (load-ucd)
  (if (have-ucd?)
      (hash 'blocks (parse-ranged-file (ucd-path "Blocks.txt") 'name)
            'scripts (parse-ranged-file (ucd-path "Scripts.txt") 'script)
            'assignedRanges (parse-unicode-data (ucd-path "UnicodeData.txt"))
            'source "data/ucd")
      (hash 'blocks seed-blocks
            'scripts '()
            'assignedRanges '()
            'source "seed")))

(define (command-lines exe . args)
  (if exe
      (string-split
       (with-output-to-string
         (lambda ()
           (with-handlers ([exn:fail? (lambda (_err) #f)])
             (apply system* exe args))))
       "\n"
       #:trim? #t)
      '()))

(define (parse-fontconfig-charset text)
  (for/list ([token (in-list (string-split text))]
             #:do [(define pieces (string-split token "-"))]
             #:when (or (= 1 (length pieces)) (= 2 (length pieces))))
    (match pieces
      [(list one)
       (define cp (hex->number one))
       (and cp (cons cp cp))]
      [(list start end)
       (define start-cp (hex->number start))
       (define end-cp (hex->number end))
       (and start-cp end-cp (cons start-cp end-cp))])))

(define (merge-ranges raw-ranges)
  (define ranges
    (sort (filter values raw-ranges) < #:key car))
  (define merged '())
  (for ([range (in-list ranges)])
    (match merged
      ['() (set! merged (list range))]
      [(cons current rest)
       (if (<= (car range) (add1 (cdr current)))
           (set! merged (cons (cons (car current) (max (cdr current) (cdr range))) rest))
           (set! merged (cons range merged)))]))
  (reverse merged))

(define (system-font-coverage-ranges)
  (define fc-list (find-executable-path "fc-list"))
  (define fc-query (find-executable-path "fc-query"))
  (cond
    [(not (and fc-list fc-query)) '()]
    [else
     (define font-files
       (remove-duplicates
        (filter (lambda (file)
                  (and (not (string=? file ""))
                       (not (regexp-match? #px"LastResort" file))))
                (command-lines fc-list "-f" "%{file}\n"))))
     (define raw-ranges
       (append*
        (for/list ([font-file (in-list font-files)])
          (parse-fontconfig-charset
           (string-join
            (command-lines fc-query "--format" "%{charset}\n" font-file)
            " ")))))
     (for/list ([range (in-list (merge-ranges raw-ranges))])
       (hash 'start (car range) 'end (cdr range)))]))

(define (build)
  (ensure-output!)
  (define ucd (load-ucd))
  (define include-font-coverage? (truthy-env? "UNICODE_MAP_FONT_COVERAGE"))
  (define font-coverage-ranges
    (if include-font-coverage?
        (system-font-coverage-ranges)
        '()))
  (write-json-file "public/data/map.json"
                   (hash 'maxCodepoint max-codepoint
                         'columns columns
                         'rows rows
                         'planes planes
                         'ranges ranges
                         'blocks (hash-ref ucd 'blocks)
                         'scripts (hash-ref ucd 'scripts)
                         'assignedRanges (hash-ref ucd 'assignedRanges)
                         'fontCoverageRanges font-coverage-ranges
                         'fontCoverageSource (if (null? font-coverage-ranges)
                                                 (if include-font-coverage?
                                                     "none"
                                                     "disabled")
                                                 "fontconfig without LastResort")
                         'source (hash-ref ucd 'source)))
  (displayln "wrote public/data/map.json"))

(define (content-type path)
  (match (path-get-extension path)
    [#".html" "text/html; charset=utf-8"]
    [#".css" "text/css; charset=utf-8"]
    [#".js" "text/javascript; charset=utf-8"]
    [#".json" "application/json; charset=utf-8"]
    [#".ttf" "font/ttf"]
    [#".otf" "font/otf"]
    [#".woff" "font/woff"]
    [#".woff2" "font/woff2"]
    [_ "application/octet-stream"]))

(define (send-response out status type body)
  (fprintf out "HTTP/1.1 ~a\r\n" status)
  (fprintf out "Content-Type: ~a\r\n" type)
  (fprintf out "Content-Length: ~a\r\n" (bytes-length body))
  (fprintf out "Connection: close\r\n\r\n")
  (write-bytes body out)
  (flush-output out))

(define (read-headers in)
  (let loop ()
    (define line (read-line in 'any))
    (unless (or (eof-object? line) (string=? line ""))
      (loop))))

(define (request-path request-line)
  (match (regexp-match #px"^GET ([^ ]+) HTTP/" request-line)
    [(list _ raw)
     (define without-query (car (string-split raw "?")))
     (define decoded (uri-decode without-query))
     (if (string=? decoded "/") "/index.html" decoded)]
    [_ #f]))

(define (safe-public-path url-path)
  (define pieces
    (filter (lambda (piece)
              (and (not (string=? piece ""))
                   (not (string=? piece "."))
                   (not (string=? piece ".."))))
            (string-split url-path "/")))
  (apply build-path "public" pieces))

(define (handle-client in out)
  (with-handlers ([exn:fail? (lambda (_err)
                               (send-response out "500 Internal Server Error"
                                              "text/plain; charset=utf-8"
                                              #"server error"))])
    (define line (read-line in 'any))
    (when (and (string? line) (not (string=? line "")))
      (read-headers in)
      (define url-path (request-path line))
      (define file-path (and url-path (safe-public-path url-path)))
      (cond
        [(and file-path (file-exists? file-path))
         (send-response out "200 OK"
                        (content-type file-path)
                        (file->bytes file-path))]
        [else
         (send-response out "404 Not Found"
                        "text/plain; charset=utf-8"
                        #"not found")]))))

(define (serve)
  (build)
  (define listener (tcp-listen 8080 64 #t "127.0.0.1"))
  (displayln "serving http://127.0.0.1:8080")
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread
     (lambda ()
       (dynamic-wind
         void
         (lambda () (handle-client in out))
         (lambda ()
           (close-input-port in)
           (close-output-port out)))))
    (loop)))

(module+ main
  (command-line
   #:program "unicode-map"
   #:args ([command "build"])
   (match command
     ["build" (build)]
     ["fetch" (fetch)]
     ["serve" (serve)]
     [_ (error 'unicode-map "unknown command: ~a" command)])))
