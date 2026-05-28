#lang racket

(require (rename-in json [write-json racket-write-json])
         racket/cmdline
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         net/uri-codec)

(define max-codepoint #x10FFFF)
(define columns 256)
(define rows (ceiling (/ (+ max-codepoint 1) columns)))

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
  (make-directory* "public/data")
  (make-directory* "public/fonts"))

(define (write-json-file path value)
  (call-with-output-file path
    #:exists 'replace
    (lambda (out)
      (racket-write-json value out))))

(define (build)
  (ensure-output!)
  (write-json-file "public/data/map.json"
                   (hash 'maxCodepoint max-codepoint
                         'columns columns
                         'rows rows
                         'planes planes
                         'ranges ranges
                         'blocks seed-blocks))
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
     ["serve" (serve)]
     [_ (error 'unicode-map "unknown command: ~a" command)])))
