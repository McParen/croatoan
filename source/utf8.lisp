(in-package :de.anvi.croatoan)

(defun ascii-byte-p (octet)
  "Return t if octet is a single-byte 7-bit ASCII char.

The most significant bit is 0, so the allowed pattern is 0xxx xxxx."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmask  #b10000000)
        (template #b00000000))
    ;; bitwise and the with the bitmask #b11000000 to extract the first two bits.
    ;; check if the first two bits are equal to the template #b10000000.
    (= (logand bitmask octet) template)))

(defun multi-byte-p (octet)
  "Return t if octet is a part of a multi-byte UTF-8 sequence.

The multibyte pattern is 1xxx xxxx.

A multi-byte can be either a lead byte or a trail byte."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmask  #b10000000)
        (template #b10000000))
    ;; bitwise and the with the bitmask #b11000000 to extract the first two bits.
    ;; check if the first two bits are equal to the template #b10000000.
    (= (logand bitmask octet) template)))

(defun lead-byte-p (octet)
  "Return t if octet is one of the leading bytes of an UTF-8 sequence, nil otherwise.

Allowed leading byte patterns are 0xxx xxxx, 110x xxxx, 1110 xxxx and 1111 0xxx."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmasks  (list #b10000000 #b11100000 #b11110000 #b11111000))
        (templates (list #b00000000 #b11000000 #b11100000 #b11110000)))
    (some #'(lambda (a b) (= (logand a octet) b)) bitmasks templates)))

;; http://stackoverflow.com/questions/14380143/matching-binary-patterns-in-c
(defun n-trail-bytes (octet)
  "Take a leading utf-8 byte, return the number of continuation bytes 1-3."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmasks  (list #b10000000 #b11100000 #b11110000 #b11111000))
        (templates (list #b00000000 #b11000000 #b11100000 #b11110000)))
    (loop for i from 0 to 3
       when (= (nth i templates) (logand (nth i bitmasks) octet))
       return i)))

(defun trail-byte-p (octet)
  "Return t if octet is the continuation byte of an UTF-8 sequence.

The allowed continuation byte pattern is 10xx xxxx."
  (assert (typep octet 'integer))
  (assert (<= (integer-length octet) 8))
  (let ((bitmask  #b11000000)
        (template #b10000000))
    ;; bitwise and the with the bitmask #b11000000 to extract the first two bits.
    ;; check if the first two bits are equal to the template #b10000000.
    (= (logand bitmask octet) template)))

#|

Lower   Upper    Binary
bound   bound    Pattern
----------------------------------------------------
0x00000 0x00007F 0xxxxxxx
0x00080 0x0007FF 110xxxxx 10xxxxxx
0x00800 0x00FFFF 1110xxxx 10xxxxxx 10xxxxxx
0x10000 0x10FFFF 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

Unicode code points Range     Encoding Binary value
------------------  --------  --------------------------
U+000000-U+00007f   0xxxxxxx  0xxxxxxx

U+000080-U+0007ff   110yyyxx  00000yyy xxxxxxxx
                    10xxxxxx

U+000800-U+00ffff   1110yyyy  yyyyyyyy xxxxxxxx
                    10yyyyxx
                    10xxxxxx

U+010000-U+10ffff   11110zzz  000zzzzz yyyyyyyy xxxxxxxx
                    10zzyyyy
                    10yyyyxx
                    10xxxxxx
|#

;; two helper functions from the internal to the external format.
;; inside the app, we use lisp chars, we only convert from utf-8 on input and
;; to utf-8 on output.

(defun utf-8-to-unicode (byte-list)
  "Take a list of one to four utf-8 encoded bytes (octets), return a code point.

Since this decoder will only used for reading keyboard input, the sequences are
not checked for illegal bytes.

Since security is not considered, this decoder should not be used for anything
else."
  (let ((b1 (car byte-list)))
    (cond ((ascii-byte-p b1) b1) ; if a single byte, just return it.
          ((multi-byte-p b1)
           (if (lead-byte-p b1)
               (let ((n (n-trail-bytes b1))
                     ;; Content bits we want to extract from each lead byte.
                     (lead-templates (list #b01111111 #b00011111 #b00001111 #b00000111))
                     ;; Content bits we want to extract from each trail byte.
                     (trail-template #b00111111))
                 (if (= n (1- (list-length byte-list)))
                     ;; add lead byte
                     (+ (ash (logand (nth 0 byte-list) (nth n lead-templates)) (* 6 n))
                        ;; and the trail bytes
                        (loop for i from 1 to n sum
                             (ash (logand (nth i byte-list) trail-template) (* 6 (- n i)))))
                     (error "calculated number of bytes doesnt match the length of the byte list")))
               (error "first byte in the list isnt a lead byte"))))))

#|
(defun utf-8-to-unicode (byte-list)
  "Take a list of one to four utf-8 encoded bytes (octets), return the shortest possible unicode code point."
  (let ((b1 (car byte-list)))
    (if (lead-byte-p b1)
        (let ((n (n-trail-bytes b1)))
          (if (= n (1- (list-length byte-list)))

              (case n
                ;; if 0, we have simple ascii, so just get the char.
                (0 b1)
                ;; if 1, we have to convert 110yyyxx 10xxxxxx                   to 00000yyyxxxxxxxx
                (1 (+ (ash (logand (nth 0 byte-list) #b00011111) 6)
                      (ash (logand (nth 1 byte-list) #b00111111) 0)))
                ;; if 2, we have to convert 1110yyyy 10yyyyxx 10xxxxxx          to yyyyyyyyxxxxxxxx
                (2 (+ (ash (logand (nth 0 byte-list) #b00001111) 12)
                      (ash (logand (nth 1 byte-list) #b00111111) 6)
                      (ash (logand (nth 2 byte-list) #b00111111) 0)))
                ;; if 3, we have to convert 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx to 000zzzzz yyyyyyyy xxxxxxxx
                (3 (+ (ash (logand (nth 0 byte-list) #b00000111) 18)
                      (ash (logand (nth 1 byte-list) #b00111111) 12)
                      (ash (logand (nth 2 byte-list) #b00111111) 6)
                      (ash (logand (nth 3 byte-list) #b00111111) 0))))

              (error "calculated number of bytes doesnt match the length of the byte list")))
        (error "first byte in the list isnt a lead byte"))))
|#

#|
;; we also can determine the number of required bytes by the bit-length of the code point.
(n-bits (integer-length int))
         (n-trail-bytes (cond (                   (<= n-bits  7)  0)
                              ((and (> n-bits  7) (<= n-bits 11)) 1)
                              ((and (> n-bits 11) (<= n-bits 16)) 2)
                              ((and (> n-bits 16) (<= n-bits 21)) 3)))
|#

;; we can also just print the char to a string, and output the string.
;; this is easier than utf-8 single char input.
(defun unicode-to-utf-8 (int)
  "Take a unicode code point, return a list of one to four UTF-8 encoded bytes (octets)."
  (assert (<= (integer-length int) 21))
  (let ((n-trail-bytes (cond ((<= #x00000 int #x00007F) 0)
                             ((<= #x00080 int #x0007FF) 1)
                             ((<= #x00800 int #x00FFFF) 2)
                             ((<= #x10000 int #x10FFFF) 3)))
        (lead-templates (list #b00000000 #b11000000 #b11100000 #b11110000))
        (trail-template #b10000000)
        ;; number of content bits in the lead byte.
        (n-lead-bits (list 7 5 4 3))
        ;; number of content bits in the trail byte.
        (n-trail-bits 6)
        ;; list to put the UTF-8 encoded bytes in.
        (byte-list nil))
    (if (= n-trail-bytes 0)
        ;; if we need 0 trail bytes, ist just an ascii single byte.
        (push int byte-list)
        (progn
          ;; if we need more than one byte, first fill the trail bytes with 6 bits each.
          (loop for i from 0 to (1- n-trail-bytes)
             do (push (+ trail-template
                         (ldb (byte n-trail-bits (* i n-trail-bits)) int))
                      byte-list))
          ;; then copy the remaining content bytes to the lead byte.
          (push (+ (nth n-trail-bytes lead-templates)
                   (ldb (byte (nth n-trail-bytes n-lead-bits) (* n-trail-bytes n-trail-bits)) int))
                byte-list)))
    ;; return the list of UTF-8 encoded bytes.
    byte-list))

#|

;; %wgetch returns chars <255 and keycodes >255.
;; %wget_wch returns wide chars <255 and >255, and keycodes >255.
;; it also returns KEY_CODE_YES to designate that a >255 char is a keycode.
(defun get-char- (window &key y x)
  ""
  (let* ((winptr (.winptr window))
         (byte-list nil)
         ;; get the first byte
         (b1 (%wgetch winptr)))
    ;; -1 means "no event", >255 means function key.
    ;; TODO: problem, when we return b1>255, how do we know that it is a function key and not a unicode code point??
    ;; we have to return function keywords _before_ we assemble a code point from utf-8.
    ;; we have to merge get-char and get-event.
    (when (= b1 -1) (return-from get-char- (values b1 nil)))
    (when (> b1 255) (return-from get-char- (values b1 t)))
    ;; normal 8-bit octets in the range 0-255.
    (if (lead-byte-p b1)
        (progn
          (push b1 byte-list)
          ;;(princ b1 window)
          (let ((n (n-trail-bytes b1)))
            (loop repeat n do
                 (let ((ch (%wgetch winptr)))
                   ;;(princ ch window)
                   (push ch byte-list)))))
        (error "First byte isnt a lead byte."))
    ;;(princ (utf-8-to-unicode (reverse byte-list)) window)
    (values (utf-8-to-unicode (reverse byte-list)) nil)))

(defun get-event- (window)
  (multiple-value-bind (code-point function-key-p) (get-char window)
    (cond
      ;; -1 means no key has been pressed.
      ((= code-point -1) nil)
      ;; 0-255 are regular chars, whch can be converted to lisp chars with code-char.
      ((and (>= code-point 0) (not function-key-p)) (code-char code-point))
      ;; if the code belongs to a known function key, return a keyword symbol.
      ((and (>= code-point 0) function-key-p)
       (let ((ev (function-key code-point)))
         (if (eq ev :mouse)
             (multiple-value-bind (mev y x) (get-mouse-event)
               (values mev y x)) ; returns 3 values, see mouse.lisp
             ev)))
      ;; todo: unknown codes, like mose, resize and unknown function keys.
      (t (error "invalid value of char received from ncurses.")))))

|#

;; return t if the chosen unicode points are encoded and decoded correctly.
(defun test-utf-8 ()
  (let* ((unicodes-orig (list 65 246 1046 8364 119070))
         (unicodes-test (mapcar #'(lambda (x) (utf-8-to-unicode (unicode-to-utf-8 x)))
                                unicodes-orig)))
    (mapcar #'(lambda (x)
                (format t
                        "code point: ~A, character ~A, utf8 ~A, correct enc-dec ~A~%"
                        x
                        (code-char x)
                        (unicode-to-utf-8 x)
                        (= x (utf-8-to-unicode (unicode-to-utf-8 x)))))
            unicodes-orig)
    ;; return t if all are t
    (every #'= unicodes-orig unicodes-test)))

;; reading utf-8 chars from the keyboard works.
;; tested in t03.
;; to make t16c work too, we have to similarly be able to extract utf-8 from a window.
;; characters are saved by ncurses as wchar_t or wint_t.

;; instead of get-char, use read-byte and gray streams

;; part 2 will be correctly implementing get_wch und winwch.
;; then we do not need utf-8 conversion.

;; read one (first) char from the stream
;; check how many chars we have to read if it is an utf8 char
;; read additional n (0-3) octets.
;; combine 1-4 octets into one lisp utf-8 char and return that char.

;; all bytes here are octets.

#|
(defun get-utf-8-char (byte-list)
  "Take a list of chars, return the first UTF-8 encoded char.

Signal an error if there are malformatted octets before the first char
is successfully read."
  (let ((one-char-list nil)
        (b1 (car byte-list)))
    (if (lead-byte-p b1)
        (progn
          (push b1 one-char-list)
          (let ((n (n-trail-bytes b1)))
            (loop for i from 1 to n do (push (nth i byte-list) one-char-list))))
        (error "First byte isnt a lead byte."))
    (reverse one-char-list)))
|#

;; (code-char (utf-8-to-unicode (bytes))) => #\a

;; char-code, code-char übersetzen zwischen lisp chars und unicode code points.
;; d.h müsste der code wchar_t compatible 32-bit integer zurckgeben.

#|
;; instead of stream, we have to use window as an argument here.
(defun get-utf-8-char (stream)
  (let ((bytes nil)
        (ch (get-char window)))
    ;; check whether single-byte or multi-byte.
    ;; if single-byte: return ch
    ;; if multi-byte: read continuation bytes, combine bytes, return ch
    (if (lead-byte-p ch)
        (let ((n (n-trail-bytes ch)))
          (if (= n 0)
              (push ch bytes)
              (loop repeat n do (push (get-char window)))))
        (error "trail byte without lead byte"))))
|#

#|
(defun utf-8-number-of-bytes (first-byte)
  "returns the length of the utf-8 code in number of bytes, based on the first byte.
The length can be a number between 1 and 4."
  (declare (fixnum first-byte))
  (cond ((=       0 (ldb (byte 1 7) first-byte)) 1)
        ((=   #b110 (ldb (byte 3 5) first-byte)) 2)
        ((=  #b1110 (ldb (byte 4 4) first-byte)) 3)
        ((= #b11110 (ldb (byte 5 3) first-byte)) 4)
        (t (error "unknown number of utf-8 bytes for ~a" first-byte))))

(defun utf-8-decode-unicode-character-code-from-stream (stream)
  "Decodes byte values, from a binary byte stream, which describe a character
encoded using UTF-8.
Returns the character code and the number of bytes read."
  (let* ((first-byte (read-byte stream))
         (number-of-bytes (utf-8-number-of-bytes first-byte)))
    (declare (fixnum first-byte number-of-bytes))
    (ecase number-of-bytes
      (1 (values (ldb (byte 7 0) first-byte)
                 1))
      (2 (values (logior (ash (ldb (byte 5 0) first-byte) 6)
                         (ldb (byte 6 0) (read-byte stream)))
                 2))
      (3 (values (logior (ash (ldb (byte 5 0) first-byte) 12)
                         (ash (ldb (byte 6 0) (read-byte stream)) 6)
                         (ldb (byte 6 0) (read-byte stream)))
                 3))
      (4 (values (logior (ash (ldb (byte 3 0) first-byte) 18)
                         (ash (ldb (byte 6 0) (read-byte stream)) 12)
                         (ash (ldb (byte 6 0) (read-byte stream)) 6)
                         (ldb (byte 6 0) (read-byte stream)))
                 4))
      (t (error "wrong UTF-8 encoding for file position ~a of stream ~s"
                (file-position stream)
                stream)))))

(mapcar #'(lambda (x) (princ (code-char (utf-8-to-unicode (unicode-to-utf-8 x))))) (list 65 246 1046 8364 119070))

=> (#\A #\LATIN_SMALL_LETTER_O_WITH_DIAERESIS #\CYRILLIC_CAPITAL_LETTER_ZHE #\EURO_SIGN #\MUSICAL_SYMBOL_G_CLEF)
|#

#|

http://stackoverflow.com/questions/9356169/utf-8-continuation-bytes

http://www.herongyang.com/Unicode/UTF-8-UTF-8-Encoding-Algorithm.html

http://stackoverflow.com/questions/6240055/manually-converting-unicode-codepoints-into-utf-8-and-utf-16

http://www.codeguru.com/cpp/misc/misc/multi-lingualsupport/article.php/c10451/The-Basics-of-UTF8.htm

http://emacs.stackexchange.com/questions/5732/how-to-strip-invalid-utf-8-characters-from-a-string

http://stackoverflow.com/questions/41625257/basic-ncurses-menu

http://www.lispforum.com/viewtopic.php?f=2&t=886

https://common-lisp.net/project/trivial-utf-8/

http://stackoverflow.com/questions/1543613/how-does-utf-8-variable-width-encoding-work

http://stackoverflow.com/questions/3011272/would-it-be-possible-to-have-a-utf-8-like-encoding-limited-to-3-bytes-per-charac

https://en.wikipedia.org/wiki/Variable-width_encoding
        
http://www.gigamonkeys.com/book/practical-parsing-binary-files.html

http://zaemis.blogspot.de/2011/06/reading-unicode-utf-8-by-hand-in-c.html

http://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits

http://stackoverflow.com/questions/14380143/matching-binary-patterns-in-c

http://stackoverflow.com/questions/8011700/how-do-i-extract-specific-n-bits-of-a-32-bit-unsigned-integer-in-c

http://stackoverflow.com/questions/5290182/how-many-bytes-does-one-unicode-character-take

http://www.cprogramming.com/tutorial/unicode.html

http://stackoverflow.com/questions/31897544/parsing-utf-8-string-of-known-length-in-common-lisp-one-byte-at-a-time

http://stackoverflow.com/questions/39711335/python-reading-a-utf-8-encoded-string-byte-by-byte

http://stackoverflow.com/questions/14690159/is-ascii-code-7-bit-or-8-bit

https://linuxprograms.wordpress.com/tag/c-utf-8-handling/

http://stackoverflow.com/questions/32459506/ncurses-and-getch-handling-erase-and-arrow-characters

http://stackoverflow.com/questions/526430/c-programming-how-to-program-for-unicode

http://stackoverflow.com/questions/14690159/is-ascii-code-7-bit-or-8-bit

https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node132.html

http://www.cliki.net/CloserLookAtCharacters

https://common-lisp.net/project/trivial-utf-8/

http://clhs.lisp.se/Body/f_ldb.htm#ldb

http://weitz.de/flexi-streams/

https://en.wikipedia.org/wiki/%C3%96

https://de.wikipedia.org/wiki/UTF-8

http://www.utf8-zeichentabelle.de/unicode-utf8-table.pl?number=1024&htmlent=1

|#

