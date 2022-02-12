(in-package :6502)

(defun make-stream (text)
  "Make a string displaced onto the given text."
  (make-array (length text) :element-type 'character :adjustable t
              :displaced-to text :displaced-index-offset 0))

(defun try-fetch (stream regex)
  "If the stream begins with a regex match, returns the matched text and move
   the stream past it. Otherwise, returns nil."
  (let ((result (cl-ppcre:scan-to-strings regex stream)))
    (when result
      (multiple-value-bind (original index-offset) (array-displacement stream)
        (adjust-array stream (- (length stream) (length result))
                      :displaced-to original
                      :displaced-index-offset (+ index-offset (length result))))
      result)))

(defun substream-advance (stream start end)
  "Set the stream to a substream at START positions ahead and finishing
   at END position."
  (multiple-value-bind (original index-offset) (array-displacement stream)
    (unless original
      (error "substream-advance called with a string, not a stream"))
    (adjust-array stream (- end start) :displaced-to original
                  :displaced-index-offset (+ index-offset start))))

(defun skip-white-space (stream)
  "Fetches white space from the stream, ignores it and returns the stream."
  (try-fetch stream "^\\s+")
  stream)

(defun fetch-label (stream)
  "Fetches a label from the stream, or returns nil."
  (let ((result (try-fetch stream "^[a-zA-Z][a-zA-Z0-9_]*:")))
    (when result (string-right-trim ":" result))))

(defun fetch-opcode (stream)
  "Fetches an opcode from the stream as a keyword, or returns nil."
  (let ((result (try-fetch stream "^[a-zA-Z]{3}")))
    (when result (intern (string-upcase result) :keyword))))

(defun fetch-hex (stream)
  (when-let (result (try-fetch stream "^[$&][a-fA-F0-9]+"))
    (parse-integer (subseq result 1) :radix 16)))

(defun fetch-binary (stream)
  (when-let (result (try-fetch stream "^%[01]+"))
    (parse-integer (subseq result 1) :radix 2)))

(defun fetch-decimal (stream)
  (when-let (result (try-fetch stream "^[0-9]+"))
    (parse-integer result :radix 10)))

(defun fetch-literal (stream)
  "Fetches a literal value from the stream and returns its integer value
   or NIL if no number could be parsed."
  (or (fetch-hex stream)
      (fetch-binary stream)
      (fetch-decimal stream)))

(defun fetch-name (stream)
  "Fetches a name from the stream, or returns nil."
  (try-fetch stream "^[a-zA-Z][a-zA-Z0-9_]*"))

(defun fetch-term (stream)
  "Fetches a literal or name from the stream, or returns nil."
  (or (fetch-literal stream) (fetch-name stream)))

(defun match-operand-address-modes (stream)
  "Matches the stream against all address-mode readers, returning those that
   match, as well as the positions where the match occurs."
  (let ((value-match (list nil nil)))
    (list (loop for address-mode in *address-modes*
             when (multiple-value-bind (start end match-start match-end)
                      (cl-ppcre:scan (reader address-mode) stream)
                    (declare (ignore end))
                    (when start
                      (setf value-match (list match-start match-end))))
             collect address-mode) value-match)))

(defun operand-possible-modes-and-value (stream)
  "Returns all matching address-modes for the operand, along with positions
   where the match occurs."
  (destructuring-bind (address-modes (match-starts match-ends))
      (match-operand-address-modes stream)
    (cond
      ((find 'implied address-modes) (list (list 'implied) 0 0))
      ((find 'accumulator address-modes) (list (list 'accumulator) 0 1))
      (t (list address-modes (aref match-starts 0) (aref match-ends 0))))))

(defun fetch-expression (stream)
  "Fetches an expression from the stream."
  (if-let (hi-or-lo (try-fetch (skip-white-space stream) "(?i)^.(hi|lo) "))
    (list (intern (string-upcase (subseq hi-or-lo 1 3)) :keyword) (fetch-expression (skip-white-space stream)))
    (let ((term-1 (fetch-term stream)))
      (if (try-fetch (skip-white-space stream) "^\\+")
          (list '+ term-1 (fetch-expression (skip-white-space stream)))
          term-1))))

(defun fetch-operand (stream)
  "Fetches the operand, returning its numerical value and possible
   address-modes."
  (destructuring-bind (possible-modes value-start value-end)
      (operand-possible-modes-and-value stream)
    (substream-advance stream value-start value-end)
    (list (fetch-expression stream) possible-modes)))

(defun fetch-directive (stream)
  (let ((directive (try-fetch stream "^\.[a-zA-Z][a-zA-Z0-9_]*")))
    (when directive
      (intern (string-upcase (subseq directive 1)) 'keyword))))

(defun next-char (stream)
  (when (plusp (length (skip-white-space stream)))
    (aref stream 0)))

(defun parse-directive (line-number line label stream)
  (make-instruction :line-number line-number
                    :line line
                    :label label
                    :directive (fetch-directive stream)
                    :value (loop for expression = (fetch-expression (skip-white-space stream))
                                 while expression
                                 collect expression)))

(defun parse-instruction (line-number line label stream)
  (let* ((opcode (fetch-opcode (skip-white-space stream)))
         (operand (fetch-operand (skip-white-space stream)))
         (value (first operand))
         (address-modes (second operand)))
    (make-instruction :line-number line-number
                      :line line
                      :label label
                      :opcode opcode
                      :value value
                      :address-mode address-modes)))

(defun strip-comment (text)
  "Removes comment and white space from end of string."
  (let ((pos (position #\; text)))
    (when pos (setf text (subseq text 0 pos))))
  (string-right-trim " " text))

(defun parse-line (line-number line labels)
  "Converts a line of text into an instruction representing the assembly code."
  (let* ((stream (make-stream (strip-comment line)))
         (label (fetch-label (skip-white-space stream))))
    (when label
      (when (gethash label labels)
        (error "Duplicate label ~S" label))
      (setf (gethash label labels) t))
    (if (eql (next-char stream) #\.)
        (parse-directive line-number line label stream)
        (parse-instruction line-number line label stream))))

(defun parse-code (text)
  "Parses the assembly source text and returns the assembled code as a list of
   alists."
  (loop with labels = (make-hash-table :test #'equalp)
        for line in (cl-ppcre:split "\\n" text)
        for line-number from 1
        when (parse-line line-number line labels) collect it))
