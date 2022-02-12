(in-package :6502)

(defconstant +relative-branch-size-byte+ 2)
(defconstant +max-byte+ 256)
(defparameter +absolute-modes+ '(absolute absolute-x absolute-y))
(defparameter +zero-page-modes+ '(zero-page zero-page-x zero-page-y))

(defgeneric asm (source &key env org &allow-other-keys)
  (:documentation "Assemble SOURCE into a bytevector and return it."))

(defmethod asm ((source list) &rest keys)
  (apply #'assemble-code-block (list-to-instructions source) keys))

(defmethod asm ((source string) &rest keys)
  (apply #'assemble-code-block (parse-code source) keys))

(defstruct instruction
  "Represents a single assembly line."
  (line-number  nil :type (or null integer))
  (line         nil :type (or null string))
  (label        nil :type (or null string))
  (directive    nil :type (or null symbol))
  (opcode       nil :type (or null symbol))
  (address-mode nil :type (or null symbol list))
  (value        nil :type (or null u16 list string)))

(defun list-to-instructions (instructions)
  "Given a list of assembly tuples, convert them to instructions."
  (unless (listp (first instructions))
    (setf instructions (list instructions)))
  (loop for tuple in instructions collect (apply #'tuple-to-instruction tuple)))

(defun tuple-to-instruction (opcode &optional operand)
  "Given an opcode and value, as symbols, convert them to an instruction."
  (unless operand
    (return-from tuple-to-instruction
      (make-instruction :opcode opcode :address-mode 'implied)))
  (let ((token (transform-sexp-syntax operand)))
    (destructuring-bind (possible-modes value-start value-end)
        (operand-possible-modes-and-value token)
      (let ((stream (make-stream (coerce (subseq token value-start value-end)
                                         '(vector character)))))
        (make-instruction :opcode opcode :value (fetch-literal stream)
                          :address-mode possible-modes)))))

(defun transform-sexp-syntax (sexp-token)
  "Given a SEXP-token using an indirect, *.x or *.y addressing mode, transform
   it to use the classic string assembly syntax."
  (substitute #\, #\. (cl-ppcre:regex-replace "\@([^.]*)(.*)?"
                                              (string sexp-token) "($\\1)\\2")))

(defmacro resolve-byte (place env)
  "Given a place and an environment, if that place is a function, call that
   function with the environment and assign the result to the place."
  (let ((byte-name (gensym)))
    `(when (functionp ,place)
       (let ((,byte-name (funcall ,place ,env)))
         (setf ,place ,byte-name)))))

(defun resolve-bytes (bytes env)
  (loop for i from 0 below (length bytes)
        do (resolve-byte (nth i bytes) env)))

(defun sorted-symbols (hash-table)
  (sort (loop for key being the hash-keys of hash-table collect key) #'string-lessp))

(defgeneric format-assembly-line (directive pc bytes instruction)
  (:method (directive pc bytes instruction)
    (if (and bytes (length bytes))
        (format t "~4,'0X  ~{~2,'0X ~}~16T  ~4,' D  ~A~%"
                pc
                bytes
                (instruction-line-number instruction)
                (instruction-line instruction))
        (format t "~16T  ~4,' D  ~A~%"
                (instruction-line-number instruction)
                (instruction-line instruction)))))

(defun create-listing (assembly env)
  (loop for (pc bytes instruction) in assembly
        do (format-assembly-line (instruction-directive instruction) pc bytes instruction))
  (format t ";;~%;; Symbols:~%;;~%")
  (dolist (symbol (sorted-symbols env))
    (format t "~20,,,' A ~4,'0X~%" symbol (gethash symbol env))))

(defun assemble-code-block (code-block &key (env (make-hash-table :test 'equal)) (org 0) listp)
  "Given a list of instructions, assemble to a byte vector."
  (let*
      ;; First pass - Symbolic bytes to be resolved in second pass
      ((assembly (loop for instruction in code-block
                       for pc = org then (+ pc (length bytes))
                       for bytes = (assemble-instruction instruction pc env)
                       collect (list pc bytes instruction)))
       ;; Second pass - Resolve symbolic bytes
       (output-bytes (flex:with-output-to-sequence (output)
                       (loop for (pc bytes instruction) in assembly
                             do (resolve-bytes bytes env)
                             do (write-sequence bytes output)))))
    (when listp
      (create-listing assembly env))
    (values output-bytes env)))

(defgeneric process-directive (name value pc)
  (:method (name value pc)
    (format t "Unknown assembly directive ~A skipped~%" name)
    nil))

(defmacro defdirective (name () &rest specs)
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (what args &rest body) spec
                   (ecase what
                     (:assemble (destructuring-bind (value pc) args
                                  `(defmethod process-directive ((,(gensym) (eql ,name)) ,value ,pc)
                                     ,@body)))
                     (:format (destructuring-bind (pc bytes instruction) args
                                `(defmethod format-assembly-line ((,(gensym) (eql ,name)) ,pc ,bytes ,instruction)
                                   ,@body))))))
               specs)))

(defun print-directive-with-dump (pc bytes instruction)
  (cond
    ((< (length bytes) 4)
     (format t "~4,'0X  ~{~2,'0X ~}~16T  ~4,' D  ~A~%"
             pc
             bytes
             (instruction-line-number instruction)
             (instruction-line instruction)))
    (t
     (format t "~4,'0X  ~16T  ~4,' D  ~A~%"
             pc
             (instruction-line-number instruction)
             (instruction-line instruction))
     (loop for i below (length bytes) by 8
           do (format t "~4,'0X  ~{~2,'0X ~}~%"
                      (+ pc i)
                      (subseq bytes i (min (length bytes) (+ i 8))))))))

(defdirective :byte ()
  (:assemble (value pc)
    (loop for byte in value
          collect (make-byte byte pc nil)))
  (:format (pc bytes instruction)
           (print-directive-with-dump pc bytes instruction)))

(defdirective :word ()
  (:assemble (value pc)
    (loop for word in value
          append (list (make-byte word pc :low) (make-byte word pc :high))))
  (:format (pc bytes instruction)
           (print-directive-with-dump pc bytes instruction)))

(defdirective :align ()
  (:assemble (value pc)
    (destructuring-bind (modulo) value
      (loop for i below (- modulo (mod pc modulo)) collect 0))))

(defdirective :fill ()
  (:assemble (value pc)
    (destructuring-bind (count &optional (byte 0)) value
      (loop for i below count collect byte)))
  (:format (pc bytes instruction)
           (format t "~4,'0X  ~16T  ~4,' D  ~A~%"
                   pc
                   (instruction-line-number instruction)
                   (instruction-line instruction))))

(defdirective :org ()
  (:assemble (value pc)
    (destructuring-bind (origin) value
      (assert (<= pc origin))
      (loop for i from pc below origin collect 0)))
  (:format (pc bytes instruction)
           (format t "~16T  ~4,' D  ~A~%"
                   (instruction-line-number instruction)
                   (instruction-line instruction))))

(defun assemble-instruction (instruction pc env)
  "Given an instruction, and the current program counter, fill the environment
   with any labels and assemble instruction to a list of bytes."
  (with-slots (opcode value label directive) instruction
    (when label
      (setf (gethash label env) pc))
    (assert (not (and directive opcode)))
    (cond
      (directive
       (process-directive directive value pc))
      (opcode
       (let ((mode (decide-address-mode instruction env)))
         (list* (find-opcode opcode mode) (process-args value mode pc)))))))

(defun find-opcode (opcode mode)
  "Finds an opcode matching OPCODE and MODE, raising ILLEGAL-OPCODE otherwise."
  (let ((match (position-if #'(lambda (e) (match-opcode-data e opcode mode))
                            *opcode-meta*)))
    (or match (error 'illegal-opcode :opcode (list opcode mode)))))

(defun process-args (value address-mode pc)
  "Given the operand value, address-mode, and program counter, return a list of
   assembled bytes, using delayed functions for labels or expressions."
  (case address-mode
    ((absolute absolute-x absolute-y indirect)
     (list (make-byte value pc :low) (make-byte value pc :high)))
    ((implied accumulator) nil)
    (relative (list (make-byte value pc :relative)))
    (otherwise (list (make-byte value pc :low)))))

(defun decide-address-mode (instruction env)
  "Finds the desired address mode, matching what the opcode allows to what was
   parsed from the operand's syntax."
  (with-slots (opcode address-mode value) instruction
    (let ((modes (if (listp address-mode) address-mode (list address-mode)))
          (opcode-modes (get-opcode-address-modes opcode)))
      (if (and (zero-page-address value env)
               (intersection opcode-modes +zero-page-modes+))
          (setf modes (set-difference modes +absolute-modes+))
          (setf modes (set-difference modes +zero-page-modes+)))
      (first (intersection modes opcode-modes)))))

(defun get-opcode-address-modes (opcode)
  "Given an opcode, return the possible address modes for that operation."
  (loop for e across *opcode-meta*
     when (match-opcode-data e opcode :any) collect (fifth e)))

(defun match-opcode-data (data opcode &optional (address-mode :any))
  "Returns whether the asm metadata matches the given opcode, and address-mode
   if it is provided."
  (and (eq (first data) (intern (symbol-name opcode) :6502))
                   (or (eq address-mode :any) (eq address-mode (fifth data)))))

(defun zero-page-address (addr env)
  "Returns whether the address is a zero page access."
  (cond
    ((numberp addr) (< addr +max-byte+))
    ((stringp addr)
     (let ((addr (gethash addr env)))
       (and (numberp addr) (< addr +max-byte+))))
    ((listp addr) nil)
    (t (error "Invalid address ~A" addr))))

(defun make-byte (value pc type)
  "Given an integer, return a single byte for the required type. Given a label,
   return a delayed function to calculate the same, once labels are defined."
  (etypecase value
    (string (lambda (env)
              (let ((addr (or (gethash value env)
                              (error "Undefined label ~A" value))))
                (when (eq type :relative)
                  (setf addr (- addr pc +relative-branch-size-byte+)))
                (make-byte addr pc type))))
    (list
     (ecase (first value)
       (+ (lambda (env)
            (destructuring-bind (unused-plus operand-1 operand-2) value
              (declare (ignore unused-plus))
              (+ (make-and-resolve-byte operand-1 pc type env)
                 (make-and-resolve-byte operand-2 pc type env)))))
       (:hi (lambda (env)
              (make-and-resolve-byte (second value) pc :high env)))
       (:lo (lambda (env)
              (make-and-resolve-byte (second value) pc :low env)))))
    (number
     (if (eq type :high)
         (floor (/ value +max-byte+))
         (mod value +max-byte+)))))

(defun make-and-resolve-byte (operand pc type env)
  "Given an operand, convert it to a byte, resolving any delayed functions."
  (let ((value (make-byte operand pc type)))
    (resolve-byte value env)
    value))
