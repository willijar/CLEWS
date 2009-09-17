(in-package :jarw.math)

(defstruct convolutional-code
  (k 1 :type fixnum) ;; bits at a time
  (taps #(#*111 #*101) :type (array simple-bit-vector *)))

(defun convolutional-code-constraint-length(code)
  (/ (length (aref (convolutional-code-taps code) 0))
     (convolutional-code-k code)))

(defun convolutional-code-n(code) (length (convolutional-code-taps code)))

(defun convolutional-code-rate(code)
  (/ (convolutional-code-k code) (convolutional-code-n code)))

(defun bit-vector-to-integer(vec)
  (let ((result 0)
        (length (length vec)))
    (do((i (1- length) (1- i)))
       ((< i 0) result)
      (when (= (bit vec i) 1)
        (setf result (logior result (ash 1 (- length i 1))))))))

(defun integer-to-bit-vector(integer &optional (length (integer-length integer)))
  (let ((result (make-array length :element-type 'bit :initial-element 0)))
    (do((i (1- length) (1- i)))
       ((< i 0) result)
      (when (logtest integer (ash 1 (- length i 1)))
        (setf (bit result i) 1)))))

(defun make-initial-state(code)
  (make-array
   (etypecase code
     (convolutional-code
      (* (convolutional-code-k code) (1- (convolutional-code-constraint-length code))))
     (integer code))
   :element-type 'bit :initial-element 0))

(defun random-bit-vector(length)
  "Return a random bit sequence of given length"
  (let ((a (make-array length :element-type 'bit)))
    (dotimes(i length) (setf (bit a i) (random 2)))
    a))

(defun encode-k(code input state)
  "Given a convolutional code, input and a state,
return the output and new state"
  (let* ((k (convolutional-code-k code))
         (l (* k (1- (convolutional-code-constraint-length code)))))
    (assert (and (bit-vector-p input) (= (length input) k))
            (input)
            "Input to coder must be a bit vector of length ~A" k)
    (assert (and (bit-vector-p state) (= (length state) l))
            (state)
            "State is not a (bit-vector ~D)" l)
    (let ((vec (concatenate 'simple-bit-vector input state)))
      (values
       (map 'simple-bit-vector
            #'(lambda(tap) (reduce #'logxor (map 'simple-bit-vector #'logand vec tap)))
            (convolutional-code-taps code))
       (subseq vec 0 l)))))

(defun encode(code input &optional (state (make-initial-state code)))
  (let ((k (convolutional-code-k code))
        (n (convolutional-code-n code)))
    (assert (zerop (mod (length input) k))
            (input)
            "Length of input is not an integral multiple of ~A" k)
    (do((i 0 (+ k i))
        (o 0 (+ n o))
        (output (make-array (* n (/ (length input) k)) :element-type 'bit))
        (states (list state) (cons state states)))
       ((>= i (length input))
        (values output state (nreverse states)))
      (multiple-value-bind(op new-state)
          (encode-k code (subseq input i (+ k i)) state)
        (setf (subseq output o (+ o n)) op)
        (setf state new-state)))))

(defun next-state(bits)
  (let ((bits (copy-seq bits)))
    (do ((i (1- (length bits)) (1- i)))
        ((< i 0))
      (if (= (bit bits i) 0)
          (progn
            (setf (bit bits i) 1)
            (return bits))
          (setf (bit bits i) 0)))))

(defun all-states(no-bits)
  "Return a list of all states with given no of bits"
  (do*((next (make-array no-bits :element-type 'bit :initial-element 0)
             (next-state next))
         (results (list next) (push next results)))
        ((not next) (nreverse (cdr results)))))

(defun state-diagram(code)
  "State diagram is a list of transitions. Each transition is a list
of the initial state, input bit vector, output state and outbit bit
vector"
  (let ((k (convolutional-code-k code))
        (results nil))
    (do ((state (make-initial-state code)
                (next-state state)))
        ((not state))
      (do ((input (make-array k :element-type 'bit :initial-element 0)
                  (next-state input)))
          ((not input))
        (multiple-value-bind(output new-state)
            (encode-k code input state)
          (push (list state input new-state output)  results))))
    (nreverse results)))

(defstruct path-node
  (d 0 :type fixnum)
  (state #* :type (array bit *))
  (output #* :type (array bit *)))

;; default chooser
(defun choose-one(a b) (declare (ignore b)) a)

(defun make-initial-paths(code)
  (list
   (list
    (make-path-node
     :d 0
     :state (make-array
             (* (convolutional-code-k code)
                (1- (convolutional-code-constraint-length code)))
                :element-type 'bit
                :initial-element 0)))))

(defun hamming-distance(a b)
  (count 1 (map 'simple-bit-vector #'logxor a b)))

(defun viterbi-extend(code input paths)
  "Use trellis decoding - returns a new set of paths after given input."
  (let* ((n (convolutional-code-n code))
         (outputs (all-states (convolutional-code-k code)))
         (results))
    (assert (and (bit-vector-p input) (= (length input) n))
            (input)
            "Input to decoder must be a bit vector of length ~A" n)
    (dolist(p paths)
      (let*((node (first p))
            (state (path-node-state node)))
        (dolist(output outputs)
          (multiple-value-bind(trial-input new-state)
              (encode-k code output state)
            (let ((d (+ (hamming-distance input trial-input)
                        (path-node-d (first p))))
                  (another
                   (find new-state results
                         :key #'(lambda(p) (path-node-state (first p)))
                         :test #'equal)))
              (when (and another (< d (path-node-d (first another))))
                (setf results (delete another results)
                      another nil))
              (unless another
                (push (cons (make-path-node
                             :state new-state :output output :d d)
                            p)
                      results)))))))
    results))

(defun path-decoded(path)
  (let*((n (length (path-node-output (first path))))
       (result (make-array (* n (1- (length path))) :element-type 'bit))
       (i 0))
    (mapcar
     #'(lambda(node)
         (setf (subseq result i (+ i n)) (path-node-output node))
         (incf i n))
     (rest (reverse path)))
    result))

(defun path-hamming(path)
  (path-node-d (first path)))

(defun decode(code input &optional (paths (make-initial-paths code)))
  (let ((n (convolutional-code-n code)))
    (assert (zerop (mod (length input) n))
            (input)
            "Length of input is not an integral multiple of n=~A" n)
    (do((i 0 (+ n i)))
       ((>= i (length input)))
      (setf paths (viterbi-extend code (subseq input i (+ i n)) paths)))
    (setf paths (sort paths #'< :key #'path-hamming))
    (values
     (mapcar #'path-decoded paths)
     (mapcar #'path-hamming paths)
     paths)))

(defun sbvec(bvec) (format nil "~{~D~}" (map 'list #'identity bvec)))

(defvar *dot-transition-styles*
  '((#*0 . ",style=\"dashed\"")
    (#*1 . "")
    (#*00 . ",style=\"dashed\",color=\"red\"")
    (#*01 . ",style=\"dashed\",color=\"blue\"")
    (#*10 . ",color=\"red\"")
    (#*11 . ",color=\"blue\""))
  "Dot Line styles for transitions")

(defun write-state-diagram-dot(code &key (stream *standard-output*)
                               (styles *dot-transition-styles*))
  "Output state diagram in dot syntax."
  (write-line "digraph state_diagram {
 rankdir=LR;
 edge [penwidth=1.5];
 node [shape=circle];" stream)
  (dolist(transition (state-diagram code))
    (format stream "~A -> ~A [label=\"~A/~A\"~A];~%"
            (sbvec (first transition))
            (sbvec (third transition))
            (sbvec (second transition))
            (sbvec (fourth transition))
            (cdr (assoc (second transition) styles :test #'equal))))
  (write-line "}" stream))

(defun state-ordering(code)
  (let ((states nil))
    (dolist(transition (state-diagram code))
      (let ((from (first transition))
            (to (third transition)))
        (unless (member from states :test #'equal)
          (push from states))
        (unless (member to states :test #'equal)
          (push to states))))
    (nreverse states)))

(defvar *mpost-transition-styles*
   '((#*0 . "dashed evenly withcolor red")
    (#*1 . "withcolor blue")
    (#*00 . "dashed evenly withcolor red")
    (#*01 . "dashed evenly withcolor blue")
    (#*10 . "withcolor red")
    (#*11 . "withcolor blue"))
  "mpost line styles for transitions")

(defparameter example1
  (make-convolutional-code :k 1 :taps #(#*111 #*101)))

(defparameter example2
  (make-convolutional-code :k 2 :taps #(#*1011 #*1101 #*1010)))

(defparameter example3
  (make-convolutional-code :k 1 :taps #(#*1111 #*1101)))

(defparameter example4
  (make-convolutional-code :k 1 :taps #(#*111 #*011 #*101)))

(defparameter nasa
  (make-convolutional-code :k 1 :taps #(#*1111001 #*1011011)))`

(defun write-transitions-mpost(code &key
                               (number-levels 7)
                               (stream *standard-output*)
                               (styles *mpost-transition-styles*))
  "Output the transitions drawing code from a state diagram"
  (let ((states (state-ordering code)))
    (flet ((state-index(state) (1+ (position state states :test #'equal))))
      (format stream "def draw_transitions(suffix $)(expr level)=~%")
      (dolist(transition (state-diagram code))
        (format stream " draw_trellis_transition($,level,~D,~D,\"~A/~A\") ~A;~%"
                (state-index (first transition))
                (state-index (third transition))
                (sbvec (second transition)) (sbvec (fourth transition))
                (cdr (assoc (second transition) styles :test #'equal))))
      (format stream "enddef;~%~%")
      (format stream "draw_trellis_grid(trellis,~D~{,~S~});~%"
              (1+ number-levels)
              (map 'list #'sbvec states)))))

(defun split-bits(sequence n)
  (do((i 0 (+ i n))
      (subseq nil))
     ((>= i (length sequence))
      (nreverse subseq))
    (push (subseq sequence i (+ i n)) subseq)))

(defun state-indices(code states)
  (let ((state-ordering (state-ordering code)))
    (mapcar
     #'(lambda(state) (1+ (position state state-ordering :test #'equal)))
     states)))

(defun write-encode-path(code input
                         &key (stream *standard-output*)
                         (initial-state (make-initial-state code))
                         (style "withcolor green withpen pencircle scaled 5pt"))
  (multiple-value-bind(output state states) (encode code input initial-state)
    (format stream "draw_trellis_inputs(trellis~{,~S~});~%"
            (mapcar #'sbvec (split-bits input (convolutional-code-k code))))
    (format stream "draw_trellis_outputs(trellis~{,~S~});~%"
            (mapcar #'sbvec (split-bits output (convolutional-code-n code))))
    (format stream "draw_trellis_path(trellis~{,~D,\"\"~}) ~A;~%"
            (state-indices code states)
            style)
    state))

(defun write-finite-state-machine(code &key (stream *standard-output*))
  (format stream "draw_coder(reg,adders,~D,~D,~D);~%"
          (convolutional-code-k code)
          (convolutional-code-constraint-length code)
          (convolutional-code-n code))
  (do((i 0 (1+ i)))
     ((>= i (convolutional-code-n code)))
    (let ((adder (aref (convolutional-code-taps code) i)))
      (do ((j 0 (1+ j)))
          ((>= j (length adder)))
        (unless (zerop (aref adder j))
          (format stream "connect(reg[~D],adders[~D]);~%"
                  (1+ j) (1+ i)))))))

(defun write-decode-path(code path
                          &key (stream *standard-output*)
                          (style "withcolor green withpen pencircle scaled 5pt"))
  (format stream "draw_trellis_outputs(trellis~{,~S~});~%"
          (mapcar #'sbvec (mapcar #'path-node-output (rest (reverse path)))))
  (let ((state-ordering (state-ordering code)))
  (format stream "draw_trellis_path(trellis~{,~D,\"e=~D\"~}) ~A;~%"
          (mapcan
           #'(lambda(p)
               (list (1+ (position (path-node-state p) state-ordering
                                   :test #'equal))
                     (path-node-d p)))
           (reverse path))
          style)))

(defun write-decode-paths(code input &key  (stream *standard-output*)
                          (styles '("withcolor green withpen pencircle scaled 5pt" "withcolor red withpen pencircle scaled 2pt")))
  (format stream "draw_trellis_inputs(trellis~{,~S~});~%"
            (mapcar #'sbvec (split-bits input (convolutional-code-n code))))

  (flet ((nextstyle()
           (if (stringp styles) styles
               (prog1 (car styles)
                 (setf styles (or (rest styles) (car styles)))))))
    (multiple-value-bind(outputs hamming paths) (decode code input)
      (declare (ignore outputs hamming))
      (dolist(path paths)
        (write-decode-path code path :stream stream :style (nextstyle))))))

(defun make-viterbi-decoding-problem(code sequence-length no-errors &optional (no-attempts 10))
  "Given a convolutional code, generate a random sequence of given
length and with no-errors errors ensuring that there is at most 1 valid
decoded sequence, Returns the encoded sequence with errors and the
corrected decoded sequence."
  (assert (zerop (mod sequence-length (convolutional-code-n code)))
          (sequence-length)
          "Sequence length must be an integer mutiple times n=~A"
          (convolutional-code-n code))
  (dotimes(attempt no-attempts)
    (let* ((decoded
            (random-bit-vector (* sequence-length (convolutional-code-rate code))))
           (encoded (encode code decoded))
           (errors (make-array sequence-length :element-type 'bit)))
      (do((i (random sequence-length) (random sequence-length)))
         ((= no-errors (count 1 errors)))
        (setf (aref errors i) 1))
      (let ((error-encoded (map '(vector bit *) #'logxor errors encoded)))
        (multiple-value-bind(p h)
            (decode code error-encoded)
          ;; if there is only one minumum distance we have a solution
          (when (and (= (count (elt h 0) h) 1)
                     (equalp decoded (elt p 0)))
            (return-from make-viterbi-decoding-problem
              (values error-encoded decoded)))))))
  (error "Unable to generate a decoding problem with only one solution"))

;; e.g. (make-viterbi-decoding-problem example1 10 2)
;; #*1100010111
;; #*11001

(export '(make-convolutional-code bit-vector-to-integer integer-to-bit-vector convolutional-code random-bit-vector encode hamming-distance decode))








#|
states = ('Rainy', 'Sunny')

observations = ('walk', 'shop', 'clean')

start_probability = {'Rainy': 0.6, 'Sunny': 0.4}

transition_probability = {
   'Rainy' : {'Rainy': 0.7, 'Sunny': 0.3},
   'Sunny' : {'Rainy': 0.4, 'Sunny': 0.6},
   }

emission_probability = {
   'Rainy' : {'walk': 0.1, 'shop': 0.4, 'clean': 0.5},
   'Sunny' : {'walk': 0.6, 'shop': 0.3, 'clean': 0.1},
   }
def forward_viterbi(obs, states, start_p, trans_p, emit_p):
   T = {}
   for state in states:
       ##          prob.           V. path  V. prob.
       T[state] = (start_p[state], [state], start_p[state])
   for output in obs:
       U = {}
       for next_state in states:
           total = 0
           argmax = None
           valmax = 0
           for source_state in states:
               (prob, v_path, v_prob) = T[source_state]
               p = emit_p[source_state][output] * trans_p[source_state][next_state]
               prob *= p
               v_prob *= p
               total += prob
               if v_prob > valmax:
                   argmax = v_path + [next_state]
                   valmax = v_prob
           U[next_state] = (total, argmax, valmax)
       T = U
   ## apply sum/max to the final states:
   total = 0
   argmax = None
   valmax = 0
   for state in states:
       (prob, v_path, v_prob) = T[state]
       total += prob
       if v_prob > valmax:
           argmax = v_path
           valmax = v_prob
   return (total, argmax, valmax)

def example():
    return forward_viterbi(observations,
                           states,
                           start_probability,
                           transition_probability,
                           emission_probability)
print example()
|#