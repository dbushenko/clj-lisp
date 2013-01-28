(ns clj-lisp.core
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment -- ref list of list of maps {:name 'name, :value 'value}
;; The request from environment returns result: {:value 'value, :error true/false}
;; Error in request is true when the name is not found in the environment.
(declare push-frame)
(declare lisp-eval)
(declare lisp-apply)

(defn ret-error []
  {:value nil, :error true})

(defn ret-value [v]
  {:value v, :error false})

(defn create-frame [] '())

(defn add-to-frame [frame n v]
  (cons (hash-map :name n, :value v) frame))

(defn get-from-frame [frame n]
  (loop [curr (first frame)
         rest-frame (rest frame)]
    (cond
     (= (:name curr) n) (ret-value (:value curr))
     (empty? rest-frame) (ret-error)
     :else (recur (first rest-frame) (rest rest-frame)))))

(defn remove-from-frame [frame n]
  (filter #(not (= (:name %) n)) frame))

(defn get-from-env [env n]
  (loop [curr (first env)
         rest-env (rest env)]
    (let [res (get-from-frame curr n)]
      (cond
       (not (:error res)) res
       (empty? rest-env) (ret-error)
       :else (recur (first rest-env) (rest rest-env))))))

(defn add-to-env [env n v]
  (let [frame (or (first env) '())
        rest-env (rest env)]
    (push-frame rest-env (add-to-frame frame n v))))

(defn push-frame [env frame]
  (cons frame env))

(defn pop-frame [env]
  (rest env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp core

(declare lisp-eval)
(declare lisp-apply)

;; Utilities

(defn boolean? [expr]
  (or (= expr 'true)
      (= expr 'false)))

(defn lookup-variable [env n]
  (let [res (get-from-env env n)]
    (if (:error res)
      (throw (Exception. (str "Can't find variable '" n "'!")))
      (:value res))))

;; Core

(defn self-evaluating? [expr]
  (or (number? expr)
      (string? expr)
      (boolean? expr)
      (nil? expr)))

(defn variable? [expr]
  (and (symbol? expr)))

(defn quoted? [expr]
  (and (list? expr)
       (= (first expr) 'quote)))

(defn text-of-quotation [expr]
  (first (rest expr)))

(defn tagged-list? [expr tag]
  (and (list? expr)
       (= (first expr) tag)))

(defn let? [expr]
  (tagged-list? expr 'let))

(defn let-params [expr]
  (let [orig (first expr)]
    (doall
     (map second
          (filter #(even? (first %))
                  (map #(list %1 %2) (range) orig))))))

(defn let-values [expr]
  (let [orig (first expr)]
    (doall
     (map second
          (filter #(odd? (first %))
                  (map #(list %1 %2) (range) orig))))))

(defn let-body [expr] (second expr))

(defn eval-let [env params values body]
  (loop [curr-p (first params)
         rest-p (rest params)
         curr-v (first values)
         rest-v (rest values)
         e env]
    (if (nil? curr-p)
      (lisp-eval e body)
      (recur (first rest-p) (rest rest-p)
             (first rest-v) (rest rest-v)
             (add-to-env e curr-p (lisp-eval e curr-v))))))

(defn lambda? [expr]
  (tagged-list? expr 'lambda))

(defn lambda-parameters [expr]
  (first expr))

(defn lambda-body [expr]
  (first (rest expr)))

(defn make-procedure [env parameters body]
  (list 'procedure parameters body env))

(defn do? [expr]
  (tagged-list? expr 'do))

(defn if? [expr]
  (tagged-list? expr 'if))

(defn if-predicate [expr]
  (first expr))

(defn if-consequent [expr]
  (second expr))

(defn if-alternative [expr]
  (nth expr 2))

(defn eval-if [env expr]
  (if (true? (lisp-eval env (if-predicate expr)))
    (lisp-eval env (if-consequent expr))
    (lisp-eval env (if-alternative expr))))


(defn application? [expr]
  (list? expr))

(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))

(defn apply-primitive-procedure [proc args]
  (apply (resolve (second proc)) args)) ;; resolve finds the function by its symbol

(defn operator [expr]
  (first expr))

(defn operands [expr]
  (rest expr))

(defn list-of-values [env exprs]
  (doall (map #(lisp-eval env %) exprs)))

(defn compound-procedure? [proc]
  (tagged-list? proc 'procedure))

(defn procedure-body [proc] ;; ([a b] (do ((primitive +) a b)) (({:name a, :value 17})))
  (rest (second proc)))

(defn procedure-parameters [proc]
  (first proc))

(defn procedure-environment [proc]
  (second (rest proc)))

(defn extend-environment [env params args]
  (loop [curr-p (first params)
         prm (rest params)
         curr-a (first args)
         arg (rest args)
         frame '()]
    (if (nil? curr-p)
      (push-frame env frame)
      (recur (first prm)
             (rest prm)
             (first arg)
             (rest arg)
             (add-to-frame frame curr-p curr-a)))))

(defn eval-sequence [env proc-body]
  (if (empty? (rest proc-body))
    (lisp-eval env (first proc-body))
    (do (lisp-eval env (first proc-body))
        (eval-sequence env (rest proc-body)))))

(defn lisp-eval [env expr]
  (try
    (cond
     (self-evaluating? expr) expr
     (quoted? expr) (text-of-quotation expr)
     (let? expr) (eval-let env
                           (let-params (rest expr))
                           (let-values (rest expr))
                           (let-body (rest expr)))
     (primitive-procedure? expr) expr
     (do? expr) (eval-sequence env (rest expr))

     (variable? expr) (lookup-variable env expr)
     (if? expr) (eval-if env (rest expr))
     (lambda? expr) (make-procedure env
                                    (lambda-parameters (rest expr))
                                    (lambda-body (rest expr)))

     (application? expr) (lisp-apply (lisp-eval env (operator expr))
                                     (list-of-values env (operands expr)))
     :else (throw (Exception. "Error: Unknown expression type!")))
    (catch Exception e
      (println e))))

(defn lisp-apply [proc args]
  (cond
   (primitive-procedure? proc) (apply-primitive-procedure proc args)
   (compound-procedure? proc) (eval-sequence
                               (extend-environment
                                (procedure-environment (rest proc))
                                (procedure-parameters (rest proc))
                                args)
                               (procedure-body (rest proc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main
  [fname]
  (lisp-eval '() (read-string (slurp fname))))

