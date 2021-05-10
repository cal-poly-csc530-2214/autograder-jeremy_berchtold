#lang racket

(require rackunit)

; Reference program (the professor's program the student is trying to emulate)
(define REFERENCE_PROGRAM '(+ 7 (+ 4 (+ 2 1))))
; This is the program that will be graded. This value can be changed to a different program and the rules error correcting rules will still be applied correctly
(define STUDENT_PROGRAM '(+ 7 (+ 4 (+ 2 (target 0)))))

; The set of rules to be applied when the given (target value) is given as input to the program.
(define RULES '(
				(+ n 1)
				(- n 1)))

;; get-rule-numbers: List -> List[Integer]
;; Outputs an the rule number given the list of rules
(define (get-rule-numbers rules n)
	(if (empty? rules)
		empty
		(cons n (get-rule-numbers (rest rules) (+ n 1)))))

(check-equal? (get-rule-numbers '((+ n 1) (- n 1)) 0) '(0 1))

;; expr-to-sketch: Sexpr -> String
;; Converts an Sexpr to Sketch code
(define (expr-to-sketch expr)
	(match expr
		[(list '+ a b) (string-append "(" (expr-to-sketch a) ") + (" (expr-to-sketch b) ")")]
		[(list '- a b) (string-append "(" (expr-to-sketch a) ") - (" (expr-to-sketch b) ")")]
		[(list 'target n) (string-append "applyRules(" (expr-to-sketch n) ")")]
		[(? number? x) (number->string x)]
		[(? symbol? x) (symbol->string x)]
		[else (error "Unsupported operation" expr)]))

(check-equal? (expr-to-sketch '(+ 1 2)) "(1) + (2)")
(check-equal? (expr-to-sketch '(- 2 2)) "(2) - (2)")

;; rule-to-sketch: Sexpr Integer -> String
;; Converts an individual rule to a Sketch function with a hole
(define (rule-to-sketch rule idx)
	(string-append
		"int applyRule" (number->string idx) "(int n) {\n"
		"\tif (??) return n;\n"
		"\treturn " (expr-to-sketch rule) ";\n"
		"}\n"))

;; rules-to-sketch: List[Sexpr] -> String
;; Converts all rules to Sketch functions and includes a single function called "applyRules" that applies all rules at once.
(define (rules-to-sketch rules)
	(string-append
		(string-append* (map rule-to-sketch rules (get-rule-numbers rules 0)))
		"int applyRules(int n) {\n"
		"\treturn "
		(foldl 
			(lambda (x y) (string-append "applyRule" (number->string x) "(" y ")"))
			 "n"
			(get-rule-numbers rules 0))
		";\n"
		"}\n"))

;; func-to-sketch: Sexpr String -> String
;; Converts an expression and function name to a Sketch function
(define (func-to-sketch func name)
	(string-append
		"int " name "() {\n"
		"\treturn " (expr-to-sketch func) ";\n"
		"}\n"))

;; convert-to-sketch: Sexpr Sexpr List[Sexpr] -> String
;; Converts the student and professor implementations, along with a set of rules, to Sketch to solve. The student implementation should contain mPy-like expressions of target location(s) where rules should be applied (currently only 1 target per program supported).
(define (convert-to-sketch student reference rules)
	(string-append
		(rules-to-sketch rules)
		(func-to-sketch reference "professorReferenceImpl")
		(func-to-sketch student "studentImpl")
		"harness void main() {\n"
		"\tassert professorReferenceImpl() == studentImpl();\n"
		"}\n"))
		
;; Displays the output to stdout so I can pipe it to a Sketch file to run thru the Sketch evaluator
(display (convert-to-sketch STUDENT_PROGRAM REFERENCE_PROGRAM RULES))
