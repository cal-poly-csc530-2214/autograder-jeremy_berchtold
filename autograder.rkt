#lang racket


(define REFERENCE_PROGRAM '(+ 7 (+ 4 (+ 2 1))))
; This is the program that will be graded. This value can be changed to a different program and the rules error correcting rules will still be applied correctly
(define STUDENT_PROGRAM '(+ 7 (+ 4 (+ 2 (target 0)))))

(define RULES '(
				(+ n 1)
				(- n 1)))


(define (get-rule-numbers rules n)
	(if (empty? rules)
		empty
		(cons n (get-rule-numbers (rest rules) (+ n 1)))))

(define (expr-to-sketch expr)
	(match expr
		[(list '+ a b) (string-append "(" (expr-to-sketch a) ") + (" (expr-to-sketch b) ")")]
		[(list '- a b) (string-append "(" (expr-to-sketch a) ") - (" (expr-to-sketch b) ")")]
		[(list 'target n) (string-append "applyRules(" (expr-to-sketch n) ")")]
		[(? number? x) (number->string x)]
		[(? symbol? x) (symbol->string x)]
		[else (error "Unsupported operation" expr)]))

(define (rule-to-sketch rule idx)
	(string-append
		"int applyRule" (number->string idx) "(int n) {\n"
		"\tif (??) return n;\n"
		"\treturn " (expr-to-sketch rule) ";\n"
		"}\n"))

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

(define (func-to-sketch func name)
	(string-append
		"int " name "() {\n"
		"\treturn " (expr-to-sketch func) ";\n"
		"}\n"))

(define (convert-to-sketch student reference rules)
	(string-append
		(rules-to-sketch rules)
		(func-to-sketch reference "professorReferenceImpl")
		(func-to-sketch student "studentImpl")
		"harness void main() {\n"
		"\tassert professorReferenceImpl() == studentImpl();\n"
		"}\n"))
		
(display (convert-to-sketch STUDENT_PROGRAM REFERENCE_PROGRAM RULES))
