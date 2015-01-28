#lang typed/racket
(require typed/test-engine/racket-tests)


(: leap? : Integer -> Boolean)
;; determine whether a year is a leap year
(define (leap? year)
  (or (and (= 0 (remainder year 4))
           (not (= 0 (remainder year 100))))
      (= 0 (remainder year 400))))
;; TESTS
(check-expect (leap? 1) #f)
(check-expect (leap? 200) #f)
(check-expect (leap? 2000) #t)
(check-expect (leap? 2004) #t)


(: date-valid? : Integer Integer Integer -> Boolean)
;; determine whether a given month, day, year combination is a possible date
(define (date-valid? month day year)
  (cond
    [(< year 1900) #f]
    [(or (> day 31) (< day 1)) #f]
    [(or (> month 12) (< month 1)) #f]
    [(and (or (= month 4) (= month 6) (= month 9) (= month 11)) (= day 31)) #f]
    [(and (= month 2) (> day 29)) #f]
    [(and (= month 2) (equal? (leap? year) #f) (= day 29)) #f]
    [else #t]))
;;GRADER: Years after 2099 are also supposed to be invalid. -2
;; TESTS
(check-expect (date-valid? 1 1 1) #f)
(check-expect (date-valid? 1 31 1900) #t)
(check-expect (date-valid? 4 31 2003) #f)
(check-expect (date-valid? 2 29 2000) #t)
(check-expect (date-valid? 2 29 1900) #f)
(check-expect (date-valid? 8 31 2014) #t)


(: day-of-week : Integer Integer Integer -> String)
;; determine the day of the week of a given month, day, year
(define (day-of-week month day year)
  (cond
    [(= (remainder (n month day year) 7) 0) "Sunday"]
    [(= (remainder (n month day year) 7) 1) "Monday"]
    [(= (remainder (n month day year) 7) 2) "Tuesday"]
    [(= (remainder (n month day year) 7) 3) "Wednesday"]
    [(= (remainder (n month day year) 7) 4) "Thursday"]
    [(= (remainder (n month day year) 7) 5) "Friday"]
    [(= (remainder (n month day year) 7) 6) "Saturday"]
    [else "Not a valid date. You shouldn't even be able to get this error."]))  ; with a realistic input, this should never be triggered
;;GRADER: Please keep your lines shorter; makes it easier to read.
;;GRADER: This would have been a good opportunity to store the output of a helper function in a variable. -1
;; TESTS
(check-expect (day-of-week 10 8 2014) "Wednesday")
(check-expect (day-of-week 11 8 2014) "Saturday")
(check-expect (day-of-week 12 8 2014) "Monday")
(check-expect (day-of-week 1 8 2015) "Thursday")
(check-expect (day-of-week 2 8 2015) "Sunday")
(check-expect (day-of-week 3 8 2015) "Sunday")
(check-expect (day-of-week 4 8 2015) "Wednesday")
(check-expect (day-of-week 5 8 2015) "Friday")
(check-expect (day-of-week 6 8 2015) "Monday")
(check-expect (day-of-week 7 8 2015) "Wednesday")
(check-expect (day-of-week 8 8 2015) "Saturday")
(check-expect (day-of-week 9 8 2015) "Tuesday")

(: n : Integer Integer Integer -> Integer)
;; [AUX 1] calculate n according to formula
;; n = (year - 1900) + j + day + exact-floor(year/4)
;; where j is month-adjustment given by the table on the lab page
(define (n month day year)
  (+ (- year 1900) (j month year) day (exact-floor (/ year 4))))
;; [AUX 1] TESTS
(check-expect (n 10 8 2014) 626)
(check-expect (n 10 9 2014) 627)

(: j : Integer Integer -> Integer)
;; [AUX 2] dish out the "month adjustments" given in the table on the lab page
;; based on the month and year (does not need day)
(define (j month year)
  (cond
    [(and (= month 1) (equal? (leap? year) #t)) 0]
    [(and (= month 2) (equal? (leap? year) #t)) 3]
    [(= month 1) 1]
    [(= month 2) 4]
    [(= month 3) 4]
    [(= month 4) 0]
    [(= month 5) 2]
    [(= month 6) 5]
    [(= month 7) 0]
    [(= month 8) 3]
    [(= month 9) 6]
    [(= month 10) 1]
    [(= month 11) 4]
    [(= month 12) 6]
    [else 9001]))  ; with a realistic input, this should never be triggered
;; [AUX 2] TESTS
(check-expect (j 1 2014) 1)
(check-expect (j 2 2014) 4)
(check-expect (j 1 2008) 0)
(check-expect (j 2 2008) 3)


(: date->string : Integer Integer Integer -> String)
;; generate a conventional representation of a date from given
;; month, day, year
(define (date->string month day year)
  (cond
    [(equal? (date-valid? month day year) #t)
     (string-append (day-of-week month day year)
                    " "
                    (number->string day)
                    " "
                    (month-abbr month)
                    " "
                    (number->string year))]
    [else "[invalid date]"]))
;; TESTS
(check-expect (date->string 10 8 2014) "Wednesday 8 Oct 2014")
(check-expect (date->string 10 9 2014) "Thursday 9 Oct 2014")
(check-expect (date->string 9 31 2014) "[invalid date]")
(check-expect (date->string 2 29 2014) "[invalid date]")

(: month-abbr : Integer -> String)
;; [AUX] output an abbreviated version of a month's name from the month number
(define (month-abbr month)
  (cond
    [(= month 1) "Jan"]
    [(= month 2) "Feb"]
    [(= month 3) "Mar"]
    [(= month 4) "Apr"]
    [(= month 5) "May"]
    [(= month 6) "Jun"]
    [(= month 7) "Jul"]
    [(= month 8) "Aug"]
    [(= month 9) "Sep"]
    [(= month 10) "Oct"]
    [(= month 11) "Nov"]
    [(= month 12) "Dec"]
    [else "This is not a month and you should not be getting this error."]))  ; with a realistic input, this should never be triggered
;; [AUX] TESTS
(check-expect (month-abbr 10) "Oct")
(check-expect (month-abbr 11) "Nov")

(test)

;; GRADER TESTS ;;

(check-expect (leap? 1900) #f)
(check-expect (leap? 1904) #t)
(check-expect (leap? 1600) #t)
(check-expect (leap? 1601) #f)
(check-expect (leap? 2014) #f)

(check-expect (date-valid? 13 13 2013) #f)
(check-expect (date-valid? 1 32 2013) #f)
(check-expect (date-valid? 2 29 2013) #f)
(check-expect (date-valid? 2 28 1899) #f)
(check-expect (date-valid? 2 28 2013) #t)

(check-expect (day-of-week 1 1 2000) "Saturday")
(check-expect (day-of-week 4 8 1980) "Tuesday")
(check-expect (day-of-week 10 20 2014) "Monday")
(check-expect (day-of-week 3 9 2013) "Saturday")
(check-expect (day-of-week 2 28 1900) "Wednesday")
(check-expect (day-of-week 2 29 2000) "Tuesday")

(check-expect (date->string 1 1 2000) "Saturday 1 Jan 2000")
(check-expect (date->string 4 8 1980) "Tuesday 8 Apr 1980")
(check-expect (date->string 10 20 2014) "Monday 20 Oct 2014")
(check-expect (date->string 3 9 2013) "Saturday 9 Mar 2013")
(check-expect (date->string 9 32 1920) "[invalid date]")
(check-expect (date->string 2 10 1) "[invalid date]")
(check-expect (date->string 2 29 1900) "[invalid date]")
(check-expect (date->string 2 28 1900) "Wednesday 28 Feb 1900")
(check-expect (date->string 2 29 2000) "Tuesday 29 Feb 2000")

(test)

;; evaluation

;; === correctness ===

;; leap?         6/ 6
;; date-valid?   8/10
;; day-of-week  16/16
;; date->string 12/12

;; === style ===

;; code layout                       7/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)   4/ 4

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                   97/ 100

;; graded by olsona