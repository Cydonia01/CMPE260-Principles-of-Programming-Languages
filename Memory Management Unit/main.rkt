; mehmet ali ozdemir
; 2021400000
; compiling: yes
; complete: yes

#lang scheme

;; Function 1: Converts binary string to decimal
(define (binary_to_decimal binary)
    (string->number binary 2))

;; Function 2: Converts logical addresses to physical addresses
(define (relocation_mapping args limit base)
   (cond
     ((null? args) '()) ;; Base case: if the argument list is empty, return empty list
     (else
       (cond
         ((> (binary_to_decimal (car args)) limit) ;; If logical address exceeds limit, add -1 to list
          (cons -1 (relocation_mapping (cdr args) limit base)))
         (else (cons (+ base (binary_to_decimal (car args))) ;; Otherwise, add base to logical address and add to list
                     (relocation_mapping (cdr args) limit base)))))))

;; Function 3: Divides a logical address to page number and page offset according to given page_size
(define (divide_address_space num page_size)
  (cons (substring num 0 (page_number_bits num page_size))
        (list (substring num (page_number_bits num page_size)))))

;; Helper function to find the base 2 logarithm of a number
(define (log_base_2 n)
  (inexact->exact (/ (log n) (log 2))))

;; Helper function to find the length of the page number
(define (page_number_bits args page_size)
  (- (string-length args) (+ 10 (log_base_2 page_size))))

;; Function 4: Returns the list of physical addresses of the given logical adresses using page_table and page_size
(define (page args page_table page_size)
  (cond
    ((null? args) '())
    (else (let ((new_bits (list-ref page_table (binary_to_decimal (substring (car args) 0 (page_number_bits (car args) page_size)))))) ;; find corresponding bits to replace the page number
               (cons (replace_bits (car args) (page_number_bits (car args) page_size) new_bits) (page (cdr args) page_table page_size)))))) ;; construct list of physical addresses recursively

;; Helper function to replace the page number with the new bits
(define (replace_bits binary_string num_of_bits new_bits)
  (string-append new_bits (substring binary_string num_of_bits (string-length binary_string))))

;; Function 5: Finds the sine of an angle given in num by using the Taylor series expansion, up to a predefined number sum
(define (find_sin value num)
  (cond
    ((even? num) (- (compute_terms value num) (compute_terms value (- num 1))))
    (else (- (compute_terms value (- num 1)) (compute_terms value num)))))

;; Helper function to find the factorial of a number
(define (factorial n)
  (cond
    ((<= n 1) 1)
    (else (* n (factorial (- n 1))))))

;; Helper function to compute the terms of the Taylor series expansion. It computes either positive ones or negative ones
(define (compute_terms value num)
  (cond
    ((< num 0) 0)
    (else (let ((x_term (expt (* value (/ pi 180)) (+ 1 (* 2 num))))) ;; Calculate the x term
               (+ (/ x_term (factorial (+ 1 (* 2 num)))) ;; Divides x term with factorial and sums it with other terms recursively 
                  (compute_terms value (- num 2)))))))

;; Function 6: Returns the hash value of a given binary number.
(define (myhash arg table_size)
  (let ((number (binary_to_decimal arg)))
       (remainder (sum_digits (find_sin number (remainder number 5))) table_size))) ;; Sums first ten digits after decimal point and takes mod of table_size

;; Helper function to convert digits to list
(define (convert_digits_to_list n)
  (cond
    ((= n 0) '())
    (else (append (convert_digits_to_list (quotient n 10)) ;; Eliminates the least significant digit
                  (list (inexact->exact (remainder n 10))))))) ;; Appends the least significant digit

;; Helper function to find the first 10 digits after decimal point
(define (digits_after_decimal_point number)
  (let ((new_number (inexact->exact (floor (* number (expt 10 10))))))
       (convert_digits_to_list (remainder new_number (expt 10 10)))))

;; Helper function to sum digits
(define (sum_digits number)
  (apply + (digits_after_decimal_point number)))

;; Function 7: Returns the physical adress for a given logical addres args by using hashed page table
(define (hashed_page args table_size page_table page_size)
  (let ((page_number (substring args 0 (page_number_bits args page_size)))) ;; Finds the page number
       (replace_bits args (page_number_bits args page_size) ;; Concatenates the frame number with the page offset
                          (compare_heads page_number (list-ref page_table (myhash page_number table_size))))))

;; Helper function to find the fram number. It compares the hashed page number with the heads of the lists at the corresponding hash table index.
(define (compare_heads page_number inner_page_table)
  (cond
    ((equal? page_number (caar inner_page_table)) (cadar inner_page_table)) ;; If there is a match return the tail of the list (frame number)
    (else (compare_heads page_number (cdr inner_page_table))))) ;; Else compare with the head of next list

;; Function 8: Returns a list of logical addresses splitting the stream of logical addresses by the size
(define (split_addresses args size)
  (cond
    ((equal? args "") '())
    (else (cons (list (substring args 0 size)) (split_addresses (substring args size) size)))))

;; Function 9: Returns a list of physical adresses for a stream of logical addresses by using a hashed page table.
(define (map_adresses args table_size page_table page_size space_size)
  (cond
    ((equal? args "") '())
    (else (cons (hashed_page (substring args 0 space_size) table_size page_table page_size)
                (map_adresses (substring args space_size) table_size page_table page_size space_size)))))