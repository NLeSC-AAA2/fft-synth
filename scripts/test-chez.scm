#!/usr/bin/env scheme-script

(import (rnrs (6))

        (format)
        (utility cut)

        (only (chezscheme)
              file-regular? file-directory? directory-list
              directory-separator)

        (testing run)
        (testing suite)
        (testing reports)
        (testing private generic))

#| Join paths
 |#
(define (path-join first . rest)
  (let ((sep (make-string 1 (directory-separator))))
    (if (null? rest)
      first
      (apply path-join (string-append first sep (car rest)) (cdr rest)))))

#| Scan path recursively through folders matching dir? for files
 | matching file?.
 |#
(define (recursive-directory-list file? dir? path)
  (let ((ls    (map (cut path-join path <>)
                    (directory-list path)))
        (recur (cut recursive-directory-list file? dir? <>)))
    (apply append (filter file? ls) (map recur (filter dir? ls)))))

#| Find all tests in a path.
 |#
(define (find-tests path)
  (recursive-directory-list
    (lambda (f)
      (and (file-regular? f)
           (string-ends-with? ".scm" f)
           (string-starts-with? "test" f)))
    (lambda (f)
      (and (file-directory? f)))
           ; (not (string-starts-with? "." f))))
    path))

(let* ((path   (cadr (command-line)))
       (files  (find-tests path))
       (suites (apply append (map file->suites files))))
  (println "Found {} test suites." (length suites))
  (let* ((reports  (map (lambda (s)
                          (let ((r (run-suite s)))
                            (newline)
                            r)) suites))
         (n-failed (apply + (map report-n-failures reports)))
         (n-passed (apply + (map report-n-successes reports))))
    (println "{} passed, {} failed" n-passed n-failed)
    (if (zero? n-failed)
      (exit 0)
      (exit 1))))

; vim:ft=scheme
