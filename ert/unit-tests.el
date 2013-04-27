(ert-deftest wacs--alist-delete ()
  (let ((alist
         '(("a" . 1)
           ("b" . 2)
           ("c" . 3)
           ("b" . 4)))
        (expected-result
         '(("a" . 1)
           ("c" . 3))))
    (wacs--alist-delete "b" alist)
    (should (equal alist
                   expected-result))
    (wacs--alist-delete "d" alist)
    (should (equal alist
                   expected-result))))

(ert-deftest wacs--alist-put ()
  (let ((alist
         '(("a" . 1)
           ("b" . 2)
           ("c" . 3)))
        (expected-result
         '(("b" . 5)
           ("a" . 1)
           ("c" . 3))))
    (wacs--alist-put "b" 5 alist)
    (should (equal alist
                   expected-result))))

(ert-deftest wacs--alist-put-nil ()
  (let ((alist nil)
        (expected-result
         '(("a" . 4)
           ("c" . 5))))
    (wacs--alist-put "a" 3 alist)
    (wacs--alist-put "c" 5 alist)
    (wacs--alist-put "a" 4 alist)
    (should (equal alist
                  expected-result))))

(ert-deftest wacs--alist-get ()
  (let ((alist
         (list (cons "a" '(something cool))
               (cons 'b '(something else))
               (cons 'c 'foo))))
    (should (equal (wacs--alist-get "a" alist)
                   '(something cool)))
    (should (equal (wacs--alist-get 'b alist)
                   '(something else)))
    (should (equal (wacs--alist-get 'c alist)
                   'foo))))

(ert-deftest wacs--update-open-projects ()
  (let ((wacs--open-projects nil)
        (wacs--project-name-fn
         (lambda ()
           "project")))
    (wacs--update-open-projects "buffer1" 1)
    (wacs--update-open-projects "buffer2" nil)
    (wacs--update-open-projects "buffer2" 3)
    (wacs--update-open-projects "buffer1" 2)
    (wacs--update-open-projects "buffer1" 4)
    (wacs--update-open-projects "buffer1" 7)
    (should (equal wacs--open-projects
                   (list (cons "project"
                               '("buffer1" . 7)))))
    (should (equal (wacs--alist-get "project" wacs--open-projects)
                   '("buffer1" . 7)))))
