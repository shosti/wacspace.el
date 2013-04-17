Feature: Use configuration functions with dynamic vars
  In order to set up my workspace with the correct paths
  As an Emacs user
  I want to use wacspace's built-in dynamic vars and functions

  Scenario: Eshell in same directory
    When I load the following:
    """
    (defwacspace (emacs-lisp-mode)
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd wacs-eshell))))

    (setq wacs-project-base-file nil)
    """
    And I visit the file "main.el"
    And I save the buffer
    And I turn on emacs-lisp-mode
    And I press "C-c w"
    And I switch to the next window
    Then I should be in the project eshell buffer
    And the current directory should be the base directory

  Scenario: Eshell in project directory
    When I load the following:
    """
    (defwacspace (emacs-lisp-mode)
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd wacs-eshell))))

    (setq wacs-project-base-file ".git")
    """
    And I create the directory "subdir"
    And I create the directory ".git"
    And I visit the file "subdir/main.el"
    And I save the buffer
    And I turn on emacs-lisp-mode
    And I press "C-c w"
    And I switch to the next window
    Then I should be in the project eshell buffer
    And the current directory should be the base directory

  Scenario: Shell in project directory
    When I load the following:
    """
    (defwacspace (emacs-lisp-mode)
      (:base-file ".project")
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd wacs-shell))))
    """
    And I create the directory "subdir"
    And I create the directory ".project"
    And I visit the file "subdir/main.el"
    And I save the buffer
    And I turn on emacs-lisp-mode
    And I press "C-c w"
    And I switch to the next window
    Then I should be in shell-mode
    And the current directory should be the base directory

  Scenario: Two files in the same project should share the same eshell buffer
    When I load the following:
    """
    (defwacspace (emacs-lisp-mode)
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd wacs-eshell))))

    (setq wacs-project-base-file ".git")
    """
    And I create the directory "subdir"
    And I create the directory ".git"
    And I visit the file "subdir/main.el"
    And I save the buffer
    And I press "C-c w"
    And I switch to the next window
    Then I should be in the project eshell buffer
    When I visit the file "other.el"
    And I press "C-c w"
    And I switch to the next window
    Then I should be in the project eshell buffer
