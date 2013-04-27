Feature: Use configuration functions with dynamic vars
  In order to set up my workspace with the correct paths
  As an Emacs user
  I want to use wacspace's built-in dynamic vars and functions

  Background: In project directory
    When I am in the project "wacsproject"

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
    Then I should be in buffer "*eshell*<wacsproject>"
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
    Then I should be in buffer "*eshell*<wacsproject>"
    And the current directory should be the base directory
    When I visit the file "other.el"
    And I press "C-c w"
    And I switch to the next window
    Then I should be in buffer "*eshell*<wacsproject>"


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
    Then I should be in buffer "*shell*<wacsproject>"
    And the current directory should be the base directory
    And the value of wacs-project-base-file should be ".project"

  Scenario: Using the project name option
    When I load the following:
    """
    (defwacspace (emacs-lisp-mode)
      (:project-name-fn (lambda ()
                          "coolproj"))
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd wacs-eshell))))
    """
    And I visit the file "main.el"
    And I save the buffer
    And I turn on emacs-lisp-mode
    And I press "C-c w"
    Then the 2nd window should be in buffer "*eshell*<coolproj>"
