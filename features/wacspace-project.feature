Feature: Use wacspace to manage projects
  In order to keep my projects organized
  As an Emacs user
  I want to use wacspace

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
    And I am visiting the project file "main.el" in emacs-lisp-mode
    And I press "C-z C-w"
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
    And I am visiting the project file "subdir/main.el" in emacs-lisp-mode
    And I press "C-z C-w"
    And I switch to the next window
    Then I should be in buffer "*eshell*<wacsproject>"
    And the current directory should be the base directory
    When I am visiting the project file "other.el" in emacs-lisp-mode
    And I press "C-z C-w"
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
    And I am visiting the project file "subdir/main.el" in emacs-lisp-mode
    And I press "C-z C-w"
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
    And I am visiting the project file "main.el" in emacs-lisp-mode
    And I press "C-z C-w"
    Then the 2nd window should be in buffer "*eshell*<coolproj>"

  Scenario: Quickly switch between projects
    When I load the following:
    """
    (defwacspace (emacs-lisp-mode)
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd wacs-eshell))))
    """
    And I am visiting the project file "main.el" in emacs-lisp-mode
    And I press "C-z C-w"
    And I am in the project "otherproject"
    And I am visiting the project file "other.el" in emacs-lisp-mode
    And I press "C-z C-w"
    Then the 2nd window should be in buffer "*eshell*<otherproject>"
    When I start an action chain
    And I press "C-z C-p"
    And I type "wacsproject"
    And I execute the action chain
    Then I should be in buffer "main.el"
    And the 2nd window should be in buffer "*eshell*<wacsproject>"

  Scenario: Quickly switching saves the last-used prefix key
    When I load the following:
    """
    (defwacspace (emacs-lisp-mode)
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd wacs-eshell)))
      (:1
       (:aux1 (:buffer "*scratch*"))))
    """
    And I am visiting the project file "main.el" in emacs-lisp-mode
    And I press "C-z C-1"
    And I press "C-z C-w"
    And I press "C-z C-1"
    Then the 2nd window should be in buffer "*scratch*"
    And I am in the project "otherproject"
    And I am visiting the project file "other.el" in emacs-lisp-mode
    And I press "C-z C-w"
    And I start an action chain
    And I press "C-z C-p"
    And I type "wacsproject"
    And I execute the action chain
    Then I should be in buffer "main.el"
    And the 2nd window should be in buffer "*scratch*"

  Scenario: Using the :before-switch option
    When I load the following:
    """
    (setq switch-count 0)

    (defwacspace (octave-mode)
      (:before-switch (lambda () (incf switch-count)))
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd wacs-eshell))))
     """
     And I am visiting the project file "main.octave" in octave-mode
     And I press "C-z C-w"
     Then the numeric value of switch-count should be 1
     When I am in the project "otherproject"
     And I am visiting the project file "other.octave" in octave-mode
     And I press "C-z C-w"
     Then the numeric value of switch-count should be 2
     When I start an action chain
     And I press "C-z C-p"
     And I type "wacsproject"
     And I execute the action chain
     Then the numeric value of switch-count should be 3
     When I start an action chain
     And I press "C-z C-p"
     And I type "otherproject"
     And I execute the action chain
     Then the numeric value of switch-count should be 4
