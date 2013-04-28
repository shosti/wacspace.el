Feature: Set up workspace
  In order to efficiently develop in Ruby with or without Rails
  As a Rails developer
  I want to use wacspace

  Background:
    When I load the following:
    """
    (defwacspace (ruby-mode rinari-minor-mode)
      (:default
       (:winconf 3winv)
       (:aux1 (:buffer "*rails console*"))
       (:aux2 (:cmd eshell)))
      (:1
       (:frame full))
      (:2
       (:winconf 2winh)
       (:frame left))
      (:3
       (:winconf 2winh)
       (:frame right)
       (:main (:buffer "*rails console*"))
       (:aux1 (:buffer :main))))

    (defwacspace (ruby-mode)
      (:before run-ruby)
      (:default
       (:winconf 2winv)
       (:aux1 (:buffer "*ruby*")))
      (:1
       (:frame full))
      (:2
       (:winconf 2winh)
       (:frame left))
      (:3
       (:winconf 2winh)
       (:frame right)
       (:main (:buffer "*ruby*"))
       (:aux1 (:buffer :main))))

    (defwacspace (:default)
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd shell))))

    (defwacspace (:default rinari-minor-mode)
      (:default
       (:winconf 3winv)
       (:aux1 (:cmd eshell))
       (:aux2 (:cmd shell))))
    """
    And I am in buffer "*main*" in ruby-mode

  Scenario: wacspace in rinari-mode
    When I turn on rinari-minor-mode
    And I press "C-c w"
    Then there should be 3 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*rails console*"
    And the 3rd window should be in buffer "*eshell*"

  Scenario: wacspace in rinari-mode half-screen
    When I turn on rinari-minor-mode
    And I press "C-3 C-c w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the frame should be right aligned
    And the 1st window should be in buffer "*rails console*"

  Scenario: wacspace in ruby-mode without rinari
    When I turn off rinari-minor-mode
    And I press "C-c w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: wacspace with regexp buffer matching
    When I load the following:
    """
    (defwacspace (ruby-mode)
        (:before run-ruby)
        (:default
         (:winconf 2winv)
         (:aux1 (:buffer "*ruby"))))

     (setq wacs-regexp-buffer-switching t)
     """
     And I am in buffer "*main*" in ruby-mode
     And I press "C-c w"
     Then there should be 2 windows
     And I should be in buffer "*main*"
     And the 2nd window should be in buffer "*ruby*"

   Scenario: :var syntax for auxiliary condition
     When I load the following:
     """
     (setq foo t)

     (defwacspace (ruby-mode (:var foo))
       (:default
        (:winconf 2winv)
        (:aux1 (:buffer "*baz*"))))
     """
     And I am in buffer "*main*" in ruby-mode
     And I press "C-c w"
     Then there should be 2 windows
     And I should be in buffer "*main*"
     And the 2nd window should be in buffer "*baz*"

   Scenario: :fn syntax for auxiliary condition
     When I load the following:
     """
     (defun in-foo-buffer ()
       (equal (buffer-name) "*foo*"))

     (defwacspace (ruby-mode (:fn in-foo-buffer))
       (:default
        (:winconf 2winv)
        (:aux1 (:buffer "*bar*"))))
     """
     And I am in buffer "*foo*" in ruby-mode
     And I press "C-c w"
     Then there should be 2 windows
     And I should be in buffer "*foo*"
     And the 2nd window should be in buffer "*bar*"
     When I switch to buffer "*main*"
     And I press "C-c w"
     Then there should be 2 windows
     And the 2nd window should be in buffer "*ruby*"

   Scenario: Default configuration
     When I am in buffer "*py*" in python-mode
     And I press "C-c w"
     Then there should be 2 windows
     And I should be in buffer "*py*"
     And the 2nd window should be in buffer "*shell*"

   Scenario: Default configuration with auxiliary condition
     When I am in buffer "*html*" in html-mode
     And I turn on rinari-minor-mode
     And I press "C-c w"
     Then there should be 3 windows
     And I should be in buffer "*html*"
     And the 2nd window should be in buffer "*eshell*"
     And the 3rd window should be in buffer "*shell*"
