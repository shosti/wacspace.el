Feature: Save/restore workspace
  In order to not have to re-set up my workspace
  As an Emacs user
  I want to use wacspace

  Background:
    When I load the following:
    """
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
    """
    And I am in buffer "*main*"
    And I turn on ruby-mode
    And I press "C-c w"

  Scenario: Restore workspace without saving
    When I press "C-x 0"
    And I press "C-c w"
    Then there should be 2 windows
    And I should be in buffer "*ruby*"
    And the 1st window should be in buffer "*main*"

  Scenario: Save and restore
    When I press "C-x o"
    And I press "C-x 2"
    And I press "C-x o"
    And I run eshell
    And I press "C-c s"
    And I press "C-c w"
    Then there should be 3 windows
    And I should be in buffer "*eshell*"
    And the 1st window should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"
