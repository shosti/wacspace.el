Feature: Set up Ruby/Rails workspace
  In order to efficiently develop in Ruby with or without Rails
  As a Rails developer
  I want to use wacspace

  Background:
    When I load the following:
    """
    (defwacspace ruby-mode :cond rinari-minor-mode
      (:before 'rinari)
      (:default
       (:winconf '3win)
       (:aux1 (:buffer "*rails console*"))
       (:aux2 'eshell))
      (:1
       (:frame :full))
      (:2
       (:winconf '2winh)
       (:frame :left))
      (:3
       (:winconf '2winh)
       (:frame :right)
       (:main (:buffer "*rails console*"))
       (:aux1 (:buffer :main))))

    (defwacspace ruby-mode
      (:before 'run-ruby)
      (:default
       (:winconf '2winv)
       (:aux1 (:buffer "*ruby*")))
      (:1
       (:frame :full))
      (:2
       (:winconf '2winh)
       (:frame :left))
      (:3
       (:winconf '2winh)
       (:frame :right)
       (:main (:buffer "*ruby*"))
       (:aux1 (:buffer :main))))
    """

  Scenario: wacspace in rinari-mode
    When I am in buffer "*main*"
    And I turn on ruby-mode
    And I run rinari-launch
    And I press "C-c C-w"
    Then there should be 3 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*rails console*"
    And the 3rd window should be in buffer "*eshell*"

  Scenario: wacspace in rinari-mode half-screen
    When I am in buffer "*main*"
    And I turn on ruby-mode
    And I run rinari-launch
    And I press "C-3 C-c C-w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the 1st window should be in buffer "*rails console*"

  Scenario: wacspace in ruby-mode without rinari
    When I am in buffer "*main*"
    And I turn on ruby-mode
    And I turn off rinari-minor-mode
    And I press "C-c C-w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"
