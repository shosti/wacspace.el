Feature: Save/restore workspace
  In order to not have to re-set up my workspace
  As an Emacs user
  I want to use wacspace

  Background:
    When I load the following:
    """
    (defwacspace ruby-mode
      (:before run-ruby)
      (:default
       (:winconf 2winv)
       (:aux1 "*ruby*"))
      (:1
       (:frame full))
      (:2
       (:winconf 2winh)
       (:frame left))
      (:3
       (:winconf 2winh)
       (:frame right)
       (:main "*ruby*")
       (:aux1 :main))
      (:4
       (:winconf 1win)))

    (defwacspace inf-ruby-mode
     (:default
      (:winconf 1win))
     (:1))
    """
    And I am in buffer "*main*"
    And I turn on ruby-mode
    And I press "C-z C-w"

  Scenario: Restore workspace without saving
    When I press close the current window
    And I press "C-z C-w"
    Then there should be 2 windows
    And I should be in buffer "*ruby*"
    And the 1st window should be in buffer "*main*"

  Scenario: Save and restore
    When I switch to the next window
    And I split the window vertically
    And I switch to the next window
    And I run eshell
    And I press "C-z C-s"
    And I press "C-z C-w"
    Then there should be 3 windows
    And I should be in buffer "*eshell*"
    And the 1st window should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: Killing the buffer means no window restoring
    When I switch to the next window
    And I split the window vertically
    And I press "C-z C-s"
    And I kill the current buffer
    And I split the window horizontally
    And I switch to buffer "*main*"
    And I press "C-z C-w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: Saving and restoring numbered workspaces
    When I switch to the next window
    And I split the window vertically
    And I switch to the next window
    And I run eshell
    And I press "C-1 C-z C-s"
    And I split the window horizontally
    And I press "C-z C-1"
    Then there should be 3 windows
    And I should be in buffer "*eshell*"
    And the 1st window should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: Saving with a numbered workspace shouldn't affect other workspaces
    When I switch to the next window
    And I split the window vertically
    And I switch to the next window
    And I run eshell
    And I press "C-1 C-z C-s"
    And I switch to the next window
    And I press "C-z C-w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: Save and restore without a wacspace configuration
    When I turn on python-mode
    And I switch to the next window
    And I run eshell
    And I switch to the next window
    And I press "C-z C-s"
    And I split the window horizontally
    And I press "C-z C-w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*eshell*"

  Scenario: Prefixes don't affect each other
    When I switch to the next window
    And I split the window vertically
    And I switch to the next window
    And I switch to the next window
    And I press "C-z C-s"
    And I press "C-z C-2"
    And I press "C-z C-w"
    Then there should be 3 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: Restoring a window should not affect the cursor position
    When I switch to the next window
    And I switch to buffer "*something-else*"
    And I press "C-z C-s"
    And I insert "Hello how are you today?"
    And I press "M-b"
    And I press "C-z C-w"
    Then there should be 2 windows
    And I should be in buffer "*something-else*"
    And the 1st window should be in buffer "*main*"
    And the cursor should be after "you "

  Scenario: Clearing saved buffers
    When I switch to the next window
    And I switch to buffer "*something-else*"
    And I press "C-z C-s"
    And I start an action chain
    And I press "M-x"
    And I type "wacs-clear-all-saved"
    And I execute the action chain
    And I switch to the next window
    And I press "C-z C-w"
    Then there should be 2 windows
    And I should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: Prefix forces re-se-up
    When I switch to the next window
    And I switch to buffer "*scratch*"
    And I press "C-z C-s"
    And I switch to the next window
    And I press "C-z C-w"
    Then I should be in buffer "*main*"
    And the 2nd window should be in buffer "*scratch*"
    When I press "C-u C-z C-w"
    Then I should be in buffer "*main*"
    And the 2nd window should be in buffer "*ruby*"

  Scenario: Wacspace only saves relevant windows
    When I press "C-z C-4"
    Then there should be 1 window
    And I should be in buffer "*main*"
    When I switch to buffer "*ruby*"
    And I press "C-z C-1"
    Then there should be 1 window
    And I should be in buffer "*ruby*"
