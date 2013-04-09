Feature: Set up workspace
  In order to have a nicely set up workspace
  As an Emacs user
  I want to use wacspace

  Background:
    When I load the following:
    """
    (defwacspace (emacs-lisp-mode)
      (:default
       (:winconf 3winv)))
    """

  Scenario: wacspace in emacs-lisp-mode
    Given I am in buffer "*main*"
    And I close all the other windows
    And I turn on emacs-lisp-mode
    And I press "C-c w"
    Then there should be 3 windows
    And I should be in buffer "*main*"
