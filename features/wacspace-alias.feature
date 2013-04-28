Feature: Use aliases to organize similar modes
  In order to always have a nice workspace
  As an Emacs user
  I want to use wacspace aliases

  Background: Config with aliases
    When I load the following:
    """
    (defwacspace (ruby-mode rinari-minor-mode)
      (:default
       (:winconf 3winv)
       (:aux1 (:buffer "*rails console*"))
       (:aux2 (:cmd eshell))))

     (defwacspace (js-mode)
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd eshell))))

     (defwacsalias (js-mode rinari-minor-mode)
      (ruby-mode rinari-minor-mode))
     """

   Scenario: Without the alias
     When I am in buffer "*js*" in javascript-mode
     And I press "C-c w"
     Then there should be 2 windows
     And the 2nd window should be in buffer "*eshell*"

   Scenario: With the alias
     When I am in buffer "*js*" in javascript-mode
     And I turn on rinari-minor-mode
     And I press "C-c w"
     Then there should be 3 windows
     And the 2nd window should be in buffer "*rails console*"
     And the 3rd window should be in buffer "*eshell*"

   Scenario: When define a similar wacspace it overrides the alias
     When I load the following:
     """
     (defwacspace (js-mode rinari-minor-mode)
      (:default
       (:winconf 2winv)
       (:aux1 (:cmd eshell))))
     """
     When I am in buffer "*js*" in javascript-mode
     And I turn on rinari-minor-mode
     And I press "C-c w"
     Then there should be 2 windows
     And the 2nd window should be in buffer "*eshell*"
