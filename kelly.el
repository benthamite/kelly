;;; kelly.el --- Elisp Kelly criterion calculator -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/kelly
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Elisp Kelly criterion calculator.

;;; Code:

;;;; User options

(defgroup kelly nil
  "Settings for calculating the Kelly criterion."
  :group 'applications)

(defcustom kelly-b-parameter-format 'odds
  "Specifies the format of the `b' parameter.
If set to `odds', prompt the user to enter net odds (e.g. \"3\"). If set to
`probability', prompt the user to enter a payout probability (e.g. \"0.75\")."
  :type '(choice (const :tag "Odds" odds)
		 (const :tag "Probability" probability))
  :group 'kelly)

(defcustom kelly-fraction 1
  "A fractional multiplier applied to the computed Kelly fraction.
Default is 1, which means using the full Kelly recommendation.
If set to a value less than 1, the recommended wager is scaled accordingly."
  :type 'number
  :group 'kelly)

(defcustom kelly-bankroll nil
  "The amount of capital you are willing to risk."
  :type 'number
  :group 'kelly)

(defcustom kelly-bankroll-currency "$"
  "The bankroll currency symbol."
  :type 'string
  :group 'kelly)

;;;; Functions

;;;###autoload
(defun kelly ()
  "Display the Kelly criterion for a bet with user-specified parameters."
  (interactive)
  (let* ((p (kelly-read-p))
	 (b (kelly-read-b))
	 (kelly (kelly-calculate p b)))
    (if (< kelly 0)
	(message "Kelly fraction is negative (%f)." kelly)
      (message "%s %s" (kelly-format-wager-amount kelly) (kelly-format-expected-profit p b)))))

(defun kelly-calculate (p b)
  "Calculate the Kelly criterion for a bet with parameters P and B.
P is the probability of a win. B is the net odds received on a win.

The Kelly criterion is computed using the formula:

    f = ((b + 1) × p – 1) / b

and then scaled by `kelly-fraction'."
  (let ((kelly (/ (- (* (+ 1 b) p) 1) b)))
    (* kelly kelly-fraction)))

;;;;; Format strings

(defun kelly-format-wager-amount (kelly)
  "Return a string with the amount to wager.
KELLY is the Kelly criterion."
  (let ((wager-amount (* kelly (kelly-get-bankroll)))
	(percent-of-bankroll (* kelly 100)))
    (format "Amount to wager: %s%.2f (%.2f%% of bankroll)."
	    kelly-bankroll-currency wager-amount percent-of-bankroll)))

(defun kelly-format-expected-profit (p b)
  "Return a string with the expected net profit of a bet with parameters P and B.
P is the probability of a win. B is the net odds received on a win."
  (let* ((kelly (kelly-calculate p b))
	 (expectation (kelly-get-expectation p b))
	 (expected-profit (* kelly expectation (kelly-get-bankroll)))
	 (return-on-investment (- (* expectation 100) 100)))
    (format "Expected net profit: %s%.2f (%.2f%% return on investment)."
	    kelly-bankroll-currency expected-profit return-on-investment)))

;;;;; Read parameters

(defun kelly-read-p ()
  "Read the `p' parameter of the Kelly criterion."
  (kelly-read-probability 'win-probability))

(defun kelly-read-b ()
  "Read the `b' parameter of the Kelly criterion."
  (pcase kelly-b-parameter-format
    ('odds
     (kelly-read-odds))
    ('probability
     (let ((prob (kelly-read-probability 'betting-odds)))
       (/ prob (- 1 prob))))
    (_ (user-error "Invalid `kelly-b-parameter-format': must be `odds' or `probability'"))))

;;;;; Read numbers

(defun kelly-read-odds ()
  "Prompt the user for positive betting odds, and return the valid number."
  (let ((odds (kelly-read-number 'betting-odds)))
    (while (not (> odds 0))
      (setq odds (read-number "Please enter a positive number for the odds: ")))
    odds))

(defun kelly-read-probability (label)
  "Prompt the user for a probability (a number between 0 and 1) using LABEL.
Return a valid probability number."
  (let ((prob (kelly-read-number label)))
    (while (or (< prob 0) (> prob 1))
      (setq prob (read-number "Please enter a number between 0 and 1: ")))
    prob))

(defun kelly-read-number (type)
  "Return a string to prompt the user for a probability.
TYPE is the type of probability to be prompted: either `win-probability' or
`betting-odds'."
  (let ((prompt (pcase type
		  ('win-probability "Win probability: ")
		  ('betting-odds  "Betting odds: ")
		  (_ (user-error "Invalid probability type: must be `win-probability' or `betting-odds'")))))
    (read-number prompt)))

;;;;; Misc

(defun kelly-get-expectation (p b)
  "Compute the expected profit multiplier for a wager.
Given a win probability P and net odds B, return:

    E = p × b – (1 – p)

which represents the net profit per unit wagered."
  (- (* p b) (- 1 p)))

(defun kelly-get-bankroll ()
  "Return the value of `kelly-bankroll'.
If `kelly-bankroll' is nil, prompt the user to set its value before returning
it."
  (or kelly-bankroll
      (setq kelly-bankroll (kelly-read-bankroll))))

(defun kelly-read-bankroll ()
  "Prompt the user for a positive bankroll amount and return it.
Does not modify any global variable."
  (let ((amount (read-number (format "Bankroll (%s): " kelly-bankroll-currency))))
    (while (<= amount 0)
      (setq amount (read-number (format "Please enter a positive bankroll amount (%s): " kelly-bankroll-currency))))
    amount))

(provide 'kelly)
;;; kelly.el ends here
