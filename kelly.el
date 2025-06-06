;;; kelly.el --- Elisp Kelly criterion calculator -*- lexical-binding: t -*-

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

(defcustom kelly-b-parameter-format 'fractional-odds
  "Specifies the format of the `b' parameter.
Four formats are supported:

- `fractional-odds' (default): fractional (British) odds (e.g. `3').
- `decimal-odds': decimal (European) odds (e.g. `4').
- `moneyline-odds': moneyline (American) odds (e.g. `300').
- `implied-probability': implied betting odds probability (e.g. `0.25').
- `implied-percent': implied betting odds percentage (e.g. `25%')."
  :type '(choice (const :tag "Fractional (British) odds" fractional-odds)
		 (const :tag "Decimal (European) odds" decimal-odds)
		 (const :tag "Moneyline (American) odds" moneyline-odds)
		 (const :tag "Implied probability" implied-probability)
		 (const :tag "Implied percent" implied-percent))
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
         (kelly (kelly-calculate p b))
         (bankroll (kelly-get-bankroll)))
    (if (< kelly 0)
        (message "Kelly fraction is negative (%f). No wager is recommended." kelly)
      (message "%s %s"
	       (kelly-format-wager-amount kelly bankroll)
	       (kelly-format-expected-profit p b kelly bankroll)))))

(defun kelly-calculate (p b)
  "Calculate the Kelly criterion for a bet with parameters P and B.
P is the probability of a win. B is the net odds received on a win.

The Kelly criterion is computed using the formula:

    f = ((b + 1) × p – 1) / b

and then scaled by `kelly-fraction'."
  (let ((kelly (/ (- (* (+ 1 b) p) 1) b)))
    (* kelly kelly-fraction)))

;;;;; Format strings

(defun kelly-format-wager-amount (kelly bankroll)
  "Return a string with the amount to wager.
KELLY is the Kelly criterion. BANKROLL is the amount of capital you are willing
to risk."
  (let ((wager-amount (kelly-get-wager-amount kelly bankroll))
	(percent-of-bankroll (* kelly 100)))
    (format "Amount to wager: %s%.2f (%.2f%% of bankroll)."
	    kelly-bankroll-currency wager-amount percent-of-bankroll)))

(defun kelly-format-expected-profit (p b kelly bankroll)
  "Return a string with the expected net profit of a bet with parameters P and B.
P is the probability of a win. B is the net odds received on a win. KELLY is the
Kelly criterion. BANKROLL is the amount of capital you are willing to risk."
  (let* ((expectation (kelly-get-expectation p b))
	 (expected-profit (* expectation kelly bankroll))
	 (wager-amount (kelly-get-wager-amount kelly bankroll))
	 (return-on-investment (* (/ expected-profit wager-amount) 100)))
    (format "Expected net profit: %s%.2f (%.2f%% return on investment)."
	    kelly-bankroll-currency expected-profit return-on-investment)))

;;;;; Read numbers

(defun kelly-read-probability-number (prompt)
  "Prompt the user for a number higher than 0 and lower than 1.
PROMPT is the prompt message."
  (let ((prob (read-number prompt)))
    (while (or (<= prob 0) (>= prob 1))
      (setq prob (read-number "Please enter a number higher than 0 and lower than 1: ")))
    prob))

;;;;; Read odds

(defun kelly-read-fractional-odds ()
  "Read and return fractional odds."
  (let ((odds (read-number "Fractional odds: ")))
    (while (not (> odds 0))
      (setq odds (read-number "Please enter a positive number: ")))
    odds))

(defun kelly-read-decimal-odds ()
  "Read decimal odds and return it as fractional odds."
  (let ((odds (read-number "Decimal odds: ")))
    (while (not (> odds 0))
      (setq odds (read-number "Please enter a positive number: ")))
    (- odds 1)))

(defun kelly-read-moneyline-odds ()
  "Read moneyline odds and return it as fractional odds."
  (let ((odds (read-number "Moneyline odds: ")))
    (while (zerop odds)
      (setq odds (read-number "Please enter a positive or a negative number: ")))
    (if (< odds 0)
	(* (/ 100.0 odds) -1)
      (/ odds 100.0))))

(defun kelly-read-implied-probability ()
  "Read implied probability and return it as fractional odds."
  (let ((prob (kelly-read-probability-number "Implied probability: ")))
    (/ (- 1 prob) prob)))

(defun kelly-read-implied-percent ()
  "Read implied percentage and return it as fractional odds."
  (let* ((percent (string-to-number (read-string "Implied percentage: "))))
    (while (or (<= percent 0) (>= percent 100))
      (setq percent (string-to-number (read-string "Please enter a number higher than 0 and lower than 100: "))))
    (/ (- 100 percent) percent)))

;;;;; Read parameters

(defun kelly-read-p ()
  "Read the `p' parameter of the Kelly criterion."
  (kelly-read-probability-number "Win probability: "))

(defun kelly-read-b ()
  "Read the `b' parameter of the Kelly criterion."
  (pcase kelly-b-parameter-format
    ('fractional-odds (kelly-read-fractional-odds))
    ('decimal-odds (kelly-read-decimal-odds))
    ('moneyline-odds (kelly-read-moneyline-odds))
    ('implied-probability (kelly-read-implied-probability))
    ('implied-percent (kelly-read-implied-percent))
    (_ (user-error (concat "Invalid `kelly-b-parameter-format': must be `fracional-odds', "
			   "`decimal-odds', `moneyline-odds', `implied-probability', or "
			   "`implied-percent'.")))))

;;;;; Misc

(defun kelly-get-expectation (p b)
  "Compute the expected profit multiplier for a wager.
Given a win probability P and net odds B, return:

    E = p × b – (1 – p)

which represents the net profit per unit wagered."
  (- (* p b) (- 1 p)))

(defun kelly-get-wager-amount (kelly bankroll)
  "Return the amount to wager, given KELLY and BANKROLL."
  (* kelly bankroll))

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
