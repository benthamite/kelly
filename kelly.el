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
  (let ((wager-amount (* kelly (kelly-get-or-set-bankroll)))
	(percent-of-bankroll (* kelly 100)))
    (format "Amount to wager: %s%f (%f%% of bankroll)."
	    kelly-bankroll-currency wager-amount percent-of-bankroll)))

(defun kelly-format-expected-profit (p b)
  "Return a string with the expected net profit of a bet with parameters P and B.
P is the probability of a win. B is the net odds received on a win."
  (let* ((kelly (kelly-calculate p b))
	 (expectation (kelly-get-expectation p b))
	 (expected-profit (* kelly expectation (kelly-get-or-set-bankroll)))
	 (return-on-investment (* expectation 100)))
    (format "Expected net profit: %s%f (%f%% return on investment)."
	    kelly-bankroll-currency expected-profit return-on-investment)))

;;;;; Read parameters

(defun kelly-read-p ()
  "Read the `p' parameter of the Kelly criterion."
  (kelly-read-probability 'win))

(defun kelly-read-b ()
  "Read the `b' parameter of the Kelly criterion."
  (pcase kelly-b-parameter-format
    ('odds
     (kelly-read-odds))
    ('probability
     (let ((prob (kelly-read-probability 'payout)))
       (/ prob (- 1 prob))))
    (_ (user-error "Invalid `kelly-b-parameter-format': must be `odds' or `probability'"))))

;;;;; Read numbers

(defun kelly-read-odds ()
  "Prompt the user for net odds (a positive number).
If the input is not positive, keep prompting until it is."
  (let ((value (read-number "Betting odds: ")))
    (while (not (> value 0))
      (setq value (read-number "Please enter a positive number: ")))
    value))

(defun kelly-read-probability (type)
  "Prompt the user for a probability between 0 and 1.
TYPE is the type of probability to be prompted: either `win' or `payout'. If the
input is not within the valid range, keep prompting until it is."
  (let ((value (read-number (kelly-format-probability-prompt type))))
    (while (or (< value 0) (> value 1))
      (setq value (read-number "Please enter a number between 0 and 1: ")))
    value))

(defun kelly-format-probability-prompt (type)
  "Return a string to prompt the user for a probability.
TYPE is the type of probability to be prompted: either `win' or `payout'."
  (pcase type
    ('win "Estimated win probability: ")
    ;; TODO; improve message (should be equivalent to ‘betting odds’, but for probabilities)
    ('payout  "Payout probability: " )
    (_ (user-error "Invalid probability type: must be `win' or `payout'"))))

;;;;; Misc

(defun kelly-get-expectation (p b)
  "Return the expectation based on parameters P and B."
  (- (* p b) (- 1 p)))

(defun kelly-get-or-set-bankroll ()
  "Return the value of `kelly-bankroll', or prompt the user to set its value."
  (or kelly-bankroll
      (setq kelly-bankroll (read-number "Bankroll: "))))

(provide 'kelly)
;;; kelly.el ends here
