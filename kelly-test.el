;;; kelly-test.el --- Tests for kelly.el -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;; Author: Pablo Stafforini

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT test suite for kelly.el.

;;; Code:

(require 'ert)
(require 'kelly)

;;;; kelly-calculate

(ert-deftest kelly-test-calculate-standard-case ()
  "A fair coin at 2:1 odds yields f = 0.25."
  (let ((kelly-fraction 1))
    (should (= (kelly-calculate 0.5 2) 0.25))))

(ert-deftest kelly-test-calculate-even-money-60-percent ()
  "60% win probability at even money yields f = 0.20."
  (let ((kelly-fraction 1))
    (should (< (abs (- (kelly-calculate 0.6 1) 0.2)) 1e-10))))

(ert-deftest kelly-test-calculate-negative-expectation ()
  "When the bet has negative expectation, Kelly fraction is negative."
  (let ((kelly-fraction 1))
    (should (< (kelly-calculate 0.3 1) 0))))

(ert-deftest kelly-test-calculate-certain-win ()
  "With probability 1, the full bankroll should be wagered."
  (let ((kelly-fraction 1))
    (should (= (kelly-calculate 1.0 1) 1.0))))

(ert-deftest kelly-test-calculate-high-odds-low-probability ()
  "High odds with low probability: 10% chance at 15:1."
  (let ((kelly-fraction 1))
    (should (< (abs (- (kelly-calculate 0.1 15) 0.04)) 1e-10))))

(ert-deftest kelly-test-calculate-breakeven ()
  "At breakeven (p = 1/(b+1)), Kelly fraction is zero."
  (let ((kelly-fraction 1))
    (should (< (abs (kelly-calculate 0.5 1)) 1e-10))))

(ert-deftest kelly-test-calculate-fractional-kelly ()
  "Half-Kelly scales the result by 0.5."
  (let ((kelly-fraction 0.5))
    (should (= (kelly-calculate 0.5 2) 0.125))))

(ert-deftest kelly-test-calculate-quarter-kelly ()
  "Quarter-Kelly scales the result by 0.25."
  (let ((kelly-fraction 0.25))
    (should (= (kelly-calculate 0.5 2) 0.0625))))

(ert-deftest kelly-test-calculate-zero-odds-error ()
  "Zero odds signals a user error."
  (let ((kelly-fraction 1))
    (should-error (kelly-calculate 0.5 0) :type 'user-error)))

(ert-deftest kelly-test-calculate-invalid-fraction-zero ()
  "Zero `kelly-fraction' signals a user error."
  (let ((kelly-fraction 0))
    (should-error (kelly-calculate 0.5 2) :type 'user-error)))

(ert-deftest kelly-test-calculate-invalid-fraction-negative ()
  "Negative `kelly-fraction' signals a user error."
  (let ((kelly-fraction -1))
    (should-error (kelly-calculate 0.5 2) :type 'user-error)))

(ert-deftest kelly-test-calculate-invalid-fraction-string ()
  "Non-numeric `kelly-fraction' signals a user error."
  (let ((kelly-fraction "bad"))
    (should-error (kelly-calculate 0.5 2) :type 'user-error)))

;;;; kelly-get-expectation

(ert-deftest kelly-test-get-expectation-positive ()
  "Positive expectation for a favorable bet."
  (should (< (abs (- (kelly-get-expectation 0.6 2) 0.8)) 1e-10)))

(ert-deftest kelly-test-get-expectation-negative ()
  "Negative expectation for an unfavorable bet."
  (should (< (kelly-get-expectation 0.3 1) 0)))

(ert-deftest kelly-test-get-expectation-breakeven ()
  "Breakeven bet has zero expectation."
  (should (< (abs (kelly-get-expectation 0.5 1)) 1e-10)))

(ert-deftest kelly-test-get-expectation-certain-win ()
  "Certain win at 3:1 yields expectation equal to odds."
  (should (= (kelly-get-expectation 1.0 3) 3.0)))

(ert-deftest kelly-test-get-expectation-certain-loss ()
  "Zero probability yields expectation of -1."
  (should (= (kelly-get-expectation 0.0 5) -1.0)))

;;;; kelly-get-wager-amount

(ert-deftest kelly-test-get-wager-amount-standard ()
  "Wager amount is Kelly fraction times bankroll."
  (should (= (kelly-get-wager-amount 0.25 1000) 250.0)))

(ert-deftest kelly-test-get-wager-amount-zero-kelly ()
  "Zero Kelly fraction means zero wager."
  (should (= (kelly-get-wager-amount 0 1000) 0)))

(ert-deftest kelly-test-get-wager-amount-full-kelly ()
  "Full Kelly fraction wagers the entire bankroll."
  (should (= (kelly-get-wager-amount 1.0 500) 500.0)))

;;;; kelly-format-wager-amount

(ert-deftest kelly-test-format-wager-amount-contains-currency ()
  "Formatted wager string includes the currency symbol."
  (let ((kelly-bankroll-currency "$"))
    (should (string-match-p "\\$" (kelly-format-wager-amount 0.25 1000)))))

(ert-deftest kelly-test-format-wager-amount-contains-percentage ()
  "Formatted wager string includes the percentage of bankroll."
  (should (string-match-p "25\\.00%" (kelly-format-wager-amount 0.25 1000))))

;;;; kelly-format-expected-profit

(ert-deftest kelly-test-format-expected-profit-contains-currency ()
  "Formatted profit string includes the currency symbol."
  (let ((kelly-bankroll-currency "$"))
    (should (string-match-p "\\$"
			    (kelly-format-expected-profit 0.6 2 0.25 1000)))))

(ert-deftest kelly-test-format-expected-profit-contains-roi ()
  "Formatted profit string includes return on investment."
  (should (string-match-p "return on investment"
			  (kelly-format-expected-profit 0.6 2 0.25 1000))))

;;;; kelly-get-bankroll

(ert-deftest kelly-test-get-bankroll-returns-set-value ()
  "Return `kelly-bankroll' when it is already set."
  (let ((kelly-bankroll 5000))
    (should (= (kelly-get-bankroll) 5000))))

;;;; Odds conversion via kelly-read-* functions

(ert-deftest kelly-test-read-decimal-odds-conversion ()
  "Decimal odds of 4.0 convert to fractional odds of 3.0."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 4.0)))
    (should (= (kelly-read-decimal-odds) 3.0))))

(ert-deftest kelly-test-read-decimal-odds-just-above-one ()
  "Decimal odds of 1.5 convert to fractional odds of 0.5."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 1.5)))
    (should (= (kelly-read-decimal-odds) 0.5))))

(ert-deftest kelly-test-read-moneyline-positive ()
  "Positive moneyline +300 converts to fractional odds 3.0."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 300)))
    (should (= (kelly-read-moneyline-odds) 3.0))))

(ert-deftest kelly-test-read-moneyline-negative ()
  "Negative moneyline -200 converts to fractional odds 0.5."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) -200)))
    (should (= (kelly-read-moneyline-odds) 0.5))))

(ert-deftest kelly-test-read-moneyline-negative-150 ()
  "Negative moneyline -150 converts to fractional odds 2/3."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) -150)))
    (should (< (abs (- (kelly-read-moneyline-odds) (/ 2.0 3))) 1e-10))))

(ert-deftest kelly-test-read-implied-probability-conversion ()
  "Implied probability 0.25 converts to fractional odds 3.0."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 0.25)))
    (should (= (kelly-read-implied-probability) 3.0))))

(ert-deftest kelly-test-read-implied-probability-half ()
  "Implied probability 0.5 converts to fractional odds 1.0."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 0.5)))
    (should (= (kelly-read-implied-probability) 1.0))))

(ert-deftest kelly-test-read-implied-percent-conversion ()
  "Implied percentage 25 converts to fractional odds 3.0."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 25)))
    (should (= (kelly-read-implied-percent) 3.0))))

(ert-deftest kelly-test-read-implied-percent-fifty ()
  "Implied percentage 50 converts to fractional odds 1.0."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 50)))
    (should (= (kelly-read-implied-percent) 1.0))))

(ert-deftest kelly-test-read-fractional-odds-passthrough ()
  "Fractional odds are returned as-is."
  (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 3.0)))
    (should (= (kelly-read-fractional-odds) 3.0))))

;;;; kelly-read-b dispatch

(ert-deftest kelly-test-read-b-fractional ()
  "Dispatch to fractional odds reader."
  (let ((kelly-b-parameter-format 'fractional-odds))
    (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 2.0)))
      (should (= (kelly-read-b) 2.0)))))

(ert-deftest kelly-test-read-b-decimal ()
  "Dispatch to decimal odds reader."
  (let ((kelly-b-parameter-format 'decimal-odds))
    (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 3.0)))
      (should (= (kelly-read-b) 2.0)))))

(ert-deftest kelly-test-read-b-moneyline ()
  "Dispatch to moneyline odds reader."
  (let ((kelly-b-parameter-format 'moneyline-odds))
    (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 200)))
      (should (= (kelly-read-b) 2.0)))))

(ert-deftest kelly-test-read-b-implied-probability ()
  "Dispatch to implied probability reader."
  (let ((kelly-b-parameter-format 'implied-probability))
    (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 0.25)))
      (should (= (kelly-read-b) 3.0)))))

(ert-deftest kelly-test-read-b-implied-percent ()
  "Dispatch to implied percent reader."
  (let ((kelly-b-parameter-format 'implied-percent))
    (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 25)))
      (should (= (kelly-read-b) 3.0)))))

(ert-deftest kelly-test-read-b-invalid-format ()
  "Invalid format signals a user error."
  (let ((kelly-b-parameter-format 'bogus))
    (should-error (kelly-read-b) :type 'user-error)))

;;;; Integration: kelly-calculate round-trip with odds conversions

(ert-deftest kelly-test-integration-decimal-odds ()
  "Full calculation using decimal odds of 4.0 and 60% win probability.
Decimal odds 4.0 = fractional odds 3.0.  f = ((3+1)*0.6 - 1)/3 = 7/15."
  (let ((kelly-fraction 1))
    (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 4.0)))
      (let ((b (kelly-read-decimal-odds)))
        (should (< (abs (- (kelly-calculate 0.6 b) (/ 7.0 15))) 1e-10))))))

(ert-deftest kelly-test-integration-moneyline-positive ()
  "Full calculation using +200 moneyline and 50% win probability."
  (let ((kelly-fraction 1))
    (cl-letf (((symbol-function 'read-number) (lambda (_prompt) 200)))
      (let ((b (kelly-read-moneyline-odds)))
        (should (< (abs (- (kelly-calculate 0.5 b) 0.25)) 1e-10))))))

(provide 'kelly-test)
;;; kelly-test.el ends here
