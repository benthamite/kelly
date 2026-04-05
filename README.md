# `kelly`: Elisp Kelly criterion calculator

## Overview

The [Kelly criterion](https://en.wikipedia.org/wiki/Kelly_criterion) is a formula from probability theory that determines the optimal fraction of a bankroll to wager on a bet with positive expected value. Given the probability of winning and the odds offered, the Kelly formula maximizes the expected logarithm of wealth over repeated bets.

<img src="kelly.png" align="right" width="300">

<blockquote>When I deployed my card counting system for blackjack, it made intuitive sense to bet more when the expected gain, or edge, was greater. The question was, how much? The answer was in a 1956 article by Bell Labs physicist John L. Kelly, who some said was the smartest person there, after Claude Shannon.

— Ed Thorp, *A Man for All Markets*</blockquote>

The formula is:

$$f^* = p - \frac{1 - p}{b}$$

where $p$ is the probability of winning, $b$ is the net odds received on the bet (e.g. 2 for 2:1 fractional odds), and $f^*$ is the fraction of the bankroll to wager.

`kelly` brings this calculation into Emacs. The package provides a single interactive command, `kelly`, which prompts for the win probability and the betting odds, then displays the recommended wager amount along with the expected net profit. All computation is done in the minibuffer.

Five odds formats are supported: fractional (British), decimal (European), moneyline (American), implied probability, and implied percentage. The package also supports *fractional Kelly* betting, a widely used risk-reduction strategy in which only a fixed fraction of the full Kelly recommendation is wagered.

## Installation

`kelly` requires Emacs 25.1 or later and has no external dependencies.

### package-vc (built-in since Emacs 30)

```emacs-lisp
(use-package kelly
  :vc (:url "https://github.com/benthamite/kelly"))
```

### Elpaca

```emacs-lisp
(use-package kelly
  :ensure (:host github :repo "benthamite/kelly"))
```

### straight.el

```emacs-lisp
(use-package kelly
  :straight (:host github :repo "benthamite/kelly"))
```

## Quick start

```emacs-lisp
(use-package kelly
  :ensure (kelly :host github :repo "benthamite/kelly")
  :config
  ;; Optional: set your bankroll to skip the prompt
  (setopt kelly-bankroll 1000)
  ;; Optional: use half Kelly for a more conservative strategy
  (setopt kelly-fraction 0.5)
  ;; Optional: switch to decimal (European) odds
  (setopt kelly-b-parameter-format 'decimal-odds))
```

Run `M-x kelly`, enter your estimated win probability (e.g. `0.6`) and the betting odds (e.g. `2` for 2:1 fractional odds). The echo area will display the recommended wager and expected net profit.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](README.org).

## License

`kelly` is licensed under the GPL-3.0. See [COPYING.txt](COPYING.txt) for details.
