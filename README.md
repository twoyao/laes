# laes

**THIS PLUGIN WILL NOT WORK PROPERLY DUE TO LEETCODE API HAS CHANGED** 

**Solve [Leetcode](https://leetcode.com/) problem via emacs**

![screenshot](https://raw.githubusercontent.com/twoyao/laes/master/screenshot.png)

## Install & Setup

Just download and put`leetcode.el` to your `load-path` and add following to your .emacs
``` lisp
(require 'leetcode)
(setq lc-user-name-email "your-leetcode-name-or-email")
(setq lc-user-password "your-leetcode-password")
(setq lc-perfer-lang "python")
```
Currently, `lc-perfer-lang` can be one of following:

- cpp
- java
- python
- c
- csharp
- javascript
- ruby
- swift

## Usage

`M-x leetcode-list` display leetcode problem list.

`M-x leetcode-submit` submit current buffer as solution to leetcode.

## Dependencies
- [request](https://github.com/tkf/emacs-request)
