;;; leetcode.el --- Submit leetcode via emacs.

;; Author: twoyao <bisyao@gmail.com>
;; URL: https://github.com/twoyao/laes
;; Version: 0.1
;; Package-Required: (request json)
;; Keywords: leetcode

;;; Commentary:

;; Emacs is nice, so does leetcode. Let's combine them together.
;; Using emacs to solve and submit leetcode problem.

;; To setup this package, just add to you .emacs
;;
;;    (require 'leetcode)
;;    (setq lc-user-name-email "your-leetcode-name-or-email")
;;    (setq lc-user-password "your-leetcode-password")
;;    (setq lc-perfer-lang "python")
;;
;; If you want to show leetcode problem list, run `leetcode-list'.
;; It will pop a menu of leetcode problem list. Mouse-1 or RET on entry 
;; will generate a code buffer with initialized default code.
;;
;; Run `leetcode-submit' to submit your solution. It will guess source address
;; from comment which start with `Source:' and `Question:'. You should probally
;; not modify that.

;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'request)
(require 'json)

(defconst lc-site-prefix "https://leetcode.com")

(defvar lc-user-name-email)
(defvar lc-user-password)
(defvar lc-perfer-lang "c")

(defvar lc-lang-config)
(defvar lc-menu-list)
(defvar lc-source-name-table (make-hash-table :test 'equal))
(defvar lc-last-submission-id)

(defconst lc-status-code-map
  '((10 . "Accepted")
    (11 . "Wrong Answer")
    (12 . "Memory Limit Exceeded")
    (13 . "Output Limit Exceeded")
    (14 . "Time Limit Exceeded")
    (15 . "Runtime Error")
    (16 . "Internal Error")
    (20 . "Compile Error")
    (21 . "Unknown Error")
    (30 . "Timeout")))
	    
(setq request-backend 'url-retrieve)

(defun lc-request-header ()
  `(("Referer" . ,lc-site-prefix)
    ("Host" . "leetcode.com")
    ("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:35.0) Gecko/20100101 Firefox/35.0")
    ("X-CSRFToken" . ,(leetcode-cookie-val "csrftoken"))))

(defun lc-set-lang-config (lang)
  (setq lc-lang-config
	(nth 1 (assoc lang
		      '(("cpp" (:buffer-postfix ".cpp"
				:block-comment-start "/**"
				:block-comment-end "*/"))
			("java" (:buffer-postfix ".java"
 				 :block-comment-start "/**"
				 :block-comment-end "*/"))
			("python" (:buffer-postfix ".py"
				   :block-comment-start "'''"
				   :block-comment-end "'''"))
			("c" (:buffer-postfix ".c"
			      :block-comment-start "/**"
			      :block-comment-end "*/"))
			("csharp" (:buffer-postfix ".cs"
				   :block-comment-start "/**"
				   :block-comment-end "*/"))
			("javascript" (:buffer-postfix ".js"
				       :block-comment-start "/**"
				       :block-comment-end "*/"))
			("ruby" (:buffer-postfix ".rb"
				 :block-comment-start "=begin"
				 :block-comment-end "=end"))
			("swift" (:buffer-postfix ".swift"
				  :block-comment-start "/**"
				  :block-comment-end "*/")))))))

(defun xml-get-by-attr (node attr id)
  (when (listp node)
    (if (string= (xml-get-attribute node attr) id)
	node
      (let ((children (xml-node-children node)) found)
	(while (and children (not found))
	  (setq found (xml-get-by-attr (car children) attr id))
	  (setq children (cdr children)))
	found))))

(defun xml-get-children-chain (node chain)
  (setq chain (split-string chain))
  (while chain
    (let ((next (car chain)))
      (cond ((string-match "\\`[0-9]+\\'" next)
	     (setq node (nth (string-to-number next) (xml-node-children node))))
	    ((string= "~" next)
	     (setq node (nth 2 node)))
	    ((string-prefix-p "@" next)
	     (setq node (xml-get-attribute node (intern (substring next 1)) )))
	    ((string-prefix-p "#" next)
	     (setq node (xml-get-by-attr node 'id (substring next 1))))
	    (t
	     (setq node (car (xml-get-children node (intern next)))))))
    (setq chain (cdr chain)))
  node)

(defun safe-less (a b)
  (> (if a a 0) (if b b 0)))

(defun leetcode-menu--no-sort-p (sa sb)
  (setq sa (aref (nth 1 sa) 1)
	sb (aref (nth 1 sb) 1))
  (safe-less (string-to-number sa) (string-to-number sb)))

(defun leetcode-menu--accept-sort-p (sa sb)
  (setq sa (aref (nth 1 sa) 3)
	sb (aref (nth 1 sb) 3))
  (safe-less (string-to-number (substring sa 0 -1))
     (string-to-number (substring sb 0 -1))))

(defun leetcode-menu--status-sort-p (sa sb)
  (setq sa (aref (nth 1 sa) 0)
	sb (aref (nth 1 sb) 0))
  (let ((priority '(("notac" 1) ("ac" 2) ("None" 3) ("locked" 4))))
    (setq sa (nth 1 (assoc sa priority))
	  sb (nth 1 (assoc sb priority)))
    (safe-less sa sb)))

(defun leetcode-menu--difficulty-sort-p (sa sb)
  (setq sa (aref (nth 1 sa) 4)
	sb (aref (nth 1 sb) 4))
  (let ((priority '(("Easy" 1) ("Medium" 2) ("Hard" 3))))
    (setq sa (nth 1 (assoc sa priority))
	  sb (nth 1 (assoc sb priority)))
    (safe-less sa sb)))

(define-derived-mode leetcode-menu-mode tabulated-list-mode "Leetcode Problem Menu"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-format
	`[("Status" 6 leetcode-menu--status-sort-p)
	  ("No" 5 leetcode-menu--no-sort-p)
	  ("Title" 55 nil)
	  ("Acceptance" 10 leetcode-menu--accept-sort-p)
	  ("Difficulty" 0 leetcode-menu--difficulty-sort-p)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" "No"))
  (local-set-key (kbd "RET") 'leetcode-menu--entry-action)
  (tabulated-list-init-header))

(defun leetcode-menu--action (btn)
  (let ((locked (button-get btn 'locked))
	(href   (button-get btn 'href)))
    (if locked
	(browse-url (concat lc-site-prefix "/subscribe/"))
      (let* ((que-id (button-get btn 'que-id))
	     (name (button-label btn))
	     (buf-name (leetcode-get-buffer-name name))
	     (buf  (get-buffer buf-name))
	     (source (concat lc-site-prefix (button-get btn 'href))))
	(if buf (switch-to-buffer buf)
	  (puthash source (cons que-id name) lc-source-name-table)
	  (request source
	   :parser 'buffer-string :success 'lc-generate-problem))))))

(defun leetcode-menu--entry-action ()
  (interactive)
  (leetcode-menu--action (next-button (line-beginning-position))))

(defun leetcode-menu--parser ()
  (let*  ((problem-list
	   (xml-get-by-attr (libxml-parse-html-region (point-min) (point-max)) 'id "problemList"))
	  (tbody (xml-get-children-chain problem-list "tbody"))
	  (entry nil)
	  (entry-list nil))
    (dolist (tr (xml-node-children tbody))
      (let ((locked (if (xml-get-children-chain tr "4 i") t nil))
	    (status (xml-get-children-chain tr "0 span @class"))
	    (que-id (xml-get-children-chain tr "2 ~"))
	    (label (xml-get-children-chain tr "4 a ~"))
	    (href   (xml-get-children-chain tr "4 a @href"))
	    (acceptance (xml-get-children-chain tr "6 ~"))
	    (difficulty (xml-get-children-chain tr "12 ~")))
	(setq entry (vector (if locked "locked" status)
			    que-id
			    (cons label (list 'action 'leetcode-menu--action
					      'href href
					      'locked locked
					      'que-id que-id))
			    acceptance
			    difficulty)))
      (setq entry-list (cons (list (aref entry 1) entry) entry-list)))
    (setq lc-menu-list entry-list)))

(defun leetcode-get-buffer-name (name)
  (concat name (plist-get lc-lang-config :buffer-postfix)))

(defun leetcode-list ()
  (interactive)
  (if (leetcode-logged)
      (leetcode-fetch-menu)
    (leetcode-login)))

(defun leetcode-cookie-val (key)
  (cdr (assoc key (request-cookie-alist "leetcode.com" "/" t))))

(defun leetcode-logged ()
  (leetcode-cookie-val "PHPSESSID"))

(defun leetcode-fetch-menu (&rest _)
  (message "login success, fetching problem list...")
  (lc-set-lang-config lc-perfer-lang)
  (with-current-buffer (get-buffer-create "*Leetcode*")
    (leetcode-menu-mode)
    (tabulated-list-init-header)    
    (request (concat lc-site-prefix "/problemset/algorithms/")
	     :headers (lc-request-header)
	     :parser 'leetcode-menu--parser
	     :sync t)
    (setq tabulated-list-entries lc-menu-list)
    (tabulated-list-print nil)
    (switch-to-buffer (current-buffer))))

(defun leetcode-login ()
  (unless lc-user-name-email
    (setq lc-user-name-email (read-string "leetcode username or email: " user-mail-address)))
  (unless lc-user-password
    (setq lc-user-password (read-passwd "leetcode password: ")))

  (url-cookie-clean-up t)
  (request lc-site-prefix :sync t :headers (lc-request-header))
  (request (concat lc-site-prefix "/accounts/login/")
	   :type "POST"
	   :complete 'leetcode-fetch-menu
	   :headers (lc-request-header)
	   :data `(("login" . ,lc-user-name-email)
		   ("password" . ,lc-user-password)
		   ("csrfmiddlewaretoken" . ,(leetcode-cookie-val "csrftoken")))))
  
(defun leetcode-show (submission-result)
  (with-current-buffer (get-buffer-create "*LeetCode Submission*")
    (erase-buffer)
    (let* ((start nil)
	   (status_code (cdr (assoc 'status_code submission-result)))
	   (status (cdr (assoc status_code lc-status-code-map))))
      (insert-string (if status status "Invalid Error Code") "\n")
      (setq start (point))
      (insert (json-encode submission-result))
      (json-pretty-print start (point))
      (pop-to-buffer (current-buffer)))))
  
(defun leetcode-check-submit ()
  (when lc-last-submission-id
    (request (format "%s/submissions/detail/%d/check/" lc-site-prefix lc-last-submission-id)
	     :headers (lc-request-header)
	     :parser 'json-read
	     :complete 'leetcode-check-submit-callback)))

(cl-defun leetcode-check-submit-callback (&key data &allow-other-keys)
  (let ((state (cdr (assoc 'state data))))
    (if (or (string= state "STARTED") (string= state "PENDING"))
	(run-at-time "3 sec" nil 'leetcode-check-submit)
      (progn (setq lc-last-submission-id nil)
	     (leetcode-show data)))))

(defun leetcode-submit ()
  (interactive)
  (let* ((code (buffer-string))
	 (source nil)
	 (que-id nil))
    (string-match "\\(Source:\\s-*\\)\\(http[^ \t\n]*\\)" code)
    (setq source (match-string-no-properties 2 code))
    (string-match "\\(Question:\\s-*\\)\\([0-9]+\\)" code)
    (setq que-id (match-string-no-properties 2 code))
    (if (not (and source que-id))
	(message "Submit Fail: can not found source or question id")
      (request (concat source "submit/")
	       :type "POST"
	       :headers (lc-request-header)
	       :parser 'json-read
	       :data (json-encode
		      `(("data_input" . 2)
			("judge_type" . "large")
			("lang" . ,lc-perfer-lang)
			("question_id" . ,que-id)
			("typed_code" . ,code)))
	       :success
	       (function* (lambda (&key data &allow-other-keys)
			    (setq lc-last-submission-id (cdr (assoc 'submission_id data)))
			    (leetcode-check-submit)))))))

(defun unescape-html (str)
  (let ((unescape-map '(("\r" "") ("&gt;" ">") ("&le;" "<") ("&amp;" ";")
			("&nbsp;" " ") ("&quot;" "\""))))
    (dolist (m unescape-map str)
      (setq str (replace-regexp-in-string (nth 0 m) (nth 1 m) str)))))

(defun get-default-code-by-lang (config lang)
  (let (found (i 0) (config-len (length config)))
    (while (and (not found) (< 0 config-len))
      (print (gethash "value" (aref config i)))
      (when (equal (gethash "value" (aref config i)) lang)
	(setq found (gethash "defaultCode" (aref config i))))
      (setq i (1+ i)))
    found ))

(defun lc-set-major-mode (buf-name)
  (let ((alist auto-mode-alist) mode)
    (while (and alist (not mode))
      (when (string-match-p (car (car alist)) buf-name)
	(setq mode (cdr (car alist))))
      (setq alist (cdr alist)))
    (when mode (funcall mode))))

(cl-defun lc-generate-problem (&key data response &allow-other-keys)
  (let* ((source (request-response-url response))
	 (name (cdr (gethash source lc-source-name-table)))
	 (buffer-name (leetcode-get-buffer-name name))
	 (que-id (car (gethash source lc-source-name-table)))
	default-code
	description
	tree
	start
	end)
    (with-current-buffer (get-buffer-create buffer-name)
      (when (= (buffer-size) 0)
	(insert data)
	;; parse probolem description 
	(setq tree (libxml-parse-html-region (point-min) (point-max)))
	(setq tree (xml-get-by-attr tree 'name "description"))
	(setq description (xml-get-children-chain tree "@content"))
	(setq description (unescape-html description))
	;; parse default code 
	(goto-char (point-min))
	(setq start (re-search-forward "aceCtrl.init(\\s-*"))
	(forward-sexp)
	(setq end (point))
	(setq default-code (buffer-substring start end))
	(setq default-code (replace-regexp-in-string "'" "\"" default-code))
	(let ((json-object-type 'hash-table))
	  (setq default-code (get-default-code-by-lang
			      (json-read-from-string default-code)
			      lc-perfer-lang)))
	;; insert to buffer and indent
	(erase-buffer)
	(let ((comment-start (plist-get lc-lang-config :block-comment-start))
	      (comment-end   (plist-get lc-lang-config :block-comment-end  )))
	  (insert-string comment-start "\n"
			 "Author:\t" lc-user-name-email "\n"
			 "Date:\t" (current-time-string) "\n"
			 "Question:\t" que-id ". " name "\n"
			 "Source:\t" source "\n"
			 (make-string 30 ?-) "\n"
			 description "\n"
			 comment-end "\n\n"
			 default-code "\n")
	  (print (buffer-name))
	  (lc-set-major-mode (buffer-name))
	  (replace-regexp "\r" "" nil (point-min) (point-max))
	  (indent-region (point-min) (point-max))))
      (switch-to-buffer-other-window (current-buffer)))))
   
(provide 'leetcode)

;;; leetcode.el ends here
