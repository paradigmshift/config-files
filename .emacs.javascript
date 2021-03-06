(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)

;; syntax highlight amount
(setq js2-highlight-level 3)

;; initalize tern, auto-complete, and electric-pair
(add-hook 'js-mode-hook (lambda ()
                          (tern-mode t)
                          (electric-pair-mode 1)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
     (tern-ac-setup)))

;; fix Tern refresh bug
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;; nodejs path
(setenv "PATH" (concat "/home/mo/.nvm/v0.11.14/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/home/mo/.nvm/v0.11.14/bin")))

;; eval js
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c c") 'nodejs-repl-eval-dwim))

(defun nodejs-repl-eval-dwim ()
  "Heuristic evaluation of JS code in a NodeJS repl.

Evaluates the region, if active, or the first statement found at
or prior to the point.

If the point is at the end of a line, evaluation is done from one
character prior.  In many cases, this will be a semicolon and will
change what is evaluated to the statement on the current line."
  (interactive)
  (cond
   ((use-region-p) (nodejs-repl-eval-region (region-beginning) (region-end)))
   ((= (line-end-position) (point)) (nodejs-repl-eval-first-stmt (1- (point))))
   (t (nodejs-repl-eval-first-stmt (point)))))

(defun nodejs-repl-eval-function ()
  "Evaluate the current or previous function."
  (interactive)
  (let* ((fn-above-node (lambda (node)
                         (js2-mode-function-at-point (js2-node-abs-pos node))))
        (fn (funcall fn-above-node
             (nodejs-repl--find-current-or-prev-node
              (point) (lambda (node)
                        (not (null (funcall fn-above-node node))))))))
    (unless (null fn)
      (nodejs-repl-eval-node fn))))

(defun nodejs-repl-eval-first-stmt (pos)
"Evaluate the first statement found from `POS' by `js2-mode'.
 
If this statement is a block statement, its first parent
statement is found. This will be either a function declaration,
function call, or assignment statement."
(let ((node (js2-mode-find-first-stmt (nodejs-repl--find-current-or-prev-node pos))))
(cond
((js2-block-node-p node) (nodejs-repl-eval-node (js2-node-parent-stmt node)))
((not (null node)) (nodejs-repl-eval-node node)))))

(defun nodejs-repl--find-current-or-prev-node (pos &optional include-comments)
"Locate the first node before `POS'. Return a node or nil.
 
If `INCLUDE-COMMENTS' is set to t, then comments are considered
valid nodes. This is stupid, don't do it."
(let ((node (js2-node-at-point pos (not include-comments))))
(if (or (null node)
(js2-ast-root-p node))
(unless (= 0 pos)
(nodejs-repl--find-current-or-prev-node (1- pos) include-comments))
node)))

(defun nodejs-repl-eval-node (node)
"Evaluate `NODE', a `js2-mode' node."
(let ((beg (js2-node-abs-pos node))
(end (js2-node-abs-end node)))
(nodejs-repl-eval-region beg end)))

(defun nodejs-repl-eval-region (start end)
"Evaluate the region specified by `START' and `END'."
(let ((proc (get-process nodejs-repl-process-name)))
(comint-simple-send proc (buffer-substring-no-properties start end))))

