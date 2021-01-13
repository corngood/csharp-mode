;;; csharp-mode.el --- C# mode derived mode  -*- lexical-binding: t; -*-

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Jostein Kjønigsen <jostein@gmail.com>
;;            : Theodor Thornhill <theo@thornhill.no>
;; Created    : September 2020
;; Modified   : 2020
;; Version    : 0.11.0
;; Keywords   : c# languages oop mode
;; X-URL      : https://github.com/emacs-csharp/csharp-mode
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1") (tree-sitter-indent "0.1") (tree-sitter-langs "0.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:


(when (version< emacs-version "25.1")
  (require 'cl))
(require 'cc-mode)
(require 'cc-langs)

(eval-when-compile
  (require 'cc-fonts))

(require 'csharp-compilation)

(defgroup csharp nil
  "Major mode for editing C# code."
  :group 'prog-mode)

(eval-and-compile
  (defconst csharp--regex-identifier
    "[A-Za-z][A-Za-z0-9_]*"
    "Regex describing an dentifier in C#.")

  (defconst csharp--regex-identifier-matcher
    (concat "\\(" csharp--regex-identifier "\\)")
    "Regex matching an identifier in C#.")

  (defconst csharp--regex-type-name
    "[A-Z][A-Za-z0-9_]*"
    "Regex describing a type identifier in C#.")

  (defconst csharp--regex-type-name-matcher
    (concat "\\(" csharp--regex-type-name "\\)")
    "Regex matching a type identifier in C#.")

  (defconst csharp--regex-using-or-namespace
    (concat "^using" "\\|" "namespace"
            "\\s *"
            csharp--regex-type-name-matcher)
    "Regex matching identifiers after a using or namespace
    declaration."))

(eval-and-compile
  (c-add-language 'csharp-mode 'java-mode))

(c-lang-defconst c-make-mode-syntax-table
  csharp `(lambda ()
            (let ((table (make-syntax-table)))
              (c-populate-syntax-table table)
              (modify-syntax-entry ?@ "_" table)
              table)))

(c-lang-defconst c-identifier-syntax-modifications
  csharp (append '((?@ . "w"))
                 (c-lang-const c-identifier-syntax-modifications)))

(c-lang-defconst c-symbol-start
  csharp (concat "[" c-alpha "_@]"))

(c-lang-defconst c-opt-type-suffix-key
  csharp (concat "\\(\\[" (c-lang-const c-simple-ws) "*\\]\\|\\?\\)"))

(c-lang-defconst c-identifier-ops
  csharp '((left-assoc ".")))

(c-lang-defconst c-overloadable-operators
  csharp '("+" "-" "*" "/" "%" "&" "|" "^" "<<" ">>" "=="
           "!=" ">" "<" ">=" "<="))

(c-lang-defconst c-multiline-string-start-char
  csharp ?@)

(c-lang-defconst c-type-prefix-kwds
  csharp '("class" "interface" "struct"))

(c-lang-defconst c-class-decl-kwds
  csharp '("class" "interface" "struct"))

;;; Keyword lists

(c-lang-defconst c-primitive-type-kwds
  csharp '("bool" "byte" "sbyte" "char" "decimal" "double" "float" "int" "uint"
           "long" "ulong" "short" "ushort" "void" "object" "string" "var"))

(c-lang-defconst c-other-decl-kwds
  csharp nil)

(c-lang-defconst c-type-list-kwds
  csharp nil)

(c-lang-defconst c-other-block-decl-kwds
  csharp nil)

(c-lang-defconst c-return-kwds
  csharp '("return"))

(c-lang-defconst c-typedef-kwds
  csharp nil)

(c-lang-defconst c-typeof-kwds
  csharp '("typeof" "is" "as"))

(c-lang-defconst c-type-modifier-prefix-kwds
  csharp '("volatile"))

(c-lang-defconst c-type-modifier-kwds
  csharp '("readonly" "new"))

(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum" "new"))

(c-lang-defconst c-recognize-post-brace-list-type-p
  csharp t)

(c-lang-defconst c-ref-list-kwds
  csharp nil)

(c-lang-defconst c-using-kwds
  csharp '("using"))

(c-lang-defconst c-equals-type-clause-kwds
  csharp '("using"))

(defun csharp-at-vsemi-p (&optional pos)
  (if pos (goto-char pos))
  (save-excursion
    (beginning-of-line)
    (c-forward-syntactic-ws)
    (looking-at "using\\s *(")))

(c-lang-defconst c-at-vsemi-p-fn
  csharp 'csharp-at-vsemi-p)

(defun csharp-vsemi-status-unknown () t)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  csharp 'csharp-vsemi-status-unknown-p)


(c-lang-defconst c-modifier-kwds
  csharp '("abstract" "default" "final" "native" "private" "protected"
           "public" "partial" "internal" "readonly" "static" "event" "transient"
           "volatile" "sealed" "ref" "out" "virtual" "implicit" "explicit"
           "fixed" "override" "params" "async" "await" "extern" "unsafe"
           "get" "set" "this" "const" "delegate"))

(c-lang-defconst c-other-kwds
  csharp '("select" "from" "where" "join" "in" "on" "equals" "into"
           "orderby" "ascending" "descending" "group" "when"
           "let" "by" "namespace"))

(c-lang-defconst c-colon-type-list-kwds
  csharp '("class" "struct" "interface"))

(c-lang-defconst c-block-stmt-1-kwds
  csharp '("do" "else" "finally" "try"))

(c-lang-defconst c-block-stmt-1-2-kwds
  csharp '("try"))

(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach" "fixed" "checked"
           "unchecked" "using" "lock"))

(c-lang-defconst c-simple-stmt-kwds
  csharp '("break" "continue" "goto" "throw" "return" "yield"))

(c-lang-defconst c-constant-kwds
  csharp  '("true" "false" "null" "value"))

(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base" "operator"))

(c-lang-defconst c-inexpr-class-kwds
  csharp nil)

(c-lang-defconst c-class-decl-kwds
  csharp '("class" "struct" "interface"))

(c-lang-defconst c-std-abbrev-keywords
  csharp (append (c-lang-const c-std-abbrev-keywords) '("catch" "finally")))

(c-lang-defconst c-decl-prefix-re
  csharp "\\([{}(;,<]+\\)")

(c-lang-defconst c-recognize-typeless-decls
  csharp t)

(c-lang-defconst c-recognize-<>-arglists
  csharp t)

(c-lang-defconst c-opt-cpp-prefix
  csharp "\\s *#\\s *")

(c-lang-defconst c-opt-cpp-macro-define
  csharp (if (c-lang-const c-opt-cpp-prefix)
             "define"))

(c-lang-defconst c-cpp-message-directives
  csharp '("error" "warning" "region"))

(c-lang-defconst c-cpp-expr-directives
  csharp '("if" "elif"))

(c-lang-defconst c-other-op-syntax-tokens
  csharp  (append '("#")
                  (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-line-comment-starter
  csharp "//")

(c-lang-defconst c-doc-comment-start-regexp
  csharp "///")

(c-add-style "csharp"
             '("java"
               (c-basic-offset . 4)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . ((inline-open           . 0)
                                   (arglist-intro         . +)
                                   (arglist-close         . 0)
                                   (inexpr-class          . 0)
                                   (case-label            . +)
                                   (cpp-macro             . c-lineup-dont-change)
                                   (substatement-open     . 0)))))

(eval-and-compile
  (unless (or (stringp c-default-style)
              (assoc 'csharp-mode c-default-style))
    (setq c-default-style
          (cons '(csharp-mode . "csharp")
                c-default-style))))

(defun csharp--color-forwards (font-lock-face)
  (let (id-beginning)
    (goto-char (match-beginning 0))
    (forward-word)
    (while (and (not (or (eq (char-after) ?\;)
                         (eq (char-after) ?\{)))
                (progn
                  (forward-char)
                  (c-forward-syntactic-ws)
                  (setq id-beginning (point))
                  (> (skip-chars-forward
                      (c-lang-const c-symbol-chars))
                     0))
                (not (get-text-property (point) 'face)))
      (c-put-font-lock-face id-beginning (point) font-lock-face)
      (c-forward-syntactic-ws))))

(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;; Warning face on unclosed strings
           ,@(if (version< emacs-version "27.0")
                 ;; Taken from 26.1 branch
                 `(,(c-make-font-lock-search-function
                     (concat ".\\(" c-string-limit-regexp "\\)")
                     '((c-font-lock-invalid-string))))
               `(("\\s|" 0 font-lock-warning-face t nil)))

           ;; Invalid single quotes
           c-font-lock-invalid-single-quotes

           ;; Keyword constants
           ,@(when (c-lang-const c-constant-kwds)
               (let ((re (c-make-keywords-re nil (c-lang-const c-constant-kwds))))
                 `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                                 1 c-constant-face-name)))))

           ;; Keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
              1 font-lock-keyword-face)

           ;; Chained identifiers in using/namespace statements
           ,`(,(c-make-font-lock-search-function
                csharp--regex-using-or-namespace
                `((csharp--color-forwards font-lock-variable-name-face)
                  nil
                  (goto-char (match-end 0)))))


           ;; Negation character
           (eval . (list "\\(!\\)[^=]" 1 c-negation-char-face-name))

           ;; Types after 'new'
           (eval . (list (concat "\\<new\\> *" csharp--regex-type-name-matcher)
                         1 font-lock-type-face))

           ;; Single identifier in attribute
           (eval . (list (concat "\\[" csharp--regex-type-name-matcher "\\][^;]")
                         1 font-lock-variable-name-face t))

           ;; Function names
           (eval . (list "\\([A-Za-z0-9_]+\\)\\(<[a-zA-Z0-9, ]+>\\)?("
                         1 font-lock-function-name-face))

           ;; Nameof
           (eval . (list (concat "\\(\\<nameof\\>\\) *(")
                         1 font-lock-function-name-face))

           (eval . (list (concat "\\<nameof\\> *( *"
                                 csharp--regex-identifier-matcher
                                 " *) *")
                         1 font-lock-variable-name-face))

           ;; Catch statements with type only
           (eval . (list (concat "\\<catch\\> *( *"
                                 csharp--regex-type-name-matcher
                                 " *) *")
                         1 font-lock-type-face))
           ))

(c-lang-defconst c-basic-matchers-after
  csharp (append
          ;; Merge with cc-mode defaults - enables us to add more later
          (c-lang-const c-basic-matchers-after)))

(defcustom csharp-codedoc-tag-face 'c-doc-markup-face-name
  "Face to be used on the codedoc docstring tags.

Should be one of the font lock faces, such as
`font-lock-variable-name-face' and friends.

Needs to be set before `csharp-mode' is loaded, because of
compilation and evaluation time conflicts."
  :type 'symbol
  :group 'csharp)

(defcustom csharp-font-lock-extra-types
  (list csharp--regex-type-name)
  (c-make-font-lock-extra-types-blurb "C#" "csharp-mode" (concat))
  :type 'c-extra-types-widget
  :group 'c)

(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal font locking for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal font locking for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal font locking for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")

(defun csharp-font-lock-keywords-2 ()
  (c-compose-keywords-list csharp-font-lock-keywords-2))
(defun csharp-font-lock-keywords-3 ()
  (c-compose-keywords-list csharp-font-lock-keywords-3))
(defun csharp-font-lock-keywords ()
  (c-compose-keywords-list csharp-font-lock-keywords))

;;; Doc comments

(defconst codedoc-font-lock-doc-comments
  ;; Most of this is taken from the javadoc example, however, we don't use the
  ;; '@foo' syntax, so I removed that. Supports the XML tags only
  `((,(concat "</?\\sw"         ; XML tags.
              "\\("
              (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
                      "\"[^\"]*\"\\|'[^']*'")
              "\\)*/?>")
     0 ,csharp-codedoc-tag-face prepend nil)
    ;; ("\\([a-zA-Z0-9_]+\\)=" 0 font-lock-variable-name-face prepend nil)
    ;; ("\".*\"" 0 font-lock-string-face prepend nil)
    ("&\\(\\sw\\|[.:]\\)+;"     ; XML entities.
     0 ,csharp-codedoc-tag-face prepend nil)))

(defconst codedoc-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "///" limit
          codedoc-font-lock-doc-comments)))))

;;; End of doc comments

;;; Adding syntax constructs

(advice-add 'c-looking-at-inexpr-block
            :around 'csharp-looking-at-inexpr-block)

(defun csharp-looking-at-inexpr-block (orig-fun &rest args)
  (let ((res (csharp-at-lambda-header)))
    (if res
        res
      (apply orig-fun args))))

(defun csharp-at-lambda-header ()
  (save-excursion
    (c-backward-syntactic-ws)
    (unless (bobp)
      (backward-char)
      (c-safe (goto-char (scan-sexps (point) -1)))
      (when (or (looking-at "([[:alnum:][:space:]_,]*)[ \t\n]*=>[ \t\n]*{")
                (looking-at "[[:alnum:]_]+[ \t\n]*=>[ \t\n]*{"))
        ;; If we are at a C# lambda header
        (cons 'inexpr (point))))))

(advice-add 'c-guess-basic-syntax
            :around 'csharp-guess-basic-syntax)

(defun csharp-guess-basic-syntax (orig-fun &rest args)
  (cond
   (;; Attributes
    (save-excursion
      (goto-char (c-point 'iopl))
      (and
       (eq (char-after) ?\[)
       (save-excursion
         (c-go-list-forward)
         (and (eq (char-before) ?\])
              (not (eq (char-after) ?\;))))))
    `((annotation-top-cont ,(c-point 'iopl))))

   ((and
     ;; Heuristics to find object initializers
     (save-excursion
       ;; Next non-whitespace character should be '{'
       (goto-char (c-point 'boi))
       (eq (char-after) ?{))
     (save-excursion
       ;; 'new' should be part of the line
       (goto-char (c-point 'iopl))
       (looking-at ".*\\s *new\\s *.*"))
     ;; Line should not already be terminated
     (save-excursion
       (goto-char (c-point 'eopl))
       (or (not (eq (char-before) ?\;))
           (not (eq (char-before) ?\{)))))
    (if (save-excursion
          ;; if we have a hanging brace on line before
          (goto-char (c-point 'eopl))
          (eq (char-before) ?\{))
        `((brace-list-intro ,(c-point 'iopl)))
      `((block-open) (statement ,(c-point 'iopl)))))
   (t
    (apply orig-fun args))))

;;; End of new syntax constructs



;;; Fix for strings on version 27.1

(when (and
       (version<= "27.1" emacs-version)
       (version<= emacs-version "27.2"))
  ;; See:
  ;; https://github.com/emacs-csharp/csharp-mode/issues/175
  ;; https://github.com/emacs-csharp/csharp-mode/issues/151
  ;; for the full story.
  (defun c-pps-to-string-delim (end)
    (let* ((start (point))
           (no-st-s `(0 nil nil ?\" nil nil 0 nil ,start nil nil))
           (st-s `(0 nil nil t nil nil 0 nil ,start nil nil))
           no-st-pos st-pos
           )
      (parse-partial-sexp start end nil nil no-st-s 'syntax-table)
      (setq no-st-pos (point))
      (goto-char start)
      (while (progn
               (parse-partial-sexp (point) end nil nil st-s 'syntax-table)
               (unless (bobp)
                 (c-clear-syn-tab (1- (point))))
               (setq st-pos (point))
               (and (< (point) end)
                    (not (eq (char-before) ?\")))))
      (goto-char (min no-st-pos st-pos))
      nil))

  (defun c-multiline-string-check-final-quote ()
    (let (pos-ll pos-lt)
      (save-excursion
        (goto-char (point-max))
        (skip-chars-backward "^\"")
        (while
            (and
             (not (bobp))
             (cond
              ((progn
                 (setq pos-ll (c-literal-limits)
                       pos-lt (c-literal-type pos-ll))
                 (memq pos-lt '(c c++)))
               ;; In a comment.
               (goto-char (car pos-ll)))
              ((save-excursion
                 (backward-char)        ; over "
                 (c-is-escaped (point)))
               ;; At an escaped string.
               (backward-char)
               t)
              (t
               ;; At a significant "
               (c-clear-syn-tab (1- (point)))
               (setq pos-ll (c-literal-limits)
                     pos-lt (c-literal-type pos-ll))
               nil)))
          (skip-chars-backward "^\""))
        (cond
         ((bobp))
         ((eq pos-lt 'string)
          (c-put-syn-tab (1- (point)) '(15)))
         (t nil)))))

  (defun c-before-change-check-unbalanced-strings (beg end)
    ;; If BEG or END is inside an unbalanced string, remove the syntax-table
    ;; text property from respectively the start or end of the string.  Also
    ;; extend the region (c-new-BEG c-new-END) as necessary to cope with the
    ;; coming change involving the insertion or deletion of an odd number of
    ;; quotes.
    ;;
    ;; POINT is undefined both at entry to and exit from this function, the
    ;; buffer will have been widened, and match data will have been saved.
    ;;
    ;; This function is called exclusively as a before-change function via
    ;; `c-get-state-before-change-functions'.
    (c-save-buffer-state
        ((end-limits
          (progn
            (goto-char (if (c-multiline-string-start-is-being-detached end)
                           (1+ end)
                         end))
            (c-literal-limits)))
         (end-literal-type (and end-limits
                                (c-literal-type end-limits)))
         (beg-limits
          (progn
            (goto-char beg)
            (c-literal-limits)))
         (beg-literal-type (and beg-limits
                                (c-literal-type beg-limits))))

      ;; It is possible the buffer change will include inserting a string quote.
      ;; This could have the effect of flipping the meaning of any following
      ;; quotes up until the next unescaped EOL.  Also guard against the change
      ;; being the insertion of \ before an EOL, escaping it.
      (cond
       ((c-characterp c-multiline-string-start-char)
        ;; The text about to be inserted might contain a multiline string
        ;; opener.  Set c-new-END after anything which might be affected.
        ;; Go to the end of the putative multiline string.
        (goto-char end)
        (c-pps-to-string-delim (point-max))
        (when (< (point) (point-max))
          (while
              (and
               (progn
                 (while
                     (and
                      (c-syntactic-re-search-forward
                       (if c-single-quotes-quote-strings
                           "[\"']\\|\\s|"
                         "\"\\|\\s|")
                       (point-max) t t)
                      (progn
                        (c-clear-syn-tab (1- (point)))
                        (not (memq (char-before) c-string-delims)))))
                 (memq (char-before) c-string-delims))
               (if (eq (char-before (1- (point)))
                       c-multiline-string-start-char)
                   (progn
                     (c-pps-to-string-delim (point-max))
                     (< (point) (point-max)))
                 (c-pps-to-string-delim (c-point 'eoll))
                 (< (point) (c-point 'eoll))))))
        (setq c-new-END (max (point) c-new-END)))

       (c-multiline-string-start-char
        (setq c-bc-changed-stringiness
              (not (eq (eq end-literal-type 'string)
                       (eq beg-literal-type 'string))))
        ;; Deal with deletion of backslashes before "s.
        (goto-char end)
        (if (and (looking-at (if c-single-quotes-quote-strings
                                 "\\\\*[\"']"
                               "\\\\*\""))
                 (c-is-escaped (point)))
            (setq c-bc-changed-stringiness (not c-bc-changed-stringiness)))
        (if (eq beg-literal-type 'string)
            (setq c-new-BEG (min (car beg-limits) c-new-BEG))))

       ((< end (point-max))
        ;; Have we just escaped a newline by deleting characters?
        (if (and (eq end-literal-type 'string)
                 (memq (char-after end) '(?\n ?\r)))
            (cond
             ;; Are we escaping a newline by deleting stuff between \ and \n?
             ((and (> end beg)
                   (c-will-be-escaped end beg end))
              (c-remove-string-fences end)
              (goto-char (1+ end)))
             ;; Are we unescaping a newline by inserting stuff between \ and \n?
             ((and (eq end beg)
                   (c-is-escaped end))
              (goto-char (1+ end))) ; To after the NL which is being unescaped.
             (t
              (goto-char end)))
          (goto-char end))

        ;; Move to end of logical line (as it will be after the change, or as it
        ;; was before unescaping a NL.)
        (re-search-forward "\\(\\\\\\(.\\|\n\\)\\|[^\\\n\r]\\)*" nil t)
        ;; We're at an EOLL or point-max.
        (if (equal (c-get-char-property (point) 'syntax-table) '(15))
            (if (memq (char-after) '(?\n ?\r))
                ;; Normally terminated invalid string.
                (c-remove-string-fences)
              ;; Opening " at EOB.
              (c-clear-syn-tab (1- (point))))
          (when (and (c-search-backward-char-property 'syntax-table '(15) c-new-BEG)
                     (memq (char-after) c-string-delims)) ; Ignore an unterminated raw string's (.
            ;; Opening " on last line of text (without EOL).
            (c-remove-string-fences)
            (setq c-new-BEG (min c-new-BEG (point))))))

       (t (goto-char end)			; point-max
          (when
              (and
               (c-search-backward-char-property 'syntax-table '(15) c-new-BEG)
               (memq (char-after) c-string-delims))
            (c-remove-string-fences))))

      (unless
          (or (and
               ;; Don't set c-new-BEG/END if we're in a raw string.
               (eq beg-literal-type 'string)
               (c-at-c++-raw-string-opener (car beg-limits)))
              (and c-multiline-string-start-char
                   (not (c-characterp c-multiline-string-start-char))))
        (when (and (eq end-literal-type 'string)
                   (not (eq (char-before (cdr end-limits)) ?\())
                   (memq (char-after (car end-limits)) c-string-delims)
                   (equal (c-get-char-property (car end-limits) 'syntax-table)
                          '(15)))
          (c-remove-string-fences (car end-limits))
          (setq c-new-END (max c-new-END (cdr end-limits))))

        (when (and (eq beg-literal-type 'string)
                   (memq (char-after (car beg-limits)) c-string-delims))
          (c-remove-string-fences (car beg-limits))
          (setq c-new-BEG (min c-new-BEG (car beg-limits))))))))

;;; End of fix for strings on version 27.1



(defvar csharp-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table csharp))
  "Syntax table used in csharp-mode buffers.")

(defvar csharp-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in csharp-mode buffers.")

(easy-menu-define csharp-mode-menu csharp-mode-map "C# Mode Commands"
  (cons "C#" (c-lang-const c-mode-menu csharp)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;; Custom variables
;;;###autoload
(defcustom csharp-mode-hook nil
  "*Hook called by `csharp-mode'."
  :type 'hook
  :group 'csharp)

;;;###autoload
(define-derived-mode csharp-mode prog-mode "C#"
  "Major mode for editing Csharp code.

Key bindings:
\\{csharp-mode-map}"
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (c-init-language-vars csharp-mode)
  (c-common-init 'csharp-mode)
  (easy-menu-add csharp-mode-menu)
  (setq-local c-doc-comment-style '((csharp-mode . codedoc)))
  (c-run-mode-hooks 'c-mode-common-hook 'csharp-mode-hook))

(provide 'csharp-mode)

;;; csharp-mode.el ends here
