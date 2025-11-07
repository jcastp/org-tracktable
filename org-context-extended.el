;;; org-context-extended.el --- Helper functions for detecting Org mode context

;; Author: jcastp
;; Created: 2025-11-06
;; Keywords: org, convenience
;; Package-Requires: ((emacs "24")(cl-lib "0.5"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides helper functions for detecting various Org mode
;; contexts at point, plus a flexible word counting function that uses
;; these helpers. These functions are useful for word counting, navigation,
;; and other operations that need to understand the context of the current
;; position in an Org mode buffer.
;;
;; Customizable Variables:
;; - org-context-ignore-tags: List of tags to ignore when counting words
;;
;; Helper Functions:
;; - org-context-in-commented-line: Check if point is in a commented line
;; - org-context-in-drawer-p: Check if point is in a drawer
;; - org-context-in-heading-p: Check if point is in a heading
;; - org-context-at-property-p: Check if point is at a property
;; - org-context-in-table-p: Check if point is in an Org mode table
;; - org-context-in-block-p: Check if point is inside a #+BEGIN...#+END block
;; - org-context-under-heading-with-tag-p: Check if point is under a heading with specific tags
;;
;; Main Function:
;; - org-context-count-words: Flexible word counting with configurable context filtering
;;
;; The org-context-count-words function accepts positional boolean arguments that
;; control which contexts to ignore when counting words. This allows precise
;; control over what gets counted. For example:
;;
;;   (org-context-count-words nil nil t t nil nil nil nil nil)
;;     ;; Ignores comments and drawers only
;;
;;   (org-context-count-words nil nil nil nil nil nil nil nil nil)
;;     ;; Counts everything (no filtering)
;;
;; When called interactively (M-x org-context-count-words), it applies sensible
;; defaults (ignoring comments, drawers, headings, properties, tables, blocks,
;; and content under 'noexport' tagged headings).

;;; Code:

(require 'org)
(require 'cl-lib)

(defcustom org-context-ignore-tags '("noexport")
  "A list of tags to ignore when counting words with `org-context-count-words'.
Content under headings with any of these tags will be excluded from word count.
Common values might include \"noexport\", \"ignore\", or \"nowc\"."
  :type '(repeat string)
  :group 'org)

(defun org-context-in-commented-line ()
  "Return non-nil if point is in a commented line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*#")))

(defun org-context-in-drawer-p ()
  "Return non-nil if point is in an Org mode drawer."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*:[a-zA-Z]+:")))

(defun org-context-in-heading-p ()
  "Return non-nil if point is in an Org mode heading."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^*")))

(defun org-context-at-property-p ()
  "Return non-nil if point is at an Org mode property."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*:[a-zA-Z0-9_-]+:")))

(defun org-context-in-table-p ()
  "Return non-nil if point is in an Org mode table."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*|")))

(defun org-context-in-block-p ()
  "Return non-nil if point is inside an Org mode #+BEGIN...#+END block.
Checks for any block type (SRC, EXAMPLE, QUOTE, EXPORT, etc.)."
  (save-excursion
    (let ((current-line (line-number-at-pos)))
      ;; Search backward for opening #+BEGIN_
      (when (re-search-backward "^[ \t]*#\\+BEGIN_" nil t)
        (let ((begin-line (line-number-at-pos)))
          ;; Search forward for closing #+END_
          (when (re-search-forward "^[ \t]*#\\+END_" nil t)
            (let ((end-line (line-number-at-pos)))
              ;; Return t if current line is between the markers
              (and (>= current-line begin-line)
                   (<= current-line end-line)))))))))

(defun org-context-under-heading-with-tag-p (tags-to-ignore)
  "Return non-nil if point is under a heading that has any tag from TAGS-TO-IGNORE.
TAGS-TO-IGNORE should be a list of strings like (\"noexport\" \"ignore\" \"nowc\").
Returns nil if point is before the first heading or not under a tagged heading."
  (when tags-to-ignore
    (save-excursion
      ;; Try to find the current heading, return nil if before first heading
      (condition-case nil
          (when (org-back-to-heading t)
            (let ((heading-tags (org-get-tags)))
              ;; Check if any of the heading tags match our ignore list
              (when heading-tags
                (cl-some (lambda (tag)
                           (member tag tags-to-ignore))
                         heading-tags))))
        ;; Catch the error when before first headline and return nil
        (error nil)))))

(defun org-context-count-words (&optional begin end
                                     ignore-comments-p
                                     ignore-drawers-p
                                     ignore-headings-p
                                     ignore-properties-p
                                     ignore-tables-p
                                     ignore-blocks-p
                                     tags-to-ignore)
  "Count words in the region if active, or the whole buffer if not.
Selectively ignores Org mode contexts based on boolean arguments.

BEGIN and END specify the region to count (defaults to region or whole buffer).

Boolean arguments control which contexts to ignore:
- IGNORE-COMMENTS-P: Ignore commented lines (lines starting with #).
- IGNORE-DRAWERS-P: Ignore drawer content.
- IGNORE-HEADINGS-P: Ignore heading lines (lines starting with *).
- IGNORE-PROPERTIES-P: Ignore property lines.
- IGNORE-TABLES-P: Ignore Org mode table lines (lines starting with |).
- IGNORE-BLOCKS-P: Ignore content inside #+BEGIN...#+END blocks.
- TAGS-TO-IGNORE: List of tag strings. Content under headings with these tags
  will be excluded (e.g., '(\"noexport\" \"ignore\" \"nowc\")).
  Defaults to the value of `org-context-ignore-tags'.

Each boolean argument defaults to nil (don't ignore). Set to t to ignore.

Examples:
  (org-context-count-words nil nil t t nil nil nil nil nil)
    ;; Ignore comments and drawers, count everything else

  (org-context-count-words nil nil t t t t t t '(\"noexport\" \"nowc\"))
    ;; Ignore all contexts plus content under tagged headings

  (org-context-count-words (point-min) (point-max) nil nil nil nil nil nil nil)
    ;; Count all words in buffer, ignoring nothing

When called interactively, counts region if active or whole buffer,
ignoring all contexts by default (comments, drawers, headings, properties,
tables, blocks, and content under tags specified in `org-context-ignore-tags')."
  (interactive)
  (let ((begin (or begin (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end) (point-max))))
        ;; When interactive, use sensible defaults
        (ignore-comments-p (if (called-interactively-p 'any) t ignore-comments-p))
        (ignore-drawers-p (if (called-interactively-p 'any) t ignore-drawers-p))
        (ignore-headings-p (if (called-interactively-p 'any) t ignore-headings-p))
        (ignore-properties-p (if (called-interactively-p 'any) t ignore-properties-p))
        (ignore-tables-p (if (called-interactively-p 'any) t ignore-tables-p))
        (ignore-blocks-p (if (called-interactively-p 'any) t ignore-blocks-p))
        (tags-to-ignore (if (called-interactively-p 'any)
                            org-context-ignore-tags
                          (or tags-to-ignore org-context-ignore-tags)))
        (word-count 0))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward "\\w+" end t)
        (unless (or (and ignore-comments-p (org-context-in-commented-line))
                    (and ignore-drawers-p (org-context-in-drawer-p))
                    (and ignore-headings-p (org-context-in-heading-p))
                    (and ignore-properties-p (org-context-at-property-p))
                    (and ignore-tables-p (org-context-in-table-p))
                    (and ignore-blocks-p (org-context-in-block-p))
                    (org-context-under-heading-with-tag-p tags-to-ignore))
          (setq word-count (1+ word-count)))))
    (when (called-interactively-p 'any)
      (message "Word count: %d" word-count))
    word-count))

(provide 'org-context)
;;; org-context-extended.el ends here
