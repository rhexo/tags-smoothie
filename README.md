# tags-smoothie
Self recursion tags generation agorithm based on exctags

# requirments
 - FreeBSD 10.2
 - projectile (see https://github.com/bbatsov/projectile.git)

# configuration
Need to be added in .emacs

```elisp
(setq projectile-tags-command "/usr/local/bin/exctags -Re -f \"%s\" %s %s")

(add-to-list 'load-path "/home/rhexo/.emacs.d/tags-smoothie")
(require 'tags-smoothie)

;; redefinition of projectile regeneration tags procedure
(defun projectile-regenerate-tags ()
  "Regenerate the project's [e|g]tags. Override function"
  (interactive)
  (let* ((project-root (projectile-project-root))
         (tags-exclude (projectile-tags-exclude-patterns))
         (default-directory project-root)
         (tags-file (expand-file-name projectile-tags-file-name))
         (tags-includes (tags-smoothie-get-files-stream project-root))
         (command (format projectile-tags-command tags-file tags-includes tags-exclude))
         shell-output exit-code)
    (with-temp-buffer
      (setq exit-code
            (call-process-shell-command command nil (current-buffer))
            shell-output (projectile-trim-string
                          (buffer-substring (point-min) (point-max)))))
    (unless (zerop exit-code)
      (error shell-output))
    (visit-tags-table tags-file)))
```


