;;; ../.dotfiles/doom.d/packages/treesit.el -*- lexical-binding: t; -*-

(use-package! treesit
  :when (treesit-available-p)
  :init
  ;; Where grammars will be installed
  (setq treesit-extra-load-path (list (expand-file-name "tree-sitter/" doom-user-dir)))

  ;; Grammar source locations
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (toml "https://github.com/ikatyang/tree-sitter-toml")))

  ;; Auto-remap old major modes to tree-sitter modes
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (python-mode     . python-ts-mode)
          (js-mode         . js-ts-mode)
          (json-mode       . json-ts-mode)
          (css-mode        . css-ts-mode)
          (yaml-mode       . yaml-ts-mode)
          (toml-mode       . toml-ts-mode)
          (html-mode       . html-ts-mode)
          (bash-mode       . bash-ts-mode)))
  )
